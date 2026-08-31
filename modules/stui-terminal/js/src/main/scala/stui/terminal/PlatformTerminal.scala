package stui.terminal

import cats.syntax.all.*
import stui.core.capability.{Capabilities, CapabilitiesPatch}
import stui.core.event.Event
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.terminal.decoder.{Decoded, Decoder, DecoderInput, DecoderState, Reply}
import stui.terminal.internal.{NodeBytes, NodeProcess, NodeTty}
import stui.terminal.probe.{ProbePolicy, ProbeQueries, ProbeResult}
import stui.unicode.WidthPolicy

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.scalajs.js
import scala.scalajs.js.timers

/** The Node entry point (design doc 7, 7.3, and 7.4): capabilities from the environment, raw mode, the same one-round-trip fail-open
  * probe as the other platforms run asynchronously (D15: the DA1 sentinel or a timer deadline ends it), the three-layer capabilities
  * merge, the shared ANSI backend anchored at the probed cursor row, and the push event source continuing from the probe's decoder
  * state. One `'data'` handler is installed for the process lifetime and staged from the probe to the event source (Node is
  * single-threaded, so nothing falls between). Restore is by construction: the `'exit'` hook closes the terminal (an uncaught
  * exception runs it and the trace lands after the restore, M0-verified), and the mandatory SIGTERM and SIGINT handlers close and
  * exit with 128 plus the signal number (143 and 130, the Native convention), because the exit event does not fire on unhandled
  * signals. Both screen modes are supported. Because nothing may block on Node, `use` is a continuation receiving the session or the
  * error, and the application ends the session with `TerminalSession.close()`.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object PlatformTerminal {

  /** [[runFully]] over the process environment with no overrides. */
  def run(options: TerminalOptions)(use: Either[TerminalError, TerminalSession] => Unit): Unit =
    runWith(NodeProcess.env.toMap, options)(use)

  /** [[runFully]] with no overrides. */
  def runWith(env: Map[String, String], options: TerminalOptions)(use: Either[TerminalError, TerminalSession] => Unit): Unit =
    runFully(env, CapabilitiesPatch.empty, options)(use)

  /** Enters raw mode, probes asynchronously, merges the capabilities (environment, then probes, then the application overrides,
    * design doc 7.3), enters the terminal, and hands the session to `use`, with the exit hook and the signal handlers restoring on
    * every path the application does not close itself.
    */
  def runFully(env: Map[String, String], overrides: CapabilitiesPatch, options: TerminalOptions)(
    use: Either[TerminalError, TerminalSession] => Unit
  ): Unit = {
    val envCaps = Capabilities.fromEnv(env)
    val tty     = NodeTty()
    if (!tty.isTerminal) {
      use((TerminalError.NotATerminal: TerminalError).asLeft[TerminalSession])
    } else {
      tty.enterRawMode() match {
        case Left(error) => use(error.asLeft[TerminalSession])
        case Right(_) =>
          /* the one data handler for the process lifetime, staged from the probe to the event source */
          val consumer = new AtomicReference[IArray[Byte] => Unit](_ => ())
          NodeProcess.stdin.on("data", data => consumer.get()(NodeBytes.toIArray(data))): Unit
          NodeProcess.stdin.resume(): Unit
          probeThen(tty, options, envCaps, consumer)(probe => finish(tty, options, envCaps, overrides, consumer, probe, use))
      }
    }
  }

  /** The asynchronous probe (design doc 7.3, decision D15): the query batch is written, the staged consumer decodes the answers with
    * `expectingReplies` set, and the DA1 sentinel or the deadline timer completes it, failing open with whatever arrived.
    */
  private def probeThen(
    tty: NodeTty,
    options: TerminalOptions,
    envCaps: Capabilities,
    consumer: AtomicReference[IArray[Byte] => Unit],
  )(onComplete: ProbeResult => Unit): Unit =
    options.probing.resolve(envCaps.ssh) match {
      case None => onComplete(ProbeResult.empty)
      case Some(timeout) =>
        tty.write(ProbeQueries.batch.getBytes(StandardCharsets.UTF_8))
        val state    = new AtomicReference(DecoderState.initial.expecting(true))
        val replies  = new AtomicReference(Vector.empty[Reply])
        val events   = new AtomicReference(Vector.empty[Event])
        val done     = new AtomicBoolean(false)
        val deadline = timers.setTimeout(timeout) {
          if (done.compareAndSet(false, true)) {
            onComplete(ProbeResult(replies.get(), events.get(), state.get().expecting(false), false))
          } else {
            ()
          }
        }
        consumer.set { chunk =>
          Decoder.step(state.get(), DecoderInput.bytes(chunk)) match {
            case Decoded(next, produced, answered) =>
              state.set(next)
              replies.updateAndGet(_ ++ answered): Unit
              events.updateAndGet(_ ++ produced): Unit
              if (ProbeResult.sentinelReached(replies.get()) && done.compareAndSet(false, true)) {
                timers.clearTimeout(deadline)
                onComplete(ProbeResult(replies.get(), events.get(), next.expecting(false), true))
              } else {
                ()
              }
          }
        }
    }

  private def finish(
    tty: NodeTty,
    options: TerminalOptions,
    envCaps: Capabilities,
    overrides: CapabilitiesPatch,
    consumer: AtomicReference[IArray[Byte] => Unit],
    probe: ProbeResult,
    use: Either[TerminalError, TerminalSession] => Unit,
  ): Unit = {
    val capabilities = Capabilities.merge(envCaps, ProbePolicy.patch(envCaps, probe), overrides)
    val backend      = AnsiBackend.withEntryRow(tty, capabilities, probe.cursorRow)
    Terminal.open(WidthPolicy.default, backend, options, capabilities, Clock.system) match {
      case Left(error) =>
        tty.restoreMode()
        NodeProcess.stdin.pause(): Unit
        use(error.asLeft[TerminalSession])
      case Right(terminal) =>
        val events  = new PushEventSource(
          EffectiveSize.of(options),
          options.escTimeout.resolve(capabilities.ssh),
          (delay, body) => {
            val handle = timers.setTimeout(delay)(body())
            () => timers.clearTimeout(handle)
          },
          probe.decoder,
          probe.events,
          tty.size(),
        )
        consumer.set(events.onChunk)
        NodeProcess.stdout.on("resize", () => events.onResize(tty.size())): Unit
        NodeProcess.on("exit", (_: js.Any) => terminal.close()): Unit
        NodeProcess.on(
          "SIGTERM",
          (_: js.Any) => {
            terminal.close()
            NodeProcess.exit(143)
          },
        ): Unit
        NodeProcess.on(
          "SIGINT",
          (_: js.Any) => {
            terminal.close()
            NodeProcess.exit(130)
          },
        ): Unit
        val session = new TerminalSession(
          terminal,
          events,
          capabilities,
          () => {
            events.shutdown()
            terminal.close()
            NodeProcess.stdin.pause(): Unit
          },
        )
        use(session.asRight[TerminalError])
    }
  }

}
