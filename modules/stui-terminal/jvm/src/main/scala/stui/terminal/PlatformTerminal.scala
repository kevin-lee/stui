package stui.terminal

import cats.syntax.all.*
import stui.core.capability.{Capabilities, CapabilitiesPatch}
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.terminal.internal.{JvmSignals, JvmTty, StdinReader}
import stui.terminal.probe.ProbePolicy

import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

/** The JVM entry point (design doc 7, 7.3, and 7.4): capabilities from the environment, raw mode, the startup probe (D15: one round
  * trip ended by the DA1 sentinel, fail-open), the three-layer capabilities merge, the JNA device, the shared ANSI backend anchored at
  * the probed cursor row, the WINCH handler, a shutdown hook that closes the terminal under its lock (the M0-verified SIGTERM path,
  * the JVM exits 143 on its own), and the decoding event source continuing from the probe's decoder state.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object PlatformTerminal {

  /** [[runFully]] over the process environment with no overrides. */
  def run[A](options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] = runWith(sys.env, options)(use)

  /** [[runFully]] with no overrides. */
  def runWith[A](env: Map[String, String], options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] =
    runFully(env, CapabilitiesPatch.empty, options)(use)

  /** Enters raw mode, probes, merges the capabilities (environment, then probes, then the application overrides, design doc 7.3),
    * enters the terminal, runs `use` with a session, and restores on every path.
    */
  def runFully[A](env: Map[String, String], overrides: CapabilitiesPatch, options: TerminalOptions)(
    use: TerminalSession => A
  ): Either[TerminalError, A] = {
    val envCaps = Capabilities.fromEnv(env)
    val tty     = JvmTty()
    if (!tty.isTerminal) {
      (TerminalError.NotATerminal: TerminalError).asLeft[A]
    } else {
      tty.enterRawMode().flatMap { _ =>
        try {
          val probe        = Probe.run(tty, StdinReader.input, options, envCaps, Clock.system)
          val capabilities = Capabilities.merge(envCaps, ProbePolicy.patch(envCaps, probe), overrides)
          val backend      = AnsiBackend.withEntryRow(tty, capabilities, probe.cursorRow)
          val ref          = new AtomicReference(none[Terminal])
          val hook         = new Thread(() => ref.get().foreach(_.close()), "stui-restore")
          JvmSignals.installWinch()
          Runtime.getRuntime.addShutdownHook(hook)
          try
            Terminal.run(backend, options, capabilities, Clock.system) { terminal =>
              ref.set(terminal.some)
              val events = new DecodingEventSource(
                StdinReader.input,
                JvmSignals.resized,
                () => tty.size(),
                EffectiveSize.of(options),
                options.escTimeout.resolve(capabilities.ssh),
                Clock.system,
                probe.decoder,
                probe.events,
              )
              use(new TerminalSession(terminal, events, capabilities, () => false))
            }
          finally Try(Runtime.getRuntime.removeShutdownHook(hook)): Unit
        } finally tty.restoreMode()
      }
    }
  }

}
