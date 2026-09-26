package stui.terminal

import cats.syntax.all.*
import stui.core.capability.{Capabilities, CapabilitiesPatch}
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.terminal.internal.{NativeInput, NativeSignals, NativeTty}
import stui.terminal.kitty.KittyKeyboard
import stui.terminal.probe.ProbePolicy

import java.util.concurrent.atomic.AtomicReference

/** The Native entry point (design doc 7, 7.3, and 7.4): capabilities from the environment, raw mode, the startup probe (D15: one
  * round trip ended by the DA1 sentinel, fail-open), the three-layer capabilities merge, the posixlib device, the shared ANSI backend
  * anchored at the probed cursor row, the signal handlers and the `atexit` hook (installed before the entry with the backend's exit
  * as the closer, attached to the terminal once it exists, M3d), the decoding event source continuing from the probe's decoder state
  * with the pushed kitty flags and the stall-aware ESC timeout, and after the cleanup the late-signal exit with 128 plus the signal
  * number when a termination signal was recorded (the POSIX shell convention).
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
    * enters the terminal, runs `use` with a session, restores on every path, and exits with 128 plus the signal number when one was
    * recorded.
    */
  def runFully[A](env: Map[String, String], overrides: CapabilitiesPatch, options: TerminalOptions)(
    use: TerminalSession => A
  ): Either[TerminalError, A] = {
    val envCaps = Capabilities.fromEnv(env)
    val tty     = NativeTty()
    if (!tty.isTerminal) {
      (TerminalError.NotATerminal: TerminalError).asLeft[A]
    } else {
      tty.enterRawMode().flatMap { _ =>
        try {
          val probe        = Probe.run(tty, NativeInput.input, options, envCaps, Clock.system)
          val capabilities = Capabilities.merge(envCaps, ProbePolicy.patch(envCaps, probe), overrides)
          val backend      = AnsiBackend.withEntryRow(tty, capabilities, probe.cursorRow)
          val keyboard     = KittyKeyboard.flagsFor(options, capabilities)
          val ref          = new AtomicReference(none[Terminal])
          NativeSignals.install(() => backend.exit())
          val result       = Terminal.run(backend, options, capabilities, Clock.system) { terminal =>
            NativeSignals.attach(terminal.latch, () => terminal.close())
            ref.set(terminal.some)
            val events = new DecodingEventSource(
              NativeInput.input,
              NativeSignals.resized,
              () => tty.size(),
              EffectiveSize.of(options),
              KittyKeyboard.escTimeout(options.escTimeout.resolve(capabilities.ssh), keyboard),
              Clock.system,
              probe.decoder.withKeyboard(keyboard),
              probe.events,
            )
            use(new TerminalSession(terminal, events, capabilities, () => NativeSignals.terminating.get() > 0))
          }
          ref.get().flatMap(_.latch.recorded).orElse(NativeSignals.terminationSignal) match {
            case Some(signal) => sys.exit(128 + signal)
            case None => result
          }
        } finally tty.restoreMode()
      }
    }
  }

}
