package stui.terminal

import cats.syntax.all.*
import stui.core.capability.Capabilities
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.terminal.internal.{NativeInput, NativeSignals, NativeTty}

import java.util.concurrent.atomic.AtomicReference

/** The Native entry point (design doc 7 and 7.4): capabilities from the environment, the posixlib device, the shared ANSI backend, the
  * signal handlers and the `atexit` hook, the decoding event source over `poll`, and after the cleanup the late-signal exit with 128
  * plus the signal number when a termination signal was recorded (the POSIX shell convention).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object PlatformTerminal {

  /** [[runWith]] over the process environment. */
  def run[A](options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] = runWith(sys.env, options)(use)

  /** Enters the terminal, runs `use` with a session, restores on every path, and exits with 128 plus the signal number when one was
    * recorded.
    */
  def runWith[A](env: Map[String, String], options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] = {
    val capabilities = Capabilities.fromEnv(env)
    val tty          = NativeTty()
    val backend      = AnsiBackend(tty, capabilities)
    val ref          = new AtomicReference(none[Terminal])
    val result       = Terminal.run(backend, options, capabilities, Clock.system) { terminal =>
      ref.set(terminal.some)
      NativeSignals.install(terminal.latch, () => terminal.close())
      val events = new DecodingEventSource(
        NativeInput.input,
        NativeSignals.resized,
        () => tty.size(),
        options.escTimeout.resolve(capabilities.ssh),
        Clock.system,
      )
      use(new TerminalSession(terminal, events, capabilities, () => NativeSignals.terminating.get() > 0))
    }
    ref.get().flatMap(_.latch.recorded) match {
      case Some(signal) => sys.exit(128 + signal)
      case None => result
    }
  }

}
