package stui.terminal

import cats.syntax.all.*
import stui.core.capability.Capabilities
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.terminal.internal.{JvmSignals, JvmTty, StdinReader}

import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

/** The JVM entry point (design doc 7 and 7.4): capabilities from the environment, the JNA device, the shared ANSI backend, the WINCH
  * handler, a shutdown hook that closes the terminal under its lock (the M0-verified SIGTERM path, the JVM exits 143 on its own), and
  * the decoding event source over the standard input reader.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object PlatformTerminal {

  /** [[runWith]] over the process environment. */
  def run[A](options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] = runWith(sys.env, options)(use)

  /** Enters the terminal, runs `use` with a session, and restores on every path. */
  def runWith[A](env: Map[String, String], options: TerminalOptions)(use: TerminalSession => A): Either[TerminalError, A] = {
    val capabilities = Capabilities.fromEnv(env)
    val tty          = JvmTty()
    val backend      = AnsiBackend(tty, capabilities)
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
          options.escTimeout.resolve(capabilities.ssh),
          Clock.system,
        )
        use(new TerminalSession(terminal, events, capabilities, () => false))
      }
    finally Try(Runtime.getRuntime.removeShutdownHook(hook)): Unit
  }

}
