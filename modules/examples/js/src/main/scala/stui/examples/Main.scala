package stui.examples

import cats.syntax.all.*
import refined4s.types.numeric.PosInt
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.examples.Demo.Outcome
import stui.terminal.PlatformTerminal
import stui.terminal.TerminalSession.*
import stui.terminal.internal.NodeProcess

import java.util.concurrent.atomic.AtomicReference

/** Runs the demo on Node: the alternate screen, or with the argument `inline` (read from `process.argv`, the main-module initializer
  * passes no arguments) a twelve-row inline UI on the normal screen, with mouse capture, bracketed paste, and focus events. The
  * driver subscribes to the push event source and re-renders after every event over the shared [[Demo.drawFrame]] and
  * [[Demo.applyEvent]] (nothing may block on JS, design doc 6.3). Exit codes: 0 for `q` and the Control-C byte, 1 for an uncaught
  * exception (the `!` crash, the trace landing after the restore), 2 when standard input is not a terminal, 130 for SIGINT, and 143
  * for SIGTERM.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object Main {

  def main(args: Array[String]): Unit = {
    val inline     = NodeProcess.argv.toList.drop(2).headOption.exists(_.trim.equalsIgnoreCase("inline"))
    val screenMode = if (inline) ScreenMode.inlineOf(PosInt(12)) else ScreenMode.AlternateScreen
    val options    = TerminalOptions.of(
      screenMode,
      TerminalFeature.MouseCapture,
      TerminalFeature.BracketedPaste,
      TerminalFeature.FocusEvents,
    )
    PlatformTerminal.run(options) {
      case Left(error) =>
        System.err.println(s"stui demo: ${error.show}")
        NodeProcess.exit(2)
      case Right(session) =>
        val (first, firstFrame) = Demo.drawFrame(session.terminal, Demo.State.initial)
        val state               = new AtomicReference(first)
        val completed           = new AtomicReference(firstFrame)
        session
          .events
          .subscribe { event =>
            Demo.applyEvent(session.terminal, state.get(), event, completed.get()) match {
              case Outcome.Quit(reason) =>
                session.close()
                println(s"stui demo exited: $reason")
                NodeProcess.exit(0)
              case Outcome.Continue(stepped) =>
                val (next, frame) = Demo.drawFrame(session.terminal, stepped)
                state.set(next)
                completed.set(frame)
            }
          }: Unit
    }
  }

}
