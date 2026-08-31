package stui.examples

import cats.syntax.all.*
import refined4s.types.numeric.PosInt
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.examples.Demo.Outcome
import stui.terminal.{PlatformTerminal, TerminalSession}
import stui.terminal.TerminalSession.*

import scala.annotation.tailrec
import scala.concurrent.duration.*

/** Runs the demo on the alternate screen, or with the argument `inline` as a twelve-row inline UI on the normal screen (design doc
  * 7.2), with mouse capture, bracketed paste, and focus events (JVM and Native). `p` and `P` print above the inline UI, and under the
  * alternate screen the prints flush as a transcript at exit. The driver is the blocking poll loop over the shared [[Demo.drawFrame]]
  * and [[Demo.applyEvent]] (the Node `Main` subscribes instead, design doc 6.3).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Main {

  def main(args: Array[String]): Unit = {
    val inline     = args.headOption.exists(_.trim.equalsIgnoreCase("inline"))
    val screenMode = if (inline) ScreenMode.inlineOf(PosInt(12)) else ScreenMode.AlternateScreen
    val options    = TerminalOptions.of(
      screenMode,
      TerminalFeature.MouseCapture,
      TerminalFeature.BracketedPaste,
      TerminalFeature.FocusEvents,
    )
    PlatformTerminal.run(options)(session => loop(session, Demo.State.initial)) match {
      case Left(error) =>
        System.err.println(s"stui demo: ${error.show}")
        sys.exit(2)
      case Right(reason) => println(s"stui demo exited: $reason")
    }
  }

  /** Draws, waits for an event, and loops until `q`, Control-C, or a termination signal. Returns why it stopped. */
  @tailrec
  def loop(session: TerminalSession, state: Demo.State): String = {
    val (next, completed) = Demo.drawFrame(session.terminal, state)
    session.events.poll(100.millis) match {
      case Some(event) =>
        Demo.applyEvent(session.terminal, next, event, completed) match {
          case Outcome.Quit(reason) => reason
          case Outcome.Continue(stepped) => loop(session, stepped)
        }
      case None => if (session.terminationRequested) "signal" else loop(session, next)
    }
  }

}
