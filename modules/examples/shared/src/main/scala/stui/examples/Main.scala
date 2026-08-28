package stui.examples

import cats.syntax.all.*
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.terminal.PlatformTerminal

/** Runs the M1e demo on the alternate screen with mouse capture, bracketed paste, and focus events (JVM and Native).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Main {

  def main(args: Array[String]): Unit = {
    val options = TerminalOptions.of(
      ScreenMode.AlternateScreen,
      TerminalFeature.MouseCapture,
      TerminalFeature.BracketedPaste,
      TerminalFeature.FocusEvents,
    )
    PlatformTerminal.run(options)(session => Demo.loop(session, Demo.State.initial)) match {
      case Left(error) =>
        System.err.println(s"stui demo: ${error.show}")
        sys.exit(2)
      case Right(reason) => println(s"stui demo exited: $reason")
    }
  }

}
