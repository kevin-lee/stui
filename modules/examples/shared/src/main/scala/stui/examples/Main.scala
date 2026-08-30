package stui.examples

import cats.syntax.all.*
import refined4s.types.numeric.PosInt
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.terminal.PlatformTerminal

/** Runs the demo on the alternate screen, or with the argument `inline` as an eight-row inline UI on the normal screen (design doc
  * 7.2), with mouse capture, bracketed paste, and focus events (JVM and Native). `p` and `P` print above the inline UI, and under the
  * alternate screen the prints flush as a transcript at exit.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Main {

  def main(args: Array[String]): Unit = {
    val inline     = args.headOption.exists(_.trim.equalsIgnoreCase("inline"))
    val screenMode = if (inline) ScreenMode.inlineOf(PosInt(8)) else ScreenMode.AlternateScreen
    val options    = TerminalOptions.of(
      screenMode,
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
