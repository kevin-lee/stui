package stui.examples

import cats.syntax.all.*
import refined4s.types.numeric.PosInt
import stui.app.Stui
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}

/** The one entry of the demo, compiled to the JVM, Native, and Node (design doc 10, M3b): the alternate screen, or with the argument
  * `inline` a twelve-row inline UI on the normal screen (design doc 7.2), with mouse capture, bracketed paste, and focus events. No
  * terminal type is named here: [[Platform]] reads the argument and exits with a code, and `Stui.run` owns the loop. Exit codes: 0
  * for `q` and the Control-C byte, 1 for an uncaught exception (the `!` crash, the trace landing after the restore), 2 when standard
  * input is not a terminal, and 128 plus the signal number for SIGTERM and SIGINT.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Main {

  def main(args: Array[String]): Unit = {
    val inline     = Platform.inline(args)
    val screenMode = if (inline) ScreenMode.inlineOf(PosInt(12)) else ScreenMode.AlternateScreen
    val options    = TerminalOptions.of(
      screenMode,
      TerminalFeature.MouseCapture,
      TerminalFeature.BracketedPaste,
      TerminalFeature.FocusEvents,
    )
    Stui.run(Demo.app, options) {
      case Left(error) =>
        System.err.println(s"stui demo: ${error.show}")
        Platform.exit(2)
      case Right(state) => println(s"stui demo exited: ${state.exit.getOrElse("-")}")
    }
  }

}
