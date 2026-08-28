package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.PosInt

/** Where the user interface lives on the terminal (design doc 6.3 and 7.2, decision D12). `AlternateScreen` is the full screen, cleared
  * on entry and restored on exit (M1e). `Inline` is the last `height` rows of the normal screen with the terminal's history preserved
  * above it, anchored at the bottom only (a top anchor loses history), supported from M1f and rejected by the M1e orchestration with
  * [[TerminalError.UnsupportedScreenMode]]. An inline UI of zero rows does not exist, so `ScreenMode.Inline(PosInt(0))` does not compile.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum ScreenMode derives Eq, Show, Hash {
  case AlternateScreen
  case Inline(height: PosInt)
}

object ScreenMode {

  /** An [[Inline]] mode from an already-refined height. */
  def inlineOf(height: PosInt): ScreenMode = Inline(height)

  /** An [[Inline]] mode from a runtime height, `Left` with refined4s's message when it is not positive. */
  def inlineFrom(height: Int): Either[String, ScreenMode] = PosInt.from(height).map(Inline(_))

}
