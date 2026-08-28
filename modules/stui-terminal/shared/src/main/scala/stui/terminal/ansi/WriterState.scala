package stui.terminal.ansi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.style.CellStyle

/** The explicit state of the writer's cursor model (design doc 7.1, decision D16): the tracked cursor, the style the terminal currently
  * has, the screen row where the viewport starts (zero on the alternate screen, the inline mode of M1f sets it), and the active scroll
  * region (M1f).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class WriterState(cursor: CursorState, style: CellStyle, origin: NonNegInt, region: Option[ScrollRegion]) derives Eq, Show, Hash

object WriterState {

  /** Cursor unknown, default style, origin zero, no region. */
  val initial: WriterState = WriterState(CursorState.Unknown, CellStyle.default, NonNegInt(0), none[ScrollRegion])

}
