package stui.terminal.ansi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.style.CellStyle

/** The explicit state of the writer's cursor model (design doc 7.1, decision D16): the tracked cursor in screen coordinates, the style
  * the terminal currently has, and the armed scroll region (the inline scroll-region print strategy of 7.2). The viewport travels as a
  * screen-coordinate `Rect` parameter of every writer call, so update positions are absolute rows (plan refinement R1 of M1f).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class WriterState(cursor: CursorState, style: CellStyle, region: Option[ScrollRegion]) derives Eq, Show, Hash

object WriterState {

  /** Cursor unknown, default style, no region. */
  val initial: WriterState = WriterState(CursorState.Unknown, CellStyle.default, none[ScrollRegion])

}
