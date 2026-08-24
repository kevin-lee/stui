package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** A [[Paragraph]] scroll offset: `rows` skips display rows (wrapped rows in wrap mode), `columns` skips display columns (truncate
  * mode only, and only on left-aligned lines).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Scroll(rows: NonNegInt, columns: NonNegInt) derives Eq, Show, Hash

object Scroll {

  /** No scrolling. */
  val none: Scroll = Scroll(NonNegInt(0), NonNegInt(0))

  /** `Left` with refined4s's message when a component is negative. */
  def from(rows: Int, columns: Int): Either[String, Scroll] =
    for {
      r <- NonNegInt.from(rows)
      c <- NonNegInt.from(columns)
    } yield Scroll(r, c)

}
