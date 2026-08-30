package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** A cell-based scroll offset, shared by [[Paragraph]] and the scroll-view family (design doc 6.6, M2a): `rows` skips display rows
  * (wrapped rows in Paragraph's wrap mode, content rows in a [[ScrollView]]), `columns` skips display columns (for Paragraph in
  * truncate mode only, and only on left-aligned lines; for a ScrollView the horizontal window offset). The [[Scrolling]] rules
  * clamp it during render through the returned state.
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
