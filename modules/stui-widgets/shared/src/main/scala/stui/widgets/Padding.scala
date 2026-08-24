package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** The space a [[Block]] keeps between its border (or edge) and its content, per side. The field order matches
  * `Rect.inset(left, top, right, bottom)` (Ratatui's constructor order differs).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Padding(left: NonNegInt, top: NonNegInt, right: NonNegInt, bottom: NonNegInt) derives Eq, Show, Hash

object Padding {

  /** No padding. */
  val zero: Padding = Padding(NonNegInt(0), NonNegInt(0), NonNegInt(0), NonNegInt(0))

  /** The same padding on every side. */
  def uniform(n: NonNegInt): Padding = Padding(n, n, n, n)

  /** `horizontal` on the left and right, `vertical` on the top and bottom. */
  def symmetric(horizontal: NonNegInt, vertical: NonNegInt): Padding = Padding(horizontal, vertical, horizontal, vertical)

  /** Left and right only. */
  def horizontal(n: NonNegInt): Padding = Padding(n, NonNegInt(0), n, NonNegInt(0))

  /** Top and bottom only. */
  def vertical(n: NonNegInt): Padding = Padding(NonNegInt(0), n, NonNegInt(0), n)

  /** `Left` with refined4s's message when a side is negative. */
  def from(left: Int, top: Int, right: Int, bottom: Int): Either[String, Padding] =
    for {
      l <- NonNegInt.from(left)
      t <- NonNegInt.from(top)
      r <- NonNegInt.from(right)
      b <- NonNegInt.from(bottom)
    } yield Padding(l, t, r, b)

}
