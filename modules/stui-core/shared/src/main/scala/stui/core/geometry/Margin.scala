package stui.core.geometry

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** A horizontal and a vertical inset, applied on both sides by `Rect.inner` and `Rect.outer`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Margin(horizontal: NonNegInt, vertical: NonNegInt) derives Eq, Show, Hash

object Margin {

  /** No inset on either axis. */
  val zero: Margin = Margin(NonNegInt(0), NonNegInt(0))

  /** `Left` with refined4s's message when a side is negative. */
  def from(horizontal: Int, vertical: Int): Either[String, Margin] =
    for {
      h <- NonNegInt.from(horizontal)
      v <- NonNegInt.from(vertical)
    } yield Margin(h, v)

  /** The same inset on both axes. */
  def uniform(n: NonNegInt): Margin = Margin(n, n)

}
