package stui.core.geometry

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.internal.NonNegInts
import stui.unicode.internal.IntOps.*

/** A width and a height in cells.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Size(width: NonNegInt, height: NonNegInt) derives Eq, Show, Hash

object Size {

  val zero: Size = Size(NonNegInt(0), NonNegInt(0))

  /** `Left` with refined4s's message when a side is negative. */
  def from(width: Int, height: Int): Either[String, Size] =
    for {
      w <- NonNegInt.from(width)
      h <- NonNegInt.from(height)
    } yield Size(w, h)

  extension (size: Size) {

    /** `width * height`, exact. */
    def area: Long = NonNegInts.times(size.width, size.height)

    /** True when either side is 0. */
    def isEmpty: Boolean = size.width.value === 0 || size.height.value === 0

  }

}
