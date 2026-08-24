package stui.core.geometry

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.internal.NonNegInts

/** A cell coordinate: `x` is the column, `y` the row, both zero-based and non-negative.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Position(x: NonNegInt, y: NonNegInt) derives Eq, Show, Hash

object Position {

  /** The position (0, 0). */
  val origin: Position = Position(NonNegInt(0), NonNegInt(0))

  /** `Left` with refined4s's message when a coordinate is negative. */
  def from(x: Int, y: Int): Either[String, Position] =
    for {
      px <- NonNegInt.from(x)
      py <- NonNegInt.from(y)
    } yield Position(px, py)

  extension (position: Position) {

    /** Each axis moved by the offset, floored at 0 and saturated at `Int.MaxValue`. */
    def offset(delta: Offset): Position =
      Position(NonNegInts.offset(position.x, delta.dx), NonNegInts.offset(position.y, delta.dy))

  }

}
