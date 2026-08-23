package stui.core.geometry

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.Assertions

/** Hand-picked geometry examples, including saturation at `Int.MaxValue` and flooring at 0.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object GeometryFixturesSpec extends Properties {

  private inline def rect(inline x: Int, inline y: Int, inline width: Int, inline height: Int): Rect =
    Rect(NonNegInt(x), NonNegInt(y), NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private inline def margin(inline h: Int, inline v: Int): Margin = Margin(NonNegInt(h), NonNegInt(v))

  override def tests: List[Test] = List(
    example("inner shrinks by the margin on every side", Assertions.eqv(rect(1, 2, 10, 5).inner(margin(1, 1)), rect(2, 3, 8, 3))),
    example(
      "inner with an over-large margin is empty at the moved origin",
      Assertions.eqv(rect(0, 0, 4, 4).inner(margin(3, 3)), rect(3, 3, 0, 0)),
    ),
    example("outer floors the origin at 0", Assertions.eqv(rect(1, 1, 2, 2).outer(margin(3, 3)), rect(0, 0, 8, 8))),
    example("intersection of disjoint rects is empty", Result.assert(rect(0, 0, 2, 2).intersection(rect(5, 5, 2, 2)).isEmpty)),
    example("intersection of overlapping rects", Assertions.eqv(rect(0, 0, 4, 4).intersection(rect(2, 1, 4, 4)), rect(2, 1, 2, 3))),
    example("union covers both", Assertions.eqv(rect(0, 0, 2, 2).union(rect(5, 5, 2, 2)), rect(0, 0, 7, 7))),
    example("clamp moves a rect inside the other", Assertions.eqv(rect(8, 8, 4, 4).clamp(rect(0, 0, 10, 10)), rect(6, 6, 4, 4))),
    example("clamp shrinks a larger rect to the other", Assertions.eqv(rect(0, 0, 20, 20).clamp(rect(2, 2, 5, 5)), rect(2, 2, 5, 5))),
    example("offset floors at 0", Assertions.eqv(rect(1, 1, 2, 2).offset(Offset(-5, -5)), rect(0, 0, 2, 2))),
    example("offset moves the origin", Assertions.eqv(at(1, 1).offset(Offset(3, -1)), at(4, 0))),
    example(
      "right saturates at Int.MaxValue",
      Rect(NonNegInt.MaxValue, NonNegInt(0), NonNegInt.MaxValue, NonNegInt(1)).right.value ==== Int.MaxValue,
    ),
    example(
      "position offset saturates at Int.MaxValue",
      Assertions.eqv(Position(NonNegInt.MaxValue, NonNegInt(0)).offset(Offset(1, 0)), Position(NonNegInt.MaxValue, NonNegInt(0))),
    ),
    example("negate maps Int.MinValue to Int.MaxValue", Assertions.eqv(Offset(Int.MinValue, 3).negate, Offset(Int.MaxValue, -3))),
    example(
      "contains is inclusive at the origin and exclusive at the far edges",
      Result.all(
        List(
          Result.assert(rect(1, 1, 2, 2).contains(at(1, 1))),
          Result.assert(rect(1, 1, 2, 2).contains(at(2, 2))),
          Result.assert(!rect(1, 1, 2, 2).contains(at(3, 3))),
          Result.assert(!rect(1, 1, 2, 2).contains(at(0, 1))),
        )
      ),
    ),
    example("from rejects a negative component", Result.assert(Rect.from(-1, 0, 0, 0).isLeft)),
    example("from accepts non-negative components", Assertions.eqv(Rect.from(1, 2, 3, 4), Right(rect(1, 2, 3, 4)))),
    example(
      "rows and columns",
      Result.all(
        List(
          Assertions.eqv(rect(1, 1, 3, 2).rows, Vector(rect(1, 1, 3, 1), rect(1, 2, 3, 1))),
          Assertions.eqv(rect(1, 1, 2, 3).columns, Vector(rect(1, 1, 1, 3), rect(2, 1, 1, 3))),
        )
      ),
    ),
    example("an empty rect intersects nothing", Result.assert(!rect(1, 1, 0, 5).intersects(rect(0, 0, 10, 10)))),
  )

}
