package stui.testkit.gen

import hedgehog.{Gen, Range}
import hedgehog.extra.refined4s.gens.NumGens
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Margin, Offset, Position, Rect, Size}

/** Generators for the geometry types.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object GeometryGens {

  /** `n` floored at 0 (test helper for values that are non-negative by construction). */
  def nonNegOrZero(n: Long): NonNegInt = {
    val bounded = if (n < 0L) 0 else if (n > Int.MaxValue.toLong) Int.MaxValue else n.toInt
    NonNegInt.from(bounded).fold(_ => NonNegInt.MinValue, identity)
  }

  /** 0 to `max` inclusive, linear range. */
  def nonNegInt(max: NonNegInt): Gen[NonNegInt] = NumGens.genNonNegIntMaxTo(max)

  /** 0, 1, 2, 100, and the two largest values. */
  val extremeNonNegInt: Gen[NonNegInt] =
    Gen.element1(NonNegInt(0), NonNegInt(1), NonNegInt(2), NonNegInt(100), NonNegInt(2147483646), NonNegInt.MaxValue)

  def position(max: NonNegInt): Gen[Position] =
    for {
      x <- nonNegInt(max)
      y <- nonNegInt(max)
    } yield Position(x, y)

  def size(max: NonNegInt): Gen[Size] =
    for {
      width  <- nonNegInt(max)
      height <- nonNegInt(max)
    } yield Size(width, height)

  def rect(max: NonNegInt): Gen[Rect] =
    for {
      x      <- nonNegInt(max)
      y      <- nonNegInt(max)
      width  <- nonNegInt(max)
      height <- nonNegInt(max)
    } yield Rect(x, y, width, height)

  /** A rect contained in `outer`. */
  def rectWithin(outer: Rect): Gen[Rect] =
    for {
      width  <- nonNegInt(outer.width)
      height <- nonNegInt(outer.height)
      x      <- Gen.long(Range.linear(outer.x.value.toLong, outer.x.value.toLong + outer.width.value.toLong - width.value.toLong))
      y      <- Gen.long(Range.linear(outer.y.value.toLong, outer.y.value.toLong + outer.height.value.toLong - height.value.toLong))
    } yield Rect(nonNegOrZero(x), nonNegOrZero(y), width, height)

  def margin(max: NonNegInt): Gen[Margin] =
    for {
      horizontal <- nonNegInt(max)
      vertical   <- nonNegInt(max)
    } yield Margin(horizontal, vertical)

  def offset(range: Range[Int]): Gen[Offset] =
    for {
      dx <- Gen.int(range)
      dy <- Gen.int(range)
    } yield Offset(dx, dy)

  /** Every component from [[extremeNonNegInt]]. */
  val extremeRect: Gen[Rect] =
    for {
      x      <- extremeNonNegInt
      y      <- extremeNonNegInt
      width  <- extremeNonNegInt
      height <- extremeNonNegInt
    } yield Rect(x, y, width, height)

  /** Mostly small rects, sometimes extreme ones. */
  val anyRect: Gen[Rect] = Gen.frequency1(9 -> rect(NonNegInt(200)), 1 -> extremeRect)

}
