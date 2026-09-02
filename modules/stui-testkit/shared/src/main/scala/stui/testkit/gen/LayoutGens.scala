package stui.testkit.gen

import hedgehog.{Gen, Range}
import hedgehog.extra.refined4s.gens.NumGens
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.layout.{Axis, Constraint, Direction, Flex, Layout, Percent, Spacing}

/** Generators for the layout vocabulary.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object LayoutGens {

  /** Any direction. */
  val direction: Gen[Direction] = Gen.element1(Direction.Horizontal, Direction.Vertical)

  /** Any flex mode. */
  val flex: Gen[Flex] = Gen.element1(Flex.Start, Flex.End, Flex.Center, Flex.SpaceBetween, Flex.SpaceAround, Flex.SpaceEvenly)

  /** Mostly no spacing, sometimes a gap up to 5, sometimes an overlap up to 3. */
  val spacing: Gen[Spacing] = Gen.frequency1(
    4 -> Gen.constant(Spacing.none),
    4 -> nonNegIntTo(5).map(Spacing.spaceOf),
    2 -> nonNegIntTo(3).map(Spacing.overlapOf),
  )

  /** Any axis: a direction with [[spacing]] and [[flex]]. */
  val axis: Gen[Axis] =
    for {
      d  <- direction
      sp <- spacing
      fl <- flex
    } yield Axis(d, sp, fl)

  /** 0 to 100. */
  val percent: Gen[Percent] = Gen.int(Range.linear(0, 100)).map(p => Percent.from(p).fold(_ => Percent.MinValue, identity))

  /** 1 to `max`, linear range. */
  def posInt(max: PosInt): Gen[PosInt] = NumGens.genPosIntMaxTo(max)

  private def nonNegIntTo(max: Int): Gen[NonNegInt] =
    Gen.int(Range.linear(0, max)).map(n => GeometryGens.nonNegOrZero(n.toLong))

  /** One constraint of any kind, lengths and bounds up to `max`, ratios up to 10 over 1..10, fill weights 1..5. */
  def constraint(max: NonNegInt): Gen[Constraint] = Gen.frequency1(
    3 -> GeometryGens.nonNegInt(max).map(Constraint.lengthOf),
    2 -> percent.map(Constraint.percentageOf),
    2 -> ratio,
    2 -> GeometryGens.nonNegInt(max).map(Constraint.minOf),
    2 -> GeometryGens.nonNegInt(max).map(Constraint.maxOf),
    3 -> posInt(PosInt(5)).map(Constraint.fillOf),
  )

  private def ratio: Gen[Constraint] =
    for {
      numerator   <- GeometryGens.nonNegInt(NonNegInt(10))
      denominator <- posInt(PosInt(10))
    } yield Constraint.ratioOf(numerator, denominator)

  /** Constraint vectors with a count in the range. */
  def constraints(count: Range[Int], max: NonNegInt): Gen[Vector[Constraint]] = constraint(max).list(count).map(_.toVector)

  /** Layouts over [[constraints]] with any direction, spacing, and flex. */
  def layout(count: Range[Int], max: NonNegInt): Gen[Layout] =
    for {
      d  <- direction
      cs <- constraints(count, max)
      sp <- spacing
      fl <- flex
    } yield Layout(d, cs, sp, fl)

  /** Fill-only constraint vectors with weights 1 to `maxWeight`. */
  def fillOnly(count: Range[Int], maxWeight: PosInt): Gen[Vector[Constraint]] =
    posInt(maxWeight).map(Constraint.fillOf).list(count).map(_.toVector)

  /** Fill-only constraint vectors with every weight 1. */
  def equalFills(count: Range[Int]): Gen[Vector[Constraint]] =
    Gen.constant(Constraint.fill(1)).list(count).map(_.toVector)

  /** Extreme payloads: maximal lengths, bounds, ratios, and weights, plus the percent edges. */
  val extremeConstraint: Gen[Constraint] = Gen.element1(
    Constraint.lengthOf(NonNegInt(0)),
    Constraint.lengthOf(NonNegInt.MaxValue),
    Constraint.minOf(NonNegInt.MaxValue),
    Constraint.maxOf(NonNegInt.MaxValue),
    Constraint.percentageOf(Percent(0)),
    Constraint.percentageOf(Percent(100)),
    Constraint.ratioOf(NonNegInt.MaxValue, PosInt(1)),
    Constraint.fillOf(PosInt.MaxValue),
  )

  /** Mostly small layouts, sometimes extreme constraints with small counts. */
  val anyLayout: Gen[Layout] = Gen.frequency1(
    9 -> layout(Range.linear(0, 6), NonNegInt(30)),
    1 -> extremeLayout,
  )

  private def extremeLayout: Gen[Layout] =
    for {
      d  <- direction
      cs <- extremeConstraint.list(Range.linear(0, 3)).map(_.toVector)
      sp <- spacing
      fl <- flex
    } yield Layout(d, cs, sp, fl)

}
