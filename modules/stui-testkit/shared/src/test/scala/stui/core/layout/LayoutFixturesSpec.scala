package stui.core.layout

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Rect
import stui.testkit.Assertions

/** The layout fixture table of the M1d plan: every flex mode, overlap, over-constrained cuts, growth, and the degenerate corners.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object LayoutFixturesSpec extends Properties {

  private val area: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(80), NonNegInt(4))

  private inline def r(inline x: Int, inline w: Int): Rect = Rect(NonNegInt(x), NonNegInt(0), NonNegInt(w), NonNegInt(4))

  private val threeLengths: Layout = Layout.horizontal(Constraint.length(20), Constraint.length(20), Constraint.length(20))

  override def tests: List[Test] = List(
    example("1: three lengths, Start", Assertions.eqv(threeLengths.split(area), Vector(r(0, 20), r(20, 20), r(40, 20)))),
    example(
      "2: three lengths, End",
      Assertions.eqv(threeLengths.withFlex(Flex.End).split(area), Vector(r(20, 20), r(40, 20), r(60, 20))),
    ),
    example(
      "3: three lengths, Center",
      Assertions.eqv(threeLengths.withFlex(Flex.Center).split(area), Vector(r(10, 20), r(30, 20), r(50, 20))),
    ),
    example(
      "4: three lengths, SpaceBetween",
      Assertions.eqv(threeLengths.withFlex(Flex.SpaceBetween).split(area), Vector(r(0, 20), r(30, 20), r(60, 20))),
    ),
    example(
      "5: three lengths, SpaceAround",
      Assertions.eqv(threeLengths.withFlex(Flex.SpaceAround).split(area), Vector(r(3, 20), r(30, 20), r(57, 20))),
    ),
    example(
      "6: three lengths, SpaceEvenly",
      Assertions.eqv(threeLengths.withFlex(Flex.SpaceEvenly).split(area), Vector(r(5, 20), r(30, 20), r(55, 20))),
    ),
    example(
      "7: three lengths, Start, Overlap(1)",
      Assertions.eqv(threeLengths.withSpacing(Spacing.overlap(1)).split(area), Vector(r(0, 20), r(19, 20), r(38, 20))),
    ),
    example(
      "8: two halves with Space(1) fill the area exactly",
      Assertions.eqv(
        Layout.horizontal(Constraint.percentage(50), Constraint.percentage(50)).withSpacing(Spacing.space(1)).split(area),
        Vector(r(0, 40), r(41, 39)),
      ),
    ),
    example(
      "9: three thirds by ratio",
      Assertions.eqv(
        Layout.horizontal(Constraint.ratio(1, 3), Constraint.ratio(1, 3), Constraint.ratio(1, 3)).split(area),
        Vector(r(0, 27), r(27, 27), r(54, 26)),
      ),
    ),
    example(
      "10: two over-constrained lengths, the rightmost gives way",
      Assertions.eqv(
        Layout.horizontal(Constraint.length(50), Constraint.length(50)).split(area),
        Vector(r(0, 50), r(50, 30)),
      ),
    ),
    example(
      "11: a length outranks an earlier percentage",
      Assertions.eqv(
        Layout.horizontal(Constraint.percentage(50), Constraint.length(50)).split(area),
        Vector(r(0, 30), r(30, 50)),
      ),
    ),
    example(
      "12: two over-constrained mins, the rightmost gives way",
      Assertions.eqv(Layout.horizontal(Constraint.min(50), Constraint.min(50)).split(area), Vector(r(0, 50), r(50, 30))),
    ),
    example(
      "13: a small min grows like Fill(1) beside Fill(2)",
      Assertions.eqv(Layout.horizontal(Constraint.min(10), Constraint.fill(2)).split(area), Vector(r(0, 27), r(27, 53))),
    ),
    example(
      "14: a large min keeps its floor beside Fill(2)",
      Assertions.eqv(Layout.horizontal(Constraint.min(60), Constraint.fill(2)).split(area), Vector(r(0, 60), r(60, 20))),
    ),
    example(
      "15: a max stays at its preference",
      Assertions.eqv(Layout.horizontal(Constraint.max(10), Constraint.length(60)).split(area), Vector(r(0, 10), r(10, 60))),
    ),
    example(
      "16: a max preference outranks Fill growth",
      Assertions.eqv(Layout.horizontal(Constraint.max(100), Constraint.fill(1)).split(area), Vector(r(0, 80), r(80, 0))),
    ),
    example(
      "17: SpaceBetween with one segment behaves like Start",
      Assertions.eqv(Layout.horizontal(Constraint.length(20)).withFlex(Flex.SpaceBetween).split(area), Vector(r(0, 20))),
    ),
    example(
      "18: three equal fills, the leftover to the leftmost",
      Assertions.eqv(
        Layout.horizontal(Constraint.fill(1), Constraint.fill(1), Constraint.fill(1)).split(area),
        Vector(r(0, 27), r(27, 27), r(54, 26)),
      ),
    ),
    example("19: the vertical dual of fixture 1", testVerticalDual),
    example("20: no constraints give no rects", Assertions.eqv(Layout.horizontal().split(area), Vector.empty[Rect])),
    example("21: a single fill takes the whole area", Assertions.eqv(Layout.horizontal(Constraint.fill(1)).split(area), Vector(r(0, 80)))),
    example("22: a zero-width area gives empty segments", testZeroWidth),
    example("23: split3 with three lengths equals fixture 1 as a tuple", testSplit3),
    example("24: split2 with a gap and a trailing fill", testSplit2Gap),
  )

  def testVerticalDual: Result = {
    val verticalArea                                 = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(4), NonNegInt(80))
    inline def v(inline y: Int, inline h: Int): Rect = Rect(NonNegInt(0), NonNegInt(y), NonNegInt(4), NonNegInt(h))
    Assertions.eqv(
      Layout.vertical(Constraint.length(20), Constraint.length(20), Constraint.length(20)).split(verticalArea),
      Vector(v(0, 20), v(20, 20), v(40, 20)),
    )
  }

  def testZeroWidth: Result = {
    val zeroWidth                                    = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(0), NonNegInt(5))
    inline def z(inline x: Int, inline w: Int): Rect = Rect(NonNegInt(x), NonNegInt(0), NonNegInt(w), NonNegInt(5))
    Assertions.eqv(
      Layout.horizontal(Constraint.length(2), Constraint.length(3)).split(zeroWidth),
      Vector(z(0, 0), z(0, 0)),
    )
  }

  def testSplit3: Result =
    Assertions.eqv(
      Axis.horizontal.split3(area, Constraint.length(20), Constraint.length(20), Constraint.length(20)),
      (r(0, 20), r(20, 20), r(40, 20)),
    )

  def testSplit2Gap: Result =
    Assertions.eqv(
      Axis.horizontal.withSpacing(Spacing.space(2)).split2(area, Constraint.length(10), Constraint.fill(1)),
      (r(0, 10), r(12, 68)),
    )

}
