package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Rect
import stui.core.text.Line
import stui.testkit.Assertions
import stui.testkit.gen.GeometryGens
import stui.testkit.laws.{GeometryLaws, WidgetLaws}
import stui.widgets.gen.WidgetGens

/** The widget laws for Block and the `inner` semantics, including the Ratatui-derived title-row cases (block.rs
  * `inner_takes_into_account_the_title` and `inner_takes_into_account_border_and_title` at tag ratatui-v0.30.2).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object BlockSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def rect(inline x: Int, inline y: Int, inline width: Int, inline height: Int): Rect =
    Rect(NonNegInt(x), NonNegInt(y), NonNegInt(width), NonNegInt(height))

  override def tests: List[Test] =
    WidgetLaws.laws("block", WidgetGens.blockWidget, outer) ++ List(
      property(
        "inner is contained in the area",
        for {
          block <- WidgetGens.block.forAll
          area  <- GeometryGens.rect(NonNegInt(50)).forAll
        } yield Result.assert(GeometryLaws.contained(block.inner(area), area)).log("inner not contained"),
      ),
      example(
        "a title alone reserves its row",
        Assertions.eqv(Block.empty.withTitle(Line.raw("Test")).inner(rect(0, 0, 0, 1)), rect(0, 1, 0, 0)),
      ),
      example(
        "a top title with a bottom border consumes both rows",
        Assertions.eqv(
          Block.empty.withTitleTop(Line.raw("Test")).withBorders(Borders.of(Side.Bottom)).inner(rect(0, 0, 0, 2)),
          rect(0, 1, 0, 0),
        ),
      ),
      example(
        "a bottom title with a top border consumes both rows",
        Assertions.eqv(
          Block.empty.withTitleBottom(Line.raw("Test")).withBorders(Borders.of(Side.Top)).inner(rect(0, 0, 0, 2)),
          rect(0, 1, 0, 0),
        ),
      ),
      example(
        "a bottom title shares the bottom border row",
        Assertions.eqv(
          Block.empty.withTitleBottom(Line.raw("Test")).withBorders(Borders.of(Side.Bottom)).inner(rect(0, 0, 0, 2)),
          rect(0, 0, 0, 1),
        ),
      ),
      example("a full border consumes one cell per side", Assertions.eqv(Block.bordered.inner(rect(0, 0, 10, 3)), rect(1, 1, 8, 1))),
      example(
        "padding comes off after the border",
        Assertions.eqv(Block.bordered.withPadding(Padding.uniform(NonNegInt(1))).inner(rect(0, 0, 10, 5)), rect(2, 2, 6, 1)),
      ),
    )

}
