package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.capability.{Capabilities, GlyphSet}
import stui.core.frame.{Frame, RegionId}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.text.Span
import stui.testkit.{Assertions, Rendering}
import stui.testkit.laws.{MeasurableLaws, WidgetLaws}
import stui.unicode.WidthPolicy
import stui.widgets.gen.WidgetGens

/** The [[Tabs]] contract, fixed-point, and Measurable laws, the strip scrolling, the regions, and the divider selection (design doc
  * 6.7, M2b).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object TabsSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private def sel(offset: Int, selected: Option[Int]): Selection =
    Selection(NonNegInt.from(offset).getOrElse(NonNegInt(0)), selected.flatMap(i => NonNegInt.from(i).toOption))

  private val abc: Tabs = Tabs.raw("A", "B", "C")

  private val four: Tabs = Tabs.raw("aa", "bb", "cc", "dd")

  private def corrected(tabs: Tabs, viewport: Size, state: Selection): Selection =
    Rendering.stateful(tabs, viewport, state) match {
      case (_, next) => next
    }

  override def tests: List[Test] =
    WidgetLaws.laws("tabs", WidgetGens.tabsWidget, outer) ++
      WidgetLaws.stateLaw("tabs", WidgetGens.tabsInputs, outer) ++
      MeasurableLaws.laws("tabs", WidgetGens.tabsMeasurable, outer) ++
      List(
        example(
          "measure is the natural strip width by one row",
          Assertions.eqv(abc.measure(size(30, 5), WidthPolicy.default), size(11, 1)),
        ),
        example("measure truncates at the constraints", Assertions.eqv(abc.measure(size(5, 0), WidthPolicy.default), size(5, 0))),
        example("a selection beyond the titles clamps", Assertions.eqv(corrected(abc, size(30, 1), sel(0, Some(9))), sel(0, Some(2)))),
        example(
          "the strip scrolls the selected tab into view",
          Assertions.eqv(corrected(four, size(9, 1), sel(0, Some(3))), sel(2, Some(3))),
        ),
        example(
          "the strip scrolls back for an early selection",
          Assertions.eqv(corrected(four, size(9, 1), sel(2, Some(0))), sel(0, Some(0))),
        ),
        example("no titles correct to none", Assertions.eqv(corrected(Tabs.raw(), size(9, 1), sel(2, Some(1))), Selection.none)),
        example("an empty area returns the state unchanged", Assertions.eqv(corrected(abc, size(0, 0), sel(5, Some(5))), sel(5, Some(5)))),
        example("tabs are tagged under the base region and dividers answer the base", testRegions),
        example("dividerFor follows the glyph set", testDivider),
      )

  def testRegions: Result = {
    val base  = RegionId("tabs")
    val area  = Rect(nn(0), nn(0), nn(11), nn(1))
    val frame = Frame.draw(Buffer.empty(area))(canvas => abc.withRegion(base).render(area, canvas, Selection.none): Unit)
    Result.all(
      List(
        Assertions.eqv(frame.regions.at(Position(nn(0), nn(0))), Option(ItemRegions.of(base, nn(0)))),
        Assertions.eqv(frame.regions.at(Position(nn(5), nn(0))), Option(ItemRegions.of(base, nn(1)))),
        Assertions.eqv(frame.regions.at(Position(nn(3), nn(0))), Option(base)),
        Assertions.eqv(frame.regions.at(Position(nn(10), nn(0))), Option(ItemRegions.of(base, nn(2)))),
        Assertions.eqv(frame.regions.rectsOf(base), Vector(area)),
      )
    )
  }

  def testDivider: Result =
    Result.all(
      List(
        Assertions.eqv(Tabs.dividerFor(Capabilities.conservative), Span.raw("│")),
        Assertions.eqv(Tabs.dividerFor(Capabilities.conservative.copy(glyphs = GlyphSet.Ascii)), Span.raw("|")),
        Assertions.eqv(Tabs.dividerFor(Capabilities.conservative.copy(ambiguousWide = true)), Span.raw("|")),
      )
    )

}
