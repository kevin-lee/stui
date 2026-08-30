package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.frame.{Frame, RegionId}
import stui.core.geometry.{Position, Rect, Size}
import stui.testkit.{Assertions, Rendering}
import stui.testkit.laws.WidgetLaws
import stui.widgets.gen.WidgetGens

/** The [[ListView]] contract laws, the fixed point, the selection and offset corrections, and the region scheme (design doc 6.6 and
  * 6.7, M2b).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ListViewSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private def sel(offset: Int, selected: Option[Int]): Selection =
    Selection(NonNegInt.from(offset).getOrElse(NonNegInt(0)), selected.flatMap(i => NonNegInt.from(i).toOption))

  /** i1..i8. */
  private val eight: ListView = ListView.raw((1 to 8).map(n => s"i${n.toString}")*)

  private def corrected(view: ListView, viewport: Size, state: Selection): Selection =
    Rendering.stateful(view, viewport, state) match {
      case (_, next) => next
    }

  override def tests: List[Test] =
    WidgetLaws.laws("list view", WidgetGens.listViewWidget, outer) ++
      WidgetLaws.stateLaw("list view", WidgetGens.listViewInputs, outer) ++
      List(
        example(
          "a selection beyond the items clamps and the offset follows",
          Assertions.eqv(corrected(eight, size(6, 3), sel(0, Some(20))), sel(5, Some(7))),
        ),
        example("an empty list corrects to none", Assertions.eqv(corrected(ListView.raw(), size(6, 3), sel(4, Some(2))), Selection.none)),
        example("the offset follows the selection down", Assertions.eqv(corrected(eight, size(6, 3), sel(0, Some(5))), sel(3, Some(5)))),
        example("the offset follows the selection up", Assertions.eqv(corrected(eight, size(6, 3), sel(3, Some(1))), sel(1, Some(1)))),
        example(
          "scroll padding keeps one item of context",
          Assertions.eqv(corrected(eight.withScrollPadding(nn(1)), size(6, 3), sel(0, Some(4))), sel(3, Some(4))),
        ),
        example(
          "an empty area returns the state unchanged",
          Assertions.eqv(corrected(eight, size(0, 0), sel(9, Some(9))), sel(9, Some(9))),
        ),
        example(
          "an empty inner area returns the state unchanged",
          Assertions.eqv(corrected(eight.withBlock(Block.bordered), size(2, 2), sel(9, Some(9))), sel(9, Some(9))),
        ),
        example("items are tagged under the base region and blank rows answer the base", testRegions),
        example("the symbol column is reserved only while something is selected", testSpacing),
      )

  def testRegions: Result = {
    val base  = RegionId("list")
    val view  = ListView.raw("i1", "i2", "i3").withRegion(base)
    val area  = Rect(nn(0), nn(0), nn(8), nn(5))
    val frame = Frame.draw(Buffer.empty(area))(canvas => view.render(area, canvas, Selection.none): Unit)
    Result.all(
      List(
        Assertions.eqv(frame.regions.at(Position(nn(1), nn(1))), Option(ItemRegions.of(base, nn(1)))),
        Assertions.eqv(frame.regions.at(Position(nn(7), nn(0))), Option(ItemRegions.of(base, nn(0)))),
        Assertions.eqv(frame.regions.at(Position(nn(1), nn(4))), Option(base)),
        Assertions.eqv(frame.regions.rectsOf(base), Vector(area)),
      )
    )
  }

  def testSpacing: Result = {
    val view = ListView.raw("i1", "i2").withHighlightSymbol(stui.core.text.Line.raw("> "))
    Result.all(
      List(
        Assertions
          .grid(Rendering.stateful(view, size(6, 2), Selection.none) match { case (buffer, _) => buffer }, Vector("i1    ", "i2    ")),
        Assertions.grid(
          Rendering.stateful(view, size(6, 2), Selection.first) match { case (buffer, _) => buffer },
          Vector("> i1  ", "  i2  "),
        ),
      )
    )
  }

}
