package stui.core.widget

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas}
import stui.core.frame.RegionId
import stui.core.geometry.{Rect, Size}
import stui.core.style.Style
import stui.testkit.Assertions
import stui.testkit.gen.{Gens, NastyGens, StyleGens}
import stui.testkit.laws.WidgetLaws

/** The widget contract laws run on two local test widgets, and the off-canvas clipping example.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object WidgetContractSpec extends Properties {

  /** Fills its whole area with one symbol (the canvas clips and repairs). */
  final case class FillWidget(symbol: String, style: Style) extends Widget {
    /* the method lives in the class body because it implements the Widget trait member */
    override def render(area: Rect, canvas: Canvas): Unit = canvas.fill(area, symbol, style)
  }

  /** Writes its state into the first row of its area and returns the state clamped at `limit`. */
  final case class ClampWidget(limit: Int) extends StatefulWidget[Int] {
    /* the method lives in the class body because it implements the StatefulWidget trait member */
    override def render(area: Rect, canvas: Canvas, state: Int): Int = {
      val corrected = math.min(state, limit)
      if (area.isEmpty) {
        corrected
      } else {
        canvas.putStringMax(area.position, corrected.toString, Style.empty, area.width)
        corrected
      }
    }
  }

  /** Draws nothing, records its cursor at the area's origin and the whole area as a region (nothing on an empty area). */
  final case class CaretWidget(id: RegionId) extends Widget {
    /* the method lives in the class body because it implements the Widget trait member */
    override def render(area: Rect, canvas: Canvas): Unit =
      if (area.isEmpty) {
        ()
      } else {
        canvas.cursor(area.position)
        canvas.region(id, area)
      }
  }

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private val fillWidgets: Gen[Widget] =
    for {
      symbol <- Gen.frequency1(3 -> Gens.asciiPrintableChar.map(_.toString), 1 -> NastyGens.wideCluster)
      style  <- StyleGens.style
    } yield (FillWidget(symbol, style): Widget)

  private val caretWidgets: Gen[Widget] = Gen.element1("caret", "prompt").map(id => CaretWidget(RegionId(id)): Widget)

  private val clampInputs: Gen[(StatefulWidget[Int], Int)] =
    for {
      limit <- Gen.int(Range.linear(0, 50))
      state <- Gen.int(Range.linearFrom(0, -100, 100))
    } yield (ClampWidget(limit): StatefulWidget[Int], state)

  override def tests: List[Test] =
    WidgetLaws.laws("fill widget", fillWidgets, outer) ++
      WidgetLaws.laws("caret widget", caretWidgets, outer) ++
      WidgetLaws.stateLaw("clamp widget", clampInputs, outer) ++
      List(example("an area hanging off the canvas paints only the intersection", testClipped))

  def testClipped: Result = {
    val area   = Rect(NonNegInt(2), NonNegInt(0), NonNegInt(4), NonNegInt(4))
    val buffer = Buffer
      .empty(Rect.sized(Size(NonNegInt(4), NonNegInt(2))))
      .draw(canvas => FillWidget("x", Style.empty).render(area, canvas))
    Assertions.grid(buffer, Vector("  xx", "  xx"))
  }

}
