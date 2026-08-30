package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.frame.{Frame, RegionId}
import stui.core.geometry.{Rect, Size}
import stui.core.text.{Line, Text}
import stui.testkit.{Assertions, Rendering}
import stui.testkit.laws.WidgetLaws
import stui.widgets.gen.WidgetGens

/** The [[ScrollView]] contract laws, the fixed point, and the clamp behaviour (design doc 6.6, decision D21, M2a).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ScrollViewSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  /** Four rows, each its row digit repeated ten times (a 10x4 content). */
  private val content: Text = Text.fromLines(Vector.tabulate(4)(row => Line.raw(row.toString * 10)))

  override def tests: List[Test] =
    WidgetLaws.laws("scroll view", WidgetGens.scrollViewWidget, outer) ++
      WidgetLaws.stateLaw("scroll view", WidgetGens.scrollViewInputs, outer) ++
      List(
        example(
          "an oversized offset clamps per axis",
          Rendering.stateful(ScrollView.of(content, size(10, 4)), size(5, 2), Scroll(nn(100), nn(100))) match {
            case (_, corrected) => Assertions.eqv(corrected, Scroll(nn(2), nn(5)))
          },
        ),
        example(
          "content that fits corrects to no offset",
          Rendering.stateful(ScrollView.of(Text.of(Line.raw("abc")), size(3, 1)), size(5, 2), Scroll(nn(4), nn(4))) match {
            case (_, corrected) => Assertions.eqv(corrected, Scroll.none)
          },
        ),
        example(
          "an empty area returns the state unchanged",
          Rendering.stateful(ScrollView.of(content, size(10, 4)), size(0, 0), Scroll(nn(9), nn(9))) match {
            case (_, corrected) => Assertions.eqv(corrected, Scroll(nn(9), nn(9)))
          },
        ),
        example(
          "the region covers the whole target", {
            val id    = RegionId("body")
            val area  = Rect(nn(1), nn(1), nn(6), nn(3))
            val frame = Frame.draw(Buffer.empty(Rect(nn(0), nn(0), nn(8), nn(5)))) { canvas =>
              ScrollView.of(content, size(10, 4)).withRegion(id).render(area, canvas, Scroll.none): Unit
            }
            Assertions.eqv(frame.regions.rectsOf(id), Vector(area))
          },
        ),
        example(
          "a blocked scroll view clamps against the inner size",
          Rendering.stateful(
            ScrollView.of(content, size(10, 4)).withBlock(Block.bordered),
            size(7, 5),
            Scroll(nn(100), nn(100)),
          ) match {
            case (_, corrected) => Assertions.eqv(corrected, Scroll(nn(1), nn(5)))
          },
        ),
      )

}
