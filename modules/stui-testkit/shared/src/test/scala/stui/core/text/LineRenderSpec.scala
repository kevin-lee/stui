package stui.core.text

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.geometry.{Rect, Size}
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.NastyGens

/** `Line.render` and `Text.render` as widgets: alignment placement, right truncation, the dropped wide glyph at the edge, alignment
  * inheritance, and clipping to the canvas.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object LineRenderSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  override def tests: List[Test] = List(
    example("a left line starts at the left edge", Assertions.grid(Rendering.widget(Line.raw("ab"), size(6, 1)), Vector("ab    "))),
    example(
      "a centred line sits in the middle",
      Assertions.grid(Rendering.widget(Line.raw("ab").centered, size(6, 1)), Vector("  ab  ")),
    ),
    example(
      "a right line ends at the right edge",
      Assertions.grid(Rendering.widget(Line.raw("ab").rightAligned, size(6, 1)), Vector("    ab")),
    ),
    example("a long line truncates on the right", Assertions.grid(Rendering.widget(Line.raw("abcdef"), size(4, 1)), Vector("abcd"))),
    example(
      "a wide glyph that does not fit at the edge is dropped",
      Assertions.grid(Rendering.widget(Line.raw("a" + cps(0x30b3)), size(2, 1)), Vector("a ")),
    ),
    example(
      "a text renders its lines top to bottom",
      Assertions.grid(Rendering.widget(Text.raw("a\nb"), size(3, 2)), Vector("a  ", "b  ")),
    ),
    example(
      "a line alignment wins over the text alignment",
      Assertions.grid(Rendering.widget(Text.of(Line.raw("ab").rightAligned).aligned(Alignment.Center), size(6, 1)), Vector("    ab")),
    ),
    example(
      "extra lines below the area are dropped",
      Assertions.grid(Rendering.widget(Text.raw("a\nb\nc"), size(3, 2)), Vector("a  ", "b  ")),
    ),
    example("a line rendered partly outside the canvas clips", testClipped),
  )

  def testClipped: Result = {
    val buffer = Buffer
      .empty(Rect.sized(size(3, 1)))
      .draw(canvas => Line.raw("xyz").render(Rect(NonNegInt(1), NonNegInt(0), NonNegInt(5), NonNegInt(1)), canvas))
    Assertions.grid(buffer, Vector(" xy"))
  }

}
