package stui.core.widget

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Rect, Size}
import stui.core.text.{Line, Text}
import stui.testkit.Assertions
import stui.testkit.gen.TextGens
import stui.testkit.laws.MeasurableLaws
import stui.unicode.WidthPolicy

/** The [[Measurable]] laws on [[Line]] and [[Text]] (D18, M2a), and the fixture examples of their `measure` answers.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object MeasurableSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private val lines: Gen[Widget & Measurable] =
    TextGens.line(Range.linear(0, 3), Range.linear(0, 8)).map(line => line: Widget & Measurable)

  private val texts: Gen[Widget & Measurable] =
    TextGens.text(Range.linear(0, 3), Range.linear(0, 2), Range.linear(0, 8)).map(text => text: Widget & Measurable)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  override def tests: List[Test] =
    MeasurableLaws.laws("line", lines, outer) ++
      MeasurableLaws.laws("text", texts, outer) ++
      List(
        example(
          "a line measures its width by one row",
          Assertions.eqv(Line.raw("abc").measure(size(10, 5), WidthPolicy.default), size(3, 1)),
        ),
        example(
          "a line's width truncates at the constraints",
          Assertions.eqv(Line.raw("abc").measure(size(2, 5), WidthPolicy.default), size(2, 1)),
        ),
        example(
          "a height-zero constraint gives a height-zero answer",
          Assertions.eqv(Line.raw("abc").measure(size(10, 0), WidthPolicy.default), size(3, 0)),
        ),
        example(
          "wide glyphs count two columns",
          Assertions.eqv(Line.raw("한글").measure(size(10, 3), WidthPolicy.default), size(4, 1)),
        ),
        example(
          "a text measures its widest line by its line count",
          Assertions.eqv(Text.of(Line.raw("abc"), Line.raw("hello")).measure(size(9, 9), WidthPolicy.default), size(5, 2)),
        ),
        example(
          "the empty text measures zero",
          Assertions.eqv(Text.of().measure(size(5, 5), WidthPolicy.default), size(0, 0)),
        ),
      )

}
