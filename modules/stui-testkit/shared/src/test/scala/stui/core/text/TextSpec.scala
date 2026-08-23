package stui.core.text

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.{StyleGens, TextGens}
import stui.unicode.WidthPolicy

/** Width, height, splitting, and style-patch action laws of the text model.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object TextSpec extends Properties {

  private val policy: WidthPolicy = WidthPolicy.default

  private val lines: Gen[Line] = TextGens.line(Range.linear(0, 4), Range.linear(0, 8))

  private val texts: Gen[Text] = TextGens.text(Range.linear(0, 4), Range.linear(0, 3), Range.linear(0, 8))

  override def tests: List[Test] = List(
    property(
      "line width is the sum of the span widths",
      lines.forAll.map(line => line.width(policy) ==== line.spans.map(_.width(policy)).sum),
    ),
    property(
      "text width is the widest line",
      texts.forAll.map(text => text.width(policy) ==== text.lines.map(_.width(policy)).maxOption.getOrElse(0)),
    ),
    property("text height is the number of lines", texts.forAll.map(text => text.height ==== text.lines.length)),
    property("Text.raw splits on LF and keeps every part", testRawRoundTrip),
    example("Text.raw drops a trailing CR", Assertions.eqv(Text.raw("a\r\nb").lines.map(content), Vector("a", "b"))),
    example("Text.raw keeps a trailing empty line", Text.raw("a\n").lines.length ==== 2),
    example("Text.raw of the empty string is one empty line", Assertions.eqv(Text.raw("").lines.map(content), Vector(""))),
    property("Span.patchStyle is a monoid action", testSpanAction),
    property("Line.patchStyle is a monoid action", testLineAction),
    property("Text.patchStyle is a monoid action", testTextAction),
    property(
      "resolvedSpans of a line without style are the spans",
      lines.forAll.map(line => Assertions.eqv(line.resetStyle.resolvedSpans, line.spans)),
    ),
    property("resolvedLines inherit the text alignment only where a line has none", testResolvedAlignment),
  )

  private def content(line: Line): String = line.spans.map(_.content).mkString

  def testRawRoundTrip: Property =
    TextGens.plainLines(Range.linear(1, 5), Range.linear(0, 10)).forAll.map { parts =>
      Assertions.eqv(Text.raw(parts.mkString("\n")).lines.map(content), parts)
    }

  def testSpanAction: Property =
    for {
      span <- TextGens.span(Range.linear(0, 5)).forAll
      a    <- StyleGens.style.forAll
      b    <- StyleGens.style.forAll
    } yield Assertions.eqv(span.patchStyle(a).patchStyle(b), span.patchStyle(a.patch(b)))

  def testLineAction: Property =
    for {
      line <- lines.forAll
      a    <- StyleGens.style.forAll
      b    <- StyleGens.style.forAll
    } yield Assertions.eqv(line.patchStyle(a).patchStyle(b), line.patchStyle(a.patch(b)))

  def testTextAction: Property =
    for {
      text <- texts.forAll
      a    <- StyleGens.style.forAll
      b    <- StyleGens.style.forAll
    } yield Assertions.eqv(text.patchStyle(a).patchStyle(b), text.patchStyle(a.patch(b)))

  def testResolvedAlignment: Property = texts.forAll.map { text =>
    Result.all(
      List(
        Assertions.eqv(text.resolvedLines.map(_.alignment), text.lines.map(line => line.alignment.orElse(text.alignment))),
        Assertions.eqv(text.resolvedLines.map(_.style), text.lines.map(line => text.style.patch(line.style))),
        Assertions.eqv(text.resetStyle.resolvedLines.map(_.style), text.lines.map(_.style)),
      )
    )
  }

}
