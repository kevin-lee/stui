package stui.core.text

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.style.Style
import stui.unicode.WidthPolicy

/** One line of spans with a line-level style patch and an optional alignment. The style written for a span is the line style patched
  * by the span style ([[Line.resolvedSpans]]).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Line(spans: Vector[Span], style: Style, alignment: Option[Alignment]) derives Eq, Show, Hash

object Line {

  def raw(content: String): Line = fromSpans(Vector(Span.raw(content)))

  /** One span carrying the style, the line style stays empty. */
  def styled(content: String, style: Style): Line = fromSpans(Vector(Span.styled(content, style)))

  def of(spans: Span*): Line = fromSpans(spans.toVector)

  def fromSpans(spans: Vector[Span]): Line = Line(spans, Style.empty, none[Alignment])

  extension (line: Line) {

    /** The sum of the span widths. */
    def width(policy: WidthPolicy): Int = line.spans.foldLeft(0)((acc, span) => acc + span.width(policy))

    def patchStyle(style: Style): Line = line.copy(style = line.style.patch(style))

    /** The line style only, the spans keep theirs. */
    def resetStyle: Line = line.copy(style = Style.empty)

    def aligned(alignment: Alignment): Line = line.copy(alignment = alignment.some)

    def leftAligned: Line = line.aligned(Alignment.Left)

    def centered: Line = line.aligned(Alignment.Center)

    def rightAligned: Line = line.aligned(Alignment.Right)

    /** Every span with the style the canvas writes: the line style patched by the span style. */
    def resolvedSpans: Vector[Span] = line.spans.map(span => span.copy(style = line.style.patch(span.style)))

  }

}
