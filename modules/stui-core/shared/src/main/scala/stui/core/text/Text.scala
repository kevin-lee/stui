package stui.core.text

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.geometry.{Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.core.widget.{Measurable, Widget}
import stui.unicode.WidthPolicy

/** Lines with a text-level style patch and an optional alignment that lines inherit when they have none
  * ([[Text.resolvedLines]]).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Text(lines: Vector[Line], style: Style, alignment: Option[Alignment]) extends Widget, Measurable derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the Widget trait member (the documented carve-out from the
   * extensions-in-companions rule) */
  /** Writes the [[Text.resolvedLines]] top to bottom into `area` intersected with the canvas area, one row each, every line rendered
    * with [[Line.render]] (so with its resolved style and alignment), the lines below the area dropped.
    */
  override def render(area: Rect, canvas: Canvas): Unit = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      ()
    } else {
      this.resolvedLines.take(target.height.value).zipWithIndex.foreach {
        case (line, i) =>
          line.render(Rect(target.x, NonNegInts.clamp(target.y.value.toLong + i.toLong), target.width, NonNegInt(1)), canvas)
      }
    }
  }

  /* the method lives in the class body because it implements the Measurable trait member (the documented carve-out from the
   * extensions-in-companions rule) */
  /** The widest line by the line count, both truncated by the constraints (D18, M2a). */
  override def measure(constraints: Size, policy: WidthPolicy): Size =
    Size(
      NonNegInts.clamp(math.min(this.width(policy).toLong, constraints.width.value.toLong)),
      NonNegInts.clamp(math.min(this.height.toLong, constraints.height.value.toLong)),
    )
}

object Text {

  /** Split on LF keeping every part (a trailing newline yields a final empty line, the empty string one empty line), a trailing CR of each
    * part is dropped.
    */
  def raw(content: String): Text = fromLines(splitLines(content).map(Line.raw))

  /** Raw lines with the style on the text. */
  def styled(content: String, style: Style): Text = Text(splitLines(content).map(Line.raw), style, none[Alignment])

  /** The given lines, no text style, no alignment. */
  def of(lines: Line*): Text = fromLines(lines.toVector)

  /** The given lines, no text style, no alignment. */
  def fromLines(lines: Vector[Line]): Text = Text(lines, Style.empty, none[Alignment])

  private def splitLines(content: String): Vector[String] =
    content.split("\n", -1).toVector.map(part => if (part.endsWith("\r")) part.dropRight(1) else part)

  extension (text: Text) {

    /** The widest line, 0 without lines. */
    def width(policy: WidthPolicy): Int = text.lines.map(_.width(policy)).maxOption.getOrElse(0)

    /** The number of lines. */
    def height: Int = text.lines.length

    /** The text with its text style patched by `style`. */
    def patchStyle(style: Style): Text = text.copy(style = text.style.patch(style))

    /** The text style only, the lines keep theirs. */
    def resetStyle: Text = text.copy(style = Style.empty)

    /** The text with the alignment set (lines without their own inherit it). */
    def aligned(alignment: Alignment): Text = text.copy(alignment = alignment.some)

    /** Every line with the text style patched by its own and the text alignment where it has none. */
    def resolvedLines: Vector[Line] =
      text.lines.map(line => line.copy(style = text.style.patch(line.style), alignment = line.alignment.orElse(text.alignment)))

  }

}
