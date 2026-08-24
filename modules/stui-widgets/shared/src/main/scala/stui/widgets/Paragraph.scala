package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.geometry.Rect
import stui.core.style.Style
import stui.core.text.{Alignment, Line, Text}
import stui.core.widget.Widget
import stui.unicode.WidthPolicy
import stui.widgets.internal.WordWrap

/** Styled, optionally wrapped, optionally scrolled text in an optional [[Block]] (Ratatui's `Paragraph`). Alignment lives on the text
  * and its lines (a line's alignment wins over the text's), not on the paragraph (M1d decision 5.1). With `wrap` set the text
  * word-wraps at the text area width and `scroll.rows` skips wrapped rows (`scroll.columns` is ignored). Without `wrap` every line
  * truncates at the width and `scroll.columns` drops leading display columns of left-aligned lines only.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Paragraph(
  text: Text,
  block: Option[Block],
  style: Style,
  wrap: Option[Wrap],
  scroll: Scroll,
) extends Widget derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the Widget trait member */
  /** Patches `style` over the whole area intersected with the canvas, renders the block, and renders the display lines into the
    * block's inner area (the whole area without a block).
    */
  override def render(area: Rect, canvas: Canvas): Unit = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      ()
    } else {
      canvas.patchStyle(target, style)
      block.foreach(b => b.render(target, canvas))
      val textArea = block.fold(target)(b => b.inner(target))
      if (textArea.isEmpty) {
        ()
      } else {
        Text(Paragraph.displayLines(this, textArea, canvas.policy), Style.empty, none[Alignment]).render(textArea, canvas)
      }
    }
  }
}

object Paragraph {

  /** The text alone: no block, no area style, no wrap (truncation), no scroll. */
  def of(text: Text): Paragraph = Paragraph(text, none[Block], Style.empty, none[Wrap], Scroll.none)

  /** [[of]] over [[Text.raw]]. */
  def raw(content: String): Paragraph = of(Text.raw(content))

  /** [[of]] over [[Text.styled]]. */
  def styled(content: String, style: Style): Paragraph = of(Text.styled(content, style))

  extension (paragraph: Paragraph) {

    /** The paragraph in the block. */
    def withBlock(block: Block): Paragraph = paragraph.copy(block = block.some)

    /** The paragraph with the area style replaced. */
    def withStyle(style: Style): Paragraph = paragraph.copy(style = style)

    /** The paragraph wrapping its lines. */
    def withWrap(wrap: Wrap): Paragraph = paragraph.copy(wrap = wrap.some)

    /** The paragraph truncating its lines (the default). */
    def noWrap: Paragraph = paragraph.copy(wrap = none[Wrap])

    /** The paragraph with the scroll offset replaced. */
    def withScroll(scroll: Scroll): Paragraph = paragraph.copy(scroll = scroll)

    /** The paragraph with the text alignment set (lines with their own keep it). */
    def aligned(alignment: Alignment): Paragraph = paragraph.copy(text = paragraph.text.aligned(alignment))

    /** [[aligned]] with [[Alignment.Left]]. */
    def leftAligned: Paragraph = paragraph.aligned(Alignment.Left)

    /** [[aligned]] with [[Alignment.Center]]. */
    def centered: Paragraph = paragraph.aligned(Alignment.Center)

    /** [[aligned]] with [[Alignment.Right]]. */
    def rightAligned: Paragraph = paragraph.aligned(Alignment.Right)

    /** The number of display rows the text produces at the given width: the wrapped row count with `wrap` set (0 at width 0), the
      * line count without. The block is not counted (M1d decision R4), pair with [[Block.inner]] or `Rect.inset` when sizing.
      */
    def lineCount(width: NonNegInt, policy: WidthPolicy): Int = paragraph.wrap match {
      case Some(wrap) => WordWrap.wrap(paragraph.text.resolvedLines, width.value, trims(wrap), policy).length
      case None => paragraph.text.height
    }

    /** The widest line of the text. The block is not counted (M1d decision R4). */
    def lineWidth(policy: WidthPolicy): Int = paragraph.text.width(policy)

  }

  private def trims(wrap: Wrap): Boolean = wrap match {
    case Wrap.Word => false
    case Wrap.WordTrimmed => true
  }

  /** The rows to draw: wrapped (rows scrolled after wrapping) or truncated (lines scrolled, then cut with the horizontal skip). */
  private def displayLines(paragraph: Paragraph, textArea: Rect, policy: WidthPolicy): Vector[Line] = {
    val width = textArea.width.value
    val from  = paragraph.scroll.rows.value
    val until = math.min(from.toLong + textArea.height.value.toLong, Int.MaxValue.toLong).toInt
    paragraph.wrap match {
      case Some(wrap) =>
        WordWrap.wrap(paragraph.text.resolvedLines, width, trims(wrap), policy).slice(from, until)
      case None =>
        paragraph
          .text
          .resolvedLines
          .slice(from, until)
          .map(line => WordWrap.truncate(line, width, paragraph.scroll.columns.value, policy))
    }
  }

}
