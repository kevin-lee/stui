package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.capability.{Capabilities, GlyphSet}
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.core.text.{Alignment, Line, Span}
import stui.core.widget.{Measurable, StatefulWidget}
import stui.unicode.WidthPolicy
import stui.widgets.internal.ItemWindow

import scala.annotation.tailrec

/** The tab strip of the catalogue (design doc 6.7, M2b): one row of titles, each padded by `paddingLeft` and `paddingRight` and
  * separated by `divider`, drawn from the first row of the area. The state is a [[Selection]] over the titles, corrected during render:
  * the strip scrolls through the item-offset rule of 6.6 (a tab's extent is its padded width plus its trailing divider) so the
  * selected tab is always visible - Ratatui truncates instead, a recorded divergence. `style` is the base style of every span of the
  * strip and `highlightStyle` is patched over the selected title's spans only (the paddings and dividers keep `style`). Title
  * alignment is ignored (the spans are concatenated). The strip owns nothing beyond the cells it writes, so it implements `Measurable`
  * (the natural strip width by one row) and has no block field: compose with `Block.inner` (decision D21).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Tabs(
  titles: Vector[Line],
  divider: Span,
  paddingLeft: Span,
  paddingRight: Span,
  style: Style,
  highlightStyle: Style,
  region: Option[RegionId],
) extends StatefulWidget[Selection],
      Measurable derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the StatefulWidget trait member */
  /** Records the base region over the strip, corrects the state against the strip width, writes the tabs from the corrected offset
    * (truncated at the strip's right edge, each tagged `base[index]`), and returns the corrected state. An empty target returns the
    * state unchanged; no titles correct to `Selection.none`.
    */
  override def render(area: Rect, canvas: Canvas, state: Selection): Selection = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      state
    } else {
      val strip     = Rect(target.x, target.y, target.width, NonNegInt(1))
      region.foreach(base => canvas.region(base, strip))
      val corrected = ItemWindow.correct(state, Tabs.extents(this, canvas.policy), strip.width.value, 0)
      Tabs.renderTabs(this, canvas, strip, corrected, corrected.offset.value, strip.x.value.toLong)
      corrected
    }
  }

  /* the method lives in the class body because it implements the Measurable trait member */
  /** The natural strip width (every padded title plus the dividers between them) by one row, both truncated by the constraints; a
    * narrower area scrolls the strip inside it.
    */
  override def measure(constraints: Size, policy: WidthPolicy): Size =
    Size(
      NonNegInts.clamp(
        math.min(Tabs.extents(this, policy).foldLeft(0L)((acc, extent) => acc + extent.toLong), constraints.width.value.toLong)
      ),
      NonNegInts.clamp(math.min(1L, constraints.height.value.toLong)),
    )
}

object Tabs {

  /** The vertical line divider (U+2502) of the Unicode glyph set. */
  private val UnicodeDivider: Span = Span.raw("│")

  /** The ASCII divider. */
  private val AsciiDivider: Span = Span.raw("|")

  /** The titles alone: the Unicode divider, one-space paddings, no style, the selected title reversed, no region. */
  def of(titles: Line*): Tabs = fromLines(titles.toVector)

  /** [[of]] over a vector. */
  def fromLines(titles: Vector[Line]): Tabs =
    Tabs(titles, UnicodeDivider, Span.raw(" "), Span.raw(" "), Style.empty, Style.empty.reversed, none[RegionId])

  /** [[of]] over [[stui.core.text.Line.raw]] titles. */
  def raw(titles: String*): Tabs = fromLines(titles.toVector.map(Line.raw))

  /** The divider the capabilities select (design doc 7.3): the vertical line under Unicode glyphs, `|` otherwise. */
  def dividerFor(capabilities: Capabilities): Span = capabilities.effectiveGlyphs match {
    case GlyphSet.Unicode => UnicodeDivider
    case GlyphSet.Ascii => AsciiDivider
  }

  extension (tabs: Tabs) {

    /** The strip with the divider replaced. */
    def withDivider(divider: Span): Tabs = tabs.copy(divider = divider)

    /** The strip with the divider the capabilities select ([[dividerFor]]). */
    def withDividerFor(capabilities: Capabilities): Tabs = tabs.copy(divider = dividerFor(capabilities))

    /** The strip with both paddings replaced. */
    def withPadding(left: Span, right: Span): Tabs = tabs.copy(paddingLeft = left, paddingRight = right)

    /** The strip with the base style replaced. */
    def withStyle(style: Style): Tabs = tabs.copy(style = style)

    /** The strip with the style patched over the selected title replaced. */
    def withHighlightStyle(style: Style): Tabs = tabs.copy(highlightStyle = style)

    /** The strip recording its base region and one region per drawn tab under the id ([[ItemRegions]]). */
    def withRegion(id: RegionId): Tabs = tabs.copy(region = id.some)

    /** The number of titles. */
    def size: Int = tabs.titles.length

  }

  /** A title's padded width. */
  private def tabWidth(tabs: Tabs, title: Line, policy: WidthPolicy): Int =
    tabs.paddingLeft.width(policy) + title.width(policy) + tabs.paddingRight.width(policy)

  /** Every tab's extent: its padded width plus the trailing divider (none after the last tab). */
  private[widgets] def extents(tabs: Tabs, policy: WidthPolicy): Vector[Int] = {
    val n            = tabs.titles.length
    val dividerWidth = tabs.divider.width(policy)
    tabs.titles.zipWithIndex.map {
      case (title, i) => tabWidth(tabs, title, policy) + (if (i < n - 1) dividerWidth else 0)
    }
  }

  @tailrec
  private def renderTabs(tabs: Tabs, canvas: Canvas, strip: Rect, corrected: Selection, i: Int, x: Long): Unit = {
    val right = strip.x.value.toLong + strip.width.value.toLong
    if (x >= right) {
      ()
    } else {
      tabs.titles.lift(i) match {
        case None => ()
        case Some(title) =>
          val policy   = canvas.policy
          val selected = corrected.selected.exists(_.value === i)
          val spans    =
            title.resolvedSpans.map(span => if (selected) span.copy(style = tabs.highlightStyle.patch(span.style)) else span)
          val line     = Line((tabs.paddingLeft +: spans) :+ tabs.paddingRight, tabs.style, none[Alignment])
          canvas.putLine(Position(NonNegInts.clamp(x), strip.y), line, NonNegInts.clamp(right - x))
          val width    = tabWidth(tabs, title, policy).toLong
          tabs
            .region
            .foreach(base =>
              canvas.region(
                ItemRegions.of(base, NonNegInts.clamp(i.toLong)),
                Rect(NonNegInts.clamp(x), strip.y, NonNegInts.clamp(math.min(width, right - x)), NonNegInt(1)),
              )
            )
          val afterTab = x + width
          val next     =
            if (i < tabs.titles.length - 1 && afterTab < right) {
              canvas.putLine(
                Position(NonNegInts.clamp(afterTab), strip.y),
                Line(Vector(tabs.divider), tabs.style, none[Alignment]),
                NonNegInts.clamp(right - afterTab),
              )
              afterTab + tabs.divider.width(policy).toLong
            } else {
              afterTab
            }
          renderTabs(tabs, canvas, strip, corrected, i + 1, next)
      }
    }
  }

}
