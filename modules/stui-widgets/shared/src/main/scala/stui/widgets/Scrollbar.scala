package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.core.widget.{Measurable, Widget}
import stui.unicode.WidthPolicy

import scala.annotation.tailrec

/** Where a [[Scrollbar]]'s thumb sits along its track: the first track cell it covers and how many.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Thumb(start: NonNegInt, length: NonNegInt) derives Eq, Show, Hash

/** The scrollbar of the catalogue (design doc 6.7, M2b): a view of scroll state it does not own - `content` and `viewport` lengths
  * and the `position` (the offset) come in as values, built from a [[Scroll]], a [[LogViewState]], or a [[Selection]] through the
  * companion, so there is never a second source of truth. It runs along one edge of its area ([[ScrollbarOrientation]]) and draws
  * the begin glyph, the track with the thumb, and the end glyph; the arrows are dropped when the lane is shorter than three cells.
  * The thumb rule ([[Scrollbar.thumbOf]]): the position clamps as [[Scrolling.clampOffset]] does, the thumb length is
  * `track * viewport / content` rounded half up and at least one cell, its start `(track - length) * position / (content - viewport)`
  * rounded half up, and content that fits the viewport (an empty content included) gets a thumb over the whole track - never a
  * blank bar, a recorded divergence from Ratatui. `style` draws the track and arrows, `thumbStyle` is patched on top for the thumb.
  * The bar owns only its lane, so it implements `Measurable` (one column or row by the lane length).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Scrollbar(
  orientation: ScrollbarOrientation,
  content: NonNegInt,
  viewport: NonNegInt,
  position: NonNegInt,
  set: ScrollbarSet,
  style: Style,
  thumbStyle: Style,
) extends Widget,
      Measurable derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the Widget trait member */
  /** Draws the lane along the orientation's edge of the target: the begin glyph, the track cells with the thumb, the end glyph. An
    * empty target draws nothing.
    */
  override def render(area: Rect, canvas: Canvas): Unit = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      ()
    } else {
      val vertical = orientation.isVertical
      val extent   = if (vertical) target.height.value else target.width.value
      val beginLen = set.begin.fold(0)(_ => 1)
      val endLen   = set.end.fold(0)(_ => 1)
      val arrows   = extent >= beginLen + endLen + 1
      val track    = if (arrows) extent - beginLen - endLen else extent
      val thumb    = Scrollbar.thumbOf(NonNegInts.clamp(track.toLong), content, viewport, position)
      val originX  = orientation match {
        case ScrollbarOrientation.VerticalRight => target.right.value.toLong - 1L
        case ScrollbarOrientation.VerticalLeft | ScrollbarOrientation.HorizontalBottom | ScrollbarOrientation.HorizontalTop =>
          target.x.value.toLong
      }
      val originY  = orientation match {
        case ScrollbarOrientation.HorizontalBottom => target.bottom.value.toLong - 1L
        case ScrollbarOrientation.VerticalRight | ScrollbarOrientation.VerticalLeft | ScrollbarOrientation.HorizontalTop =>
          target.y.value.toLong
      }
      val lane     = Scrollbar.Lane(canvas, vertical, originX, originY)
      val trackAt  = if (arrows) beginLen else 0
      if (arrows) set.begin.foreach(glyph => lane.put(0, glyph, style)) else ()
      Scrollbar.renderTrack(this, lane, trackAt, thumb, 0, track)
      if (arrows) set.end.foreach(glyph => lane.put(extent - 1, glyph, style)) else ()
    }
  }

  /* the method lives in the class body because it implements the Measurable trait member */
  /** One column by the height for a vertical bar, the width by one row for a horizontal one, both truncated by the constraints. */
  override def measure(constraints: Size, policy: WidthPolicy): Size =
    if (orientation.isVertical) Size(NonNegInts.clamp(math.min(1L, constraints.width.value.toLong)), constraints.height)
    else Size(constraints.width, NonNegInts.clamp(math.min(1L, constraints.height.value.toLong)))
}

object Scrollbar {

  /** A bar on the right edge with the single-line vertical set and empty styles. */
  def vertical(content: NonNegInt, viewport: NonNegInt, position: NonNegInt): Scrollbar =
    Scrollbar(ScrollbarOrientation.VerticalRight, content, viewport, position, ScrollbarSet.vertical, Style.empty, Style.empty)

  /** A bar on the bottom edge with the single-line horizontal set and empty styles. */
  def horizontal(content: NonNegInt, viewport: NonNegInt, position: NonNegInt): Scrollbar =
    Scrollbar(ScrollbarOrientation.HorizontalBottom, content, viewport, position, ScrollbarSet.horizontal, Style.empty, Style.empty)

  /** The vertical bar of a [[ScrollView]] state: the content and viewport heights, the row offset. */
  def ofScroll(scroll: Scroll, contentSize: Size, viewportSize: Size): Scrollbar =
    vertical(contentSize.height, viewportSize.height, scroll.rows)

  /** The horizontal bar of a [[ScrollView]] state: the content and viewport widths, the column offset. */
  def ofScrollColumns(scroll: Scroll, contentSize: Size, viewportSize: Size): Scrollbar =
    horizontal(contentSize.width, viewportSize.width, scroll.columns)

  /** The vertical bar of a [[LogView]] state: the kept line count, the viewport rows, and the anchor relative to the ring start (the
    * newest window's anchor when following).
    */
  def ofLogView(ring: LogRing, state: LogViewState, rows: NonNegInt): Scrollbar = {
    val anchor = if (state.following) LogView.maxAnchor(ring, rows).value else state.anchor.value
    vertical(NonNegInts.clamp(ring.size.toLong), rows, NonNegInts.clamp(anchor - ring.firstIndex.value))
  }

  /** The vertical bar of a [[Selection]]: the item count, the viewport in items, and the offset. */
  def ofSelection(selection: Selection, count: NonNegInt, viewportItems: NonNegInt): Scrollbar =
    vertical(count, viewportItems, selection.offset)

  /** The thumb over a track of `track` cells: nothing on an empty track; the whole track when the content fits the viewport (an
    * empty content included); else the length `track * viewport / content` rounded half up, at least 1 and at most the track, at
    * the start `(track - length) * position / (content - viewport)` rounded half up with the position clamped to
    * `content - viewport`.
    */
  def thumbOf(track: NonNegInt, content: NonNegInt, viewport: NonNegInt, position: NonNegInt): Thumb = {
    val t = track.value.toLong
    val c = content.value.toLong
    val v = viewport.value.toLong
    if (t === 0L) {
      Thumb(NonNegInt(0), NonNegInt(0))
    } else if (c === 0L || c <= v) {
      Thumb(NonNegInt(0), track)
    } else {
      val pos    = Scrolling.clampOffset(position, content, viewport).value.toLong
      val length = math.max(1L, math.min(t, roundHalfUp(t * v, c)))
      val start  = roundHalfUp((t - length) * pos, c - v)
      Thumb(NonNegInts.clamp(start), NonNegInts.clamp(length))
    }
  }

  /** `a / b` rounded half up without doubling `a` (the products above stay within a `Long`, their double may not). */
  private def roundHalfUp(a: Long, b: Long): Long = {
    val quotient  = a / b
    val remainder = a % b
    if (2L * remainder >= b) quotient + 1L else quotient
  }

  extension (scrollbar: Scrollbar) {

    /** The bar with the orientation replaced (the set is kept; pick one with [[withSet]] or [[withSetFor]]). */
    def withOrientation(orientation: ScrollbarOrientation): Scrollbar = scrollbar.copy(orientation = orientation)

    /** The bar with the glyph set replaced. */
    def withSet(set: ScrollbarSet): Scrollbar = scrollbar.copy(set = set)

    /** The bar with the glyph set the capabilities select for its orientation ([[ScrollbarSet.forCapabilities]]). */
    def withSetFor(capabilities: Capabilities): Scrollbar =
      scrollbar.copy(set = ScrollbarSet.forCapabilities(scrollbar.orientation, capabilities))

    /** The bar with the track and arrow style replaced. */
    def withStyle(style: Style): Scrollbar = scrollbar.copy(style = style)

    /** The bar with the style patched over the thumb replaced. */
    def withThumbStyle(style: Style): Scrollbar = scrollbar.copy(thumbStyle = style)

  }

  /** The lane's cells addressed by their index along it. */
  final private case class Lane(canvas: Canvas, vertical: Boolean, originX: Long, originY: Long) {

    def put(k: Int, glyph: String, style: Style): Unit = {
      val position =
        if (vertical) Position(NonNegInts.clamp(originX), NonNegInts.clamp(originY + k.toLong))
        else Position(NonNegInts.clamp(originX + k.toLong), NonNegInts.clamp(originY))
      canvas.putString(position, glyph, style)
    }

  }

  @tailrec
  private def renderTrack(scrollbar: Scrollbar, lane: Lane, trackAt: Int, thumb: Thumb, k: Int, track: Int): Unit =
    if (k >= track) {
      ()
    } else {
      val onThumb = k >= thumb.start.value && k < thumb.start.value + thumb.length.value
      if (onThumb) lane.put(trackAt + k, scrollbar.set.thumb, scrollbar.style.patch(scrollbar.thumbStyle))
      else lane.put(trackAt + k, scrollbar.set.track, scrollbar.style)
      renderTrack(scrollbar, lane, trackAt, thumb, k + 1, track)
    }

}
