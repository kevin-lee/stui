package stui.widgets

import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Size
import stui.core.internal.NonNegInts

/** The scrolling semantics of the scroll-view family, stated once (design doc 6.6, decision D21, M2a): offsets are cells (rows and
  * columns of the rendered content), clamping happens during render through the returned state - the largest useful offset is
  * `max(0, content - viewport)` per axis - content shorter than its viewport anchors at the top left with offset 0, and a resize
  * simply re-clamps a saved offset at the next render. Item-based offsets (the M2b widget catalogue) reuse [[maxOffset]] and
  * [[clampOffset]] over item counts instead of cells.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object Scrolling {

  /** The largest useful offset: `content - viewport` floored at 0 (0 when the content fits the viewport). */
  def maxOffset(content: NonNegInt, viewport: NonNegInt): NonNegInt =
    NonNegInts.clamp(content.value.toLong - viewport.value.toLong)

  /** `offset` clamped into `0..maxOffset(content, viewport)`. */
  def clampOffset(offset: NonNegInt, content: NonNegInt, viewport: NonNegInt): NonNegInt = {
    val max = maxOffset(content, viewport)
    if (offset.value <= max.value) offset else max
  }

  /** Both axes clamped: rows against the heights, columns against the widths. */
  def clampScroll(scroll: Scroll, content: Size, viewport: Size): Scroll =
    Scroll(
      clampOffset(scroll.rows, content.height, viewport.height),
      clampOffset(scroll.columns, content.width, viewport.width),
    )

  /** The offset moved by the deltas, floored at 0 and saturated at `Int.MaxValue` per axis. The upper clamp is render's job
    * ([[clampScroll]] through the returned state), so "to the end" is a huge delta plus the render clamp.
    */
  def scrolledBy(scroll: Scroll, deltaRows: Int, deltaColumns: Int): Scroll =
    Scroll(
      NonNegInts.clamp(scroll.rows.value.toLong + deltaRows.toLong),
      NonNegInts.clamp(scroll.columns.value.toLong + deltaColumns.toLong),
    )

}
