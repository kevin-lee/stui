package stui.widgets.internal

import cats.syntax.all.*
import stui.core.internal.NonNegInts
import stui.widgets.{Scrolling, Selection}

import scala.annotation.tailrec

/** The item-offset correction of the catalogue (design doc 6.6, M2b), stated once for [[stui.widgets.ListView]] (item heights),
  * [[stui.widgets.Table]] (row extents), and [[stui.widgets.Tabs]] (tab widths): the viewport is measured in the same unit as the
  * extents, the largest useful offset is `Scrolling.maxOffset(count, trailingFit)`, the selection clamps to the count, the offset
  * clamps to the largest useful one, and a selection is kept visible with `padding` items of context where they fit.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
private[widgets] object ItemWindow {

  /** The largest number of trailing items whose extents sum to at most `viewport`, and at least 1 when there are items (the last item
    * counts as fitting even when it is larger than the viewport, so it can be shown partially).
    */
  def trailingFit(extents: Vector[Int], viewport: Int): Int =
    if (extents.isEmpty) 0 else math.max(1, fitFromEnd(IArray.from(extents), extents.length - 1, viewport.toLong, 0))

  @tailrec
  private def fitFromEnd(extents: IArray[Int], i: Int, remaining: Long, count: Int): Int =
    if (i < 0) {
      count
    } else {
      val next = remaining - extents(i).toLong
      if (next < 0L) count else fitFromEnd(extents, i - 1, next, count + 1)
    }

  /** The corrected selection: `Selection.none` without items; else the selection clamped to the last index, the offset clamped to the
    * largest useful one, and with a selection `s` the offset lowered to `s - pad` when it was above, or raised to the smallest offset
    * at which the items from it up to `s + pad` (both clamped to the items) fit the viewport, `s` itself when none does. `pad` is
    * `padding` clamped to `(viewport - 1) / 2`. The result is a fixed point of this function (the `StatefulWidget` law): the search
    * is monotone, so a second pass finds the same offset.
    */
  def correct(selection: Selection, extents: Vector[Int], viewport: Int, padding: Int): Selection = {
    val n = extents.length
    if (n === 0) {
      Selection.none
    } else {
      val selected = selection.selected.map(i => NonNegInts.clamp(math.min(i.value.toLong, (n - 1).toLong)))
      val fit      = trailingFit(extents, viewport)
      val offset0  = Scrolling.clampOffset(selection.offset, NonNegInts.clamp(n.toLong), NonNegInts.clamp(fit.toLong)).value
      val offset   = selected match {
        case None => offset0
        case Some(s) =>
          val pad  = math.max(0, math.min(padding, (viewport - 1) / 2))
          val low  = math.max(0, s.value - pad)
          val high = math.min(n - 1, s.value + pad)
          smallestFitting(IArray.from(extents), math.min(offset0, low), s.value, high, viewport.toLong)
      }
      Selection(NonNegInts.clamp(offset.toLong), selected)
    }
  }

  /** The smallest `o` in `o..selected` whose window `o..high` fits the viewport, or `selected`. */
  @tailrec
  private def smallestFitting(extents: IArray[Int], o: Int, selected: Int, high: Int, viewport: Long): Int =
    if (o >= selected) selected
    else if (sumOf(extents, o, high, 0L) <= viewport) o
    else smallestFitting(extents, o + 1, selected, high, viewport)

  @tailrec
  private def sumOf(extents: IArray[Int], from: Int, to: Int, acc: Long): Long =
    if (from > to) acc else sumOf(extents, from + 1, to, acc + extents(from).toLong)

}
