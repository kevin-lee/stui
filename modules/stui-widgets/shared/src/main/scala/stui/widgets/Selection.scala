package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.internal.NonNegInts

/** The item-offset state shared by [[ListView]], [[Tabs]], and [[TableState]] (design doc 6.6, M2b), the item analogue of the
  * cell-based [[Scroll]]: `offset` is the index of the first displayed item and `selected` the highlighted one, if any. Render corrects
  * both through the returned state (the offset clamped to the largest useful one, the selection clamped to the item count and kept
  * visible), so the transitions below never need the item count and may leave provisional indexes such as `NonNegInt.MaxValue` for
  * "the last item".
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Selection(offset: NonNegInt, selected: Option[NonNegInt]) derives Eq, Show, Hash

object Selection {

  /** Nothing selected, the first item displayed first. */
  val none: Selection = Selection(NonNegInt(0), Option.empty[NonNegInt])

  /** The first item selected and displayed first. */
  val first: Selection = Selection(NonNegInt(0), Some(NonNegInt(0)))

  /** The item at `index` selected, the first item displayed first (render scrolls the selection into view). */
  def at(index: NonNegInt): Selection = Selection(NonNegInt(0), Some(index))

  extension (selection: Selection) {

    /** The selection replaced (`None` deselects); the offset is kept. */
    def select(index: Option[NonNegInt]): Selection = selection.copy(selected = index)

    /** Nothing selected; the offset is kept (Ratatui's documented `select(None)` resets it to 0, a recorded divergence). */
    def deselect: Selection = selection.copy(selected = Option.empty[NonNegInt])

    /** The first item selected. */
    def selectFirst: Selection = selection.copy(selected = Some(NonNegInt(0)))

    /** The last item selected: a provisional `NonNegInt.MaxValue` that render clamps to the item count. */
    def selectLast: Selection = selection.copy(selected = Some(NonNegInt.MaxValue))

    /** The next item: the first when nothing is selected, else the index plus one (saturated; render clamps to the count). */
    def selectNext: Selection =
      selection.copy(selected = Some(selection.selected.fold(NonNegInt(0))(i => NonNegInts.offset(i, 1))))

    /** The previous item: the last (provisional) when nothing is selected, else the index minus one floored at 0. */
    def selectPrevious: Selection =
      selection.copy(selected = Some(selection.selected.fold(NonNegInt.MaxValue)(i => NonNegInts.offset(i, -1))))

    /** The selection (0 when none) moved by `delta`, floored at 0 and saturated; render clamps the top. */
    def movedBy(delta: Int): Selection =
      selection.copy(selected = Some(NonNegInts.offset(selection.selected.getOrElse(NonNegInt(0)), delta)))

    /** The offset moved by `delta`, floored at 0 and saturated. With a selection outside the new window, render re-anchors the offset
      * to the selection.
      */
    def scrolledBy(delta: Int): Selection = selection.copy(offset = NonNegInts.offset(selection.offset, delta))

  }

}
