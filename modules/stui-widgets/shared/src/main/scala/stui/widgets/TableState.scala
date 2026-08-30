package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.internal.NonNegInts

/** What a [[Table]] remembers between frames (design doc 6.6, M2b): the row [[Selection]] (the `ListView` state shape, corrected by
  * the same rule) and an optional selected column, clamped to the column count at render. The row helpers delegate to [[Selection]],
  * the column helpers mirror them with the same provisional "last" index.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class TableState(rows: Selection, column: Option[NonNegInt]) derives Eq, Show, Hash

object TableState {

  /** No row selected, no column selected, the first row displayed first. */
  val none: TableState = TableState(Selection.none, Option.empty[NonNegInt])

  /** The row selection with no column selected. */
  def of(rows: Selection): TableState = TableState(rows, Option.empty[NonNegInt])

  extension (state: TableState) {

    /** [[Selection.select]] on the rows. */
    def selectRow(index: Option[NonNegInt]): TableState = state.copy(rows = state.rows.select(index))

    /** [[Selection.deselect]] on the rows. */
    def deselectRow: TableState = state.copy(rows = state.rows.deselect)

    /** [[Selection.selectFirst]] on the rows. */
    def selectFirstRow: TableState = state.copy(rows = state.rows.selectFirst)

    /** [[Selection.selectLast]] on the rows. */
    def selectLastRow: TableState = state.copy(rows = state.rows.selectLast)

    /** [[Selection.selectNext]] on the rows. */
    def selectNextRow: TableState = state.copy(rows = state.rows.selectNext)

    /** [[Selection.selectPrevious]] on the rows. */
    def selectPreviousRow: TableState = state.copy(rows = state.rows.selectPrevious)

    /** [[Selection.movedBy]] on the rows. */
    def rowsMovedBy(delta: Int): TableState = state.copy(rows = state.rows.movedBy(delta))

    /** [[Selection.scrolledBy]] on the rows. */
    def rowsScrolledBy(delta: Int): TableState = state.copy(rows = state.rows.scrolledBy(delta))

    /** The column selection replaced (`None` deselects). */
    def selectColumn(index: Option[NonNegInt]): TableState = state.copy(column = index)

    /** No column selected. */
    def deselectColumn: TableState = state.copy(column = Option.empty[NonNegInt])

    /** The first column selected. */
    def selectFirstColumn: TableState = state.copy(column = Some(NonNegInt(0)))

    /** The last column selected: a provisional `NonNegInt.MaxValue` that render clamps to the column count. */
    def selectLastColumn: TableState = state.copy(column = Some(NonNegInt.MaxValue))

    /** The next column: the first when none is selected, else the index plus one (saturated; render clamps). */
    def selectNextColumn: TableState = state.copy(column = Some(state.column.fold(NonNegInt(0))(c => NonNegInts.offset(c, 1))))

    /** The previous column: the last (provisional) when none is selected, else the index minus one floored at 0. */
    def selectPreviousColumn: TableState =
      state.copy(column = Some(state.column.fold(NonNegInt.MaxValue)(c => NonNegInts.offset(c, -1))))

  }

}
