package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.frame.{Frame, RegionId}
import stui.core.geometry.{Position, Rect, Size}
import stui.testkit.{Assertions, Rendering}
import stui.testkit.laws.WidgetLaws
import stui.widgets.gen.WidgetGens

/** The [[Table]] contract laws, the fixed point, the row and column corrections, the pinned header and footer, the regions, and the
  * [[TableState]] transitions (design doc 6.7, M2b).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object TableSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private def sel(offset: Int, selected: Option[Int]): Selection =
    Selection(NonNegInt.from(offset).getOrElse(NonNegInt(0)), selected.flatMap(i => NonNegInt.from(i).toOption))

  private val last: Option[NonNegInt] = Some(NonNegInt.MaxValue)

  /** r1 x, r2 y, r3 z. */
  private val three: Table = Table.of(Vector(Row.raw("r1", "x"), Row.raw("r2", "y"), Row.raw("r3", "z")))

  private val header: Row = Row.raw("h", "H")

  private def corrected(table: Table, viewport: Size, state: TableState): TableState =
    Rendering.stateful(table, viewport, state) match {
      case (_, next) => next
    }

  private def rendered(table: Table, viewport: Size, state: TableState): Buffer =
    Rendering.stateful(table, viewport, state) match {
      case (buffer, _) => buffer
    }

  override def tests: List[Test] =
    WidgetLaws.laws("table", WidgetGens.tableWidget, outer) ++
      WidgetLaws.stateLaw("table", WidgetGens.tableInputs, outer) ++
      List(
        example(
          "a row selection beyond the rows clamps and the offset follows",
          Assertions.eqv(corrected(three, size(10, 2), TableState.of(sel(0, Some(9)))), TableState.of(sel(1, Some(2)))),
        ),
        example(
          "a column beyond the columns clamps",
          Assertions.eqv(corrected(three, size(10, 3), TableState(Selection.none, Some(nn(7)))), TableState(Selection.none, Some(nn(1)))),
        ),
        example(
          "no columns give no column",
          Assertions.eqv(corrected(Table.of(Vector.empty[Row]), size(10, 3), TableState(Selection.none, Some(nn(0)))), TableState.none),
        ),
        example(
          "a header takes a row and the rows scroll below it",
          Assertions
            .grid(rendered(three.withHeader(header), size(10, 3), TableState.none), Vector("h     H   ", "r1    x   ", "r2    y   ")),
        ),
        example(
          "the footer is dropped when it would overlap the header",
          Assertions
            .grid(rendered(three.withHeader(header).withFooter(Row.raw("f", "F")), size(10, 1), TableState.none), Vector("h     H   ")),
        ),
        example(
          "an empty area returns the state unchanged",
          Assertions.eqv(corrected(three, size(0, 0), TableState(sel(9, Some(9)), Some(nn(9)))), TableState(sel(9, Some(9)), Some(nn(9)))),
        ),
        example(
          "an empty inner area returns the state unchanged",
          Assertions.eqv(
            corrected(three.withBlock(Block.bordered), size(2, 2), TableState(sel(9, Some(9)), Some(nn(9)))),
            TableState(sel(9, Some(9)), Some(nn(9))),
          ),
        ),
        example(
          "a body of zero rows leaves the row selection unchanged",
          Assertions.eqv(
            corrected(three.withHeader(header), size(10, 1), TableState(sel(9, Some(9)), Some(nn(9)))),
            TableState(sel(9, Some(9)), Some(nn(1))),
          ),
        ),
        example("body rows are tagged under the base region, the header answers the base", testRegions),
        example("the column helpers mirror the row helpers", testColumns),
      )

  def testRegions: Result = {
    val base  = RegionId("table")
    val area  = Rect(nn(0), nn(0), nn(10), nn(4))
    val frame =
      Frame.draw(Buffer.empty(area))(canvas => three.withHeader(header).withRegion(base).render(area, canvas, TableState.none): Unit)
    Result.all(
      List(
        Assertions.eqv(frame.regions.at(Position(nn(0), nn(0))), Option(base)),
        Assertions.eqv(frame.regions.at(Position(nn(0), nn(1))), Option(ItemRegions.of(base, nn(0)))),
        Assertions.eqv(frame.regions.at(Position(nn(9), nn(2))), Option(ItemRegions.of(base, nn(1)))),
        Assertions.eqv(frame.regions.rectsOf(base), Vector(area)),
      )
    )
  }

  def testColumns: Result =
    Result.all(
      List(
        Assertions.eqv(TableState.none.selectNextColumn.column, Option(nn(0))),
        Assertions.eqv(TableState.none.selectNextColumn.selectNextColumn.column, Option(nn(1))),
        Assertions.eqv(TableState.none.selectPreviousColumn.column, last),
        Assertions.eqv(TableState.none.selectFirstColumn.selectPreviousColumn.column, Option(nn(0))),
        Assertions.eqv(TableState.none.selectLastColumn.column, last),
        Assertions.eqv(TableState.none.selectFirstColumn.deselectColumn.column, Option.empty[NonNegInt]),
        Assertions.eqv(TableState.none.selectNextRow.rows, Selection.first),
        Assertions.eqv(TableState.none.selectLastRow.rows, Selection(nn(0), last)),
        Assertions.eqv(TableState.of(Selection.at(nn(3))).rowsMovedBy(-1).rows, Selection.at(nn(2))),
        Assertions.eqv(TableState.of(Selection.at(nn(3))).selectPreviousRow.deselectRow.rows, Selection.none),
      )
    )

}
