package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Position, Size}
import stui.core.layout.{Constraint, Flex}
import stui.core.style.{Modifier, Style}
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.GeometryGens

/** The [[Table]] golden grids: fixed and equal widths, the symbol column, the column highlight, the footer, multi-line rows, wide
  * cells, margins, and flex placement.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object TableGoldensSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val fixed: Vector[Constraint] = Vector(Constraint.length(3), Constraint.length(4))

  private val twoRows: Table = Table.of(Vector(Row.raw("a", "b"), Row.raw("c", "d")))

  private def grid(table: Table, viewport: Size, state: TableState, expected: Vector[String]): Result =
    Rendering.stateful(table, viewport, state) match {
      case (buffer, _) => Assertions.grid(buffer, expected)
    }

  override def tests: List[Test] = List(
    example(
      "a header and fixed widths",
      grid(
        twoRows.withWidths(fixed).withHeader(Row.raw("h1", "h2")),
        size(8, 3),
        TableState.none,
        Vector("h1  h2  ", "a   b   ", "c   d   "),
      ),
    ),
    example(
      "equal widths share the width after the spacing",
      grid(
        Table.of(Vector(Row.raw("abcd", "efgh"), Row.raw("1234", "5678"))),
        size(9, 2),
        TableState.none,
        Vector("abcd efgh", "1234 5678"),
      ),
    ),
    example(
      "the selected row carries the symbol",
      grid(twoRows.withHighlightSymbol(Line.raw(">>")), size(10, 2), TableState.of(Selection.at(nn(1))), Vector("  a    b  ", ">>c    d  ")),
    ),
    example(
      "a footer pins to the bottom",
      grid(
        Table.of(Vector(Row.raw("a", "b"))).withWidths(fixed).withHeader(Row.raw("h1", "h2")).withFooter(Row.raw("f1", "f2")),
        size(8, 4),
        TableState.none,
        Vector("h1  h2  ", "a   b   ", "        ", "f1  f2  "),
      ),
    ),
    example(
      "a two-line row takes two rows",
      grid(
        Table.of(Vector(Row.raw("a\nb", "x"), Row.raw("c", "y"))).withWidths(Vector(Constraint.length(3), Constraint.length(1))),
        size(6, 3),
        TableState.none,
        Vector("a   x ", "b     ", "c   y "),
      ),
    ),
    example(
      "a wide cell truncates at its column",
      grid(Table.of(Vector(Row.raw("한글abc"))).withWidths(Vector(Constraint.length(4))), size(4, 1), TableState.none, Vector("한글")),
    ),
    example(
      "a top margin leaves a blank row",
      grid(
        Table.of(Vector(Row.raw("a").withTopMargin(nn(1)), Row.raw("b"))),
        size(6, 3),
        TableState.none,
        Vector("      ", "a     ", "b     "),
      ),
    ),
    example(
      "Flex.End pushes the columns right",
      grid(
        Table.of(Vector(Row.raw("a", "b"))).withWidths(Vector(Constraint.length(2), Constraint.length(2))).withFlex(Flex.End),
        size(8, 1),
        TableState.none,
        Vector("   a  b "),
      ),
    ),
    example(
      "a bordered table",
      grid(
        twoRows.withWidths(fixed).withBlock(Block.bordered),
        size(10, 4),
        TableState.none,
        Vector("┌────────┐", "│a   b   │", "│c   d   │", "└────────┘"),
      ),
    ),
    example("the column highlight lands on body cells only", testColumnHighlight),
  )

  def testColumnHighlight: Result = {
    val table  = twoRows
      .withWidths(fixed)
      .withHeader(Row.raw("h1", "h2"))
      .withColumnHighlightStyle(Style.empty.bold)
      .withCellHighlightStyle(Style.empty.italic)
    val buffer = Rendering.stateful(table, size(8, 3), TableState(Selection.at(nn(1)), Some(nn(1)))) match {
      case (rendered, _) => rendered
    }
    def has(x: Int, y: Int, modifier: Modifier): Boolean =
      buffer
        .cell(Position(GeometryGens.nonNegOrZero(x.toLong), GeometryGens.nonNegOrZero(y.toLong)))
        .exists(_.style.modifiers.contains(modifier))
    Result.all(
      List(
        Result.assert(!has(4, 0, Modifier.Bold)).log("the header cell is highlighted"),
        Result.assert(has(4, 1, Modifier.Bold)).log("the first body cell of the column is not highlighted"),
        Result.assert(has(4, 2, Modifier.Bold)).log("the second body cell of the column is not highlighted"),
        Result.assert(!has(0, 1, Modifier.Bold)).log("another column is highlighted"),
        Result.assert(has(4, 2, Modifier.Italic)).log("the selected cell lacks the cell highlight"),
        Result.assert(!has(4, 1, Modifier.Italic)).log("an unselected row's cell has the cell highlight"),
      )
    )
  }

}
