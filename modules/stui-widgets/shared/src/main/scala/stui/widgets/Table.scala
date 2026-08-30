package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.layout.{Constraint, Direction, Flex, Layout, Spacing}
import stui.core.style.Style
import stui.core.text.Line
import stui.core.widget.StatefulWidget
import stui.widgets.internal.ItemWindow

import scala.annotation.tailrec

/** The table of the catalogue (design doc 6.7, M2b): rows of cells laid out in columns sized by the layout constraints of 9.2
  * (`widths`, one per column; equal `Fill(1)` columns when empty) with `columnSpacing` between them and the excess placed by `flex`,
  * an optional header pinned at the top and footer at the bottom, and a [[TableState]] corrected during render: the row selection
  * follows the [[ListView]] rule of 6.6 over row extents (height plus margins), the column selection clamps to the column count. The
  * highlight symbol column follows [[HighlightSpacing]] as in `ListView` (the symbol on the selected row's first line, blanks
  * elsewhere, the header and footer included). Styling order per body row: the row's style, the cells, `rowHighlightStyle` over a
  * selected row, `columnHighlightStyle` over the selected column, `cellHighlightStyle` over their intersection; the header and footer
  * take no highlight. Per-column alignment is each cell's text alignment. Recorded divergences: a row's default height is its
  * tallest cell, and the footer is dropped when the header and footer together exceed the inner height. With a `region`, the base
  * covers the whole target and every drawn body row is tagged `base[index]` ([[ItemRegions]]). The table owns its area, so it does
  * not implement `Measurable` (design doc 6.4, R3).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Table(
  rows: Vector[Row],
  widths: Vector[Constraint],
  columnSpacing: NonNegInt,
  flex: Flex,
  header: Option[Row],
  footer: Option[Row],
  block: Option[Block],
  style: Style,
  rowHighlightStyle: Style,
  columnHighlightStyle: Style,
  cellHighlightStyle: Style,
  highlightSymbol: Option[Line],
  highlightSpacing: HighlightSpacing,
  scrollPadding: NonNegInt,
  region: Option[RegionId],
) extends StatefulWidget[TableState] derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the StatefulWidget trait member */
  /** Patches `style` over the target, renders the block, records the base region, splits the columns, corrects the state against the
    * body height (the inner height minus the header and footer extents), draws the header, the visible body rows, and the footer, and
    * returns the corrected state. An empty target or inner area corrects nothing and returns the state unchanged; a body of zero rows
    * leaves the row selection unchanged too.
    */
  override def render(area: Rect, canvas: Canvas, state: TableState): TableState = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      state
    } else {
      canvas.patchStyle(target, style)
      block.foreach(_.render(target, canvas))
      val inner = block.fold(target)(_.inner(target))
      region.foreach(base => canvas.region(base, target))
      if (inner.isEmpty) {
        state
      } else {
        val columnCount   = Table.columnCount(this)
        val column        =
          if (columnCount === 0) none[NonNegInt]
          else state.column.map(c => NonNegInts.clamp(math.min(c.value.toLong, (columnCount - 1).toLong)))
        val headerExtent  = header.fold(0)(_.extent)
        val footerExtent  = footer.fold(0)(_.extent)
        val bodyHeight    = math.max(0, inner.height.value - headerExtent - footerExtent)
        val corrected     =
          if (bodyHeight === 0) state.rows
          else ItemWindow.correct(state.rows, rows.map(_.extent), bodyHeight, scrollPadding.value)
        val symbolWidth   = highlightSymbol.fold(0)(_.width(canvas.policy))
        val reserve       = highlightSpacing match {
          case HighlightSpacing.Always => true
          case HighlightSpacing.WhenSelected => corrected.selected.isDefined
          case HighlightSpacing.Never => false
        }
        val symbolColumns = if (reserve) math.min(symbolWidth, inner.width.value) else 0
        val contentX      = NonNegInts.offset(inner.x, symbolColumns)
        val contentWidth  = NonNegInts.clamp(inner.width.value.toLong - symbolColumns.toLong)
        val constraints   = if (widths.isEmpty) Vector.fill(columnCount)(Constraint.fill(1)) else widths
        val columns       = Layout
          .of(Direction.Horizontal, constraints)
          .withSpacing(Spacing.spaceOf(columnSpacing))
          .withFlex(flex)
          .split(Rect(contentX, inner.y, contentWidth, NonNegInt(1)))
        val context       = Table.Context(this, canvas, inner, columns, column, symbolColumns)
        val top           = inner.y.value.toLong
        val bottom        = inner.bottom.value.toLong
        val bodyTop       = top + headerExtent.toLong
        header.foreach(row => Table.renderRow(context, row, top, math.min(bodyTop, bottom), false, none[Int]))
        Table.renderBody(context, corrected, corrected.offset.value, bodyTop, bodyTop + bodyHeight.toLong)
        if (inner.height.value >= headerExtent + footerExtent) {
          footer.foreach(row => Table.renderRow(context, row, bottom - footerExtent.toLong, bottom, false, none[Int]))
        } else {
          ()
        }
        TableState(corrected, column)
      }
    }
  }
}

object Table {

  /** The rows alone: equal columns, one column of spacing, [[stui.core.layout.Flex.Start]], no header, footer, block, or region,
    * empty styles, no symbol, [[HighlightSpacing.WhenSelected]], no padding.
    */
  def of(rows: Vector[Row]): Table =
    Table(
      rows,
      Vector.empty[Constraint],
      NonNegInt(1),
      Flex.Start,
      none[Row],
      none[Row],
      none[Block],
      Style.empty,
      Style.empty,
      Style.empty,
      Style.empty,
      none[Line],
      HighlightSpacing.WhenSelected,
      NonNegInt(0),
      none[RegionId],
    )

  extension (table: Table) {

    /** The table with the column constraints replaced (empty means equal columns). */
    def withWidths(widths: Vector[Constraint]): Table = table.copy(widths = widths)

    /** The table with the spacing between columns replaced. */
    def withColumnSpacing(spacing: NonNegInt): Table = table.copy(columnSpacing = spacing)

    /** The table with the placement of the excess width replaced. */
    def withFlex(flex: Flex): Table = table.copy(flex = flex)

    /** The table with the header row pinned at the top. */
    def withHeader(row: Row): Table = table.copy(header = row.some)

    /** The table with the footer row pinned at the bottom. */
    def withFooter(row: Row): Table = table.copy(footer = row.some)

    /** The table in the block. */
    def withBlock(block: Block): Table = table.copy(block = block.some)

    /** The table with the area style replaced. */
    def withStyle(style: Style): Table = table.copy(style = style)

    /** The table with the style patched over the selected row replaced. */
    def withRowHighlightStyle(style: Style): Table = table.copy(rowHighlightStyle = style)

    /** The table with the style patched over the selected column's body cells replaced. */
    def withColumnHighlightStyle(style: Style): Table = table.copy(columnHighlightStyle = style)

    /** The table with the style patched over the selected cell replaced. */
    def withCellHighlightStyle(style: Style): Table = table.copy(cellHighlightStyle = style)

    /** The table with the symbol drawn in front of the selected row. */
    def withHighlightSymbol(symbol: Line): Table = table.copy(highlightSymbol = symbol.some)

    /** The table with the symbol-column rule replaced. */
    def withHighlightSpacing(spacing: HighlightSpacing): Table = table.copy(highlightSpacing = spacing)

    /** The rows of context kept visible around the selected row where they fit. */
    def withScrollPadding(padding: NonNegInt): Table = table.copy(scrollPadding = padding)

    /** The table recording its base region and one region per drawn body row under the id ([[ItemRegions]]). */
    def withRegion(id: RegionId): Table = table.copy(region = id.some)

    /** The number of body rows. */
    def size: Int = table.rows.length

  }

  /** The column count: the number of constraints when given, else the widest row (header and footer included). */
  private def columnCount(table: Table): Int =
    if (table.widths.nonEmpty) table.widths.length
    else (table.rows ++ table.header.toList ++ table.footer.toList).map(_.columnCount).maxOption.getOrElse(0)

  /** What every row of one render shares. */
  final private case class Context(
    table: Table,
    canvas: Canvas,
    inner: Rect,
    columns: Vector[Rect],
    column: Option[NonNegInt],
    symbolColumns: Int,
  )

  @tailrec
  private def renderBody(context: Context, corrected: Selection, i: Int, y: Long, limit: Long): Unit =
    if (y >= limit) {
      ()
    } else {
      context.table.rows.lift(i) match {
        case None => ()
        case Some(row) =>
          renderRow(context, row, y, limit, corrected.selected.exists(_.value === i), i.some)
          renderBody(context, corrected, i + 1, y + row.extent.toLong, limit)
      }
    }

  /** One row whose top margin starts at `y`, cut at `limit`: the row style, the cells, the symbol column, the highlights (body rows
    * only, `index` defined), and the row region.
    */
  private def renderRow(context: Context, row: Row, y: Long, limit: Long, selected: Boolean, index: Option[Int]): Unit = {
    val rowTop  = y + row.topMargin.value.toLong
    val visible = math.min(row.height.value.toLong, limit - rowTop)
    if (visible <= 0L) {
      ()
    } else {
      val canvas  = context.canvas
      val inner   = context.inner
      val rowRect = Rect(inner.x, NonNegInts.clamp(rowTop), inner.width, NonNegInts.clamp(visible))
      canvas.patchStyle(rowRect, row.style)
      context.columns.zipWithIndex.foreach {
        case (column, j) => row.cells.lift(j).foreach(_.render(Rect(column.x, rowRect.y, column.width, rowRect.height), canvas))
      }
      renderSymbol(context, rowRect, selected, 0)
      if (selected) canvas.patchStyle(rowRect, context.table.rowHighlightStyle) else ()
      index.foreach { i =>
        context.column.flatMap(c => context.columns.lift(c.value)).foreach { column =>
          val cellRect = Rect(column.x, rowRect.y, column.width, rowRect.height)
          canvas.patchStyle(cellRect, context.table.columnHighlightStyle)
          if (selected) canvas.patchStyle(cellRect, context.table.cellHighlightStyle) else ()
        }
        context.table.region.foreach(base => canvas.region(ItemRegions.of(base, NonNegInts.clamp(i.toLong)), rowRect))
      }
    }
  }

  /** The symbol column of one row: the symbol on the first line of a selected body row, blanks elsewhere. */
  @tailrec
  private def renderSymbol(context: Context, rowRect: Rect, selected: Boolean, r: Int): Unit =
    if (context.symbolColumns === 0 || r >= rowRect.height.value) {
      ()
    } else {
      val position = Position(rowRect.x, NonNegInts.offset(rowRect.y, r))
      val width    = NonNegInts.clamp(context.symbolColumns.toLong)
      context.table.highlightSymbol match {
        case Some(symbol) if selected && r === 0 => context.canvas.putLine(position, symbol, width)
        case Some(_) | None => context.canvas.putStringMax(position, " " * context.symbolColumns, Style.empty, width)
      }
      renderSymbol(context, rowRect, selected, r + 1)
    }

}
