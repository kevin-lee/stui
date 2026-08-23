package stui.core.buffer

import cats.{Eq, Show}
import cats.syntax.all.*
import extras.render.Render
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.unicode.WidthPolicy
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** An immutable grid of [[Cell]]s over `area` (row-major, `cells.length == area.area`) with the [[WidthPolicy]] that measured its glyphs.
  * Mutation exists only inside [[Buffer.draw]]. Equality compares the area and the cells, not the policy. Buffers larger than
  * `Int.MaxValue` cells are not representable (the cell count is the area clamped to `Int.MaxValue`, allocation is the limit).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final class Buffer private[buffer] (val area: Rect, val policy: WidthPolicy, val cells: IArray[Cell])

object Buffer {

  /** Blank cells with [[WidthPolicy.default]]. */
  def empty(area: Rect): Buffer = emptyWith(WidthPolicy.default, area)

  def emptyWith(policy: WidthPolicy, area: Rect): Buffer = new Buffer(area, policy, IArray.fill(cellCount(area))(Cell.blank))

  /** [[Canvas.fill]] over the whole area with [[WidthPolicy.default]]. */
  def filled(area: Rect, symbol: String, style: Style): Buffer = filledWith(WidthPolicy.default, area, symbol, style)

  def filledWith(policy: WidthPolicy, area: Rect, symbol: String, style: Style): Buffer =
    emptyWith(policy, area).draw(_.fill(area, symbol, style))

  /** A buffer at the origin as wide as the widest line and as high as the number of lines, each line written with the empty style
    * (for fixtures and goldens, like Ratatui's `with_lines`), with [[WidthPolicy.default]].
    */
  def fromLines(lines: Vector[String]): Buffer = fromLinesWith(WidthPolicy.default, lines)

  def fromLinesWith(policy: WidthPolicy, lines: Vector[String]): Buffer = {
    val width  = NonNegInts.clamp(lines.map(line => policy.width(line).toLong).maxOption.getOrElse(0L))
    val height = NonNegInts.clamp(lines.length.toLong)
    emptyWith(policy, Rect(NonNegInt(0), NonNegInt(0), width, height)).draw { canvas =>
      lines.zipWithIndex.foreach {
        case (line, y) => canvas.putString(Position(NonNegInt(0), NonNegInts.clamp(y.toLong)), line, Style.empty)
      }
    }
  }

  private def cellCount(area: Rect): Int = math.min(area.area, Int.MaxValue.toLong).toInt

  /** The cells of `next` that differ from `prev`, in row-major order with strictly increasing index. When the areas differ every cell of
    * `next` is an update (a resize is a full redraw).
    */
  def diff(prev: Buffer, next: Buffer): Vector[CellUpdate] =
    if (prev.area =!= next.area) allCells(next) else diffLoop(prev, next, 0, Vector.empty[CellUpdate])

  private def allCells(buffer: Buffer): Vector[CellUpdate] =
    Vector.tabulate(buffer.cells.length)(i => CellUpdate(positionOf(buffer, i), buffer.cells(i)))

  @tailrec
  private def diffLoop(prev: Buffer, next: Buffer, i: Int, acc: Vector[CellUpdate]): Vector[CellUpdate] =
    if (i >= next.cells.length) {
      acc
    } else {
      val cell  = next.cells(i)
      val next2 = if (prev.cells(i) =!= cell) acc :+ CellUpdate(positionOf(next, i), cell) else acc
      diffLoop(prev, next, i + 1, next2)
    }

  private def positionOf(buffer: Buffer, i: Int): Position = {
    val w = buffer.area.width.value
    if (w === 0) {
      buffer.area.position
    } else {
      Position(
        NonNegInts.clamp(buffer.area.x.value.toLong + (i % w).toLong),
        NonNegInts.clamp(buffer.area.y.value.toLong + (i / w).toLong),
      )
    }
  }

  private def indexOf(buffer: Buffer, position: Position): Int =
    (position.y.value - buffer.area.y.value) * buffer.area.width.value + (position.x.value - buffer.area.x.value)

  /** `base` with every update inside its area written as is (updates outside are ignored). `applyUpdates(prev, diff(prev, next)) == next`
    * for buffers of the same area.
    */
  def applyUpdates(base: Buffer, updates: Vector[CellUpdate]): Buffer = {
    val copy = Array.tabulate(base.cells.length)(i => base.cells(i))
    updates.withFilter(update => base.area.contains(update.position)).foreach { update =>
      copy(indexOf(base, update.position)) = update.cell
    }
    /* the array does not escape this method */
    new Buffer(base.area, base.policy, IArray.unsafeFromArray(copy))
  }

  /** One `String` per row: the glyph symbols concatenated, continuations skipped, so each row's display width is the area width. */
  def renderRows(buffer: Buffer): Vector[String] = {
    val w = buffer.area.width.value
    Vector.tabulate(buffer.area.height.value)(y => renderRow(buffer.cells, y * w, (y + 1) * w, new java.lang.StringBuilder(w)))
  }

  @tailrec
  private def renderRow(cells: IArray[Cell], i: Int, end: Int, builder: java.lang.StringBuilder): String =
    if (i >= end) {
      builder.toString
    } else {
      val next = cells(i) match {
        case Cell.Glyph(symbol, _, _) => builder.append(symbol)
        case Cell.Continuation(_) => builder
      }
      renderRow(cells, i + 1, end, next)
    }

  /** Area and cells, the policy is ignored. */
  given eq: Eq[Buffer] =
    Eq.instance((a, b) => a.area === b.area && a.cells.length === b.cells.length && sameCells(a.cells, b.cells, 0))

  @tailrec
  private def sameCells(a: IArray[Cell], b: IArray[Cell], i: Int): Boolean =
    if (i >= a.length) true else if (a(i) =!= b(i)) false else sameCells(a, b, i + 1)

  /** For logs only: the area, then the rendered rows. */
  given show: Show[Buffer] = Show.show(buffer => (s"Buffer(area = ${buffer.area.show})" +: renderRows(buffer)).mkString("\n"))

  /** The textual grid, rows joined with LF. */
  given render: Render[Buffer] = Render.render(buffer => renderRows(buffer).mkString("\n"))

  extension (buffer: Buffer) {

    /** The cell at the position, `None` outside the area. */
    def cell(position: Position): Option[Cell] =
      Option.when(buffer.area.contains(position))(buffer.cells(indexOf(buffer, position)))

    /** Copies the cells, opens a [[Canvas]] on the copy, runs `f`, closes the canvas (so a leaked canvas is inert), and returns the new
      * buffer.
      */
    def draw(f: Canvas => Unit): Buffer = {
      val copy = Array.tabulate(buffer.cells.length)(i => buffer.cells(i))
      val open = Array(true)
      f(new Canvas(buffer.area, buffer.policy, copy, open))
      open(0) = false
      /* the canvas is closed, so nothing can reach the array again */
      new Buffer(buffer.area, buffer.policy, IArray.unsafeFromArray(copy))
    }

    def rows: Vector[Vector[Cell]] = {
      val w = buffer.area.width.value
      Vector.tabulate(buffer.area.height.value)(y => Vector.tabulate(w)(x => buffer.cells(y * w + x)))
    }

  }

}
