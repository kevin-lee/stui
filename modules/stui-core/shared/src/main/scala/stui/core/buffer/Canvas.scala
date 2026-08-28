package stui.core.buffer

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.frame.{Region, RegionId}
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.style.{CellStyle, Style}
import stui.core.text.{Line, Span}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** The one blessed mutable scope of the rendering pipeline (design principle 1): [[Buffer.draw]] opens a canvas over a copy of the cells,
  * runs the caller's block, closes the canvas, and wraps the cells into a new immutable buffer. Every operation returns `Unit`, clips to
  * the buffer area, maintains the [[Cell]] invariant, and does nothing once the canvas is closed, so a canvas that leaks out of the block
  * is inert. The `Array` updates inside this class are the documented mutation sites, index arithmetic is plain `Int`. Two operations
  * record values instead of cells (design doc 6.4, decision D18): [[cursor]] and [[region]], kept by `Frame.draw` and discarded by
  * `Buffer.draw`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final class Canvas private[buffer] (
  val area: Rect,
  val policy: WidthPolicy,
  cells: Array[Cell],
  open: Array[Boolean],
  cursorRef: AtomicReference[Option[Position]],
  regionsRef: AtomicReference[Vector[Region]],
) {

  private val areaX: Int = area.x.value

  private val areaY: Int = area.y.value

  private val width: Int = area.width.value

  private val right: Long = areaX.toLong + width.toLong

  private val bottom: Long = areaY.toLong + area.height.value.toLong

  private val ReplacementCharacter: Int = 0xfffd

  private def isOpen: Boolean = open(0)

  private def inside(x: Long, y: Long): Boolean = x >= areaX.toLong && x < right && y >= areaY.toLong && y < bottom

  private def index(x: Int, y: Int): Int = (y - areaY) * width + (x - areaX)

  /** The cell at the position, `None` outside the area. Reads still answer after the canvas is closed: the array is no longer
    * mutated then, so the answer is the finished buffer's cell.
    */
  def cell(position: Position): Option[Cell] =
    Option.when(inside(position.x.value.toLong, position.y.value.toLong))(cells(index(position.x.value, position.y.value)))

  /** [[putStringMax]] limited only by the right edge of the area. */
  def putString(position: Position, text: String, style: Style): Unit =
    putStringMax(position, text, style, NonNegInts.clamp(right - position.x.value.toLong))

  /** Writes the clusters of `text` left to right from the position, each cell's style being its current style patched by `style` (so text
    * with an fg-only style keeps a filled background). Zero-width clusters (controls, lone combining marks) are dropped, lone surrogates
    * become U+FFFD, and the first cluster that does not fit in `maxWidth` or before the right edge ends the write (a wide glyph at the
    * last column is dropped, not clipped). Nothing happens when the position is outside the area.
    */
  def putStringMax(position: Position, text: String, style: Style, maxWidth: NonNegInt): Unit =
    putSpans(List(Span.styled(text, style)), position, maxWidth)

  /** [[putStringMax]] with the span's content and style. */
  def putSpan(position: Position, span: Span, maxWidth: NonNegInt): Unit = putSpans(List(span), position, maxWidth)

  /** The line's [[Line.resolvedSpans]] written left to right within `maxWidth`. Alignment is ignored here (layout is the widgets' job). */
  def putLine(position: Position, line: Line, maxWidth: NonNegInt): Unit = putSpans(line.resolvedSpans.toList, position, maxWidth)

  private def putSpans(spans: List[Span], position: Position, maxWidth: NonNegInt): Unit = {
    val px = position.x.value.toLong
    val py = position.y.value.toLong
    if (isOpen && inside(px, py)) {
      val limit = math.min(maxWidth.value.toLong, right - px).toInt
      putSpansLoop(spans, px.toInt, py.toInt, limit)
    } else {
      ()
    }
  }

  @tailrec
  private def putSpansLoop(spans: List[Span], x: Int, y: Int, remaining: Int): Unit = spans match {
    case span :: rest =>
      val boundaries = Graphemes.boundaries(span.content)
      val nextX      = putClusters(span.content, boundaries, 0, x, y, remaining, span.style)
      putSpansLoop(rest, nextX, y, remaining - (nextX - x))
    case Nil => ()
  }

  /** Writes the clusters from boundary `i` on and returns the column after the last written cluster. */
  @tailrec
  private def putClusters(text: String, boundaries: Array[Int], i: Int, x: Int, y: Int, remaining: Int, style: Style): Int =
    if (i + 1 >= boundaries.length) {
      x
    } else {
      val start = boundaries(i)
      val end   = boundaries(i + 1)
      val w     = math.min(2, policy.clusterWidth(text, start, end))
      if (w === 0) {
        putClusters(text, boundaries, i + 1, x, y, remaining, style)
      } else if (w > remaining) {
        x
      } else {
        writeGlyph(x, y, sanitize(text.substring(start, end)), w, style)
        putClusters(text, boundaries, i + 1, x + w, y, remaining - w, style)
      }
    }

  /** Writes one glyph of width `w` (1 or 2) at `(x, y)`, which is inside the area with `x + w <= right`, wrapping the measured cluster as
    * a trusted [[GlyphSymbol]] and repairing the invariant around it: a continuation at `x` blanks its owner, a two-column glyph at `x` overwritten by a narrow one blanks its continuation, and a
    * two-column glyph at `x + 1` overwritten by the new continuation blanks its own continuation.
    */
  private def writeGlyph(x: Int, y: Int, symbol: String, w: Int, style: Style): Unit = {
    val i        = index(x, y)
    val newStyle = cells(i).style.patch(style)
    val glyph    = GlyphSymbol.trusted(symbol)
    cells(i) match {
      case Cell.Continuation(_) if x > areaX => cells(i - 1) = Cell.blank
      case Cell.Glyph(_, GlyphWidth.Two, _) if w === 1 && x.toLong + 1L < right => cells(i + 1) = Cell.blank
      case Cell.Continuation(_) | Cell.Glyph(_, _, _) => ()
    }
    if (w === 2) {
      cells(i + 1) match {
        case Cell.Glyph(_, GlyphWidth.Two, _) if x.toLong + 2L < right => cells(i + 2) = Cell.blank
        case Cell.Glyph(_, _, _) | Cell.Continuation(_) => ()
      }
      cells(i) = Cell.Glyph(glyph, GlyphWidth.Two, newStyle)
      cells(i + 1) = Cell.Continuation(newStyle)
    } else {
      cells(i) = Cell.Glyph(glyph, GlyphWidth.One, newStyle)
    }
  }

  /** Every cell of the intersection of `rect` with the area replaced (not patched) by the glyph with `CellStyle.default.patch(style)`: a
    * zero-width symbol gives blanks, a two-column symbol gives glyph / continuation pairs from the left with an odd last column blank
    * carrying the fill's style, and an empty `symbol` (no cluster) fills with blanks in the fill's style. Only the first cluster of
    * `symbol` is used. The cells just outside the left and right edges are repaired (an owner of a continuation at the left edge, a
    * continuation of a wide glyph at the right edge).
    */
  def fill(rect: Rect, symbol: String, style: Style): Unit =
    if (isOpen) {
      val target = rect.intersection(area)
      if (target.isEmpty) {
        ()
      } else {
        val boundaries = Graphemes.boundaries(symbol)
        val w          = if (boundaries.length < 2) 0 else math.min(2, policy.clusterWidth(symbol, boundaries(0), boundaries(1)))
        val glyph      = if (w === 0) GlyphSymbol.space else GlyphSymbol.trusted(sanitize(symbol.substring(boundaries(0), boundaries(1))))
        val cellStyle  = CellStyle.default.patch(style)
        val tx         = target.x.value
        val tw         = target.width.value
        fillRows(target.y.value, target.bottom.value, tx, tw, w, glyph, cellStyle)
      }
    } else {
      ()
    }

  @tailrec
  private def fillRows(y: Int, endY: Int, tx: Int, tw: Int, w: Int, glyph: GlyphSymbol, cellStyle: CellStyle): Unit =
    if (y >= endY) {
      ()
    } else {
      repairEdges(y, tx, tw)
      fillRow(y, tx, tx + tw, w, glyph, cellStyle)
      fillRows(y + 1, endY, tx, tw, w, glyph, cellStyle)
    }

  @tailrec
  private def fillRow(y: Int, x: Int, endX: Int, w: Int, glyph: GlyphSymbol, cellStyle: CellStyle): Unit =
    if (x >= endX) {
      ()
    } else if (w === 2 && x + 1 < endX) {
      cells(index(x, y)) = Cell.Glyph(glyph, GlyphWidth.Two, cellStyle)
      cells(index(x + 1, y)) = Cell.Continuation(cellStyle)
      fillRow(y, x + 2, endX, w, glyph, cellStyle)
    } else if (w === 1) {
      cells(index(x, y)) = Cell.Glyph(glyph, GlyphWidth.One, cellStyle)
      fillRow(y, x + 1, endX, w, glyph, cellStyle)
    } else {
      cells(index(x, y)) = Cell.blankWith(cellStyle)
      fillRow(y, x + 1, endX, w, glyph, cellStyle)
    }

  /** Before a row range `[tx, tx + tw)` is overwritten: the owner of a continuation at `tx` and the continuation of a wide glyph at
    * `tx + tw - 1` lie outside the range and become blank.
    */
  private def repairEdges(y: Int, tx: Int, tw: Int): Unit = {
    cells(index(tx, y)) match {
      case Cell.Continuation(_) if tx > areaX => cells(index(tx - 1, y)) = Cell.blank
      case Cell.Continuation(_) | Cell.Glyph(_, _, _) => ()
    }
    val lastX = tx + tw - 1
    cells(index(lastX, y)) match {
      case Cell.Glyph(_, GlyphWidth.Two, _) if lastX.toLong + 1L < right => cells(index(lastX + 1, y)) = Cell.blank
      case Cell.Glyph(_, _, _) | Cell.Continuation(_) => ()
    }
  }

  /** Every cell of the intersection becomes [[Cell.blank]], with the same edge repairs as [[fill]]. */
  def clear(rect: Rect): Unit = fill(rect, " ", Style.empty)

  /** Records where the terminal cursor goes after the present (a text input's caret). A position outside the area is dropped, the last
    * call wins, nothing happens once the canvas is closed.
    */
  def cursor(position: Position): Unit =
    if (isOpen && inside(position.x.value.toLong, position.y.value.toLong)) cursorRef.set(position.some) else ()

  /** Records `rect` intersected with the area as a hit region tagged with `id`, in draw order (`Regions.at` lets the last one win).
    * An empty intersection records nothing, and nothing happens once the canvas is closed.
    */
  def region(id: RegionId, rect: Rect): Unit =
    if (isOpen) {
      val target = rect.intersection(area)
      if (target.isEmpty) () else regionsRef.updateAndGet(_ :+ Region(id, target)): Unit
    } else {
      ()
    }

  /** The recorded cursor, read by `Frame.draw` after the render. */
  private[core] def recordedCursor: Option[Position] = cursorRef.get()

  /** The recorded regions in draw order, read by `Frame.draw` after the render. */
  private[core] def recordedRegions: Vector[Region] = regionsRef.get()

  /** Every cell of the intersection gets its style patched. Styling either column of a two-column glyph styles the glyph, so the owner of
    * a continuation at the left edge and the continuation of a wide glyph at the right edge are patched as well.
    */
  def patchStyle(rect: Rect, style: Style): Unit =
    if (isOpen) {
      val target = rect.intersection(area)
      if (target.isEmpty) () else patchRows(target.y.value, target.bottom.value, target.x.value, target.width.value, style)
    } else {
      ()
    }

  @tailrec
  private def patchRows(y: Int, endY: Int, tx: Int, tw: Int, style: Style): Unit =
    if (y >= endY) {
      ()
    } else {
      patchRow(y, tx, tx + tw, style)
      cells(index(tx, y)) match {
        case Cell.Continuation(_) if tx > areaX => patchCell(index(tx - 1, y), style)
        case Cell.Continuation(_) | Cell.Glyph(_, _, _) => ()
      }
      val lastX = tx + tw - 1
      cells(index(lastX, y)) match {
        case Cell.Glyph(_, GlyphWidth.Two, _) if lastX.toLong + 1L < right => patchCell(index(lastX + 1, y), style)
        case Cell.Glyph(_, _, _) | Cell.Continuation(_) => ()
      }
      patchRows(y + 1, endY, tx, tw, style)
    }

  @tailrec
  private def patchRow(y: Int, x: Int, endX: Int, style: Style): Unit =
    if (x >= endX) {
      ()
    } else {
      patchCell(index(x, y), style)
      patchRow(y, x + 1, endX, style)
    }

  private def patchCell(i: Int, style: Style): Unit =
    cells(i) = cells(i) match {
      case Cell.Glyph(symbol, width, cellStyle) => Cell.Glyph(symbol, width, cellStyle.patch(style))
      case Cell.Continuation(cellStyle) => Cell.Continuation(cellStyle.patch(style))
    }

  /** The cluster unchanged when it is valid UTF-16, otherwise a copy with every unpaired surrogate replaced by U+FFFD. */
  private def sanitize(cluster: String): String =
    if (hasUnpairedSurrogate(cluster, 0)) replaceUnpaired(cluster, 0, new java.lang.StringBuilder(cluster.length)) else cluster

  @tailrec
  private def hasUnpairedSurrogate(s: String, i: Int): Boolean =
    if (i >= s.length) {
      false
    } else {
      val cp = s.codePointAt(i)
      if (isSurrogate(cp)) true else hasUnpairedSurrogate(s, i + Character.charCount(cp))
    }

  @tailrec
  private def replaceUnpaired(s: String, i: Int, builder: java.lang.StringBuilder): String =
    if (i >= s.length) {
      builder.toString
    } else {
      val cp = s.codePointAt(i)
      replaceUnpaired(s, i + Character.charCount(cp), builder.appendCodePoint(if (isSurrogate(cp)) ReplacementCharacter else cp))
    }

  private def isSurrogate(cp: Int): Boolean = cp >= 0xd800 && cp <= 0xdfff

}
