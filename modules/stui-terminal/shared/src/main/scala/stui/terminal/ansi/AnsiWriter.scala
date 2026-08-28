package stui.terminal.ansi

import cats.syntax.all.*
import stui.core.buffer.{Buffer, Cell, CellUpdate, GlyphSymbol}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.spi.TerminalOptions
import stui.core.style.CellStyle
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The pure ANSI writer with the explicit cursor model of design doc 7.1 (decision D16). Every function maps a [[WriterState]] and its
  * input to the next state and the text to send (its UTF-8 encoding is the wire form). Rules, each a law over the testkit's
  * `TerminalModel` oracle:
  *
  *   - R1: after a write in the last column the tracked cursor is `PendingWrap`, and the next move is absolute.
  *   - R2: consecutive updates advance by the glyph's terminal width without a cursor move, a `Continuation` update whose owner was
  *     not the previous emitted wide glyph is painted as a space in the default style.
  *   - R3: a wide glyph is emitted with its continuation column accounted for, and a wide glyph whose shadow would fall outside the
  *     viewport is painted as a space in its style.
  *   - R4: a control never reaches the terminal as a glyph (impossible by the `GlyphSymbol` type).
  *   - R5: only the SGR parameters that differ from the tracked style are emitted, on the capability-normalised style, and the style is
  *     reset at the end of a present when it is not the default.
  *   - R6 and R7 (the DEC 2026 bracket and the scroll-region reset) are M1f, the state carries their fields.
  *   - R8: nothing is emitted for an update outside the viewport.
  *   - R9 is the orchestration's (cleanup errors outrank emission errors).
  *   - R10: the text of a present is at most [[MaxBytesPerCell]] bytes per update plus the UTF-8 length of the glyph symbols written
  *     plus [[MaxBytesPerPresent]].
  *
  * A VS16 cluster advances by `Capabilities.vs16Width`, and when that is narrower than the cell the shadow column is painted with a
  * space in the cell's style, so the tracked cursor and the screen agree on every terminal (design doc 9.3).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object AnsiWriter {

  /** The byte budget per update (rule R10): a Cursor Position of at most 24 bytes for ten-digit coordinates, an SGR delta of at most
    * 128 bytes, and the shadow or orphan space.
    */
  val MaxBytesPerCell: Int = 160

  /** The byte budget per present beyond the updates (rule R10): the final style reset. */
  val MaxBytesPerPresent: Int = 8

  final private case class Acc(cursor: CursorState, style: CellStyle, lastWide: Option[Position])

  /** Emits the updates of one present over the viewport (whose origin is the screen row `state.origin`) and returns the state with the
    * tracked cursor after the last write and the default style.
    */
  def present(state: WriterState, capabilities: Capabilities, viewport: Rect, updates: Vector[CellUpdate]): (WriterState, String) = {
    val builder = new java.lang.StringBuilder
    val end     = loop(
      IArray.from(updates),
      0,
      Acc(state.cursor, state.style, none[Position]),
      builder,
      capabilities,
      viewport.width.value,
      viewport.height.value,
      state.origin.value,
    )
    if (end.style =!= CellStyle.default) builder.append(Sequences.SgrReset): Unit else ()
    (state.copy(cursor = end.cursor, style = CellStyle.default), builder.toString)
  }

  /** A Cursor Position to the position clamped into the viewport, nothing when the tracked cursor is already there. */
  def moveCursor(state: WriterState, viewport: Rect, position: Position): (WriterState, String) = {
    val x      = math.max(0, math.min(position.x.value, viewport.width.value - 1))
    val y      = math.max(0, math.min(position.y.value, viewport.height.value - 1))
    val target = Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong))
    state.cursor match {
      case CursorState.Known(current) if current === target => (state, "")
      case CursorState.Known(_) | CursorState.PendingWrap(_) | CursorState.Unknown =>
        (state.copy(cursor = CursorState.Known(target)), Sequences.cup(state.origin.value + y + 1, x + 1))
    }
  }

  /** DECTCEM set, the state is unchanged (the orchestration tracks visibility). */
  def showCursor(state: WriterState): (WriterState, String) = (state, Sequences.CursorShow)

  /** DECTCEM reset, the state is unchanged. */
  def hideCursor(state: WriterState): (WriterState, String) = (state, Sequences.CursorHide)

  /** Erase in Display and home, the tracked cursor becomes the viewport origin. */
  def clear(state: WriterState): (WriterState, String) =
    (state.copy(cursor = CursorState.Known(Position.origin)), Sequences.ClearScreen + Sequences.CursorHome)

  /** The entry sequence of [[Sequences.enter]] and the initial state with the cursor known at the origin (the sequence homes it). */
  def enter(options: TerminalOptions): (WriterState, String) =
    (WriterState.initial.copy(cursor = CursorState.Known(Position.origin)), Sequences.enter(options))

  /** The fixed reset of every exit path, [[Sequences.SafeReset]]. */
  val exit: String = Sequences.SafeReset

  /** The rows as styled lines at the cursor: each row's cells left to right with style deltas (the same cell emitter, no cursor moves),
    * a style reset when needed, then CR LF. The tracked cursor is unknown afterwards.
    */
  def print(state: WriterState, capabilities: Capabilities, rows: Buffer): (WriterState, String) = {
    val builder = new java.lang.StringBuilder
    val width   = rows.area.width.value
    val style   = printRows(rows.cells, 0, width, rows.area.height.value, state.style, builder, capabilities)
    (state.copy(cursor = CursorState.Unknown, style = style), builder.toString)
  }

  @tailrec
  private def printRows(
    cells: IArray[Cell],
    y: Int,
    width: Int,
    height: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
  ): CellStyle =
    if (y >= height) {
      style
    } else {
      val after = printCells(cells, y * width, (y + 1) * width, style, builder, capabilities)
      val reset =
        if (after =!= CellStyle.default) {
          builder.append(Sequences.SgrReset): Unit
          CellStyle.default
        } else {
          after
        }
      builder.append(Sequences.CrLf): Unit
      printRows(cells, y + 1, width, height, reset, builder, capabilities)
    }

  @tailrec
  private def printCells(
    cells: IArray[Cell],
    i: Int,
    end: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
  ): CellStyle =
    if (i >= end) {
      style
    } else {
      val next = cells(i) match {
        case Cell.Continuation(_) => style
        case Cell.Glyph(symbol, width, cellStyle) =>
          val tracked       = ensureStyle(style, builder, capabilities, cellStyle)
          val terminalWidth = if (symbol.isVs16Sequence) capabilities.vs16Width.columns else width.columns
          builder.append(symbol.value): Unit
          appendSpaces(builder, width.columns - terminalWidth)
          tracked
      }
      printCells(cells, i + 1, end, next, builder, capabilities)
    }

  @tailrec
  private def loop(
    updates: IArray[CellUpdate],
    i: Int,
    acc: Acc,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    width: Int,
    height: Int,
    origin: Int,
  ): Acc =
    if (i >= updates.length) {
      acc
    } else {
      val update = updates(i)
      val x      = update.position.x.value
      val y      = update.position.y.value
      val next   =
        if (x >= width || y >= height) {
          acc
        } else {
          update.cell match {
            case Cell.Continuation(_) =>
              if (acc.lastWide.exists(owner => owner.x.value === x - 1 && owner.y.value === y)) acc
              else emitGlyph(acc, builder, capabilities, width, origin, x, y, GlyphSymbol.space, 1, CellStyle.default, 1)
            case Cell.Glyph(symbol, glyphWidth, style) =>
              val columns = glyphWidth.columns
              if (columns === 2 && x + 1 >= width) {
                emitGlyph(acc, builder, capabilities, width, origin, x, y, GlyphSymbol.space, 1, style, 1)
              } else {
                val terminalWidth = if (symbol.isVs16Sequence) capabilities.vs16Width.columns else columns
                emitGlyph(acc, builder, capabilities, width, origin, x, y, symbol, columns, style, terminalWidth)
              }
          }
        }
      loop(updates, i + 1, next, builder, capabilities, width, height, origin)
    }

  private def emitGlyph(
    acc: Acc,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    width: Int,
    origin: Int,
    x: Int,
    y: Int,
    symbol: GlyphSymbol,
    columns: Int,
    style: CellStyle,
    terminalWidth: Int,
  ): Acc = {
    val cursor  = ensureCursor(acc.cursor, builder, origin, x, y)
    val tracked = ensureStyle(acc.style, builder, capabilities, style)
    builder.append(symbol.value): Unit
    appendSpaces(builder, columns - terminalWidth)
    val nx      = x + columns
    val after   =
      if (nx >= width) CursorState.PendingWrap(NonNegInts.clamp(y.toLong))
      else CursorState.Known(Position(NonNegInts.clamp(nx.toLong), NonNegInts.clamp(y.toLong)))
    val wide    = if (columns === 2) Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)).some else none[Position]
    val _       = cursor
    Acc(after, tracked, wide)
  }

  private def ensureCursor(cursor: CursorState, builder: java.lang.StringBuilder, origin: Int, x: Int, y: Int): CursorState =
    cursor match {
      case CursorState.Known(current) if current.x.value === x && current.y.value === y => cursor
      case CursorState.Known(_) | CursorState.PendingWrap(_) | CursorState.Unknown =>
        builder.append(Sequences.cup(origin + y + 1, x + 1)): Unit
        CursorState.Known(Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)))
    }

  private def ensureStyle(
    tracked: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    style: CellStyle,
  ): CellStyle = {
    val normalised = Sgr.normalise(capabilities, style)
    if (normalised === tracked) {
      tracked
    } else {
      builder.append(Sgr.delta(tracked, normalised, capabilities)): Unit
      normalised
    }
  }

  @tailrec
  private def appendSpaces(builder: java.lang.StringBuilder, n: Int): Unit =
    if (n <= 0) {
      ()
    } else {
      builder.append(' '): Unit
      appendSpaces(builder, n - 1)
    }

  /** The UTF-8 length of the glyph symbols in the updates, the `symbolBytes` term of rule R10. */
  def symbolBytes(updates: Vector[CellUpdate]): Long =
    updates.foldLeft(0L) { (acc, update) =>
      update.cell match {
        case Cell.Glyph(symbol, _, _) => acc + utf8Length(symbol.value)
        case Cell.Continuation(_) => acc
      }
    }

  /** The UTF-8 length of a string without encoding it (a lone surrogate counts 3 bytes, as the replacement character would). */
  def utf8Length(s: String): Long = utf8Loop(s, 0, 0L)

  @tailrec
  private def utf8Loop(s: String, i: Int, acc: Long): Long =
    if (i >= s.length) {
      acc
    } else {
      val cp    = s.codePointAt(i)
      val bytes = if (cp < 0x80) 1L else if (cp < 0x800) 2L else if (cp < 0x10000) 3L else 4L
      utf8Loop(s, i + Character.charCount(cp), acc + bytes)
    }

}
