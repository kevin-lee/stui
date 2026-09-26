package stui.terminal.ansi

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Cell, CellUpdate, GlyphSymbol}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.spi.TerminalOptions
import stui.core.style.CellStyle
import stui.terminal.kitty.KittyFlags
import stui.unicode.Graphemes
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The pure ANSI writer with the explicit cursor model of design doc 7.1 (decision D16). Every function maps a [[WriterState]] and its
  * input to the next state and the text to send (its UTF-8 encoding is the wire form). The viewport is a screen-coordinate `Rect`
  * (plan refinement R1 of M1f): update positions are absolute rows, the whole terminal on the alternate screen and the last rows of
  * the normal screen in inline mode, always spanning the terminal width. Rules, each a law over the testkit's `TerminalModel` oracle:
  *
  *   - R1: after a write in the last column the tracked cursor is `PendingWrap` (`Unknown` after a closing VS16 cluster, R3a), and the
  *     next move is absolute.
  *   - R2: consecutive updates advance by the glyph's terminal width without a cursor move, a `Continuation` update whose owner was
  *     not the previous emitted wide glyph is painted as a space in the default style.
  *   - R2a (2026-08-31): a glyph that would join the previously emitted cluster into one grapheme cluster on the wire (a lone
  *     regional indicator after another, a standalone mark after any glyph) is preceded by an explicit cursor placement - CUP in a
  *     present, CHA in a printed row - so the terminal keeps the cells apart ([[stui.unicode.Graphemes.joins]]); the budget of R10
  *     already covers it.
  *   - R3: a wide glyph is emitted with its continuation column accounted for, and a wide glyph whose shadow would fall outside the
  *     viewport is painted as a space in its style.
  *   - R3a (2026-09-24, issue 42): a two-column VS16 cluster is written the same way whatever width the terminal gives it. Two spaces
  *     in the cell's style go over its two cells, the cursor is placed back on its first cell (CUP in a present, CHA in a printed row),
  *     the cluster follows, and the next write is placed explicitly: the tracked cursor becomes `Unknown` in a present, and a printed
  *     row places its next cell or its closing erase with CHA. The screen then matches the buffer whether the terminal advances one
  *     column or two. iTerm2 3.7.3 advances one on the alternate screen and two on the normal screen by default.
  *   - R4: a control never reaches the terminal as a glyph (impossible by the `GlyphSymbol` type).
  *   - R5: only the SGR parameters that differ from the tracked style are emitted, on the capability-normalised style, and the style is
  *     reset at the end of a present when it is not the default.
  *   - R6: the DEC 2026 bracket wraps the flush (the backend emits it, plan refinement R9), never any safe reset.
  *   - R7: DECSTBM homes the cursor, so the region is only ever set or reset inside the DECSC / DECRC bracket
  *     ([[Sequences.armRegion]], [[Sequences.resetRegion]], [[exitInline]]).
  *   - R8: nothing is emitted for an update outside the viewport.
  *   - R9 is the orchestration's (cleanup errors outrank emission errors).
  *   - R10: the text of a present is at most [[MaxBytesPerCell]] bytes per update plus the UTF-8 length of the glyph symbols written
  *     plus [[MaxBytesPerPresent]]; the text of a print is at most [[MaxBytesPerCell]] per emitted cell plus the symbol bytes plus
  *     [[MaxBytesPerPrintedRow]] per row plus [[MaxBytesPerPrint]] (the overlay adds at most the terminal height in scroll line
  *     feeds).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object AnsiWriter {

  /** The byte budget per update (rule R10): a Cursor Position of at most 24 bytes for ten-digit coordinates (or the CHA of a join
    * break, at most 15), an SGR delta of at most 128 bytes, the orphan or cut-wide space, and for a VS16 cluster (R3a) its two blanking
    * spaces and the placement back (at most 24): 24 + 128 + 2 + 24 = 178, rounded up.
    */
  val MaxBytesPerCell: Int = 184

  /** The byte budget per present beyond the updates (rule R10): the final style reset. */
  val MaxBytesPerPresent: Int = 8

  /** The byte budget per printed row beyond its cells (rule R10): the row separator (2), a style reset (4), the CHA before the erase
    * after a closing VS16 cluster (R3a, at most 15), and Erase in Line (3).
    */
  val MaxBytesPerPrintedRow: Int = 24

  /** The byte budget per print beyond the rows (rule R10): one Cursor Position (at most 24) and Erase in Display (3). */
  val MaxBytesPerPrint: Int = 32

  /** `last` is the last emitted cluster of the run (`" "` after an orphan or cut-wide space, `""` after a cursor placement or a VS16
    * cluster).
    */
  final private case class Acc(cursor: CursorState, style: CellStyle, lastWide: Option[Position], last: String)

  /** Emits the updates of one present over the screen-coordinate viewport and returns the state with the tracked cursor after the
    * last write and the default style.
    */
  def present(state: WriterState, capabilities: Capabilities, viewport: Rect, updates: Vector[CellUpdate]): (WriterState, String) = {
    val builder = new java.lang.StringBuilder
    val end     = loop(IArray.from(updates), 0, Acc(state.cursor, state.style, none[Position], ""), builder, capabilities, viewport)
    if (end.style =!= CellStyle.default) builder.append(Sequences.SgrReset): Unit else ()
    (state.copy(cursor = end.cursor, style = CellStyle.default), builder.toString)
  }

  /** A Cursor Position to the position clamped into the viewport, nothing when the tracked cursor is already there. */
  def moveCursor(state: WriterState, viewport: Rect, position: Position): (WriterState, String) = {
    val rightEdge  = viewport.x.value.toLong + viewport.width.value.toLong - 1L
    val bottomEdge = viewport.y.value.toLong + viewport.height.value.toLong - 1L
    val x          = NonNegInts.clamp(math.max(viewport.x.value.toLong, math.min(position.x.value.toLong, rightEdge)))
    val y          = NonNegInts.clamp(math.max(viewport.y.value.toLong, math.min(position.y.value.toLong, bottomEdge)))
    val target     = Position(x, y)
    state.cursor match {
      case CursorState.Known(current) if current === target => (state, "")
      case CursorState.Known(_) | CursorState.PendingWrap(_) | CursorState.Unknown =>
        (state.copy(cursor = CursorState.Known(target)), Sequences.cup(y.value + 1, x.value + 1))
    }
  }

  /** DECTCEM set, the state is unchanged (the orchestration tracks visibility). */
  def showCursor(state: WriterState): (WriterState, String) = (state, Sequences.CursorShow)

  /** DECTCEM reset, the state is unchanged. */
  def hideCursor(state: WriterState): (WriterState, String) = (state, Sequences.CursorHide)

  /** Erase in Display and home (the alternate screen), the tracked cursor becomes the origin. */
  def clearAll(state: WriterState): (WriterState, String) =
    (state.copy(cursor = CursorState.Known(Position.origin)), Sequences.ClearScreen + Sequences.CursorHome)

  /** A move to the viewport's first row and Erase in Display below it (the inline clear: never a full-screen clear, design doc 7.2),
    * the tracked cursor becomes the viewport's position.
    */
  def clearViewport(state: WriterState, viewport: Rect): (WriterState, String) =
    (
      state.copy(cursor = CursorState.Known(viewport.position)),
      Sequences.cup(viewport.y.value + 1, viewport.x.value + 1) + Sequences.EraseBelow,
    )

  /** The entry sequence of [[Sequences.enter]] with the kitty keyboard push of `flags`, and the initial state with the cursor known at
    * the origin (the sequence homes it).
    */
  def enter(options: TerminalOptions, flags: KittyFlags): (WriterState, String) =
    (WriterState.initial.copy(cursor = CursorState.Known(Position.origin)), Sequences.enter(options, flags))

  /** The inline entry (design doc 7.2, never a full-screen clear): a carriage return to column 1, `pad` line feeds (the entry
    * scroll), the cursor hidden, the features, the kitty keyboard push of `flags` (M3d), and the region armed through the DECSC /
    * DECRC bracket when the scroll-region strategy applies (rule R7). The tracked cursor is unknown afterwards.
    */
  def enterInline(options: TerminalOptions, pad: Int, region: Option[ScrollRegion], flags: KittyFlags): (WriterState, String) = {
    val armed = region.fold("")(r => Sequences.armRegion(r.top.value + 1, r.bottom.value + 1))
    (
      WriterState(CursorState.Unknown, CellStyle.default, region),
      Sequences.Cr + (Sequences.Lf * pad) + Sequences.CursorHide + Sequences.features(options) + Sequences.kittyPush(flags) + armed,
    )
  }

  /** The reset of every alternate-screen exit path, the reset of what was entered (rule R6): the kitty keyboard pop of the pushed
    * `flags` (before mode 1049 is reset, the alternate screen's own stack, M3d), then [[Sequences.SafeReset]].
    */
  def exit(flags: KittyFlags): String = Sequences.kittyPop(flags) + Sequences.SafeReset

  /** The reset of every inline exit path (design doc 7.2): the kitty keyboard pop of the pushed `flags` (M3d), the feature modes off
    * in reverse order, the style reset, the cursor shown, the region reset through the DECSC / DECRC bracket (rule R7), and the cursor parked at column 1 of the row below the
    * viewport (a CR LF from the viewport's last row, which scrolls one line when the viewport touches the terminal bottom, leaving
    * the last frame visible as history). Never `CSI ? 1049 l` (mode 1049 reset restores the cursor as DECRC on the normal screen).
    */
  def exitInline(viewport: Rect, flags: KittyFlags): String =
    Sequences.kittyPop(
      flags
    ) + Sequences.FocusDisable + Sequences.BracketedPasteDisable + Sequences.MouseTrackingDisable + Sequences.SgrReset +
      Sequences.CursorShow + Sequences.resetRegion + Sequences.cup(viewport.y.value + viewport.height.value, 1) + Sequences.CrLf

  /** The rows as styled lines at the cursor (the transcript flush after restore, design doc 7.2): each row's cells up to the last
    * non-blank one, a style reset when needed, Erase in Line (a short row leaves no residue, plan refinement R4), and CR LF. The
    * tracked cursor is unknown afterwards. The rows start at column 1. The inline exit parks the cursor there, and for a shell-started
    * application so does the mode 1049 exit, which restores the entry cursor. R2a and R3a place cells with CHA, an absolute column.
    */
  def print(state: WriterState, capabilities: Capabilities, rows: Buffer): (WriterState, String) = {
    val builder = new java.lang.StringBuilder
    val width   = rows.area.width.value
    transcriptRows(rows.cells, 0, width, rows.area.height.value, state.style, builder, capabilities, width)
    (state.copy(cursor = CursorState.Unknown, style = CellStyle.default), builder.toString)
  }

  /** The scroll-region print (design doc 7.2, the viewport untouched): a move to the region's bottom row, then per row a line feed
    * (the region scrolls up, its top row reaching scrollback), a carriage return, and the row through the shared emitter. The
    * tracked cursor is unknown afterwards, the region stays armed.
    */
  def printRegion(
    state: WriterState,
    capabilities: Capabilities,
    region: ScrollRegion,
    terminalWidth: Int,
    rows: Buffer,
  ): (WriterState, String) = {
    val builder = new java.lang.StringBuilder
    builder.append(Sequences.cup(region.bottom.value + 1, 1)): Unit
    regionRows(rows.cells, 0, rows.area.width.value, rows.area.height.value, state.style, builder, capabilities, terminalWidth)
    (state.copy(cursor = CursorState.Unknown, style = CellStyle.default), builder.toString)
  }

  /** The overlay print (design doc 7.2, the always-correct baseline): a move to the viewport's first row, Erase in Display below it,
    * the rows each followed by CR LF, and the line feeds that scroll the screen until the viewport fits below the printed rows.
    * Returns the moved viewport (the caller reports the viewport lost, so the next present redraws it). The tracked cursor is
    * unknown afterwards.
    */
  def printOverlay(
    state: WriterState,
    capabilities: Capabilities,
    viewport: Rect,
    terminal: Size,
    rows: Buffer,
  ): (WriterState, Rect, String) = {
    val n = rows.area.height.value
    if (n === 0) {
      (state, viewport, "")
    } else {
      val builder = new java.lang.StringBuilder
      val o       = viewport.y.value
      val h       = viewport.height.value
      val termH   = terminal.height.value
      builder.append(Sequences.cup(o + 1, 1)).append(Sequences.EraseBelow): Unit
      overlayRows(rows.cells, 0, rows.area.width.value, n, state.style, builder, capabilities, terminal.width.value)
      val c1      = math.min(o + n, termH - 1)
      val o2      = math.min(o + n, termH - h)
      val s2      = (o + n - o2) - (o + n - c1)
      val extra   = if (s2 > 0) (termH - 1 - c1) + s2 else 0
      builder.append(Sequences.Lf * extra): Unit
      val moved   = Rect(NonNegInt(0), NonNegInts.clamp(o2.toLong), terminal.width, viewport.height)
      (state.copy(cursor = CursorState.Unknown, style = CellStyle.default), moved, builder.toString)
    }
  }

  @tailrec
  private def transcriptRows(
    cells: IArray[Cell],
    y: Int,
    rowWidth: Int,
    height: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    limit: Int,
  ): Unit =
    if (y >= height) {
      ()
    } else {
      val next = emitRow(cells, y * rowWidth, rowWidth, limit, style, builder, capabilities)
      builder.append(Sequences.CrLf): Unit
      transcriptRows(cells, y + 1, rowWidth, height, next, builder, capabilities, limit)
    }

  @tailrec
  private def regionRows(
    cells: IArray[Cell],
    y: Int,
    rowWidth: Int,
    height: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    limit: Int,
  ): Unit =
    if (y >= height) {
      ()
    } else {
      builder.append(Sequences.Lf).append(Sequences.Cr): Unit
      val next = emitRow(cells, y * rowWidth, rowWidth, limit, style, builder, capabilities)
      regionRows(cells, y + 1, rowWidth, height, next, builder, capabilities, limit)
    }

  @tailrec
  private def overlayRows(
    cells: IArray[Cell],
    y: Int,
    rowWidth: Int,
    height: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    limit: Int,
  ): Unit =
    if (y >= height) {
      ()
    } else {
      val next = emitRow(cells, y * rowWidth, rowWidth, limit, style, builder, capabilities)
      builder.append(Sequences.CrLf): Unit
      overlayRows(cells, y + 1, rowWidth, height, next, builder, capabilities, limit)
    }

  /** Emits one printed row: the cells up to the last non-blank one (style deltas, the R3a VS16 sequence, a wide glyph straddling the
    * limit as a space in its style), truncated at `limit` columns, then the style reset when the tracked style is not the default,
    * then a CHA to the first unwritten column after a closing VS16 cluster (R3a) and Erase in Line when fewer than `limit` columns
    * were written (plan refinement R4; a full row needs no erase, and with the cursor on the last column under a pending wrap the
    * erase would blank the cell just written). Returns the tracked style, always the default.
    */
  private def emitRow(
    cells: IArray[Cell],
    rowStart: Int,
    rowWidth: Int,
    limit: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
  ): CellStyle = {
    val width             = math.min(rowWidth, limit)
    val count             = lastNonBlank(cells, rowStart, width)
    val (tracked, synced) = emitRowCells(cells, rowStart, 0, count, limit, style, builder, capabilities, "", true)
    val reset             =
      if (tracked =!= CellStyle.default) {
        builder.append(Sequences.SgrReset): Unit
        CellStyle.default
      } else {
        tracked
      }
    if (count < limit) {
      if (synced) () else builder.append(Sequences.cha(count + 1)): Unit
      builder.append(Sequences.EraseToLineEnd): Unit
    } else {
      ()
    }
    reset
  }

  @tailrec
  private def lastNonBlank(cells: IArray[Cell], rowStart: Int, width: Int): Int =
    if (width <= 0) 0
    else if (cells(rowStart + width - 1) =!= Cell.blank) width
    else lastNonBlank(cells, rowStart, width - 1)

  /** The cells `c` until `count` of a printed row. `synced` says the terminal cursor is known to be at column `c` (false after a VS16
    * cluster, rule R3a, so the next cell is placed with CHA). Returns the tracked style and `synced` at `count`.
    */
  @tailrec
  private def emitRowCells(
    cells: IArray[Cell],
    rowStart: Int,
    c: Int,
    count: Int,
    limit: Int,
    style: CellStyle,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    last: String,
    synced: Boolean,
  ): (CellStyle, Boolean) =
    if (c >= count) {
      (style, synced)
    } else {
      val (next, emitted, known) = cells(rowStart + c) match {
        case Cell.Continuation(_) => (style, last, synced)
        case Cell.Glyph(symbol, glyphWidth, cellStyle) =>
          val tracked = ensureStyle(style, builder, capabilities, cellStyle)
          if (synced) () else builder.append(Sequences.cha(c + 1)): Unit
          if (glyphWidth.columns === 2 && c + 1 >= limit) {
            if (synced) breakJoin(builder, last, " ", c) else ()
            builder.append(' '): Unit
            (tracked, " ", true)
          } else if (glyphWidth.columns === 2 && symbol.isVs16Sequence) {
            if (synced) breakJoin(builder, last, " ", c) else ()
            builder.append("  ").append(Sequences.cha(c + 1)).append(symbol.value): Unit
            (tracked, "", false)
          } else {
            if (synced) breakJoin(builder, last, symbol.value, c) else ()
            builder.append(symbol.value): Unit
            (tracked, symbol.value, true)
          }
      }
      emitRowCells(cells, rowStart, c + 1, count, limit, next, builder, capabilities, emitted, known)
    }

  /** The CHA to cell `c`'s column when `next` would join `last` (rule R2a) in a printed row. */
  private def breakJoin(builder: java.lang.StringBuilder, last: String, next: String, c: Int): Unit =
    if (Graphemes.joins(last, next)) builder.append(Sequences.cha(c + 1)): Unit else ()

  @tailrec
  private def loop(
    updates: IArray[CellUpdate],
    i: Int,
    acc: Acc,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    viewport: Rect,
  ): Acc =
    if (i >= updates.length) {
      acc
    } else {
      val update = updates(i)
      val next   =
        if (!viewport.contains(update.position)) {
          acc
        } else {
          val x     = update.position.x.value
          val y     = update.position.y.value
          val right = viewport.x.value.toLong + viewport.width.value.toLong
          update.cell match {
            case Cell.Continuation(_) =>
              if (acc.lastWide.exists(owner => owner.x.value === x - 1 && owner.y.value === y)) acc
              else emitGlyph(acc, builder, capabilities, right, x, y, GlyphSymbol.space, 1, CellStyle.default)
            case Cell.Glyph(symbol, glyphWidth, style) =>
              val columns = glyphWidth.columns
              if (columns === 2 && x.toLong + 1L >= right) {
                emitGlyph(acc, builder, capabilities, right, x, y, GlyphSymbol.space, 1, style)
              } else if (columns === 2 && symbol.isVs16Sequence) {
                emitVs16(acc, builder, capabilities, x, y, symbol, style)
              } else {
                emitGlyph(acc, builder, capabilities, right, x, y, symbol, columns, style)
              }
          }
        }
      loop(updates, i + 1, next, builder, capabilities, viewport)
    }

  private def emitGlyph(
    acc: Acc,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    right: Long,
    x: Int,
    y: Int,
    symbol: GlyphSymbol,
    columns: Int,
    style: CellStyle,
  ): Acc = {
    val continuing = acc.cursor match {
      case CursorState.Known(current) => current.x.value === x && current.y.value === y
      case CursorState.PendingWrap(_) | CursorState.Unknown => false
    }
    val cursor0    = if (continuing && Graphemes.joins(acc.last, symbol.value)) CursorState.Unknown else acc.cursor
    val cursor     = ensureCursor(cursor0, builder, x, y)
    val tracked    = ensureStyle(acc.style, builder, capabilities, style)
    builder.append(symbol.value): Unit
    val nx         = x.toLong + columns.toLong
    val after      =
      if (nx >= right) CursorState.PendingWrap(NonNegInts.clamp(y.toLong))
      else CursorState.Known(Position(NonNegInts.clamp(nx), NonNegInts.clamp(y.toLong)))
    val wide       = if (columns === 2) Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)).some else none[Position]
    val _          = cursor
    Acc(after, tracked, wide, symbol.value)
  }

  /** Rule R3a: two spaces in the cell's style over the cluster's two cells, the CUP back on its first cell, and the cluster, leaving the
    * cursor unknown so the next write is placed explicitly, whether the terminal advanced one column or two.
    */
  private def emitVs16(
    acc: Acc,
    builder: java.lang.StringBuilder,
    capabilities: Capabilities,
    x: Int,
    y: Int,
    symbol: GlyphSymbol,
    style: CellStyle,
  ): Acc = {
    val continuing = acc.cursor match {
      case CursorState.Known(current) => current.x.value === x && current.y.value === y
      case CursorState.PendingWrap(_) | CursorState.Unknown => false
    }
    val cursor0    = if (continuing && Graphemes.joins(acc.last, " ")) CursorState.Unknown else acc.cursor
    val start      = ensureCursor(cursor0, builder, x, y)
    val tracked    = ensureStyle(acc.style, builder, capabilities, style)
    builder.append("  "): Unit
    val back       = ensureCursor(CursorState.Unknown, builder, x, y)
    builder.append(symbol.value): Unit
    val _          = start
    val _          = back
    Acc(CursorState.Unknown, tracked, Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)).some, "")
  }

  private def ensureCursor(cursor: CursorState, builder: java.lang.StringBuilder, x: Int, y: Int): CursorState =
    cursor match {
      case CursorState.Known(current) if current.x.value === x && current.y.value === y => cursor
      case CursorState.Known(_) | CursorState.PendingWrap(_) | CursorState.Unknown =>
        builder.append(Sequences.cup(y + 1, x + 1)): Unit
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

  /** The UTF-8 length of the glyph symbols in the updates, the `symbolBytes` term of rule R10. */
  def symbolBytes(updates: Vector[CellUpdate]): Long =
    updates.foldLeft(0L) { (acc, update) =>
      update.cell match {
        case Cell.Glyph(symbol, _, _) => acc + utf8Length(symbol.value)
        case Cell.Continuation(_) => acc
      }
    }

  /** The UTF-8 length of the glyph symbols in the buffer, the `symbolBytes` term of the print budget. */
  def bufferSymbolBytes(rows: Buffer): Long =
    rows.cells.foldLeft(0L) { (acc, cell) =>
      cell match {
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
