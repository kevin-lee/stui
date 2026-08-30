package stui.core.spi

import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.geometry.{Position, Rect, Size}

/** What a platform backend provides to the rendering loop (design doc 6.3). Synchronous and effect-free, the effect adapters lift it.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
trait TerminalBackend {

  /** The full terminal size in cells (the print channel renders at this width and height). */
  def size(): Size

  /** The screen area the UI occupies, in screen coordinates: the whole terminal at the origin on the alternate screen, the last rows
    * of the normal screen in inline mode (recomputed when the terminal size changes). The orchestration renders every frame over
    * exactly this rect (design doc 7.2, decision D12).
    */
  def viewport(): Rect

  /** Writes the changed cells of a frame, the output of `Buffer.diff` in row-major order. A `Continuation` update means "this column is
    * the shadow of the glyph to its left", the backend decides whether to emit anything for it (design doc 9.3).
    */
  def draw(updates: Vector[CellUpdate]): Unit

  /** Pushes buffered output to the terminal and returns the number of bytes pushed by this call (0 for a backend without a byte
    * stream). The orchestration reports it as `RenderStats.bytes`.
    */
  def flush(): NonNegInt

  /** Moves the cursor to the position. */
  def moveCursor(position: Position): Unit

  /** Makes the cursor visible. */
  def showCursor(): Unit

  /** Hides the cursor. */
  def hideCursor(): Unit

  /** Clears the whole screen. */
  def clear(): Unit

  /** Emits the rows above the viewport in inline mode (`ViewportKept` under the scroll-region strategy, `ViewportLost` when the
    * viewport was overwritten or moved and the orchestration must redraw it), or as styled lines at the cursor outside inline mode
    * (the transcript flush after restore under the alternate screen, design doc 7.2, decision D13). Rows wider than the terminal are
    * truncated at the terminal width.
    */
  def print(rows: Buffer): PrintEffect

  /** Enters raw mode, the screen mode, and the requested features, restored by [[exit]]. `Left` when standard input is not a terminal,
    * the screen mode is unsupported, or a platform call fails, in which case nothing was changed.
    */
  def enter(options: TerminalOptions): Either[TerminalError, Unit]

  /** Restores everything [[enter]] changed and discards any output not yet flushed. Idempotent, runs on every exit path. */
  def exit(): Unit

}
