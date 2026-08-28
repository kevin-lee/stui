package stui.core.spi

import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.geometry.{Position, Size}

/** What a platform backend provides to the rendering loop (design doc 6.3). Synchronous and effect-free, the effect adapters lift it.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
trait TerminalBackend {

  /** The current terminal size in cells. */
  def size(): Size

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

  /** Emits rows above the viewport into scrollback in inline mode (M1f). Under the alternate screen the orchestration buffers prints
    * instead and calls this only when flushing the transcript after restore (M1f). The M1e writer emits the rows as styled lines with
    * CR LF at the cursor.
    */
  def print(rows: Buffer): Unit

  /** Enters raw mode, the screen mode, and the requested features, restored by [[exit]]. `Left` when standard input is not a terminal,
    * the screen mode is unsupported, or a platform call fails, in which case nothing was changed.
    */
  def enter(options: TerminalOptions): Either[TerminalError, Unit]

  /** Restores everything [[enter]] changed and discards any output not yet flushed. Idempotent, runs on every exit path. */
  def exit(): Unit

}
