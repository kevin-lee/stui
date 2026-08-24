package stui.core.spi

import stui.core.buffer.CellUpdate
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

  /** Pushes buffered output to the terminal. */
  def flush(): Unit

  /** Moves the cursor to the position. */
  def moveCursor(position: Position): Unit

  /** Makes the cursor visible. */
  def showCursor(): Unit

  /** Hides the cursor. */
  def hideCursor(): Unit

  /** Clears the whole screen. */
  def clear(): Unit

  /** Enters raw mode and the requested features, restored by [[exit]]. */
  def enter(options: TerminalOptions): Unit

  /** Restores everything [[enter]] changed. Idempotent, runs on every exit path. */
  def exit(): Unit

}
