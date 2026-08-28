package stui.terminal

import stui.core.geometry.Size
import stui.core.spi.TerminalError

/** The platform half of a backend (design doc 7): the terminal device as raw bytes, size, and mode switching. The shared
  * [[AnsiBackend]] does everything else, so a platform (JVM, Native, the M2 Node backend) implements only this.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
trait Tty {

  /** True when standard input is a terminal. */
  def isTerminal: Boolean

  /** The terminal size, `None` when the query fails or reports a zero side (a zero-sized report is noise, fact 17). */
  def size(): Option[Size]

  /** Writes the bytes and flushes them to the device. */
  def write(bytes: Array[Byte]): Unit

  /** Saves the terminal mode and enters raw mode, `Left` when a platform call fails (nothing changed then). */
  def enterRawMode(): Either[TerminalError, Unit]

  /** Restores the mode saved by [[enterRawMode]], idempotent. */
  def restoreMode(): Unit

}
