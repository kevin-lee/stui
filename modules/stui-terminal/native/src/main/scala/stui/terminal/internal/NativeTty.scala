package stui.terminal.internal

import cats.syntax.all.*
import stui.core.geometry.Size
import stui.core.internal.NonNegInts
import stui.core.spi.TerminalError
import stui.terminal.Tty
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.scalanative.libc.errno
import scala.scalanative.posix.{termios, unistd}
import scala.scalanative.posix.errno.EINTR
import scala.scalanative.posix.termiosOps.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** The Native terminal device through posixlib (design doc 7, the M0 recipe): raw mode by clearing `ECHO`, `ICANON`, `ISIG`, and
  * `IEXTEN`, `IXON`, `ICRNL`, `BRKINT`, `INPCK`, and `ISTRIP`, and `OPOST`, setting `CS8`, `VMIN` and `VTIME` zero, the original
  * `termios` kept in its own `Zone` until the restore closes it, the size through the C glue's `ioctl(TIOCGWINSZ)`, output through
  * `write` on file descriptor 1 with partial writes and `EINTR` handled.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class NativeTty private (private val saved: AtomicReference[Option[NativeTty.Saved]]) extends Tty {

  /* the methods live in the class body because they implement the Tty trait members */

  override def isTerminal: Boolean = unistd.isatty(0) === 1

  override def size(): Option[Size] = {
    val rows    = stackalloc[CInt]()
    val columns = stackalloc[CInt]()
    if (NativeGlue.winsize(0, rows, columns) === 0 && !rows > 0 && !columns > 0) {
      Size(NonNegInts.clamp((!columns).toLong), NonNegInts.clamp((!rows).toLong)).some
    } else {
      none[Size]
    }
  }

  override def write(bytes: Array[Byte]): Unit = writeLoop(bytes, 0)

  @tailrec
  private def writeLoop(bytes: Array[Byte], offset: Int): Unit =
    if (offset >= bytes.length) {
      ()
    } else {
      val written = unistd.write(1, bytes.at(offset), (bytes.length - offset).toUSize)
      if (written >= 0) writeLoop(bytes, offset + written)
      else if (errno.errno === EINTR) writeLoop(bytes, offset)
      else ()
    }

  override def enterRawMode(): Either[TerminalError, Unit] = {
    val zone     = Zone.open()
    val original = alloc[termios.termios]()(using zone)
    if (termios.tcgetattr(0, original) !== 0) {
      zone.close()
      TerminalError.platformFailure("tcgetattr", errno.errno.toString).asLeft[Unit]
    } else {
      val raw = stackalloc[termios.termios]()
      !raw = !original
      raw.c_iflag = raw.c_iflag & ~(termios.IXON | termios.ICRNL | termios.BRKINT | termios.INPCK | termios.ISTRIP).toUInt
      raw.c_oflag = raw.c_oflag & ~termios.OPOST.toUInt
      raw.c_cflag = raw.c_cflag | termios.CS8.toUInt
      raw.c_lflag = raw.c_lflag & ~(termios.ECHO | termios.ICANON | termios.ISIG | termios.IEXTEN).toUInt
      raw.c_cc(termios.VMIN) = 0.toUByte
      raw.c_cc(termios.VTIME) = 0.toUByte
      if (termios.tcsetattr(0, termios.TCSANOW, raw) !== 0) {
        zone.close()
        TerminalError.platformFailure("tcsetattr", errno.errno.toString).asLeft[Unit]
      } else {
        saved.set(NativeTty.Saved(zone, original).some)
        ().asRight[TerminalError]
      }
    }
  }

  override def restoreMode(): Unit =
    saved.getAndSet(none[NativeTty.Saved]).foreach { original =>
      termios.tcsetattr(0, termios.TCSANOW, original.pointer): Unit
      original.zone.close()
    }

}

object NativeTty {

  /** The original `termios` and the zone that owns it. */
  final case class Saved(zone: Zone, pointer: Ptr[termios.termios])

  /** The device on file descriptors 0 and 1. */
  def apply(): NativeTty = new NativeTty(new AtomicReference(none[NativeTty.Saved]))

}
