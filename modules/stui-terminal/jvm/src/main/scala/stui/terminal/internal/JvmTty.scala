package stui.terminal.internal

import cats.syntax.all.*
import stui.core.geometry.Size
import stui.core.internal.NonNegInts
import stui.core.spi.TerminalError
import stui.terminal.Tty
import stui.unicode.internal.IntOps.*

import java.io.{FileDescriptor, FileOutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Locale
import java.util.concurrent.atomic.AtomicReference

/** The JVM terminal device through JNA (design doc 7, decision D3, the M0 recipe): raw mode by `tcgetattr` into an opaque blob,
  * `cfmakeraw` on a copy, `tcsetattr`, and the blob restored on exit, the size through the variadic `ioctl(TIOCGWINSZ)` bound by
  * interface mapping, output through the standard output file descriptor. Standard input must be the terminal (the piped-stdin
  * `ttyname` recipe is a later item).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class JvmTty private (private val saved: AtomicReference[Option[Array[Byte]]], private val out: FileOutputStream) extends Tty {

  /* the methods live in the class body because they implement the Tty trait members */

  override def isTerminal: Boolean = LibC.isatty(0) === 1

  override def size(): Option[Size] = {
    /* a fresh buffer per query, filled by ioctl (the documented mutation site) */
    val winsize = new Array[Byte](JvmTty.WinsizeBytes)
    if (LibC.varargs.ioctl(0, JvmTty.Tiocgwinsz, winsize) === 0) {
      val buffer  = ByteBuffer.wrap(winsize).order(ByteOrder.nativeOrder())
      val rows    = buffer.getShort(0).toInt & 0xffff
      val columns = buffer.getShort(2).toInt & 0xffff
      if (rows > 0 && columns > 0) Size(NonNegInts.clamp(columns.toLong), NonNegInts.clamp(rows.toLong)).some else none[Size]
    } else {
      none[Size]
    }
  }

  override def write(bytes: Array[Byte]): Unit = {
    out.write(bytes)
    out.flush()
  }

  override def enterRawMode(): Either[TerminalError, Unit] = if (saved.get().isDefined) {
    /* already raw: the first save wins and restoreMode restores it once (the platform runner enters before the backend does) */
    ().asRight[TerminalError]
  } else {
    /* the blob tcgetattr fills and cfmakeraw edits (the documented mutation sites) */
    val original = new Array[Byte](JvmTty.TermiosBytes)
    val got      = LibC.tcgetattr(0, original)
    if (got !== 0) {
      TerminalError.platformFailure("tcgetattr", got.toString).asLeft[Unit]
    } else {
      val raw = original.clone()
      LibC.cfmakeraw(raw)
      val set = LibC.tcsetattr(0, JvmTty.TcsaNow, raw)
      if (set !== 0) {
        TerminalError.platformFailure("tcsetattr", set.toString).asLeft[Unit]
      } else {
        saved.set(original.some)
        ().asRight[TerminalError]
      }
    }
  }

  override def restoreMode(): Unit = saved.getAndSet(none[Array[Byte]]).foreach(blob => LibC.tcsetattr(0, JvmTty.TcsaNow, blob): Unit)

}

object JvmTty {

  /** Larger than `sizeof(struct termios)` on macOS and glibc, so the opaque blob needs no per-OS layout. */
  val TermiosBytes: Int = 256

  /** `struct winsize`: four unsigned shorts (rows, columns, x pixels, y pixels). */
  val WinsizeBytes: Int = 8

  /** `TCSANOW`. */
  val TcsaNow: Int = 0

  private val isMac: Boolean = Option(System.getProperty("os.name")).exists(_.toLowerCase(Locale.ROOT).contains("mac"))

  /** `TIOCGWINSZ`: Darwin's `_IOR('t', 104, struct winsize)` or Linux's `0x5413` (verified on aarch64 and x86_64 in M0). */
  val Tiocgwinsz: Long = if (isMac) 0x40087468L else 0x5413L

  /** The device on file descriptors 0 and 1. */
  def apply(): JvmTty = new JvmTty(new AtomicReference(none[Array[Byte]]), new FileOutputStream(FileDescriptor.out))

}
