package stui.terminal.internal

import cats.syntax.all.*
import stui.core.geometry.Size
import stui.core.internal.NonNegInts
import stui.core.spi.TerminalError
import stui.terminal.Tty

import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Failure, Success, Try}

/** The Node terminal device (design doc 7, the JS row, the M0 recipe): raw mode through `stdin.setRawMode`, the size from
  * `stdout.columns` and `rows`, output through `stdout.write` as a `Uint8Array` (byte-exact, never a string re-encode). BOTH screen
  * modes are supported on Node - the alternate screen and inline mode - because the shared `AnsiBackend` owns them over this seam;
  * the inline entry anchor comes from the asynchronous probe's Cursor Position Report with the same full-screen-scroll fallback as
  * the other platforms.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
final class NodeTty private (private val saved: AtomicBoolean) extends Tty {

  /* the methods live in the class body because they implement the Tty trait members */

  override def isTerminal: Boolean = NodeProcess.stdin.isTTY.contains(true)

  override def size(): Option[Size] =
    (NodeProcess.stdout.columns.toOption, NodeProcess.stdout.rows.toOption) match {
      case (Some(columns), Some(rows)) if columns > 0 && rows > 0 =>
        Size(NonNegInts.clamp(columns.toLong), NonNegInts.clamp(rows.toLong)).some
      case (Some(_) | None, Some(_) | None) => none[Size]
    }

  override def write(bytes: Array[Byte]): Unit = NodeProcess.stdout.write(NodeBytes.toUint8Array(bytes)): Unit

  override def enterRawMode(): Either[TerminalError, Unit] = if (saved.get()) {
    /* already raw: the first save wins and restoreMode restores it once (the platform runner enters before the backend does) */
    ().asRight[TerminalError]
  } else {
    Try(NodeProcess.stdin.setRawMode(true)) match {
      case Success(_) =>
        saved.set(true)
        ().asRight[TerminalError]
      case Failure(error) => TerminalError.platformFailure("setRawMode", error.toString).asLeft[Unit]
    }
  }

  override def restoreMode(): Unit =
    if (saved.getAndSet(false)) Try(NodeProcess.stdin.setRawMode(false)).fold(_ => (), _ => ()) else ()

}

object NodeTty {

  /** The device on the process's standard input and output. */
  def apply(): NodeTty = new NodeTty(new AtomicBoolean(false))

}
