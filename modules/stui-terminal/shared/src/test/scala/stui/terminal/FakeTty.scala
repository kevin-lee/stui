package stui.terminal

import cats.syntax.all.*
import stui.core.geometry.Size
import stui.core.spi.TerminalError

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

/** A [[Tty]] that records what is written and counts mode switches.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class FakeTty private (
  private val terminal: Boolean,
  private val failing: Boolean,
  private val sizes: AtomicReference[Option[Size]],
  private val written: AtomicReference[Vector[Array[Byte]]],
  private val entered: AtomicReference[Int],
  private val restored: AtomicReference[Int],
) extends Tty {

  /* the methods live in the class body because they implement the Tty trait members */

  override def isTerminal: Boolean = terminal

  override def size(): Option[Size] = sizes.get()

  override def write(bytes: Array[Byte]): Unit = written.updateAndGet(_ :+ bytes): Unit

  override def enterRawMode(): Either[TerminalError, Unit] =
    if (failing) {
      TerminalError.platformFailure("tcsetattr", "fake").asLeft[Unit]
    } else {
      entered.updateAndGet(_ + 1): Unit
      ().asRight[TerminalError]
    }

  override def restoreMode(): Unit = restored.updateAndGet(_ + 1): Unit

}

object FakeTty {

  private def make(terminal: Boolean, failing: Boolean, size: Option[Size]): FakeTty =
    new FakeTty(
      terminal,
      failing,
      new AtomicReference(size),
      new AtomicReference(Vector.empty[Array[Byte]]),
      new AtomicReference(0),
      new AtomicReference(0),
    )

  /** A terminal of the size. */
  def of(size: Size): FakeTty = make(true, false, size.some)

  /** Not a terminal. */
  def notATerminal(size: Size): FakeTty = make(false, false, size.some)

  /** A terminal whose raw mode cannot be entered. */
  def failing(size: Size): FakeTty = make(true, true, size.some)

  extension (tty: FakeTty) {

    /** Everything written so far, decoded as UTF-8. */
    def output: String = new String(tty.written.get().flatMap(_.toVector).toArray, StandardCharsets.UTF_8)

    /** The number of `write` calls. */
    def writes: Int = tty.written.get().length

    /** The number of successful `enterRawMode` calls. */
    def enteredCount: Int = tty.entered.get()

    /** The number of `restoreMode` calls. */
    def restoredCount: Int = tty.restored.get()

    /** Changes what `size()` reports. */
    def report(size: Option[Size]): Unit = tty.sizes.set(size)

  }

}
