package stui.terminal

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.spi.{ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
import stui.terminal.ansi.{AnsiWriter, WriterState}
import stui.unicode.WidthPolicy

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

/** The shared ANSI backend (design doc 7.1 and 7.2): a [[stui.core.spi.TerminalBackend]] over a platform [[Tty]] that queues the
  * writer's output until `flush`. `enter` writes the entry sequence immediately after raw mode succeeds, `exit` discards anything not
  * yet flushed, writes the fixed safe reset, and restores the terminal mode, once. The size is the device's last non-zero report, 80 x
  * 24 before any report.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class AnsiBackend private (
  private val tty: Tty,
  val capabilities: Capabilities,
  val policy: WidthPolicy,
  private val ref: AtomicReference[AnsiBackend.State],
) extends TerminalBackend {

  /* the methods live in the class body because they implement the TerminalBackend trait members */

  /** The device's size, the last non-zero report when the query fails. */
  override def size(): Size = tty.size() match {
    case Some(size) => ref.updateAndGet(_.copy(lastSize = size)).lastSize
    case None => ref.get().lastSize
  }

  /** Queues the writer's output for the updates over the current viewport. */
  override def draw(updates: Vector[CellUpdate]): Unit = {
    val viewport = Rect.sized(size())
    ref.updateAndGet { state =>
      AnsiWriter.present(state.writer, capabilities, viewport, updates) match {
        case (writer, output) => state.copy(writer = writer, pending = state.pending :+ output)
      }
    }: Unit
  }

  /** Writes the queued output as UTF-8 and returns its byte count. */
  override def flush(): NonNegInt = {
    val state  = ref.getAndUpdate(_.copy(pending = Vector.empty[String]))
    val output = state.pending.mkString
    if (output.isEmpty) {
      NonNegInt(0)
    } else {
      val bytes = output.getBytes(StandardCharsets.UTF_8)
      tty.write(bytes)
      NonNegInts.clamp(bytes.length.toLong)
    }
  }

  /** Queues a cursor move (nothing when the tracked cursor is already there). */
  override def moveCursor(position: Position): Unit = {
    val viewport = Rect.sized(size())
    queue(state => AnsiWriter.moveCursor(state, viewport, position))
  }

  /** Queues DECTCEM set. */
  override def showCursor(): Unit = queue(AnsiWriter.showCursor)

  /** Queues DECTCEM reset. */
  override def hideCursor(): Unit = queue(AnsiWriter.hideCursor)

  /** Queues Erase in Display and home. */
  override def clear(): Unit = queue(AnsiWriter.clear)

  /** Queues the rows as styled lines at the cursor. */
  override def print(rows: Buffer): Unit = queue(state => AnsiWriter.print(state, capabilities, rows))

  /** `Left` for a non-terminal, an unsupported screen mode, or a raw-mode failure, otherwise raw mode and the entry sequence. */
  override def enter(options: TerminalOptions): Either[TerminalError, Unit] = options.screenMode match {
    case mode @ ScreenMode.Inline(_) => TerminalError.unsupportedScreenMode(mode).asLeft[Unit]
    case ScreenMode.AlternateScreen =>
      if (!tty.isTerminal) {
        (TerminalError.NotATerminal: TerminalError).asLeft[Unit]
      } else {
        tty.enterRawMode().map { _ =>
          AnsiWriter.enter(options) match {
            case (writer, output) =>
              tty.write(output.getBytes(StandardCharsets.UTF_8))
              ref.updateAndGet(_.copy(writer = writer, pending = Vector.empty[String], entered = true)): Unit
          }
        }
      }
  }

  /** Discards queued output, writes the safe reset, and restores the mode, only when entered. */
  override def exit(): Unit = {
    val state = ref.getAndUpdate(_.copy(writer = WriterState.initial, pending = Vector.empty[String], entered = false))
    if (state.entered) {
      tty.write(AnsiWriter.exit.getBytes(StandardCharsets.UTF_8))
      tty.restoreMode()
    } else {
      ()
    }
  }

  private def queue(f: WriterState => (WriterState, String)): Unit =
    ref.updateAndGet { state =>
      f(state.writer) match {
        case (writer, output) => state.copy(writer = writer, pending = state.pending :+ output)
      }
    }: Unit

}

object AnsiBackend {

  /** The backend's state: the writer state, the output queued since the last flush, the last non-zero size, and whether `enter`
    * succeeded.
    */
  final case class State(writer: WriterState, pending: Vector[String], lastSize: Size, entered: Boolean)

  /** The size assumed before the device reports one. */
  val fallbackSize: Size = Size(NonNegInt(80), NonNegInt(24))

  /** A backend over the device with [[WidthPolicy.default]]. */
  def apply(tty: Tty, capabilities: Capabilities): AnsiBackend = withPolicy(WidthPolicy.default, tty, capabilities)

  /** A backend over the device with the given policy. */
  def withPolicy(policy: WidthPolicy, tty: Tty, capabilities: Capabilities): AnsiBackend =
    new AnsiBackend(tty, capabilities, policy, new AtomicReference(State(WriterState.initial, Vector.empty[String], fallbackSize, false)))

  extension (backend: AnsiBackend) {

    /** The output queued since the last flush, for tests. */
    def pendingOutput: String = backend.ref.get().pending.mkString

    /** The writer state, for tests. */
    def writerState: WriterState = backend.ref.get().writer

  }

}
