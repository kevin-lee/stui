package stui.core.terminal

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas, Cell}
import stui.core.capability.Capabilities
import stui.core.frame.Frame
import stui.core.geometry.{Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.spi.{Clock, PrintEffect, ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
import stui.core.widget.Widget
import stui.unicode.WidthPolicy

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.duration.*

/** The orchestration that owns one backend (design doc 6.3 and 7.4, decisions D16 and D23): it renders a frame, diffs it against the
  * previous one, emits the updates, and returns the frame with its statistics. It is the single writer, and one reentrant lock
  * serialises every present and the teardown, so a JVM shutdown hook or the Native exit path cannot interleave with a frame flush.
  * [[Terminal.run]] is the only public entry: it enters, runs the block, and always exits (an exception from the block propagates
  * after the restore, an exception from the restore replaces it, so cleanup errors outrank emission errors). Drawing after the
  * terminal is closed renders and returns the frame but emits nothing.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class Terminal private (
  private val backend: TerminalBackend,
  val options: TerminalOptions,
  val capabilities: Capabilities,
  val policy: WidthPolicy,
  private val clock: Clock,
  private val lock: ReentrantLock,
  private val ref: AtomicReference[Terminal.State],
  val latch: TeardownLatch,
)

object Terminal {

  /** The observable state: the previous frame (`None` before the first present), the pending redraw reason, whether the cursor was
    * shown by the last present, whether the terminal was closed, and the print ring buffered under the alternate screen (design doc
    * 7.2, decision D13).
    */
  final case class State(previous: Option[Frame], pending: Option[RedrawReason], cursorVisible: Boolean, closed: Boolean, ring: PrintRing)

  /** [[runWith]] with [[WidthPolicy.default]]. */
  def run[A](backend: TerminalBackend, options: TerminalOptions, capabilities: Capabilities, clock: Clock)(
    use: Terminal => A
  ): Either[TerminalError, A] = runWith(WidthPolicy.default, backend, options, capabilities, clock)(use)

  /** Enters the terminal, runs `use`, and exits on every path. `Left` when the backend cannot enter (a screen mode it does not
    * implement included), in which case nothing was touched.
    */
  def runWith[A](policy: WidthPolicy, backend: TerminalBackend, options: TerminalOptions, capabilities: Capabilities, clock: Clock)(
    use: Terminal => A
  ): Either[TerminalError, A] =
    open(policy, backend, options, capabilities, clock).map { terminal =>
      try use(terminal)
      finally terminal.close()
    }

  /** Enters the terminal without the bracket, for adapters that manage the lifecycle themselves (the M5 `Resource`). */
  private[stui] def open(
    policy: WidthPolicy,
    backend: TerminalBackend,
    options: TerminalOptions,
    capabilities: Capabilities,
    clock: Clock,
  ): Either[TerminalError, Terminal] =
    backend
      .enter(options)
      .map(_ =>
        new Terminal(
          backend,
          options,
          capabilities,
          policy,
          clock,
          new ReentrantLock(),
          new AtomicReference(State(none[Frame], none[RedrawReason], false, false, PrintRing.empty(options.printBufferRows))),
          TeardownLatch(),
        )
      )

  /** The widget rendered over a screen of the size with the trailing all-blank rows trimmed (design doc 7.2), the print form every
    * terminal and the simulator share (M3c).
    */
  private[stui] def printRows(policy: WidthPolicy, size: Size, widget: Widget): Buffer = {
    val full = Buffer.emptyWith(policy, Rect.sized(size)).draw(canvas => widget.render(canvas.area, canvas))
    val keep = full.rows.lastIndexWhere(row => row.exists(cell => cell =!= Cell.blank)) + 1
    full.firstRows(NonNegInts.clamp(keep.toLong))
  }

  /** The widget rendered over the full terminal size with the trailing all-blank rows trimmed (design doc 7.2). */
  private def renderPrint(terminal: Terminal, widget: Widget): Buffer = printRows(terminal.policy, terminal.backend.size(), widget)

  private def locked[A](terminal: Terminal)(body: => A): A = {
    terminal.lock.lock()
    try body
    finally terminal.lock.unlock()
  }

  extension (terminal: Terminal) {

    /** Renders a fresh frame over the current viewport, emits every cell when there is no previous frame, a redraw reason is pending,
      * or the viewport changed size, and the diff otherwise. The protocol is `hideCursor` (only when the cursor was shown), `draw`,
      * then `moveCursor` and `showCursor` when the frame has a cursor, then `flush`, whose byte count becomes the statistics.
      */
    def draw(render: Canvas => Unit): CompletedFrame = locked(terminal) {
      val st       = terminal.ref.get()
      val start    = terminal.clock.monotonicNanos()
      val viewport = terminal.backend.viewport()
      val frame    = Frame.draw(Buffer.emptyWith(terminal.policy, viewport))(render)
      val full     = st.previous.isEmpty || st.pending.isDefined || st.previous.exists(_.buffer.area =!= viewport)
      val updates  = st.previous match {
        case Some(previous) if !full => Buffer.diff(previous.buffer, frame.buffer)
        case Some(_) | None => Buffer.allUpdates(frame.buffer)
      }
      val bytes    =
        if (st.closed) {
          NonNegInt(0)
        } else {
          if (st.cursorVisible) terminal.backend.hideCursor() else ()
          terminal.backend.draw(updates)
          frame.cursor.foreach { position =>
            terminal.backend.moveCursor(position)
            terminal.backend.showCursor()
          }
          terminal.backend.flush()
        }
      terminal.ref.set(State(frame.some, none[RedrawReason], frame.cursor.isDefined && !st.closed, st.closed, st.ring))
      val elapsed  = math.max(0L, terminal.clock.monotonicNanos() - start)
      CompletedFrame(frame, RenderStats(bytes, NonNegInts.clamp(updates.length.toLong), elapsed.nanos))
    }

    /** Forces the next [[draw]] to emit every cell. A later reason replaces an earlier pending one. */
    def redraw(reason: RedrawReason): Unit = locked(terminal)(terminal.ref.updateAndGet(_.copy(pending = reason.some)): Unit)

    /** The area the next frame is rendered into: the backend's viewport in screen coordinates (the whole terminal on the alternate
      * screen, the last rows of the normal screen in inline mode).
      */
    def viewport: Rect = terminal.backend.viewport()

    /** Renders the widget over the full terminal size through the same cell pipeline as every frame (sanitised by construction,
      * principle 9), trims trailing all-blank rows, and prints the rows above the UI (design doc 7.2, decision D13). Under the
      * alternate screen the rows join the bounded ring flushed after restore at exit. In inline mode they go to the backend and are
      * flushed immediately (a print inside a render closure joins that present's flush), and when the backend reports the viewport
      * lost the next draw emits every cell (`RedrawReason.Printed`). A blank widget prints nothing, and a print after close does
      * nothing.
      */
    def print(widget: Widget): Unit = locked(terminal) {
      val st = terminal.ref.get()
      if (st.closed) {
        ()
      } else {
        val rows = renderPrint(terminal, widget)
        if (rows.area.height.value === 0) {
          ()
        } else {
          terminal.options.screenMode match {
            case ScreenMode.AlternateScreen =>
              terminal.ref.set(st.copy(ring = st.ring.append(rows)))
            case ScreenMode.Inline(_) =>
              terminal.backend.print(rows) match {
                case PrintEffect.ViewportLost => terminal.ref.set(st.copy(pending = RedrawReason.Printed.some))
                case PrintEffect.ViewportKept => ()
              }
              terminal.backend.flush(): Unit
          }
        }
      }
    }

    /** The last presented frame, `None` before the first present. */
    def lastFrame: Option[Frame] = terminal.ref.get().previous

    /** True once [[close]] ran. */
    def isClosed: Boolean = terminal.ref.get().closed

    /** A snapshot of the state. */
    def state: State = terminal.ref.get()

    /** Exits the backend once, under the lock (a hook on another thread waits for a present in flight), then flushes the print ring
      * to the normal screen so the transcript survives on every exit path, the crash and signal paths included (design doc 7.2 and
      * 7.4, decision D13). Idempotent and reentrant.
      */
    private[stui] def close(): Unit = locked(terminal) {
      val st = terminal.ref.get()
      if (st.closed) {
        ()
      } else {
        terminal.ref.set(st.copy(closed = true, ring = PrintRing.empty(terminal.options.printBufferRows)))
        terminal.backend.exit()
        if (st.ring.isEmpty) {
          ()
        } else {
          st.ring.entries.foreach(rows => terminal.backend.print(rows): Unit)
          terminal.backend.flush(): Unit
        }
      }
    }

  }

}
