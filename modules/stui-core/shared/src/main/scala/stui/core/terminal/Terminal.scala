package stui.core.terminal

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas}
import stui.core.capability.Capabilities
import stui.core.frame.Frame
import stui.core.geometry.Rect
import stui.core.internal.NonNegInts
import stui.core.spi.{Clock, ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
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
    * shown by the last present, and whether the terminal was closed.
    */
  final case class State(previous: Option[Frame], pending: Option[RedrawReason], cursorVisible: Boolean, closed: Boolean)

  /** [[runWith]] with [[WidthPolicy.default]]. */
  def run[A](backend: TerminalBackend, options: TerminalOptions, capabilities: Capabilities, clock: Clock)(
    use: Terminal => A
  ): Either[TerminalError, A] = runWith(WidthPolicy.default, backend, options, capabilities, clock)(use)

  /** Enters the terminal, runs `use`, and exits on every path. `Left` when the screen mode is unsupported (`Inline` in M1e, nothing is
    * touched) or the backend cannot enter.
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
  ): Either[TerminalError, Terminal] = options.screenMode match {
    case mode @ ScreenMode.Inline(_) => TerminalError.unsupportedScreenMode(mode).asLeft[Terminal]
    case ScreenMode.AlternateScreen =>
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
            new AtomicReference(State(none[Frame], none[RedrawReason], false, false)),
            TeardownLatch(),
          )
        )
  }

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
      val viewport = Rect.sized(terminal.backend.size())
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
      terminal.ref.set(State(frame.some, none[RedrawReason], frame.cursor.isDefined && !st.closed, st.closed))
      val elapsed  = math.max(0L, terminal.clock.monotonicNanos() - start)
      CompletedFrame(frame, RenderStats(bytes, NonNegInts.clamp(updates.length.toLong), elapsed.nanos))
    }

    /** Forces the next [[draw]] to emit every cell. A later reason replaces an earlier pending one. */
    def redraw(reason: RedrawReason): Unit = locked(terminal)(terminal.ref.updateAndGet(_.copy(pending = reason.some)): Unit)

    /** The area the next frame is rendered into: the backend's current size at the origin. */
    def viewport: Rect = Rect.sized(terminal.backend.size())

    /** The last presented frame, `None` before the first present. */
    def lastFrame: Option[Frame] = terminal.ref.get().previous

    /** True once [[close]] ran. */
    def isClosed: Boolean = terminal.ref.get().closed

    /** A snapshot of the state. */
    def state: State = terminal.ref.get()

    /** Exits the backend once, under the lock (a hook on another thread waits for a present in flight). Idempotent and reentrant. */
    private[stui] def close(): Unit = locked(terminal) {
      val st = terminal.ref.get()
      if (st.closed) {
        ()
      } else {
        terminal.ref.set(st.copy(closed = true))
        terminal.backend.exit()
      }
    }

  }

}
