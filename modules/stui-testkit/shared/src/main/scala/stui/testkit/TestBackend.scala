package stui.testkit

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.{TerminalBackend, TerminalError, TerminalOptions}
import stui.unicode.WidthPolicy

import java.util.concurrent.atomic.AtomicReference

/** One call a [[TestBackend]] received, in order. `size()` queries are not logged, only the effectful calls are, so render-loop tests
  * can pin the effect protocol without depending on how often the loop polls the size.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum BackendCall derives Eq, Show, Hash {
  case Draw(updates: Vector[CellUpdate])
  case Flush
  case MoveCursor(position: Position)
  case ShowCursor
  case HideCursor
  case Clear
  case Print(rows: Buffer)
  case Enter(options: TerminalOptions)
  case Exit
}

/** An in-memory [[TerminalBackend]] for tests (design doc 6.1, the M1d testkit deliverable): it keeps the screen a `draw` would
  * produce (through [[Buffer.applyUpdates]]), the cursor position and visibility, the options of the last `enter`, the scrollback of
  * printed rows (design doc 12, asserted like Ratatui's scrollback assertions), and the ordered [[BackendCall]] log. State lives in one
  * `AtomicReference`, so the backend works unchanged on JVM, Scala.js, and Scala Native.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final class TestBackend private (private val ref: AtomicReference[TestBackend.State], val policy: WidthPolicy) extends TerminalBackend {

  /* the methods live in the class body because they implement the TerminalBackend trait members */

  /** The current screen size. Not logged (see [[BackendCall]]). */
  override def size(): Size = ref.get().buffer.area.size

  /** Applies the updates to the screen and logs [[BackendCall.Draw]]. */
  override def draw(updates: Vector[CellUpdate]): Unit =
    ref.updateAndGet(state =>
      state.copy(buffer = Buffer.applyUpdates(state.buffer, updates), calls = state.calls :+ BackendCall.Draw(updates))
    ): Unit

  /** Logs [[BackendCall.Flush]] and returns 0, there is no byte stream. */
  override def flush(): NonNegInt = {
    record(BackendCall.Flush)
    NonNegInt(0)
  }

  /** Moves the cursor and logs [[BackendCall.MoveCursor]]. */
  override def moveCursor(position: Position): Unit =
    ref.updateAndGet(state => state.copy(cursor = position, calls = state.calls :+ BackendCall.MoveCursor(position))): Unit

  /** Shows the cursor and logs [[BackendCall.ShowCursor]]. */
  override def showCursor(): Unit =
    ref.updateAndGet(state => state.copy(cursorVisible = true, calls = state.calls :+ BackendCall.ShowCursor)): Unit

  /** Hides the cursor and logs [[BackendCall.HideCursor]]. */
  override def hideCursor(): Unit =
    ref.updateAndGet(state => state.copy(cursorVisible = false, calls = state.calls :+ BackendCall.HideCursor)): Unit

  /** Blanks the screen and logs [[BackendCall.Clear]]. */
  override def clear(): Unit =
    ref.updateAndGet(state =>
      state.copy(buffer = Buffer.emptyWith(policy, state.buffer.area), calls = state.calls :+ BackendCall.Clear)
    ): Unit

  /** Appends the rows to the printed scrollback and logs [[BackendCall.Print]]. */
  override def print(rows: Buffer): Unit =
    ref.updateAndGet(state => state.copy(printed = state.printed :+ rows, calls = state.calls :+ BackendCall.Print(rows))): Unit

  /** Remembers the options, logs [[BackendCall.Enter]], and always succeeds. */
  override def enter(options: TerminalOptions): Either[TerminalError, Unit] = {
    ref.updateAndGet(state => state.copy(entered = options.some, calls = state.calls :+ BackendCall.Enter(options))): Unit
    ().asRight[TerminalError]
  }

  /** Forgets the entered options and logs [[BackendCall.Exit]]. */
  override def exit(): Unit =
    ref.updateAndGet(state => state.copy(entered = none[TerminalOptions], calls = state.calls :+ BackendCall.Exit)): Unit

  private def record(call: BackendCall): Unit = ref.updateAndGet(state => state.copy(calls = state.calls :+ call)): Unit

}

object TestBackend {

  /** The complete observable state of a [[TestBackend]] at one moment. */
  final case class State(
    buffer: Buffer,
    cursor: Position,
    cursorVisible: Boolean,
    entered: Option[TerminalOptions],
    printed: Vector[Buffer],
    calls: Vector[BackendCall],
  )

  /** A backend with a blank screen of the size at the origin, the cursor hidden at the origin, and [[WidthPolicy.default]]. */
  def of(size: Size): TestBackend = ofWith(WidthPolicy.default, size)

  /** [[of]] with the given policy. */
  def ofWith(policy: WidthPolicy, size: Size): TestBackend =
    new TestBackend(
      new AtomicReference(
        State(
          Buffer.emptyWith(policy, Rect.sized(size)),
          Position.origin,
          false,
          none[TerminalOptions],
          Vector.empty[Buffer],
          Vector.empty[BackendCall],
        )
      ),
      policy,
    )

  extension (backend: TestBackend) {

    /** A snapshot of the whole state. */
    def state: State = backend.ref.get()

    /** The screen as a buffer. */
    def screen: Buffer = backend.state.buffer

    /** Every effectful call so far, in order. */
    def calls: Vector[BackendCall] = backend.state.calls

    /** The cursor position. */
    def cursorPosition: Position = backend.state.cursor

    /** True after `showCursor`, false after `hideCursor` (the initial state). */
    def cursorVisible: Boolean = backend.state.cursorVisible

    /** The options of the last `enter`, `None` before it and after `exit`. */
    def entered: Option[TerminalOptions] = backend.state.entered

    /** The rows every `print` emitted, in order (the scrollback). */
    def printed: Vector[Buffer] = backend.state.printed

    /** The test harness resize: a blank screen of the new size, the cursor kept, nothing logged (a real resize reaches the render
      * loop as an event, not as a backend call).
      */
    def resize(size: Size): Unit =
      backend.ref.updateAndGet(state => state.copy(buffer = Buffer.emptyWith(backend.policy, Rect.sized(size)))): Unit

  }

}
