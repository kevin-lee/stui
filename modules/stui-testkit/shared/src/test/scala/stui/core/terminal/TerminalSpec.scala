package stui.core.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.{ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
import stui.core.style.Style
import stui.testkit.{Assertions, BackendCall, ManualClock, TestBackend}
import stui.testkit.TestBackend.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*
import scala.util.Try

/** The orchestration over the in-memory backend: the lifecycle, the present protocol, the diff, redraw reasons, resize, statistics,
  * the restore on failure, drawing after close, and the latch.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object TerminalSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val options: TerminalOptions = TerminalOptions.alternateScreen

  private val capabilities: Capabilities = Capabilities.conservative

  private def clock: ManualClock = ManualClock.of(0.nanos)

  private def text(canvas: stui.core.buffer.Canvas, s: String): Unit = canvas.putString(at(0, 0), s, Style.empty)

  /** A backend that refuses to enter and counts exits. */
  final class RefusingBackend extends TerminalBackend {
    private val exits: AtomicReference[Int]                                   = new AtomicReference(0)
    def exitCount: Int                                                        = exits.get()
    override def size(): Size                                                 = sized(1, 1)
    override def draw(updates: Vector[CellUpdate]): Unit                      = ()
    override def flush(): NonNegInt                                           = NonNegInt(0)
    override def moveCursor(position: Position): Unit                         = ()
    override def showCursor(): Unit                                           = ()
    override def hideCursor(): Unit                                           = ()
    override def clear(): Unit                                                = ()
    override def print(rows: Buffer): Unit                                    = ()
    override def enter(options: TerminalOptions): Either[TerminalError, Unit] = TerminalError.NotATerminal.asLeft[Unit]
    override def exit(): Unit                                                 = exits.updateAndGet(_ + 1): Unit
  }

  override def tests: List[Test] = List(
    example("run enters, runs the block, and exits", testRun),
    example("an inline screen mode is rejected without touching the backend", testInline),
    example("a backend that cannot enter gives Left and is not exited", testRefused),
    example("the present protocol: draw, cursor placement, hide only when shown, flush", testProtocol),
    example("the second draw emits the diff and the screen follows", testDiff),
    example("a redraw reason forces every cell once", testRedraw),
    example("a resize forces every cell at the new size", testResize),
    example("the statistics carry the cells, the bytes, and the clock delta", testStats),
    example("a failing block still exits", testFailure),
    example("drawing after close emits nothing", testAfterClose),
    example("the latch keeps the first positive signal", testLatch),
  )

  def testRun: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, clock)(_ => 42)
    Result.all(
      List(
        Assertions.eqv(result, 42.asRight[TerminalError]),
        Assertions.eqv(backend.calls, Vector(BackendCall.Enter(options), BackendCall.Exit)),
      )
    )
  }

  def testInline: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val inline  = TerminalOptions.of(ScreenMode.Inline(PosInt(5)))
    val result  = Terminal.run(backend, inline, capabilities, clock)(_ => 1)
    Result.all(
      List(
        Assertions.eqv(result, TerminalError.unsupportedScreenMode(ScreenMode.Inline(PosInt(5))).asLeft[Int]),
        Assertions.eqv(backend.calls, Vector.empty[BackendCall]),
      )
    )
  }

  def testRefused: Result = {
    val backend = new RefusingBackend
    val result  = Terminal.run(backend, options, capabilities, clock)(_ => 1)
    Result.all(List(Assertions.eqv(result, (TerminalError.NotATerminal: TerminalError).asLeft[Int]), backend.exitCount ==== 0))
  }

  def testProtocol: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, clock) { terminal =>
      val first  = terminal.draw(_ => ())
      val second = terminal.draw(_.cursor(at(1, 0)))
      val third  = terminal.draw(_ => ())
      val fourth = terminal.draw(_.cursor(at(2, 0)))
      (first.stats.cells.value, second.frame.cursor, third.frame.cursor, fourth.frame.cursor)
    }
    Result.all(
      List(
        Assertions.eqv(result.map({ case (cells, _, _, _) => cells }), 5.asRight[TerminalError]),
        Assertions.eqv(
          backend.calls.map {
            case BackendCall.Draw(updates) => s"draw(${updates.length.toString})"
            case BackendCall.Flush => "flush"
            case BackendCall.MoveCursor(position) => s"move(${position.x.value.toString},${position.y.value.toString})"
            case BackendCall.ShowCursor => "show"
            case BackendCall.HideCursor => "hide"
            case BackendCall.Clear => "clear"
            case BackendCall.Print(_) => "print"
            case BackendCall.Enter(_) => "enter"
            case BackendCall.Exit => "exit"
          },
          Vector(
            "enter",
            "draw(5)",
            "flush",
            "draw(0)",
            "move(1,0)",
            "show",
            "flush",
            "hide",
            "draw(0)",
            "flush",
            "draw(0)",
            "move(2,0)",
            "show",
            "flush",
            "exit",
          ),
        ),
      )
    )
  }

  def testDiff: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.draw(text(_, "hello")): Unit
      terminal.draw(text(_, "hallo")).stats.cells.value
    }
    Result.all(List(Assertions.eqv(result, 1.asRight[TerminalError]), Assertions.grid(backend.screen, Vector("hallo"))))
  }

  def testRedraw: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.draw(text(_, "hi")): Unit
      terminal.redraw(RedrawReason.Requested)
      val forced = terminal.draw(text(_, "hi")).stats.cells.value
      val again  = terminal.draw(text(_, "hi")).stats.cells.value
      (forced, again)
    }
    Assertions.eqv(result, (5, 0).asRight[TerminalError])
  }

  def testResize: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.draw(text(_, "hi")): Unit
      backend.resize(sized(7, 2))
      val completed = terminal.draw(text(_, "hi"))
      (completed.stats.cells.value, terminal.viewport, completed.frame.buffer.area)
    }
    Assertions.eqv(result, (14, Rect.sized(sized(7, 2)), Rect.sized(sized(7, 2))).asRight[TerminalError])
  }

  def testStats: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Terminal.run(backend, options, capabilities, ManualClock.ticking(0.nanos, 1.milli))(_.draw(_ => ()).stats)
    Assertions.eqv(result, RenderStats(NonNegInt(0), NonNegInt(5), 1.milli).asRight[TerminalError])
  }

  def testFailure: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val result  = Try(Terminal.run(backend, options, capabilities, clock)(_ => sys.error("boom")))
    Result.all(
      List(
        Result.assert(result.isFailure).log("expected a failure"),
        Assertions.eqv(backend.calls, Vector(BackendCall.Enter(options), BackendCall.Exit)),
      )
    )
  }

  def testAfterClose: Result = {
    val backend  = TestBackend.of(sized(5, 1))
    val captured = new AtomicReference(none[Terminal])
    Terminal.run(backend, options, capabilities, clock)(terminal => captured.set(terminal.some)): Unit
    captured.get() match {
      case Some(terminal) =>
        val completed = terminal.draw(text(_, "late"))
        Result.all(
          List(
            Result.assert(terminal.isClosed).log("closed"),
            Assertions.eqv(completed.stats.bytes, NonNegInt(0)),
            Assertions.eqv(backend.calls, Vector(BackendCall.Enter(options), BackendCall.Exit)),
            Assertions.grid(completed.frame.buffer, Vector("late ")),
          )
        )
      case None => Result.failure.log("the block did not run")
    }
  }

  def testLatch: Result = {
    val latch = TeardownLatch()
    val fresh = TeardownLatch()
    val empty = latch.recorded
    latch.record(15)
    latch.record(2)
    fresh.record(0)
    Result.all(List(Assertions.eqv(empty, none[Int]), Assertions.eqv(latch.recorded, 15.some), Assertions.eqv(fresh.recorded, none[Int])))
  }

}
