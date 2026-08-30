package stui.core.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.{PrintEffect, ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
import stui.core.style.Style
import stui.core.text.{Line, Text}
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
    override def viewport(): Rect                                             = Rect.sized(size())
    override def draw(updates: Vector[CellUpdate]): Unit                      = ()
    override def flush(): NonNegInt                                           = NonNegInt(0)
    override def moveCursor(position: Position): Unit                         = ()
    override def showCursor(): Unit                                           = ()
    override def hideCursor(): Unit                                           = ()
    override def clear(): Unit                                                = ()
    override def print(rows: Buffer): PrintEffect                             = PrintEffect.ViewportKept
    override def enter(options: TerminalOptions): Either[TerminalError, Unit] = TerminalError.NotATerminal.asLeft[Unit]
    override def exit(): Unit                                                 = exits.updateAndGet(_ + 1): Unit
  }

  override def tests: List[Test] = List(
    example("run enters, runs the block, and exits", testRun),
    example("an inline screen mode reaches the backend", testInline),
    example("a backend that cannot enter gives Left and is not exited", testRefused),
    example("the present protocol: draw, cursor placement, hide only when shown, flush", testProtocol),
    example("the second draw emits the diff and the screen follows", testDiff),
    example("a redraw reason forces every cell once", testRedraw),
    example("a resize forces every cell at the new size", testResize),
    example("the statistics carry the cells, the bytes, and the clock delta", testStats),
    example("a failing block still exits", testFailure),
    example("drawing after close emits nothing", testAfterClose),
    example("the latch keeps the first positive signal", testLatch),
    example("an alternate-screen print is buffered and flushed after exit", testPrintBuffered),
    example("trailing blank rows are trimmed and a blank widget prints nothing", testPrintTrim),
    example("the ring bound keeps the newest prints", testPrintBound),
    example("an inline print that loses the viewport forces a full redraw", testInlinePrintLost),
    example("an inline print that keeps the viewport leaves the diff", testInlinePrintKept),
    example("a print after close does nothing", testPrintAfterClose),
    example("a viewport override renders the frame at its rect", testViewportOverride),
    example("a failing block still flushes the transcript", testPrintOnFailure),
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
        Assertions.eqv(result, 1.asRight[TerminalError]),
        Assertions.eqv(backend.calls, Vector(BackendCall.Enter(inline), BackendCall.Exit)),
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

  def testPrintBuffered: Result = {
    val backend = TestBackend.of(sized(5, 2))
    val during  = new AtomicReference(Vector.empty[Buffer])
    Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.print(Line.raw("hi"))
      during.set(backend.printed)
    }: Unit
    Result.all(
      List(
        Assertions.eqv(during.get(), Vector.empty[Buffer]),
        Assertions.eqv(backend.printed.map(Buffer.renderRows), Vector(Vector("hi   "))),
        Assertions.eqv(
          backend.calls.map {
            case BackendCall.Enter(_) => "enter"
            case BackendCall.Exit => "exit"
            case BackendCall.Print(_) => "print"
            case BackendCall.Flush => "flush"
            case BackendCall.Draw(_) => "draw"
            case BackendCall.MoveCursor(_) => "move"
            case BackendCall.ShowCursor => "show"
            case BackendCall.HideCursor => "hide"
            case BackendCall.Clear => "clear"
          },
          Vector("enter", "exit", "print", "flush"),
        ),
      )
    )
  }

  def testPrintTrim: Result = {
    val backend = TestBackend.of(sized(4, 3))
    Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.print(Text.of(Line.raw("a"), Line.raw("")))
      terminal.print(Line.raw(""))
    }: Unit
    Assertions.eqv(backend.printed.map(Buffer.renderRows), Vector(Vector("a   ")))
  }

  def testPrintBound: Result = {
    val backend = TestBackend.of(sized(3, 1))
    val bounded = options.withPrintBufferRows(PosInt(2))
    Terminal.run(backend, bounded, capabilities, clock) { terminal =>
      terminal.print(Line.raw("a"))
      terminal.print(Line.raw("b"))
      terminal.print(Line.raw("c"))
    }: Unit
    Assertions.eqv(backend.printed.map(Buffer.renderRows), Vector(Vector("b  "), Vector("c  ")))
  }

  def testInlinePrintLost: Result = {
    val backend = TestBackend.printing(PrintEffect.ViewportLost, sized(5, 1))
    val inline  = TerminalOptions.of(ScreenMode.Inline(PosInt(1)))
    val result  = Terminal.run(backend, inline, capabilities, clock) { terminal =>
      terminal.draw(text(_, "hi")): Unit
      terminal.print(Line.raw("x"))
      (backend.printed.length, terminal.draw(text(_, "hi")).stats.cells.value)
    }
    Assertions.eqv(result, (1, 5).asRight[TerminalError])
  }

  def testInlinePrintKept: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val inline  = TerminalOptions.of(ScreenMode.Inline(PosInt(1)))
    val result  = Terminal.run(backend, inline, capabilities, clock) { terminal =>
      terminal.draw(text(_, "hi")): Unit
      terminal.print(Line.raw("x"))
      (backend.printed.length, terminal.draw(text(_, "hi")).stats.cells.value)
    }
    Assertions.eqv(result, (1, 0).asRight[TerminalError])
  }

  def testPrintAfterClose: Result = {
    val backend  = TestBackend.of(sized(3, 1))
    val captured = new AtomicReference(none[Terminal])
    Terminal.run(backend, options, capabilities, clock)(terminal => captured.set(terminal.some)): Unit
    captured.get().foreach(_.print(Line.raw("x")))
    Result.all(
      List(
        Assertions.eqv(backend.printed, Vector.empty[Buffer]),
        Assertions.eqv(backend.calls.lastOption, Option(BackendCall.Exit)),
      )
    )
  }

  def testViewportOverride: Result = {
    val backend = TestBackend.of(sized(5, 5))
    val inlined = Rect(NonNegInt(0), NonNegInt(2), NonNegInt(5), NonNegInt(3))
    backend.setViewport(inlined)
    val result  = Terminal.run(backend, options, capabilities, clock) { terminal =>
      val completed = terminal.draw(_ => ())
      (terminal.viewport, completed.frame.buffer.area, completed.stats.cells.value)
    }
    Assertions.eqv(result, (inlined, inlined, 15).asRight[TerminalError])
  }

  def testPrintOnFailure: Result = {
    val backend = TestBackend.of(sized(3, 1))
    val result  = Try(Terminal.run(backend, options, capabilities, clock) { terminal =>
      terminal.print(Line.raw("t"))
      sys.error("boom")
    })
    Result.all(
      List(
        Result.assert(result.isFailure).log("expected a failure"),
        Assertions.eqv(backend.printed.map(Buffer.renderRows), Vector(Vector("t  "))),
      )
    )
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
