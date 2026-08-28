package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Buffer
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.{ScreenMode, TerminalError, TerminalFeature, TerminalOptions}
import stui.testkit.Assertions
import stui.terminal.AnsiBackend.*
import stui.terminal.FakeTty.*
import stui.terminal.ansi.{AnsiWriter, Sequences, WriterState}

import java.nio.charset.StandardCharsets

/** The shared backend over a fake device: the entry protocol, queuing and flushing, the idempotent exit, and the size fallback.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object AnsiBackendSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val options: TerminalOptions = TerminalOptions.of(ScreenMode.AlternateScreen, TerminalFeature.MouseCapture)

  private val capabilities: Capabilities = Capabilities.lossless

  override def tests: List[Test] = List(
    example("enter writes the entry sequence after raw mode", testEnter),
    example("enter on a non-terminal is Left and writes nothing", testNotATerminal),
    example("enter with an inline mode is unsupported", testInline),
    example("enter with a failing device is a platform failure", testFailing),
    example("draw queues and flush writes the bytes", testDrawFlush),
    example("flush with nothing queued returns 0", testEmptyFlush),
    example("exit writes the safe reset once and discards queued output", testExit),
    example("the size falls back to the last report and to 80x24", testSize),
    example("the cursor and print operations queue their sequences", testQueued),
  )

  def testEnter: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    val result  = backend.enter(options)
    Result.all(
      List(Assertions.eqv(result, ().asRight[TerminalError]), Assertions.eqv(tty.output, Sequences.enter(options)), tty.enteredCount ==== 1)
    )
  }

  def testNotATerminal: Result = {
    val tty     = FakeTty.notATerminal(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    Result.all(
      List(
        Assertions.eqv(backend.enter(options), (TerminalError.NotATerminal: TerminalError).asLeft[Unit]),
        tty.writes ==== 0,
        tty.enteredCount ==== 0,
      )
    )
  }

  def testInline: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    Result.all(
      List(
        Assertions.eqv(
          backend.enter(TerminalOptions.of(ScreenMode.Inline(PosInt(3)))),
          TerminalError.unsupportedScreenMode(ScreenMode.Inline(PosInt(3))).asLeft[Unit],
        ),
        tty.writes ==== 0,
      )
    )
  }

  def testFailing: Result = {
    val tty     = FakeTty.failing(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    Result.all(
      List(Assertions.eqv(backend.enter(options), TerminalError.platformFailure("tcsetattr", "fake").asLeft[Unit]), tty.writes ==== 0)
    )
  }

  def testDrawFlush: Result = {
    val tty      = FakeTty.of(sized(5, 1))
    val backend  = AnsiBackend(tty, capabilities)
    val updates  = Buffer.allUpdates(Buffer.fromLines(Vector("hello")))
    backend.draw(updates)
    val queued   = backend.pendingOutput
    val flushed  = backend.flush()
    val expected = AnsiWriter.present(WriterState.initial, capabilities, Rect.sized(sized(5, 1)), updates)._2
    Result.all(
      List(
        Assertions.eqv(queued, expected),
        Assertions.eqv(tty.output, expected),
        Assertions.eqv(flushed.value, expected.getBytes(StandardCharsets.UTF_8).length),
        Assertions.eqv(backend.pendingOutput, ""),
      )
    )
  }

  def testEmptyFlush: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    Result.all(List(Assertions.eqv(backend.flush(), NonNegInt(0)), tty.writes ==== 0))
  }

  def testExit: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    val entered = backend.enter(options)
    backend.draw(Buffer.allUpdates(Buffer.fromLines(Vector("hello"))))
    backend.exit()
    backend.exit()
    Result.all(
      List(
        Assertions.eqv(entered, ().asRight[TerminalError]),
        Assertions.eqv(tty.output, Sequences.enter(options) + Sequences.SafeReset),
        tty.restoredCount ==== 1,
        Assertions.eqv(backend.pendingOutput, ""),
        Assertions.eqv(backend.writerState, WriterState.initial),
      )
    )
  }

  def testSize: Result = {
    val tty     = FakeTty.of(sized(10, 5))
    val backend = AnsiBackend(tty, capabilities)
    val blind   = FakeTty.of(sized(1, 1))
    blind.report(none[Size])
    val first   = backend.size()
    tty.report(none[Size])
    Result.all(
      List(
        Assertions.eqv(first, sized(10, 5)),
        Assertions.eqv(backend.size(), sized(10, 5)),
        Assertions.eqv(AnsiBackend(blind, capabilities).size(), AnsiBackend.fallbackSize),
      )
    )
  }

  def testQueued: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, capabilities)
    backend.hideCursor()
    backend.moveCursor(Position(NonNegInt(2), NonNegInt(0)))
    backend.showCursor()
    backend.clear()
    backend.print(Buffer.fromLines(Vector("ab")))
    Assertions.eqv(
      backend.pendingOutput,
      Sequences.CursorHide + Sequences.cup(
        1,
        3,
      ) + Sequences.CursorShow + Sequences.ClearScreen + Sequences.CursorHome + "ab" + Sequences.CrLf,
    )
  }

}
