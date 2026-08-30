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
    example("the inline entry pads from the probed row and arms the region", testInline),
    example("the inline entry without a probed row scrolls a full screen", testInlineNoRow),
    example("the inline entry without synchronised output arms no region", testInlineNoSync),
    example("flush wraps the output in the 2026 bracket when supported", testFlushBracket),
    example("an inline print with a region keeps the viewport", testRegionPrint),
    example("an inline print without a region overlays and moves the viewport", testOverlayPrint),
    example("the inline exit resets the region and parks the cursor", testExitInline),
    example("a terminal size change reconciles the geometry and the region", testReconcile),
    example("clear in inline mode erases only from the viewport", testInlineClear),
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

  private val syncCapabilities: Capabilities = Capabilities.lossless.copy(syncOutput = true)

  private val inlineOptions: TerminalOptions = TerminalOptions.of(ScreenMode.Inline(PosInt(3)))

  private def inlineBackend(tty: FakeTty, caps: Capabilities, row: Option[Int]): AnsiBackend =
    AnsiBackend.withEntryRow(tty, caps, row.map(NonNegInt.unsafeFrom))

  def testInline: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, Some(5))
    val entered = backend.enter(inlineOptions)
    Result.all(
      List(
        Assertions.eqv(entered, ().asRight[TerminalError]),
        Assertions.eqv(tty.output, Sequences.Cr + (Sequences.Lf * 2) + Sequences.CursorHide + Sequences.armRegion(1, 3)),
        Assertions.eqv(backend.viewport(), Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3))),
        Assertions.eqv(backend.size(), sized(8, 6)),
        tty.enteredCount ==== 1,
      )
    )
  }

  def testInlineNoRow: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, None)
    backend.enter(inlineOptions): Unit
    Result.all(
      List(
        Assertions.eqv(tty.output, Sequences.Cr + (Sequences.Lf * 5) + Sequences.CursorHide + Sequences.armRegion(1, 3)),
        Assertions.eqv(backend.viewport(), Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3))),
      )
    )
  }

  def testInlineNoSync: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, capabilities, Some(5))
    backend.enter(inlineOptions): Unit
    Result.all(
      List(
        Assertions.eqv(tty.output, Sequences.Cr + (Sequences.Lf * 2) + Sequences.CursorHide),
        Assertions.eqv(backend.writerState.region, none[stui.terminal.ansi.ScrollRegion]),
      )
    )
  }

  def testFlushBracket: Result = {
    val tty     = FakeTty.of(sized(5, 1))
    val backend = AnsiBackend(tty, syncCapabilities)
    val updates = Buffer.allUpdates(Buffer.fromLines(Vector("hi")))
    backend.draw(updates)
    val inner   = AnsiWriter.present(WriterState.initial, syncCapabilities, Rect.sized(sized(5, 1)), updates)._2
    val flushed = backend.flush()
    val empty   = backend.flush()
    Result.all(
      List(
        Assertions.eqv(tty.output, Sequences.SyncBegin + inner + Sequences.SyncEnd),
        Assertions.eqv(flushed.value, (Sequences.SyncBegin + inner + Sequences.SyncEnd).getBytes(StandardCharsets.UTF_8).length),
        Assertions.eqv(empty, NonNegInt(0)),
        tty.writes ==== 1,
      )
    )
  }

  def testRegionPrint: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, Some(5))
    backend.enter(inlineOptions): Unit
    val effect  = backend.print(Buffer.fromLines(Vector("ab")))
    val region  = stui.terminal.ansi.ScrollRegion(NonNegInt(0), NonNegInt(2))
    Result.all(
      List(
        Assertions.eqv(effect, (stui.core.spi.PrintEffect.ViewportKept: stui.core.spi.PrintEffect)),
        Assertions.eqv(
          backend.pendingOutput,
          AnsiWriter.printRegion(backend.writerState, syncCapabilities, region, 8, Buffer.fromLines(Vector("ab")))._2,
        ),
        Assertions.eqv(backend.viewport(), Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3))),
      )
    )
  }

  def testOverlayPrint: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, capabilities, Some(5))
    backend.enter(inlineOptions): Unit
    val effect  = backend.print(Buffer.fromLines(Vector("ab")))
    Result.all(
      List(
        Assertions.eqv(effect, (stui.core.spi.PrintEffect.ViewportLost: stui.core.spi.PrintEffect)),
        Result.assert(backend.pendingOutput.startsWith(Sequences.cup(4, 1) + Sequences.EraseBelow)).log(backend.pendingOutput),
        Assertions.eqv(backend.viewport(), Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3))),
      )
    )
  }

  def testExitInline: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, Some(5))
    backend.enter(inlineOptions): Unit
    val entry   = tty.output
    backend.exit()
    backend.exit()
    Result.all(
      List(
        Assertions.eqv(tty.output, entry + AnsiWriter.exitInline(Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3)))),
        tty.restoredCount ==== 1,
      )
    )
  }

  def testReconcile: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, Some(5))
    backend.enter(inlineOptions): Unit
    tty.report(sized(8, 4).some)
    val shrunk  = backend.viewport()
    Result.all(
      List(
        Assertions.eqv(shrunk, Rect(NonNegInt(0), NonNegInt(1), NonNegInt(8), NonNegInt(3))),
        Result.assert(backend.pendingOutput.contains(Sequences.resetRegion)).log(backend.pendingOutput),
        Assertions.eqv(backend.writerState.region, none[stui.terminal.ansi.ScrollRegion]),
      )
    )
  }

  def testInlineClear: Result = {
    val tty     = FakeTty.of(sized(8, 6))
    val backend = inlineBackend(tty, syncCapabilities, Some(5))
    backend.enter(inlineOptions): Unit
    backend.clear()
    Assertions.eqv(backend.pendingOutput, Sequences.cup(4, 1) + Sequences.EraseBelow)
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
    backend.print(Buffer.fromLines(Vector("ab"))): Unit
    Assertions.eqv(
      backend.pendingOutput,
      Sequences.CursorHide + Sequences.cup(
        1,
        3,
      ) + Sequences.CursorShow + Sequences.ClearScreen + Sequences.CursorHome + "ab" + Sequences.CrLf,
    )
  }

}
