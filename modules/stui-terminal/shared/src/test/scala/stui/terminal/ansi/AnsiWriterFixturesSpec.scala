package stui.terminal.ansi

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Cell, CellUpdate, GlyphWidth}
import stui.core.capability.{Capabilities, ColorProfile}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.core.style.{CellStyle, Color, Style}
import stui.testkit.Assertions
import stui.testkit.gen.NastyGens

/** Exact writer output for hand-picked presents.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object AnsiWriterFixturesSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val lossless: Capabilities = Capabilities.lossless

  private val five: Rect = Rect.sized(Size(NonNegInt(5), NonNegInt(1)))

  private def all(buffer: Buffer): Vector[CellUpdate] = Buffer.allUpdates(buffer)

  private def present(capabilities: Capabilities, viewport: Rect, updates: Vector[CellUpdate]): (WriterState, String) =
    AnsiWriter.present(WriterState.initial, capabilities, viewport, updates)

  private def cup(row: Int, column: Int): String = Sequences.cup(row, column)

  private def row(width: Int, text: String, style: Style): Buffer =
    Buffer.empty(Rect.sized(Size(NonNegInt.unsafeFrom(width), NonNegInt(1)))).draw(_.putString(at(0, 0), text, style))

  override def tests: List[Test] = List(
    example("one cell", Assertions.eqv(present(lossless, five, Vector(CellUpdate(at(0, 0), glyph("a"))))._2, cup(1, 1) + "a")),
    example(
      "consecutive cells move once",
      Assertions
        .eqv(present(lossless, five, Vector(CellUpdate(at(0, 0), glyph("a")), CellUpdate(at(1, 0), glyph("b"))))._2, cup(1, 1) + "ab"),
    ),
    example(
      "a gap moves again",
      Assertions.eqv(
        present(lossless, five, Vector(CellUpdate(at(0, 0), glyph("a")), CellUpdate(at(3, 0), glyph("b"))))._2,
        cup(1, 1) + "a" + cup(1, 4) + "b",
      ),
    ),
    example(
      "a red cell is followed by a delta back to the default",
      Assertions.eqv(
        present(lossless, five, all(row(5, "a", Style.empty.withFg(Color.Red))))._2,
        cup(1, 1) + Sequences.Csi + "31m" + "a" + Sequences.Csi + "39m" + "    ",
      ),
    ),
    example(
      "a red last cell resets at the end",
      Assertions.eqv(
        present(
          lossless,
          five,
          Vector(
            CellUpdate(
              at(4, 0),
              Cell.glyph(stui.core.buffer.GlyphSymbol.unsafeFrom("a"), GlyphWidth.One, CellStyle.default.copy(fg = Color.Red)),
            )
          ),
        )._2,
        cup(1, 5) + Sequences.Csi + "31m" + "a" + Sequences.Csi + "0m",
      ),
    ),
    example("a wide glyph is emitted once", testWide),
    example("a VS16 glyph under a narrow terminal gets a shadow space", testVs16),
    example(
      "an orphan continuation is a default space",
      Assertions.eqv(
        present(lossless, five, Vector(CellUpdate(at(1, 0), Cell.continuation(CellStyle.default.copy(fg = Color.Red)))))._2,
        cup(1, 2) + " ",
      ),
    ),
    example("the last column leaves a pending wrap", testPendingWrap),
    example("an update after a pending wrap moves absolutely", testAfterPendingWrap),
    example("an update outside the viewport emits nothing", testOutside),
    example("print writes styled rows with CR LF", testPrint),
    example(
      "Mono emits no colour parameters",
      Assertions.eqv(
        present(Capabilities.conservative.withColors(ColorProfile.Mono), five, all(row(5, "a", Style.empty.withFg(Color.Red))))._2,
        cup(1, 1) + "a    ",
      ),
    ),
    example("moveCursor to the tracked position emits nothing", testMoveCursor),
    example("enter and exit", testEnterExit),
    example("a wide glyph at the last column is a space in its style", testWideAtEdge),
    example("a present at a viewport origin addresses absolute rows", testViewportOrigin),
    example("the inline entry pads, hides, and arms the region", testEnterInline),
    example("the inline exit resets the region and parks the cursor", testExitInline),
    example("the region print scrolls rows in at the region bottom", testPrintRegion),
    example("the overlay print erases, writes, and moves the viewport", testPrintOverlay),
  )

  def testViewportOrigin: Result = {
    val viewport = Rect(NonNegInt(0), NonNegInt(2), NonNegInt(5), NonNegInt(1))
    val updates  = Vector(CellUpdate(at(0, 2), glyph("a")), CellUpdate(at(1, 2), glyph("b")))
    AnsiWriter.present(WriterState.initial, lossless, viewport, updates) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(output, cup(3, 1) + "ab"),
            Assertions.eqv(state.cursor, CursorState.Known(at(2, 2))),
          )
        )
    }
  }

  def testEnterInline: Result = {
    val options = TerminalOptions.of(ScreenMode.Inline(refined4s.types.numeric.PosInt(3)))
    val region  = ScrollRegion(NonNegInt(0), NonNegInt(1))
    AnsiWriter.enterInline(options, 2, Some(region)) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(output, Sequences.Cr + Sequences.Lf + Sequences.Lf + Sequences.CursorHide + Sequences.armRegion(1, 2)),
            Assertions.eqv(state.cursor, (CursorState.Unknown: CursorState)),
            Assertions.eqv(state.region, Some(region)),
            Assertions.eqv(
              AnsiWriter.enterInline(options, 0, None)._2,
              Sequences.Cr + Sequences.CursorHide,
            ),
          )
        )
    }
  }

  def testExitInline: Result =
    Assertions.eqv(
      AnsiWriter.exitInline(Rect(NonNegInt(0), NonNegInt(3), NonNegInt(8), NonNegInt(3))),
      Sequences.FocusDisable + Sequences.BracketedPasteDisable + Sequences.MouseTrackingDisable + Sequences.SgrReset +
        Sequences.CursorShow + Sequences.resetRegion + cup(6, 1) + Sequences.CrLf,
    )

  def testPrintRegion: Result = {
    val region = ScrollRegion(NonNegInt(0), NonNegInt(3))
    val rows   = Buffer.fromLines(Vector("ab"))
    AnsiWriter.printRegion(WriterState.initial.copy(region = Some(region)), lossless, region, 5, rows) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(output, cup(4, 1) + Sequences.Lf + Sequences.Cr + "ab" + Sequences.EraseToLineEnd),
            Assertions.eqv(state.cursor, (CursorState.Unknown: CursorState)),
            Assertions.eqv(state.region, Some(region)),
          )
        )
    }
  }

  def testPrintOverlay: Result = {
    val viewport = Rect(NonNegInt(0), NonNegInt(2), NonNegInt(5), NonNegInt(2))
    val terminal = Size(NonNegInt(5), NonNegInt(5))
    AnsiWriter.printOverlay(WriterState.initial, lossless, viewport, terminal, Buffer.fromLines(Vector("x"))) match {
      case (state, moved, output) =>
        Result.all(
          List(
            Assertions.eqv(output, cup(3, 1) + Sequences.EraseBelow + "x" + Sequences.EraseToLineEnd + Sequences.CrLf),
            Assertions.eqv(moved, Rect(NonNegInt(0), NonNegInt(3), NonNegInt(5), NonNegInt(2))),
            Assertions.eqv(state.cursor, (CursorState.Unknown: CursorState)),
          )
        )
    }
  }

  private def glyph(symbol: String): Cell = Cell.glyph(stui.core.buffer.GlyphSymbol.unsafeFrom(symbol), GlyphWidth.One, CellStyle.default)

  def testWide: Result = {
    val ko = cps(0x30b3)
    Assertions.eqv(present(lossless, five, all(row(5, ko + "a", Style.empty)))._2, cup(1, 1) + ko + "a  ")
  }

  def testVs16: Result = {
    val vs16 = cps(0x2328, 0xfe0f)
    val caps = lossless.withVs16Width(GlyphWidth.One)
    Result.all(
      List(
        Assertions.eqv(present(caps, five, all(row(5, vs16 + "a", Style.empty)))._2, cup(1, 1) + vs16 + " " + "a  "),
        Assertions.eqv(present(lossless, five, all(row(5, vs16 + "a", Style.empty)))._2, cup(1, 1) + vs16 + "a  "),
      )
    )
  }

  def testPendingWrap: Result =
    present(lossless, Rect.sized(Size(NonNegInt(3), NonNegInt(1))), all(row(3, "abc", Style.empty))) match {
      case (state, output) =>
        Result.all(List(Assertions.eqv(state.cursor, CursorState.PendingWrap(NonNegInt(0))), Assertions.eqv(output, cup(1, 1) + "abc")))
    }

  def testAfterPendingWrap: Result = {
    val three = Rect.sized(Size(NonNegInt(3), NonNegInt(1)))
    present(lossless, three, all(row(3, "abc", Style.empty))) match {
      case (state, _) =>
        Assertions.eqv(AnsiWriter.present(state, lossless, three, Vector(CellUpdate(at(0, 0), glyph("z"))))._2, cup(1, 1) + "z")
    }
  }

  def testOutside: Result =
    present(lossless, five, Vector(CellUpdate(at(5, 0), glyph("a")), CellUpdate(at(0, 1), glyph("b")))) match {
      case (state, output) => Result.all(List(Assertions.eqv(output, ""), Assertions.eqv(state, WriterState.initial)))
    }

  def testPrint: Result = {
    val rows =
      Buffer.fromLines(Vector("ab", "c")).draw(_.patchStyle(Rect.sized(Size(NonNegInt(1), NonNegInt(1))), Style.empty.withFg(Color.Red)))
    AnsiWriter.print(WriterState.initial, lossless, rows) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(
              output,
              Sequences.Csi + "31m" + "a" + Sequences.Csi + "39m" + "b" + Sequences.CrLf +
                "c" + Sequences.EraseToLineEnd + Sequences.CrLf,
            ),
            Assertions.eqv(state.cursor, CursorState.Unknown),
            Assertions.eqv(state.style, CellStyle.default),
          )
        )
    }
  }

  def testMoveCursor: Result = {
    val known = WriterState.initial.copy(cursor = CursorState.Known(at(2, 0)))
    Result.all(
      List(
        Assertions.eqv(AnsiWriter.moveCursor(known, five, at(2, 0))._2, ""),
        Assertions.eqv(AnsiWriter.moveCursor(known, five, at(4, 0))._2, cup(1, 5)),
        Assertions.eqv(AnsiWriter.moveCursor(known, five, at(9, 9))._2, cup(1, 5)),
        Assertions.eqv(AnsiWriter.moveCursor(WriterState.initial, five, at(0, 0))._2, cup(1, 1)),
      )
    )
  }

  def testEnterExit: Result = {
    val options = TerminalOptions.of(ScreenMode.AlternateScreen, TerminalFeature.MouseCapture)
    AnsiWriter.enter(options) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(output, Sequences.enter(options)),
            Assertions.eqv(state.cursor, CursorState.Known(Position.origin)),
            Assertions.eqv(AnsiWriter.exit, Sequences.SafeReset),
            Assertions.eqv(AnsiWriter.clearAll(state)._2, Sequences.ClearScreen + Sequences.CursorHome),
            Assertions.eqv(AnsiWriter.hideCursor(state)._2, Sequences.CursorHide),
            Assertions.eqv(AnsiWriter.showCursor(state)._2, Sequences.CursorShow),
          )
        )
    }
  }

  def testWideAtEdge: Result = {
    val ko     = stui.core.buffer.GlyphSymbol.unsafeFrom(cps(0x30b3))
    val red    = CellStyle.default.copy(fg = Color.Red)
    val update = CellUpdate(at(4, 0), Cell.glyph(ko, GlyphWidth.Two, red))
    present(lossless, five, Vector(update)) match {
      case (state, output) =>
        Result.all(
          List(
            Assertions.eqv(output, cup(1, 5) + Sequences.Csi + "31m" + " " + Sequences.Csi + "0m"),
            Assertions.eqv(state.cursor, CursorState.PendingWrap(NonNegInt(0))),
          )
        )
    }
  }

}
