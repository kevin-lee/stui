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
  )

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
            Assertions.eqv(output, Sequences.Csi + "31m" + "a" + Sequences.Csi + "39m" + "b" + Sequences.CrLf + "c " + Sequences.CrLf),
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
            Assertions.eqv(AnsiWriter.clear(state)._2, Sequences.ClearScreen + Sequences.CursorHome),
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
