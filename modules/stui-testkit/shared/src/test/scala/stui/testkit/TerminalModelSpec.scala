package stui.testkit

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Cell, GlyphSymbol, GlyphWidth}
import stui.core.geometry.{Position, Size}
import stui.core.style.{CellStyle, Color, UnderlineStyle}
import stui.testkit.TerminalModel.{ModelError, QuirkProfile, Screen}
import stui.testkit.gen.{BufferGens, NastyGens}

/** The oracle itself: the projection under the default profile is the identity, the VS16 projection, and the interpreter's handling of
  * text, wrapping, scrolling, clearing, SGR, unknown input, cursor moves, and private modes.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object TerminalModelSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private val esc: String = cps(0x1b)

  private val csi: String = esc + "["

  private def cup(row: Int, column: Int): String = s"$csi${row.toString};${column.toString}H"

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val default: QuirkProfile = QuirkProfile.default

  private val narrow: QuirkProfile = QuirkProfile(GlyphWidth.One, true)

  private def blank(width: Int, height: Int): Screen =
    Screen.blank(default, Size(NonNegInt.unsafeFrom(width), NonNegInt.unsafeFrom(height)))

  private def rows(result: Either[ModelError, Screen]): Either[ModelError, Vector[String]] = result.map(s => Buffer.renderRows(s.buffer))

  override def tests: List[Test] = List(
    property(
      "expected under the default profile is the identity",
      BufferGens.buffer.forAll.map(b => Assertions.eqv(TerminalModel.expected(default, b), b)),
    ),
    example("a VS16 glyph under a narrow profile becomes the glyph and a blank", testNarrowVs16),
    example(
      "text is written at the cursor",
      Assertions.eqv(rows(TerminalModel.interpret(default, blank(3, 1), cup(1, 1) + "ab")), Right(Vector("ab "))),
    ),
    example("the cursor advances by the text", testCursorAdvance),
    example("the last column sets pending wrap", testPendingWrap),
    example(
      "a printable after pending wrap on the last row scrolls",
      Assertions.eqv(TerminalModel.interpret(default, blank(3, 1), cup(1, 1) + "abcd"), Left(ModelError.Scrolled)),
    ),
    example("a printable after pending wrap on an upper row wraps", testWraps),
    example("a wide glyph at the last column wraps first", testWideWraps),
    example(
      "CSI 2 J blanks the screen",
      Assertions.eqv(rows(TerminalModel.interpret(default, blank(3, 1), "ab" + csi + "2J")), Right(Vector("   "))),
    ),
    example("SGR 31 then a gives a red a", testRed),
    example(
      "SGR 4:3 gives a curly underline",
      Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), csi + "4:3m").map(_.style.underline), Right(UnderlineStyle.Curly)),
    ),
    example(
      "SGR 38;2 gives an RGB foreground",
      Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), csi + "38;2;1;2;3m").map(_.style.fg), Right(Color.rgb(1, 2, 3))),
    ),
    example(
      "SGR 99 is unknown",
      Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), csi + "99m"), Left(ModelError.Unknown("SGR 99"))),
    ),
    example("a TAB is unknown", Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), "\t"), Left(ModelError.Unknown("9")))),
    example("a lone ESC is unknown", Result.assert(TerminalModel.interpret(default, blank(1, 1), esc + "7").isLeft)),
    example(
      "CUP outside the viewport is refused",
      Assertions.eqv(TerminalModel.interpret(default, blank(10, 5), cup(40, 1)), Left(ModelError.OutsideViewport(at(0, 39)))),
    ),
    example(
      "?1003h records the mode",
      Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), csi + "?1003h").map(_.modes), Right(Set(1003))),
    ),
    example(
      "?25l hides the cursor",
      Assertions.eqv(TerminalModel.interpret(default, blank(1, 1), csi + "?25l").map(_.cursorVisible), Right(false)),
    ),
    example("?1049h clears under a clearing profile and keeps under the other", testAlternateEntry),
    example("an unknown private mode is refused", Result.assert(TerminalModel.interpret(default, blank(1, 1), csi + "?2026h").isLeft)),
  )

  def testNarrowVs16: Result = {
    val vs16   = cps(0x2328, 0xfe0f)
    val buffer = Buffer.fromLines(Vector(vs16 + "a"))
    val glyph  = GlyphSymbol.unsafeFrom(vs16)
    Result.all(
      List(
        buffer.area.width.value ==== 3,
        Assertions.eqv(
          TerminalModel.expected(narrow, buffer).cells.toVector,
          Vector(
            Cell.glyph(glyph, GlyphWidth.One, CellStyle.default),
            Cell.blank,
            Cell.glyph(GlyphSymbol.unsafeFrom("a"), GlyphWidth.One, CellStyle.default),
          ),
        ),
      )
    )
  }

  def testCursorAdvance: Result =
    TerminalModel.interpret(default, blank(3, 1), cup(1, 1) + "ab") match {
      case Right(screen) => Result.all(List(Assertions.eqv(screen.cursor, at(2, 0)), Result.assert(!screen.pendingWrap)))
      case Left(error) => Result.failure.log(error.show)
    }

  def testPendingWrap: Result =
    TerminalModel.interpret(default, blank(3, 1), cup(1, 1) + "abc") match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(Buffer.renderRows(screen.buffer), Vector("abc")),
            Assertions.eqv(screen.cursor, at(2, 0)),
            Result.assert(screen.pendingWrap),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }

  def testWraps: Result =
    TerminalModel.interpret(default, blank(3, 2), cup(1, 1) + "abcd") match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(Buffer.renderRows(screen.buffer), Vector("abc", "d  ")),
            Assertions.eqv(screen.cursor, at(1, 1)),
            Result.assert(!screen.pendingWrap),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }

  def testWideWraps: Result = {
    val ko = cps(0x30b3)
    TerminalModel.interpret(default, blank(3, 2), cup(1, 3) + ko) match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(Buffer.renderRows(screen.buffer), Vector("   ", ko + " ")),
            Assertions.eqv(screen.cursor, at(2, 1)),
            Result.assert(!screen.pendingWrap),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }
  }

  def testRed: Result =
    TerminalModel.interpret(default, blank(3, 1), csi + "31m" + "a") match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(screen.buffer.cell(at(0, 0)).map(_.style.fg), Color.Red.some),
            Assertions.eqv(screen.style.fg, Color.Red),
            Assertions.eqv(screen.buffer.cell(at(1, 0)).map(_.style.fg), Color.Reset.some),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }

  def testAlternateEntry: Result = {
    val keeping = QuirkProfile(GlyphWidth.Two, false)
    val cleared = TerminalModel.interpret(default, blank(2, 1), "ab" + csi + "?1049h")
    val kept    = TerminalModel.interpret(keeping, Screen.blank(keeping, sized(2, 1)), "ab" + csi + "?1049h")
    Result.all(
      List(
        Assertions.eqv(rows(cleared), Right(Vector("  "))),
        Assertions.eqv(rows(kept), Right(Vector("ab"))),
        Assertions.eqv(cleared.map(_.alternate), Right(true)),
        Assertions.eqv(kept.flatMap(s => TerminalModel.interpret(keeping, s, csi + "?1049l")).map(_.alternate), Right(false)),
      )
    )
  }

}
