package stui.core.buffer

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Position, Rect}
import stui.core.style.{CellStyle, Color, Style}
import stui.core.text.{Line, Span}
import stui.testkit.Assertions
import stui.testkit.gen.NastyGens

/** Hand-picked canvas scenarios: invariant repairs, dropped clusters, sanitisation, patch versus replace, clipping, and the inert leaked
  * canvas. Non-ASCII inputs are built from code points.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object CanvasFixturesSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private inline def rect(inline x: Int, inline y: Int, inline width: Int, inline height: Int): Rect =
    Rect(NonNegInt(x), NonNegInt(y), NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val ko: String = cps(0x30b3)

  private val blueBg: Style = Style.empty.withBg(Color.Blue)

  private def glyph(symbol: String): Cell = Cell.glyph(GlyphSymbol.unsafeFrom(symbol), GlyphWidth.One, CellStyle.default)

  private def cellsOf(buffer: Buffer): Vector[Cell] = buffer.cells.toVector

  override def tests: List[Test] = List(
    example("writing over a continuation blanks the owner", testOverContinuation),
    example("writing a narrow glyph over a wide one blanks its continuation", testOverWide),
    example("writing a wide glyph over the owner of the next column", testWideOverWide),
    example("a wide glyph at the last column is dropped", testWideAtEdge),
    example("a lone combining mark writes nothing", testZeroWidth),
    example("a lone surrogate becomes U+FFFD", testSurrogate),
    example("a control character is dropped", testControl),
    example("putString patches the existing style", testPatchOnWrite),
    example("fill replaces the style", testFillReplaces),
    example("patchStyle over the continuation column patches the owner", testPatchContinuation),
    example("putStringMax truncates at maxWidth", testMaxWidth),
    example("writes outside the area are no-ops", testOutside),
    example("a leaked canvas is inert", testLeak),
    example("putLine writes the resolved span styles", testPutLine),
  )

  def testOverContinuation: Result = {
    val buffer = Buffer.empty(rect(0, 0, 3, 1)).draw { canvas =>
      canvas.putString(at(0, 0), ko, Style.empty)
      canvas.putString(at(1, 0), "a", Style.empty)
    }
    Assertions.eqv(cellsOf(buffer), Vector(Cell.blank, glyph("a"), Cell.blank))
  }

  def testOverWide: Result = {
    val buffer = Buffer.empty(rect(0, 0, 3, 1)).draw { canvas =>
      canvas.putString(at(0, 0), ko, Style.empty)
      canvas.putString(at(0, 0), "b", Style.empty)
    }
    Assertions.eqv(cellsOf(buffer), Vector(glyph("b"), Cell.blank, Cell.blank))
  }

  def testWideOverWide: Result = {
    val buffer = Buffer.empty(rect(0, 0, 4, 1)).draw { canvas =>
      canvas.putString(at(1, 0), ko, Style.empty)
      canvas.putString(at(0, 0), ko, Style.empty)
    }
    Assertions.eqv(
      cellsOf(buffer),
      Vector(
        Cell.glyph(GlyphSymbol.unsafeFrom(ko), GlyphWidth.Two, CellStyle.default),
        Cell.continuation(CellStyle.default),
        Cell.blank,
        Cell.blank,
      ),
    )
  }

  def testWideAtEdge: Result = {
    val empty  = Buffer.empty(rect(0, 0, 3, 1))
    val buffer = empty.draw(_.putString(at(2, 0), ko, Style.empty))
    Assertions.eqv(buffer, empty)
  }

  def testZeroWidth: Result = {
    val empty = Buffer.empty(rect(0, 0, 3, 1))
    Assertions.eqv(empty.draw(_.putString(at(0, 0), cps(0x301), Style.empty)), empty)
  }

  def testSurrogate: Result = {
    val buffer = Buffer.empty(rect(0, 0, 2, 1)).draw(_.putString(at(0, 0), cps(0xd800), Style.empty))
    Assertions.eqv(cellsOf(buffer), Vector(glyph(cps(0xfffd)), Cell.blank))
  }

  def testControl: Result = {
    val buffer = Buffer.empty(rect(0, 0, 5, 1)).draw(_.putString(at(0, 0), cps(0x1b) + "[31m", Style.empty))
    Assertions.eqv(Buffer.renderRows(buffer), Vector("[31m "))
  }

  def testPatchOnWrite: Result = {
    val buffer = Buffer.empty(rect(0, 0, 2, 1)).draw { canvas =>
      canvas.fill(rect(0, 0, 2, 1), " ", blueBg)
      canvas.putString(at(0, 0), "a", Style.empty.withFg(Color.Red))
    }
    Assertions.eqv(cellsOf(buffer).map(_.style.bg), Vector(Color.Blue, Color.Blue))
  }

  def testFillReplaces: Result = {
    val buffer = Buffer.empty(rect(0, 0, 2, 1)).draw { canvas =>
      canvas.fill(rect(0, 0, 2, 1), "x", blueBg)
      canvas.fill(rect(0, 0, 2, 1), "y", Style.empty)
    }
    Assertions.eqv(cellsOf(buffer), Vector(glyph("y"), glyph("y")))
  }

  def testPatchContinuation: Result = {
    val buffer = Buffer.empty(rect(0, 0, 3, 1)).draw { canvas =>
      canvas.putString(at(0, 0), ko, Style.empty)
      canvas.patchStyle(rect(1, 0, 1, 1), blueBg)
    }
    Assertions.eqv(cellsOf(buffer).map(_.style.bg), Vector(Color.Blue, Color.Blue, Color.Reset))
  }

  def testMaxWidth: Result = {
    val buffer = Buffer.empty(rect(0, 0, 6, 1)).draw(_.putStringMax(at(0, 0), "abcdef", Style.empty, NonNegInt(3)))
    Assertions.eqv(Buffer.renderRows(buffer), Vector("abc   "))
  }

  def testOutside: Result = {
    val empty = Buffer.empty(rect(1, 1, 3, 2))
    val drawn = empty.draw { canvas =>
      canvas.putString(at(10, 10), "a", Style.empty)
      canvas.putString(at(0, 0), "b", Style.empty)
      canvas.fill(rect(4, 1, 2, 2), "c", Style.empty)
      canvas.patchStyle(rect(0, 3, 9, 9), blueBg)
    }
    Assertions.eqv(drawn, empty)
  }

  def testLeak: Result = {
    val leak   = Array(Option.empty[Canvas])
    val buffer = Buffer.empty(rect(0, 0, 2, 1)).draw(canvas => leak(0) = canvas.some)
    leak(0).foreach(_.putString(at(0, 0), "z", Style.empty))
    Result.all(
      List(Assertions.eqv(buffer, Buffer.empty(rect(0, 0, 2, 1))), Result.assert(!Buffer.renderRows(buffer).exists(_.contains("z"))))
    )
  }

  def testPutLine: Result = {
    val line   = Line(Vector(Span("ab", Style.empty.withFg(Color.Red)), Span("c", blueBg)), Style.empty.withBg(Color.Green), None)
    val buffer = Buffer.empty(rect(0, 0, 4, 1)).draw(_.putLine(at(0, 0), line, NonNegInt(4)))
    Result.all(
      List(
        Assertions.eqv(Buffer.renderRows(buffer), Vector("abc ")),
        Assertions.eqv(
          cellsOf(buffer).map(_.style),
          Vector(
            CellStyle.default.copy(fg = Color.Red, bg = Color.Green),
            CellStyle.default.copy(fg = Color.Red, bg = Color.Green),
            CellStyle.default.copy(bg = Color.Blue),
            CellStyle.default,
          ),
        ),
      )
    )
  }

}
