package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Result}
import hedgehog.runner.*
import stui.core.buffer.{Buffer, Cell, GlyphWidth}
import stui.testkit.Assertions
import stui.testkit.gen.BufferGens
import stui.testkit.gen.BufferGens.CanvasOp
import stui.unicode.Graphemes
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The buffer laws: the cell invariant, draw identity, clipping, and rendered row widths.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object BufferLaws {

  @tailrec
  private def hasUnpairedSurrogate(s: String, i: Int): Boolean =
    if (i >= s.length) {
      false
    } else {
      val cp = s.codePointAt(i)
      if (cp >= 0xd800 && cp <= 0xdfff) true else hasUnpairedSurrogate(s, i + Character.charCount(cp))
    }

  private def glyphChecks(buffer: Buffer, index: Int, symbol: String, width: GlyphWidth): List[Result] = List(
    Result.assert(symbol.nonEmpty).log(s"cell $index: empty symbol"),
    Result.assert(Graphemes.count(symbol) === 1).log(s"cell $index: not one cluster"),
    Result.assert(!hasUnpairedSurrogate(symbol, 0)).log(s"cell $index: unpaired surrogate"),
    Result.assert(buffer.policy.clusterWidth(symbol) === width.columns).log(s"cell $index: width mismatch"),
  )

  /** The [[Cell]] invariant on every cell. */
  def wellFormed(buffer: Buffer): Result = {
    val width  = buffer.area.width.value
    val cells  = buffer.cells
    val checks = (0 until cells.length).toList.flatMap { i =>
      val column = if (width === 0) 0 else i % width
      cells(i) match {
        case Cell.Glyph(symbol, GlyphWidth.Two, style) =>
          val next = if (column + 1 < width) Option(cells(i + 1)) else Option.empty[Cell]
          Result.assert(next.exists(_ === Cell.Continuation(style))).log(s"cell $i: wide glyph without its continuation") ::
            glyphChecks(buffer, i, symbol, GlyphWidth.Two)
        case Cell.Glyph(symbol, GlyphWidth.One, _) =>
          glyphChecks(buffer, i, symbol, GlyphWidth.One)
        case Cell.Continuation(style) =>
          val previous = if (column > 0) Option(cells(i - 1)) else Option.empty[Cell]
          List(
            Result
              .assert(previous.exists {
                case Cell.Glyph(_, GlyphWidth.Two, ownerStyle) => ownerStyle === style
                case Cell.Glyph(_, GlyphWidth.One, _) | Cell.Continuation(_) => false
              })
              .log(s"cell $i: continuation without its owner")
          )
      }
    }
    Result.all(Result.assert(cells.length.toLong === buffer.area.area).log("cell count") :: checks)
  }

  /** Well-formedness, draw identity, outside-writes-change-nothing, and rendered row widths, each test prefixed with `name`. */
  def laws(name: String, buffers: Gen[Buffer], outside: Gen[(Buffer, List[CanvasOp])]): List[Test] = List(
    property(s"[$name] every buffer is well-formed", buffers.forAll.map(wellFormed)),
    property(s"[$name] draw with no operation is the identity", buffers.forAll.map(b => Assertions.eqv(b.draw(_ => ()), b))),
    property(
      s"[$name] operations outside the area change nothing",
      outside.forAll.map { case (b, ops) => Assertions.eqv(b.draw(canvas => BufferGens.runAll(canvas, ops)), b) },
    ),
    property(
      s"[$name] rendered rows have the area width",
      buffers.forAll.map { b =>
        val rows = Buffer.renderRows(b)
        Result.all(
          Result.assert(rows.length === b.area.height.value).log("row count") ::
            rows.toList.map(row => Result.assert(b.policy.width(row) === b.area.width.value).log(s"row width: $row"))
        )
      },
    ),
  )

}
