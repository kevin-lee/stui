package stui.core.buffer

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.style.CellStyle

/** One cell of a [[Buffer]]: a glyph (exactly one extended grapheme cluster of valid UTF-16 whose width under the buffer's policy is
  * `width.columns`) or the continuation column of a two-column glyph, carrying that glyph's style because the terminal paints the column
  * with it.
  *
  * Invariant of every well-formed buffer: a `Glyph` of width `Two` is immediately followed in its row by a `Continuation` with the same
  * style, and every `Continuation` is immediately preceded by such a glyph. The [[Canvas]] maintains it on every write.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum Cell derives Eq, Show, Hash {
  case Glyph(symbol: String, width: GlyphWidth, style: CellStyle)
  case Continuation(style: CellStyle)
}

object Cell {

  /** A space with the default style. */
  val blank: Cell = Glyph(" ", GlyphWidth.One, CellStyle.default)

  def blankWith(style: CellStyle): Cell = Glyph(" ", GlyphWidth.One, style)

  def glyph(symbol: String, width: GlyphWidth, style: CellStyle): Cell = Glyph(symbol, width, style)

  def continuation(style: CellStyle): Cell = Continuation(style)

  extension (cell: Cell) {

    def style: CellStyle = cell match {
      case Glyph(_, _, style) => style
      case Continuation(style) => style
    }

    /** 1 for a continuation. */
    def columns: Int = cell match {
      case Glyph(_, width, _) => width.columns
      case Continuation(_) => 1
    }

    def isBlank: Boolean = cell match {
      case Glyph(symbol, _, _) => symbol === " "
      case Continuation(_) => false
    }

    def symbolOption: Option[String] = cell match {
      case Glyph(symbol, _, _) => symbol.some
      case Continuation(_) => none[String]
    }

  }

}
