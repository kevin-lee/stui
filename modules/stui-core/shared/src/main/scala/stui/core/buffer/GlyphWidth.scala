package stui.core.buffer

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** The columns a glyph occupies: one, or two with a [[Cell.Continuation]] in the next column.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum GlyphWidth derives Eq, Show, Hash {
  case One
  case Two
}

object GlyphWidth {

  /** `One` for 1, `Two` for 2, `None` otherwise. */
  def fromColumns(columns: Int): Option[GlyphWidth] = columns match {
    case 1 => One.some
    case 2 => Two.some
    case _ => none[GlyphWidth]
  }

  extension (width: GlyphWidth) {

    def columns: Int = width match {
      case One => 1
      case Two => 2
    }

  }

}
