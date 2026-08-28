package stui.core.style

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The underline of a cell, at most one per cell (decision D22). `None` is the terminal default (no underline, SGR `24`), the others map
  * to SGR `4:1` to `4:5` on terminals with extended underlines and to a plain `4` elsewhere (the writer decides from the capabilities).
  * The case is named `None` because it plays the role `Color.Reset` plays for colours: a `Style` patch that carries it turns the
  * underline off. It is always written qualified as `UnderlineStyle.None`.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum UnderlineStyle derives Eq, Show, Hash {
  case None
  case Single
  case Double
  case Curly
  case Dotted
  case Dashed
}

object UnderlineStyle {

  /** Every style in ordinal order. */
  val all: List[UnderlineStyle] = values.toList

}
