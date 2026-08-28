package stui.core.style

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A text attribute. The bit of a modifier in [[Modifiers]] is its ordinal. Underlining is not a modifier but an [[UnderlineStyle]] on
  * the cell style (decision D22).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum Modifier derives Eq, Show, Hash {
  case Bold
  case Dim
  case Italic
  case SlowBlink
  case RapidBlink
  case Reversed
  case Hidden
  case CrossedOut
}

object Modifier {

  /** Every modifier in ordinal order. */
  val all: List[Modifier] = values.toList

}
