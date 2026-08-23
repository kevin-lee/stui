package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A modifier key held during a key or mouse event. The bit of a modifier in [[KeyModifiers]] is its ordinal.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum KeyModifier derives Eq, Show, Hash {
  case Shift
  case Control
  case Alt
  case Super
  case Hyper
  case Meta
}

object KeyModifier {

  /** Every modifier in ordinal order. */
  val all: List[KeyModifier] = values.toList

}
