package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A modifier key pressed on its own, reported by the kitty keyboard protocol.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum ModifierKey derives Eq, Show, Hash {
  case LeftShift
  case LeftControl
  case LeftAlt
  case LeftSuper
  case LeftHyper
  case LeftMeta
  case RightShift
  case RightControl
  case RightAlt
  case RightSuper
  case RightHyper
  case RightMeta
  case IsoLevel3Shift
  case IsoLevel5Shift
}
