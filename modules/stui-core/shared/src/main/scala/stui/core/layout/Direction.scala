package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The axis a [[Layout]] splits along: `Horizontal` cuts the area into segments side by side (along x), `Vertical` stacks them top to
  * bottom (along y).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Direction derives Eq, Show, Hash {
  case Horizontal
  case Vertical
}
