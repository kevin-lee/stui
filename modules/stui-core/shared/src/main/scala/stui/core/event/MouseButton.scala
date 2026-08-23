package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** @author Kevin Lee
  * @since 2026-08-23
  */
enum MouseButton derives Eq, Show, Hash {
  case Left
  case Middle
  case Right
}
