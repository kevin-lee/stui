package stui.core.text

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Horizontal alignment of a line inside its area. Applied by the layout and widgets (M1d), carried by the text model.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum Alignment derives Eq, Show, Hash {
  case Left
  case Center
  case Right
}
