package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The direction a [[ListView]] stacks its items in: `TopToBottom` (the default) puts the first displayed item on the top row,
  * `BottomToTop` puts it on the bottom row and stacks upward, so a short list sticks to the bottom edge.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum ListDirection derives Eq, Show, Hash {
  case TopToBottom
  case BottomToTop
}
