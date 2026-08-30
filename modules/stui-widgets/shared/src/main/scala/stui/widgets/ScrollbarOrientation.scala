package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Which edge of its area a [[Scrollbar]] runs along: the right or left column, or the bottom or top row.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum ScrollbarOrientation derives Eq, Show, Hash {
  case VerticalRight
  case VerticalLeft
  case HorizontalBottom
  case HorizontalTop
}

object ScrollbarOrientation {

  extension (orientation: ScrollbarOrientation) {

    /** True for the two vertical orientations. */
    def isVertical: Boolean = orientation match {
      case VerticalRight | VerticalLeft => true
      case HorizontalBottom | HorizontalTop => false
    }

  }

}
