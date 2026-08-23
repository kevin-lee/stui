package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** What the mouse did.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum MouseEventKind derives Eq, Show, Hash {
  case Down(button: MouseButton)
  case Up(button: MouseButton)
  case Drag(button: MouseButton)
  case Moved
  case ScrollUp
  case ScrollDown
  case ScrollLeft
  case ScrollRight
}

object MouseEventKind {

  def down(button: MouseButton): MouseEventKind = Down(button)

  def up(button: MouseButton): MouseEventKind = Up(button)

  def drag(button: MouseButton): MouseEventKind = Drag(button)

}
