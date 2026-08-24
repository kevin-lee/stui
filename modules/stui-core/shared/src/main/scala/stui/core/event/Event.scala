package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Size

/** The event vocabulary every backend translates into and every app, widget, and test speaks (design doc 6.2). Apps never see backend
  * types.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum Event derives Eq, Show, Hash {
  case Key(event: KeyEvent)
  case Mouse(event: MouseEvent)
  case Resize(size: Size)
  case Paste(text: String)
  case FocusGained
  case FocusLost
}

object Event {

  /** A [[Key]] event. */
  def key(event: KeyEvent): Event = Key(event)

  /** A [[Mouse]] event. */
  def mouse(event: MouseEvent): Event = Mouse(event)

  /** A [[Resize]] event carrying the new size. */
  def resize(size: Size): Event = Resize(size)

  /** A [[Paste]] event carrying the pasted text. */
  def paste(text: String): Event = Paste(text)

}
