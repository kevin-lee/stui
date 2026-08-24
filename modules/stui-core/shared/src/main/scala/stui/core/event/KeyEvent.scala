package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A key with the modifiers held and the kind of the event.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class KeyEvent(code: KeyCode, modifiers: KeyModifiers, kind: KeyEventKind) derives Eq, Show, Hash

object KeyEvent {

  /** A press without modifiers. */
  def press(code: KeyCode): KeyEvent = KeyEvent(code, KeyModifiers.empty, KeyEventKind.Press)

  /** A press with the given modifiers. */
  def pressWith(code: KeyCode, modifiers: KeyModifiers): KeyEvent = KeyEvent(code, modifiers, KeyEventKind.Press)

  /** A key event with every field given. */
  def of(code: KeyCode, modifiers: KeyModifiers, kind: KeyEventKind): KeyEvent = KeyEvent(code, modifiers, kind)

}
