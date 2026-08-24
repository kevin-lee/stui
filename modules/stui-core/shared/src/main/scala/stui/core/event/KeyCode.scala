package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The key of a [[KeyEvent]]. `Char` carries one UTF-16 unit: a scalar value outside the Basic Multilingual Plane arrives as two
  * consecutive events carrying the surrogate halves, which concatenate into the correct `String`. Which codes a backend can produce
  * depends on its decoder and the terminal's keyboard protocol.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum KeyCode derives Eq, Show, Hash {
  case Char(c: scala.Char)
  case F(n: FunctionKeyNumber)
  case Backspace
  case Enter
  case Left
  case Right
  case Up
  case Down
  case Home
  case End
  case PageUp
  case PageDown
  case Tab
  case BackTab
  case Delete
  case Insert
  case Escape
  case CapsLock
  case ScrollLock
  case NumLock
  case PrintScreen
  case Pause
  case Menu
  case KeypadBegin
  case Media(key: MediaKey)
  case Modifier(key: ModifierKey)
}

object KeyCode {

  /** A [[Char]] key code. */
  def char(c: scala.Char): KeyCode = Char(c)

  /** A function key from a literal, validated at compile time (1 to 35). Use [[fFrom]] for runtime values. */
  inline def f(inline n: Int): KeyCode = F(FunctionKeyNumber(n))

  /** A function key from a runtime value, `Left` with refined4s's message outside 1 to 35. */
  def fFrom(n: Int): Either[String, KeyCode] = FunctionKeyNumber.from(n).map(F(_))

  /** A function key from an already-refined number. */
  def fOf(n: FunctionKeyNumber): KeyCode = F(n)

  /** A [[Media]] key code. */
  def media(key: MediaKey): KeyCode = Media(key)

  /** A [[Modifier]] key code. */
  def modifier(key: ModifierKey): KeyCode = Modifier(key)

}
