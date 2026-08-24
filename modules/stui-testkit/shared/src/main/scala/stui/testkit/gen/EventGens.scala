package stui.testkit.gen

import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.event.*

/** Generators for the event vocabulary.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object EventGens {

  /** Any single key modifier. */
  val keyModifier: Gen[KeyModifier] = Gen.elementUnsafe(KeyModifier.all)

  /** A random subset. */
  val keyModifiers: Gen[KeyModifiers] = keyModifier.list(Range.linear(0, 6)).map(KeyModifiers.of)

  /** Any key event kind. */
  val keyEventKind: Gen[KeyEventKind] = Gen.element1(KeyEventKind.Press, KeyEventKind.Repeat, KeyEventKind.Release)

  /** F1 to F35. */
  val functionKeyNumber: Gen[FunctionKeyNumber] =
    Gen.int(Range.linear(1, 35)).map(n => FunctionKeyNumber.from(n).fold(_ => FunctionKeyNumber.MinValue, identity))

  /** Any media key. */
  val mediaKey: Gen[MediaKey] = Gen.elementUnsafe(MediaKey.values.toList)

  /** Any modifier key. */
  val modifierKey: Gen[ModifierKey] = Gen.elementUnsafe(ModifierKey.values.toList)

  private val namedKeyCode: Gen[KeyCode] = Gen.element1(
    KeyCode.Backspace,
    KeyCode.Enter,
    KeyCode.Left,
    KeyCode.Right,
    KeyCode.Up,
    KeyCode.Down,
    KeyCode.Home,
    KeyCode.End,
    KeyCode.PageUp,
    KeyCode.PageDown,
    KeyCode.Tab,
    KeyCode.BackTab,
    KeyCode.Delete,
    KeyCode.Insert,
    KeyCode.Escape,
    KeyCode.CapsLock,
    KeyCode.ScrollLock,
    KeyCode.NumLock,
    KeyCode.PrintScreen,
    KeyCode.Pause,
    KeyCode.Menu,
    KeyCode.KeypadBegin,
  )

  /** Favours printable characters. */
  val keyCode: Gen[KeyCode] = Gen.frequency1(
    6 -> Gens.asciiPrintableChar.map(KeyCode.char),
    2 -> Gen.unicode.map(KeyCode.char),
    4 -> namedKeyCode,
    1 -> functionKeyNumber.map(KeyCode.fOf),
    1 -> mediaKey.map(KeyCode.media),
    1 -> modifierKey.map(KeyCode.modifier),
  )

  /** Key events over the full code, modifier, and kind space. */
  val keyEvent: Gen[KeyEvent] =
    for {
      code      <- keyCode
      modifiers <- keyModifiers
      kind      <- keyEventKind
    } yield KeyEvent(code, modifiers, kind)

  /** Any mouse button. */
  val mouseButton: Gen[MouseButton] = Gen.element1(MouseButton.Left, MouseButton.Middle, MouseButton.Right)

  /** Any mouse event kind. */
  val mouseEventKind: Gen[MouseEventKind] = Gen.frequency1(
    3 -> mouseButton.map(MouseEventKind.down),
    3 -> mouseButton.map(MouseEventKind.up),
    2 -> mouseButton.map(MouseEventKind.drag),
    2 -> Gen.constant(MouseEventKind.Moved),
    1 -> Gen.element1(MouseEventKind.ScrollUp, MouseEventKind.ScrollDown, MouseEventKind.ScrollLeft, MouseEventKind.ScrollRight),
  )

  /** Mouse events at positions within 0..max. */
  def mouseEvent(max: NonNegInt): Gen[MouseEvent] =
    for {
      kind      <- mouseEventKind
      position  <- GeometryGens.position(max)
      modifiers <- keyModifiers
    } yield MouseEvent(kind, position, modifiers)

  /** Events of every kind, mostly key events, positions and sizes within 0..max. */
  def event(max: NonNegInt): Gen[Event] = Gen.frequency1(
    6 -> keyEvent.map(Event.key),
    2 -> mouseEvent(max).map(Event.mouse),
    1 -> GeometryGens.size(max).map(Event.resize),
    1 -> Gens.asciiPrintable(Range.linear(0, 20)).map(Event.paste),
    1 -> Gen.element1(Event.FocusGained, Event.FocusLost),
  )

}
