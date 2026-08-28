package stui.terminal.decoder

import cats.syntax.all.*
import stui.core.event.*
import stui.unicode.internal.IntOps.*

import java.nio.charset.StandardCharsets

/** The canonical xterm byte sequence of an event, for the events that have exactly one (design doc 7.5, the round-trip law): the
  * decoder gives the event back from these bytes (followed by a tick for the lone Escape). `None` for events the decoder never
  * produces from one sequence: an uppercase letter without Shift, Super or Hyper modifiers, key repeats and releases, media and
  * modifier keys, function keys above F20, resizes, and a paste containing ESC.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Encoder {

  private val Esc: Byte = 0x1b

  private val EscChar: Char = '\u001b'

  private val Csi: Vector[Byte] = Vector(Esc, '['.toByte)

  private val Ss3: Vector[Byte] = Vector(Esc, 'O'.toByte)

  /** The bytes, or `None` when the event has no canonical sequence. */
  def encode(event: Event): Option[IArray[Byte]] = {
    val bytes = event match {
      case Event.Key(KeyEvent(code, modifiers, KeyEventKind.Press)) => key(code, modifiers)
      case Event.Key(KeyEvent(_, _, KeyEventKind.Repeat | KeyEventKind.Release)) => none[Vector[Byte]]
      case Event.Mouse(MouseEvent(kind, position, modifiers)) => mouse(kind, position.x.value, position.y.value, modifiers)
      case Event.Resize(_) => none[Vector[Byte]]
      case Event.Paste(text) =>
        if (text.contains(EscChar)) none[Vector[Byte]]
        else (Csi ++ ascii("200~") ++ text.getBytes(StandardCharsets.UTF_8).toVector ++ Csi ++ ascii("201~")).some
      case Event.FocusGained => (Csi :+ 'I'.toByte).some
      case Event.FocusLost => (Csi :+ 'O'.toByte).some
    }
    bytes.map(v => IArray.from(v))
  }

  private def ascii(s: String): Vector[Byte] = s.getBytes(StandardCharsets.US_ASCII).toVector

  private def key(code: KeyCode, modifiers: KeyModifiers): Option[Vector[Byte]] = code match {
    case KeyCode.Char(c) => char(c, modifiers)
    case KeyCode.Enter => plain(modifiers, Vector(0x0d.toByte))
    case KeyCode.Tab => plain(modifiers, Vector(0x09.toByte))
    case KeyCode.Backspace => plain(modifiers, Vector(0x7f.toByte))
    case KeyCode.Escape => plain(modifiers, Vector(Esc))
    case KeyCode.Up => cursorKey('A', modifiers)
    case KeyCode.Down => cursorKey('B', modifiers)
    case KeyCode.Right => cursorKey('C', modifiers)
    case KeyCode.Left => cursorKey('D', modifiers)
    case KeyCode.Home => cursorKey('H', modifiers)
    case KeyCode.End => cursorKey('F', modifiers)
    case KeyCode.Insert => tildeKey(2, modifiers)
    case KeyCode.Delete => tildeKey(3, modifiers)
    case KeyCode.PageUp => tildeKey(5, modifiers)
    case KeyCode.PageDown => tildeKey(6, modifiers)
    case KeyCode.F(n) => functionKey(n.value, modifiers)
    case KeyCode.BackTab => if (modifiers === KeyModifiers(KeyModifier.Shift)) (Csi :+ 'Z'.toByte).some else none[Vector[Byte]]
    case KeyCode.CapsLock | KeyCode.ScrollLock | KeyCode.NumLock | KeyCode.PrintScreen | KeyCode.Pause | KeyCode.Menu |
        KeyCode.KeypadBegin | KeyCode.Media(_) | KeyCode.Modifier(_) =>
      none[Vector[Byte]]
  }

  private def plain(modifiers: KeyModifiers, bytes: Vector[Byte]): Option[Vector[Byte]] =
    if (modifiers.isEmpty) bytes.some else none[Vector[Byte]]

  /** `1 + Shift 1 + Alt 2 + Control 4 + Meta 8`, `None` for Super or Hyper. */
  private def parameter(modifiers: KeyModifiers): Option[Int] =
    if (modifiers.contains(KeyModifier.Super) || modifiers.contains(KeyModifier.Hyper)) {
      none[Int]
    } else {
      val shift   = if (modifiers.contains(KeyModifier.Shift)) 1 else 0
      val alt     = if (modifiers.contains(KeyModifier.Alt)) 2 else 0
      val control = if (modifiers.contains(KeyModifier.Control)) 4 else 0
      val meta    = if (modifiers.contains(KeyModifier.Meta)) 8 else 0
      (1 + shift + alt + control + meta).some
    }

  private def cursorKey(fin: Char, modifiers: KeyModifiers): Option[Vector[Byte]] =
    if (modifiers.isEmpty) (Csi :+ fin.toByte).some
    else parameter(modifiers).map(m => Csi ++ ascii(s"1;${m.toString}") :+ fin.toByte)

  private def tildeKey(n: Int, modifiers: KeyModifiers): Option[Vector[Byte]] =
    if (modifiers.isEmpty) (Csi ++ ascii(s"${n.toString}~")).some
    else parameter(modifiers).map(m => Csi ++ ascii(s"${n.toString};${m.toString}~"))

  private def functionKey(n: Int, modifiers: KeyModifiers): Option[Vector[Byte]] =
    if (n >= 1 && n <= 4) {
      val fin = ('P'.toInt + n - 1).toChar
      if (modifiers.isEmpty) (Ss3 :+ fin.toByte).some else parameter(modifiers).map(m => Csi ++ ascii(s"1;${m.toString}") :+ fin.toByte)
    } else {
      val code = n match {
        case 5 => 15.some
        case 6 => 17.some
        case 7 => 18.some
        case 8 => 19.some
        case 9 => 20.some
        case 10 => 21.some
        case 11 => 23.some
        case 12 => 24.some
        case 13 => 25.some
        case 14 => 26.some
        case 15 => 28.some
        case 16 => 29.some
        case 17 => 31.some
        case 18 => 32.some
        case 19 => 33.some
        case 20 => 34.some
        case _ => none[Int]
      }
      code.flatMap(c => tildeKey(c, modifiers))
    }

  private def isSpecialAfterEsc(c: Char): Boolean =
    c === '[' || c === 'O' || c === 'P' || c === 'X' || c === ']' || c === '^' || c === '_'

  private def char(c: Char, modifiers: KeyModifiers): Option[Vector[Byte]] = {
    val upper    = c >= 'A' && c <= 'Z'
    val ascii    = c >= ' ' && c <= '~'
    val altRange = c >= '0' && c <= '~' && !isSpecialAfterEsc(c)
    if (modifiers.isEmpty) {
      if (upper) none[Vector[Byte]]
      else if (ascii) Vector(c.toByte).some
      else if (c < ' ' || c.toInt === 0x7f || Character.isSurrogate(c)) none[Vector[Byte]]
      else c.toString.getBytes(StandardCharsets.UTF_8).toVector.some
    } else if (modifiers === KeyModifiers(KeyModifier.Shift)) {
      if (upper) Vector(c.toByte).some else none[Vector[Byte]]
    } else if (modifiers === KeyModifiers(KeyModifier.Control)) {
      if (c >= 'a' && c <= 'z' && c =!= 'h' && c =!= 'i' && c =!= 'j' && c =!= 'm') Vector((c.toInt - 'a'.toInt + 1).toByte).some
      else if (c === ' ') Vector(0x00.toByte).some
      else if (c >= '4' && c <= '7') Vector((0x1c + c.toInt - '4'.toInt).toByte).some
      else none[Vector[Byte]]
    } else if (modifiers === KeyModifiers(KeyModifier.Alt)) {
      if (altRange && !upper) Vector(Esc, c.toByte).some else none[Vector[Byte]]
    } else if (modifiers === KeyModifiers(KeyModifier.Alt, KeyModifier.Shift)) {
      if (upper && altRange) Vector(Esc, c.toByte).some else none[Vector[Byte]]
    } else {
      none[Vector[Byte]]
    }
  }

  private def mouse(kind: MouseEventKind, x: Int, y: Int, modifiers: KeyModifiers): Option[Vector[Byte]] = {
    val extra = modifiers.toList.exists(m => m =!= KeyModifier.Shift && m =!= KeyModifier.Alt && m =!= KeyModifier.Control)
    if (extra || x + 1 > DecoderLimits.MaxCoordinate || y + 1 > DecoderLimits.MaxCoordinate) {
      none[Vector[Byte]]
    } else {
      val bits     =
        (if (modifiers.contains(KeyModifier.Shift)) 4 else 0) + (if (modifiers.contains(KeyModifier.Alt)) 8 else 0) +
          (if (modifiers.contains(KeyModifier.Control)) 16 else 0)
      val (b, fin) = kind match {
        case MouseEventKind.Down(button) => (buttonBits(button), 'M')
        case MouseEventKind.Up(button) => (buttonBits(button), 'm')
        case MouseEventKind.Drag(button) => (buttonBits(button) + 32, 'M')
        case MouseEventKind.Moved => (35, 'M')
        case MouseEventKind.ScrollUp => (64, 'M')
        case MouseEventKind.ScrollDown => (65, 'M')
        case MouseEventKind.ScrollLeft => (66, 'M')
        case MouseEventKind.ScrollRight => (67, 'M')
      }
      (Csi ++ ascii(s"<${(b + bits).toString};${(x + 1).toString};${(y + 1).toString}") :+ fin.toByte).some
    }
  }

  private def buttonBits(button: MouseButton): Int = button match {
    case MouseButton.Left => 0
    case MouseButton.Middle => 1
    case MouseButton.Right => 2
  }

}
