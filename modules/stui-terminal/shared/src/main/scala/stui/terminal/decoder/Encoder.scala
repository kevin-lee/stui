package stui.terminal.decoder

import cats.syntax.all.*
import stui.core.event.*
import stui.terminal.kitty.KittyFlags
import stui.unicode.internal.IntOps.*

import java.nio.charset.StandardCharsets

/** The canonical byte sequences of events, for the round-trip law (design doc 7.5), in two modes.
  *
  *   - [[encode]], the legacy xterm form: the decoder in its initial state gives the event back from these bytes (followed by a tick
  *     for the lone Escape). `None` for events the decoder never produces from one sequence: an uppercase letter without Shift, Super
  *     or Hyper modifiers, key repeats and releases, media and modifier keys, function keys above F20, resizes, and a paste
  *     containing ESC.
  *   - [[encodeKitty]], the kitty keyboard protocol form (M3d): the decoder with the flags pushed gives the event back. Keys use the
  *     kitty modifier bits and kinds, `CSI u` for characters and the named and Private Use Area keys, the letter forms and the tilde
  *     forms the specification keeps (F3 as `CSI 13 ~`), and a character's own text as associated text when the flags ask for all keys
  *     and text. `None` for the forms the decoder normalises away: a letter whose case disagrees with Shift, Tab with Shift (BackTab),
  *     BackTab without Shift, controls, surrogates, Private Use Area characters, and repeats or releases without `EventTypes`.
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

  /** The canonical kitty keyboard protocol bytes of the event for a decoder with `flags` pushed, or `None` when there are none (M3d). */
  def encodeKitty(flags: KittyFlags, event: Event): Option[IArray[Byte]] = {
    val bytes = event match {
      case Event.Key(KeyEvent(code, modifiers, kind)) => kittyKey(flags, code, modifiers, kind)
      case Event.Mouse(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => encode(event).map(_.toVector)
      case Event.Resize(_) => none[Vector[Byte]]
    }
    bytes.map(v => IArray.from(v))
  }

  /** `Shift 1 + Alt 2 + Control 4 + Super 8 + Hyper 16 + Meta 32`. */
  private def kittyBits(modifiers: KeyModifiers): Int =
    (if (modifiers.contains(KeyModifier.Shift)) 1 else 0) + (if (modifiers.contains(KeyModifier.Alt)) 2 else 0) +
      (if (modifiers.contains(KeyModifier.Control)) 4 else 0) + (if (modifiers.contains(KeyModifier.Super)) 8 else 0) +
      (if (modifiers.contains(KeyModifier.Hyper)) 16 else 0) + (if (modifiers.contains(KeyModifier.Meta)) 32 else 0)

  private def kittyKey(flags: KittyFlags, code: KeyCode, modifiers: KeyModifiers, kind: KeyEventKind): Option[Vector[Byte]] = {
    val suffix = kind match {
      case KeyEventKind.Press => "".some
      case KeyEventKind.Repeat => Option.when(flags.contains(KittyFlags.EventTypes))(":2")
      case KeyEventKind.Release => Option.when(flags.contains(KittyFlags.EventTypes))(":3")
    }
    suffix.flatMap { kindSuffix =>
      val parameter                         = 1 + kittyBits(modifiers)
      val field                             = if (parameter === 1 && kindSuffix.isEmpty) "" else ";" + parameter.toString + kindSuffix
      val shift                             = modifiers.contains(KeyModifier.Shift)
      def u(n: Int): Vector[Byte]           = Csi ++ ascii(n.toString + field + "u")
      def tilded(n: Int): Vector[Byte]      = Csi ++ ascii(n.toString + field + "~")
      def lettered(fin: Char): Vector[Byte] = if (field.isEmpty) Csi :+ fin.toByte else (Csi ++ ascii("1" + field)) :+ fin.toByte
      code match {
        case KeyCode.Char(c) => kittyChar(flags, c, modifiers, kind, field)
        case KeyCode.Enter => u(13).some
        case KeyCode.Escape => u(27).some
        case KeyCode.Backspace => u(127).some
        case KeyCode.Tab => if (shift) none[Vector[Byte]] else u(9).some
        case KeyCode.BackTab => if (shift) u(9).some else none[Vector[Byte]]
        case KeyCode.Up => lettered('A').some
        case KeyCode.Down => lettered('B').some
        case KeyCode.Right => lettered('C').some
        case KeyCode.Left => lettered('D').some
        case KeyCode.Home => lettered('H').some
        case KeyCode.End => lettered('F').some
        case KeyCode.Insert => tilded(2).some
        case KeyCode.Delete => tilded(3).some
        case KeyCode.PageUp => tilded(5).some
        case KeyCode.PageDown => tilded(6).some
        case KeyCode.F(n) =>
          n.value match {
            case 1 => lettered('P').some
            case 2 => lettered('Q').some
            case 3 => tilded(13).some
            case 4 => lettered('S').some
            case 5 => tilded(15).some
            case 6 => tilded(17).some
            case 7 => tilded(18).some
            case 8 => tilded(19).some
            case 9 => tilded(20).some
            case 10 => tilded(21).some
            case 11 => tilded(23).some
            case 12 => tilded(24).some
            case _ => KittyKeys.codeOf(code).map(u)
          }
        case KeyCode.CapsLock | KeyCode.ScrollLock | KeyCode.NumLock | KeyCode.PrintScreen | KeyCode.Pause | KeyCode.Menu |
            KeyCode.KeypadBegin | KeyCode.Media(_) | KeyCode.Modifier(_) =>
          KittyKeys.codeOf(code).map(u)
      }
    }
  }

  /** A character: the lowercase key code with Shift for `A` to `Z`, the character itself otherwise, and its own text as associated
    * text when the flags ask for all keys and text and the modifiers are exactly the text's (Shift only for `A` to `Z`).
    */
  private def kittyChar(flags: KittyFlags, c: Char, modifiers: KeyModifiers, kind: KeyEventKind, field: String): Option[Vector[Byte]] = {
    val upper = c >= 'A' && c <= 'Z'
    val lower = c >= 'a' && c <= 'z'
    val shift = modifiers.contains(KeyModifier.Shift)
    val cp    = c.toInt
    if (
      Character.isSurrogate(c) || c < ' ' || cp === 0x7f || (cp >= KittyKeys.First && cp <= KittyKeys.Last) || (upper && !shift) ||
      (lower && shift)
    ) {
      none[Vector[Byte]]
    } else {
      val keyCode   = if (upper) cp + 0x20 else cp
      val textMods  = if (upper) KeyModifiers(KeyModifier.Shift) else KeyModifiers.empty
      val textField = kind match {
        case KeyEventKind.Press => "".some
        case KeyEventKind.Repeat => "1:2".some
        case KeyEventKind.Release => none[String]
      }
      textField.filter(_ =>
        flags.contains(KittyFlags.AllKeys) && flags.contains(KittyFlags.AssociatedText) && modifiers === textMods
      ) match {
        case Some(textKind) => (Csi ++ ascii(keyCode.toString + ";" + textKind + ";" + cp.toString + "u")).some
        case None => (Csi ++ ascii(keyCode.toString + field + "u")).some
      }
    }
  }

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
