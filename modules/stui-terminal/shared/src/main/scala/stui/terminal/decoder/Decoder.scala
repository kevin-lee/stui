package stui.terminal.decoder

import cats.syntax.all.*
import stui.core.event.*
import stui.core.geometry.Position
import stui.core.internal.NonNegInts
import stui.terminal.decoder.DecoderMode.*
import stui.terminal.kitty.KittyFlags
import stui.unicode.internal.IntOps.*

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

/** The pure input decoder (design doc 7.5, decision D17): `(state, input) => Decoded(state, events, replies)` over raw bytes and clock
  * ticks, following the DEC ANSI parser reference (Paul Flo Williams) and xterm ctlseqs.
  *
  *   - Keys: C0 bytes (`0x0d` and `0x0a` Enter, `0x09` Tab, `0x7f` and `0x08` Backspace, `0x00` Control+Space, `0x01`-`0x1a` Control
  *     plus a letter, `0x1c`-`0x1f` Control plus `4` to `7`), printable ASCII (`A` to `Z` carry Shift), UTF-8 characters (U+FFFD for an
  *     invalid or truncated sequence, an astral scalar as two surrogate `Char` events), `ESC` plus a key as Alt plus the key, xterm's
  *     cursor, editing, and function keys in normal and application mode (Keypad Begin as `CSI E` and `SS3 E`), `CSI Z` as
  *     Shift+BackTab. The modifier parameter is xterm's `1 + Shift 1 + Alt 2 + Control 4 + Meta 8`, or the kitty keyboard protocol's
  *     `1 + Shift 1 + Alt 2 + Control 4 + Super 8 + Hyper 16 + Meta 32` (Caps Lock 64 and Num Lock 128 dropped) while
  *     `DecoderState.keyboard` holds pushed flags, and its colon sub-field is the event kind (1 press, 2 repeat, 3 release).
  *   - The kitty keyboard protocol (M3d, decision D30): `CSI code:shifted:base ; modifiers:kind ; text u` reports decode in every state,
  *     always with the kitty modifier bits. A report carrying text decodes to exactly the events the same text gives as UTF-8 (so an
  *     input-method commit on Enter is text, never an Enter press), with the report's kind. Otherwise 9 is Tab (BackTab with Shift), 13
  *     Enter, 27 Escape, 127 Backspace, the Private Use Area keys map through [[KittyKeys]] (keypad keys to their plain equivalents),
  *     a letter is uppercase with Shift exactly when Shift xor Caps Lock holds, another character with Shift is its shifted key (Shift
  *     dropped) when the report names one, and unknown or control codes are dropped. Kinds are filtered: a release is dropped and a
  *     repeat delivered as a press unless the pushed flags contain `EventTypes` (the Press-only default of design doc 6.2).
  *   - Mouse: SGR 1006 (`CSI < b ; x ; y M` or `m`), X10 (`CSI M` plus three bytes), and urxvt 1015 (`CSI b ; x ; y M`), with the
  *     button, motion, and wheel bits of xterm ctlseqs, 1-based coordinates made 0-based, and reports outside `1..MaxCoordinate` dropped.
  *   - Focus (`CSI I`, `CSI O`) and bracketed paste (`CSI 200 ~` to `CSI 201 ~`, the body decoded as UTF-8 with replacement).
  *   - Replies (design doc 7.3): DA1 (`CSI ? ... c`), DA2 (`CSI > ... c`), DECRPM (`CSI ? Pd ; Ps $ y`), XTGETTCAP and XTVERSION
  *     answers (DCS strings, buffered up to `DecoderLimits.MaxStringSequence` and parsed at their terminator), and, only while
  *     `DecoderState.expectingReplies` holds, the Cursor Position Report (`CSI Pl ; Pc R`, F3 with modifiers otherwise, plan
  *     refinement R5).
  *   - The kitty flags answer `CSI ? flags u` is always a reply. With flags pushed and no reply expected, `CSI ... R` is a late Cursor
  *     Position Report and is dropped (kitty sends F3 as `CSI 13 ~`).
  *   - Consumed without an event or reply: OSC, APC, PM, and SOS strings to their terminator, unknown sequences, and charset
  *     designations.
  *   - A tick resolves a lone ESC as Escape, `ESC [` as Alt+`[`, `ESC O` as Alt+Shift+`O`, a partial sequence as Escape followed by
  *     its bytes re-read as keys, an unfinished UTF-8 character as U+FFFD, and drops a partial string or X10 report. While the pushed
  *     flags contain `Disambiguate` no key starts with a bare ESC, so a tick means a stalled sequence: a lone ESC and every partial
  *     sequence are dropped, with no Escape and no re-read (the stall rule, M3d). Keys typed before the terminal applies the push arrive
  *     in the legacy form, so a lone ESC among them is dropped too.
  *
  * Bounded by [[DecoderLimits]], deterministic, and independent of how the bytes are split into chunks.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Decoder {

  final private case class Step(state: DecoderState, events: Vector[Event], replies: Vector[Reply], consumed: Boolean)

  private val NoReplies: Vector[Reply] = Vector.empty[Reply]

  private val Esc: Int = 0x1b

  private val Bel: Int = 0x07

  private val Del: Int = 0x7f

  private val Replacement: Char = '�'

  private val PasteTerminator: IArray[Int] = IArray(0x1b, 0x5b, 0x32, 0x30, 0x31, 0x7e)

  private val TerminatorLength: Int = 6

  /** The state after one input with the events and replies it produced, in order. */
  def step(state: DecoderState, input: DecoderInput): Decoded = input match {
    case DecoderInput.Bytes(chunk) => feed(chunk, 0, state, Vector.empty[Event], NoReplies)
    case DecoderInput.Tick => tick(state)
  }

  /** [[step]] over every input in order, the events and replies concatenated. */
  def stepAll(state: DecoderState, inputs: Vector[DecoderInput]): Decoded =
    inputs.foldLeft(Decoded(state, Vector.empty[Event], NoReplies)) {
      case (Decoded(current, events, replies), input) =>
        step(current, input) match {
          case Decoded(next, produced, answered) => Decoded(next, events ++ produced, replies ++ answered)
        }
    }

  @tailrec
  private def feed(chunk: IArray[Byte], i: Int, state: DecoderState, events: Vector[Event], replies: Vector[Reply]): Decoded =
    if (i >= chunk.length) {
      Decoded(state, events, replies)
    } else {
      val step = byte(state, chunk(i).toInt & 0xff)
      feed(chunk, if (step.consumed) i + 1 else i, step.state, events ++ step.events, replies ++ step.replies)
    }

  private def byte(state: DecoderState, b: Int): Step = state.mode match {
    case Ground => ground(state, b)
    case Escape => escape(state, b)
    case EscapeIntermediate(bytes) => escapeIntermediate(state, bytes, b)
    case Csi(bytes, ignoring) => csi(state, bytes, ignoring, b)
    case Ss3 => ss3(state, b)
    case StringSeq(kind, escPending, length, bytes) => stringSeq(state, kind, escPending, length, bytes, b)
    case X10Mouse(bytes) => x10(state, bytes, b)
    case Paste(body, matched) => paste(state, body, matched, b)
  }

  private def ground(state: DecoderState, b: Int): Step =
    if (state.utf8Need > 0) {
      utf8Continue(state, b)
    } else if (b === Esc) {
      Step(state.copy(mode = Escape), Vector.empty[Event], NoReplies, true)
    } else if (b < 0x20 || b === Del) {
      Step(state.copy(altPending = false), Vector(control(b, state.altPending)), NoReplies, true)
    } else if (b < 0x80) {
      Step(state.copy(altPending = false), Vector(printable(b.toChar, state.altPending)), NoReplies, true)
    } else if (b >= 0xc2 && b <= 0xdf) {
      Step(state.copy(utf8 = Vector(b.toByte), utf8Need = 1), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0xe0 && b <= 0xef) {
      Step(state.copy(utf8 = Vector(b.toByte), utf8Need = 2), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0xf0 && b <= 0xf4) {
      Step(state.copy(utf8 = Vector(b.toByte), utf8Need = 3), Vector.empty[Event], NoReplies, true)
    } else {
      Step(state.copy(altPending = false), Vector(printable(Replacement, state.altPending)), NoReplies, true)
    }

  private def utf8Continue(state: DecoderState, b: Int): Step =
    if (b >= 0x80 && b <= 0xbf) {
      val bytes = state.utf8 :+ b.toByte
      val need  = state.utf8Need - 1
      if (need === 0) {
        Step(
          state.copy(utf8 = Vector.empty[Byte], utf8Need = 0, altPending = false),
          scalarEvents(decodeUtf8(bytes), state.altPending),
          NoReplies,
          true,
        )
      } else {
        Step(state.copy(utf8 = bytes, utf8Need = need), Vector.empty[Event], NoReplies, true)
      }
    } else {
      Step(
        state.copy(utf8 = Vector.empty[Byte], utf8Need = 0, altPending = false),
        Vector(printable(Replacement, state.altPending)),
        NoReplies,
        false,
      )
    }

  /** The scalar value of a complete lead-plus-continuations sequence, U+FFFD for an overlong, surrogate, or out-of-range one. */
  private def decodeUtf8(bytes: Vector[Byte]): Int = {
    val ints = bytes.map(_.toInt & 0xff)
    ints.toList match {
      case b0 :: b1 :: Nil =>
        val cp = ((b0 & 0x1f) << 6) | (b1 & 0x3f)
        if (cp >= 0x80) cp else Replacement.toInt
      case b0 :: b1 :: b2 :: Nil =>
        val cp = ((b0 & 0x0f) << 12) | ((b1 & 0x3f) << 6) | (b2 & 0x3f)
        if (cp >= 0x800 && (cp < 0xd800 || cp > 0xdfff)) cp else Replacement.toInt
      case b0 :: b1 :: b2 :: b3 :: Nil =>
        val cp = ((b0 & 0x07) << 18) | ((b1 & 0x3f) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f)
        if (cp >= 0x10000 && cp <= 0x10ffff) cp else Replacement.toInt
      case _ => Replacement.toInt
    }
  }

  private def scalarEvents(cp: Int, alt: Boolean): Vector[Event] =
    if (cp <= 0xffff) {
      Vector(printable(cp.toChar, alt))
    } else {
      val offset = cp - 0x10000
      Vector(printable((0xd800 + (offset >> 10)).toChar, alt), printable((0xdc00 + (offset & 0x3ff)).toChar, alt))
    }

  private def escape(state: DecoderState, b: Int): Step =
    if (b === '['.toInt) {
      Step(state.copy(mode = Csi(Vector.empty[Byte], false)), Vector.empty[Event], NoReplies, true)
    } else if (b === 'O'.toInt) {
      Step(state.copy(mode = Ss3), Vector.empty[Event], NoReplies, true)
    } else if (b === 'P'.toInt) {
      Step(state.copy(mode = StringSeq(StringKind.Dcs, false, 0, Vector.empty[Byte])), Vector.empty[Event], NoReplies, true)
    } else if (b === ']'.toInt) {
      Step(state.copy(mode = StringSeq(StringKind.Osc, false, 0, Vector.empty[Byte])), Vector.empty[Event], NoReplies, true)
    } else if (b === 'X'.toInt) {
      Step(state.copy(mode = StringSeq(StringKind.Sos, false, 0, Vector.empty[Byte])), Vector.empty[Event], NoReplies, true)
    } else if (b === '^'.toInt) {
      Step(state.copy(mode = StringSeq(StringKind.Pm, false, 0, Vector.empty[Byte])), Vector.empty[Event], NoReplies, true)
    } else if (b === '_'.toInt) {
      Step(state.copy(mode = StringSeq(StringKind.Apc, false, 0, Vector.empty[Byte])), Vector.empty[Event], NoReplies, true)
    } else if (b === Esc) {
      Step(state, Vector(key(KeyCode.Escape, KeyModifiers.empty)), NoReplies, true)
    } else if (b >= 0x20 && b <= 0x2f) {
      Step(state.copy(mode = EscapeIntermediate(Vector(b.toByte))), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0x30 && b <= 0x7e) {
      Step(state.copy(mode = Ground, altPending = false), Vector(printable(b.toChar, true)), NoReplies, true)
    } else if (b < 0x20 || b === Del) {
      Step(state.copy(mode = Ground, altPending = false), Vector(control(b, true)), NoReplies, true)
    } else {
      Step(state.copy(mode = Ground, altPending = true), Vector.empty[Event], NoReplies, false)
    }

  private def escapeIntermediate(state: DecoderState, bytes: Vector[Byte], b: Int): Step =
    if (b === Esc) {
      Step(state.copy(mode = Escape), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0x20 && b <= 0x2f) {
      val next = if (bytes.lengthIs >= DecoderLimits.MaxControlSequence) bytes else bytes :+ b.toByte
      Step(state.copy(mode = EscapeIntermediate(next)), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0x30 && b <= 0x7e) {
      Step(state.copy(mode = Ground), Vector.empty[Event], NoReplies, true)
    } else {
      Step(state.copy(mode = Ground), Vector.empty[Event], NoReplies, false)
    }

  private def csi(state: DecoderState, bytes: Vector[Byte], ignoring: Boolean, b: Int): Step =
    if (b === Esc) {
      Step(state.copy(mode = Escape), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0x20 && b <= 0x3f) {
      if (ignoring || bytes.lengthIs >= DecoderLimits.MaxControlSequence)
        Step(state.copy(mode = Csi(bytes, true)), Vector.empty[Event], NoReplies, true)
      else Step(state.copy(mode = Csi(bytes :+ b.toByte, false)), Vector.empty[Event], NoReplies, true)
    } else if (b >= 0x40 && b <= 0x7e) {
      if (ignoring) {
        Step(state.copy(mode = Ground), Vector.empty[Event], NoReplies, true)
      } else {
        dispatch(bytes, b.toChar, state) match {
          case (mode, events, replies) => Step(state.copy(mode = mode), events, replies, true)
        }
      }
    } else {
      Step(state.copy(mode = Ground), Vector.empty[Event], NoReplies, false)
    }

  private def ss3(state: DecoderState, b: Int): Step =
    if (b === Esc) {
      Step(state.copy(mode = Escape), Vector.empty[Event], NoReplies, true)
    } else if (b < 0x20 || b === Del) {
      Step(state.copy(mode = Ground), Vector.empty[Event], NoReplies, false)
    } else {
      val events = b.toChar match {
        case 'A' => Vector(key(KeyCode.Up, KeyModifiers.empty))
        case 'B' => Vector(key(KeyCode.Down, KeyModifiers.empty))
        case 'C' => Vector(key(KeyCode.Right, KeyModifiers.empty))
        case 'D' => Vector(key(KeyCode.Left, KeyModifiers.empty))
        case 'H' => Vector(key(KeyCode.Home, KeyModifiers.empty))
        case 'F' => Vector(key(KeyCode.End, KeyModifiers.empty))
        case 'P' => Vector(key(KeyCode.f(1), KeyModifiers.empty))
        case 'Q' => Vector(key(KeyCode.f(2), KeyModifiers.empty))
        case 'R' => Vector(key(KeyCode.f(3), KeyModifiers.empty))
        case 'S' => Vector(key(KeyCode.f(4), KeyModifiers.empty))
        case 'M' => Vector(key(KeyCode.Enter, KeyModifiers.empty))
        case 'E' => Vector(key(KeyCode.KeypadBegin, KeyModifiers.empty))
        case _ => Vector.empty[Event]
      }
      Step(state.copy(mode = Ground), events, NoReplies, true)
    }

  private def stringSeq(state: DecoderState, kind: StringKind, escPending: Boolean, length: Int, bytes: Vector[Byte], b: Int): Step =
    if (escPending) {
      if (b === '\\'.toInt) Step(state.copy(mode = Ground), Vector.empty[Event], stringReply(kind, bytes), true)
      else Step(state.copy(mode = Escape), Vector.empty[Event], NoReplies, false)
    } else if (b === Bel) {
      Step(state.copy(mode = Ground), Vector.empty[Event], stringReply(kind, bytes), true)
    } else if (b === Esc) {
      Step(state.copy(mode = StringSeq(kind, true, length, bytes)), Vector.empty[Event], NoReplies, true)
    } else {
      val buffered =
        if (kind === StringKind.Dcs && length < DecoderLimits.MaxStringSequence) bytes :+ b.toByte else bytes
      Step(
        state.copy(mode = StringSeq(kind, false, math.min(length + 1, DecoderLimits.MaxStringSequence), buffered)),
        Vector.empty[Event],
        NoReplies,
        true,
      )
    }

  /** A terminated DCS body parsed as a reply: `1 + r` and `0 + r` termcap answers, `> |` version answers, anything else dropped. */
  private def stringReply(kind: StringKind, bytes: Vector[Byte]): Vector[Reply] =
    kind match {
      case StringKind.Dcs => dcsReply(new String(bytes.toArray, StandardCharsets.UTF_8)).fold(NoReplies)(reply => Vector(reply))
      case StringKind.Osc | StringKind.Apc | StringKind.Pm | StringKind.Sos => NoReplies
    }

  private def dcsReply(body: String): Option[Reply] =
    if (body.startsWith("1+r")) Reply.TermcapReply(true, termcapEntries(body.substring(3))).some
    else if (body.startsWith("0+r")) Reply.TermcapReply(false, termcapEntries(body.substring(3))).some
    else if (body.startsWith(">|")) Reply.VersionReply(body.substring(2)).some
    else none[Reply]

  private def termcapEntries(payload: String): Vector[Reply.TermcapEntry] =
    if (payload.isEmpty) {
      Vector.empty[Reply.TermcapEntry]
    } else {
      payload.split(";", -1).toVector.flatMap { entry =>
        entry.split("=", 2).toList match {
          case name :: Nil => hexDecode(name).map(Reply.TermcapEntry(_, none[String]))
          case name :: value :: Nil => hexDecode(name).map(n => Reply.TermcapEntry(n, hexDecode(value)))
          case _ => none[Reply.TermcapEntry]
        }
      }
    }

  /** Two lowercase or uppercase hex digits per character, `None` for a malformed payload. */
  private def hexDecode(hex: String): Option[String] =
    if (hex.isEmpty || (hex.length % 2 !== 0) || !hex.forall(isHexDigit)) {
      none[String]
    } else {
      val builder = new java.lang.StringBuilder(hex.length / 2)
      hexLoop(hex, 0, builder)
      builder.toString.some
    }

  @tailrec
  private def hexLoop(hex: String, i: Int, builder: java.lang.StringBuilder): Unit =
    if (i >= hex.length) {
      ()
    } else {
      builder.append(Integer.parseInt(hex.substring(i, i + 2), 16).toChar): Unit
      hexLoop(hex, i + 2, builder)
    }

  private def isHexDigit(c: Char): Boolean = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  private def x10(state: DecoderState, bytes: Vector[Byte], b: Int): Step = {
    val next = bytes :+ b.toByte
    if (next.lengthIs < 3) {
      Step(state.copy(mode = X10Mouse(next)), Vector.empty[Event], NoReplies, true)
    } else {
      val values = next.map(_.toInt & 0xff)
      val events = values.toList match {
        case cb :: cx :: cy :: Nil if cb >= 32 && cx >= 32 && cy >= 32 => mouse(cb - 32, cx - 32, cy - 32, false, true)
        case _ => Vector.empty[Event]
      }
      Step(state.copy(mode = Ground), events, NoReplies, true)
    }
  }

  private def paste(state: DecoderState, body: Vector[Byte], matched: Int, b: Int): Step =
    if (b === PasteTerminator(matched)) {
      if (matched + 1 >= TerminatorLength) {
        val text = new String(body.toArray, StandardCharsets.UTF_8)
        Step(state.copy(mode = Ground), Vector(Event.paste(text)), NoReplies, true)
      } else {
        Step(state.copy(mode = Paste(body, matched + 1)), Vector.empty[Event], NoReplies, true)
      }
    } else {
      val flushed = appendPaste(body, PasteTerminator.take(matched).map(_.toByte).toVector)
      if (b === PasteTerminator(0)) Step(state.copy(mode = Paste(flushed, 1)), Vector.empty[Event], NoReplies, true)
      else Step(state.copy(mode = Paste(appendPaste(flushed, Vector(b.toByte)), 0)), Vector.empty[Event], NoReplies, true)
    }

  private def appendPaste(body: Vector[Byte], bytes: Vector[Byte]): Vector[Byte] = {
    val room = DecoderLimits.MaxPaste - body.length
    if (room <= 0) body else body ++ bytes.take(room)
  }

  private def tick(state: DecoderState): Decoded = {
    val flushedUtf8 = if (state.utf8Need > 0) Vector(printable(Replacement, state.altPending)) else Vector.empty[Event]
    val cleared     = state.copy(utf8 = Vector.empty[Byte], utf8Need = 0, altPending = false)
    val stalled     = state.keyboard.contains(KittyFlags.Disambiguate)
    state.mode match {
      case Ground | Paste(_, _) => Decoded(cleared, flushedUtf8, NoReplies)
      case Escape | EscapeIntermediate(_) | Csi(_, _) | Ss3 if stalled => Decoded(cleared.copy(mode = Ground), flushedUtf8, NoReplies)
      case Escape => Decoded(cleared.copy(mode = Ground), flushedUtf8 :+ key(KeyCode.Escape, KeyModifiers.empty), NoReplies)
      case EscapeIntermediate(bytes) => refeed(cleared, flushedUtf8, bytes)
      case Csi(bytes, _) =>
        if (bytes.isEmpty) Decoded(cleared.copy(mode = Ground), flushedUtf8 :+ printable('[', true), NoReplies)
        else refeed(cleared, flushedUtf8, '['.toByte +: bytes)
      case Ss3 => Decoded(cleared.copy(mode = Ground), flushedUtf8 :+ printable('O', true), NoReplies)
      case StringSeq(_, _, _, _) | X10Mouse(_) => Decoded(cleared.copy(mode = Ground), flushedUtf8, NoReplies)
    }
  }

  /** Escape, then the buffered bytes read again from the ground state (plan refinement R5 of M1e). */
  private def refeed(cleared: DecoderState, before: Vector[Event], bytes: Vector[Byte]): Decoded =
    feed(IArray.from(bytes), 0, cleared.copy(mode = Ground), before :+ key(KeyCode.Escape, KeyModifiers.empty), NoReplies)

  /** A parsed control sequence: the private marker, each `;` parameter as its `:` sub-fields (empty ones `None`), the intermediates. */
  final private case class Parsed(marker: Option[Char], params: Vector[Vector[Option[Int]]], intermediates: Vector[Char])

  private def parse(bytes: Vector[Byte]): Parsed = {
    val chars         = bytes.map(b => (b.toInt & 0xff).toChar)
    val marker        = chars.headOption.filter(c => c === '<' || c === '=' || c === '>' || c === '?')
    val rest          = if (marker.isDefined) chars.drop(1) else chars
    val paramChars    = rest.takeWhile(c => c >= '0' && c <= '?')
    val intermediates = rest.drop(paramChars.length).filter(c => c >= ' ' && c <= '/')
    val params        =
      if (paramChars.isEmpty) Vector.empty[Vector[Option[Int]]]
      else paramChars.mkString.split(";", -1).toVector.map(p => p.split(":", -1).toVector.map(number))
    Parsed(marker, params, intermediates)
  }

  /** Sub-field 0 of parameter `i`. */
  private def param(parsed: Parsed, i: Int): Option[Int] = parsed.params.lift(i).flatMap(_.headOption).flatten

  /** Sub-field `j` of parameter `i`. */
  private def sub(parsed: Parsed, i: Int, j: Int): Option[Int] = parsed.params.lift(i).flatMap(_.lift(j)).flatten

  private def number(s: String): Option[Int] =
    if (s.nonEmpty && s.length <= 7 && s.forall(c => c >= '0' && c <= '9')) s.toInt.some else none[Int]

  private def dispatch(bytes: Vector[Byte], fin: Char, state: DecoderState): (DecoderMode, Vector[Event], Vector[Reply]) = {
    val parsed    = parse(bytes)
    val plain     = parsed.marker.isEmpty && parsed.intermediates.isEmpty
    val kitty     = !state.keyboard.isEmpty
    val modifiers =
      param(parsed, 1).map(p => if (kitty) kittyModifiersOf(p) else modifiersOf(p)).getOrElse(KeyModifiers.empty)
    val kind      = kindOf(sub(parsed, 1, 1))
    fin match {
      case 'A' if plain => (Ground, keyed(KeyCode.Up, modifiers, kind, state), NoReplies)
      case 'B' if plain => (Ground, keyed(KeyCode.Down, modifiers, kind, state), NoReplies)
      case 'C' if plain => (Ground, keyed(KeyCode.Right, modifiers, kind, state), NoReplies)
      case 'D' if plain => (Ground, keyed(KeyCode.Left, modifiers, kind, state), NoReplies)
      case 'E' if plain => (Ground, keyed(KeyCode.KeypadBegin, modifiers, kind, state), NoReplies)
      case 'H' if plain => (Ground, keyed(KeyCode.Home, modifiers, kind, state), NoReplies)
      case 'F' if plain => (Ground, keyed(KeyCode.End, modifiers, kind, state), NoReplies)
      case 'P' if plain => (Ground, keyed(KeyCode.f(1), modifiers, kind, state), NoReplies)
      case 'Q' if plain => (Ground, keyed(KeyCode.f(2), modifiers, kind, state), NoReplies)
      case 'R' if plain && state.expectingReplies => (Ground, Vector.empty[Event], cursorReport(parsed))
      case 'R' if plain && kitty => (Ground, Vector.empty[Event], NoReplies)
      case 'R' if plain => (Ground, keyed(KeyCode.f(3), modifiers, kind, state), NoReplies)
      case 'S' if plain => (Ground, keyed(KeyCode.f(4), modifiers, kind, state), NoReplies)
      case 'Z' if plain => (Ground, Vector(key(KeyCode.BackTab, KeyModifiers(KeyModifier.Shift))), NoReplies)
      case 'I' if plain => (Ground, Vector(Event.FocusGained), NoReplies)
      case 'O' if plain => (Ground, Vector(Event.FocusLost), NoReplies)
      case '~' if plain =>
        param(parsed, 0) match {
          case Some(200) => (Paste(Vector.empty[Byte], 0), Vector.empty[Event], NoReplies)
          case Some(n) => (Ground, tilde(n).fold(Vector.empty[Event])(code => keyed(code, modifiers, kind, state)), NoReplies)
          case None => (Ground, Vector.empty[Event], NoReplies)
        }
      case 'M' | 'm' if parsed.marker === '<'.some && parsed.params.length === 3 =>
        (param(parsed, 0), param(parsed, 1), param(parsed, 2)) match {
          case (Some(b), Some(x), Some(y)) => (Ground, mouse(b, x, y, fin === 'm', false), NoReplies)
          case (_, _, _) => (Ground, Vector.empty[Event], NoReplies)
        }
      case 'M' if plain && parsed.params.isEmpty => (X10Mouse(Vector.empty[Byte]), Vector.empty[Event], NoReplies)
      case 'M' if plain && parsed.params.length === 3 =>
        (param(parsed, 0), param(parsed, 1), param(parsed, 2)) match {
          case (Some(b), Some(x), Some(y)) if b >= 32 => (Ground, mouse(b - 32, x, y, false, true), NoReplies)
          case (_, _, _) => (Ground, Vector.empty[Event], NoReplies)
        }
      case 'c' if parsed.marker === '?'.some => (Ground, Vector.empty[Event], Vector(Reply.PrimaryDeviceAttributes(flatParams(parsed))))
      case 'c' if parsed.marker === '>'.some =>
        (Ground, Vector.empty[Event], Vector(Reply.SecondaryDeviceAttributes(flatParams(parsed))))
      case 'y' if parsed.marker === '?'.some && parsed.intermediates === Vector('$') =>
        (param(parsed, 0), param(parsed, 1)) match {
          case (Some(mode), Some(value)) => (Ground, Vector.empty[Event], Vector(Reply.PrivateModeReport(mode, value)))
          case (_, _) => (Ground, Vector.empty[Event], NoReplies)
        }
      case 'u' if parsed.marker === '?'.some && parsed.intermediates.isEmpty =>
        (Ground, Vector.empty[Event], Vector(Reply.KeyboardFlags(param(parsed, 0).getOrElse(0))))
      case 'u' if plain => (Ground, kittyKey(parsed, state), NoReplies)
      case _ => (Ground, Vector.empty[Event], NoReplies)
    }
  }

  private def flatParams(parsed: Parsed): Vector[Int] = parsed.params.map(_.headOption.flatten.getOrElse(0))

  /** The Cursor Position Report, 1-based in the wire form, 0-based in the reply, dropped outside `1..MaxCoordinate`. */
  private def cursorReport(parsed: Parsed): Vector[Reply] =
    (param(parsed, 0), param(parsed, 1)) match {
      case (Some(row), Some(column))
          if parsed.params.length === 2 && row >= 1 && column >= 1 && row <= DecoderLimits.MaxCoordinate &&
            column <= DecoderLimits.MaxCoordinate =>
        Vector(Reply.CursorPosition(Position(NonNegInts.clamp(column.toLong - 1L), NonNegInts.clamp(row.toLong - 1L))))
      case (_, _) => NoReplies
    }

  private def tilde(n: Int): Option[KeyCode] = n match {
    case 1 | 7 => KeyCode.Home.some
    case 2 => KeyCode.Insert.some
    case 3 => KeyCode.Delete.some
    case 4 | 8 => KeyCode.End.some
    case 5 => KeyCode.PageUp.some
    case 6 => KeyCode.PageDown.some
    case 11 => KeyCode.f(1).some
    case 12 => KeyCode.f(2).some
    case 13 => KeyCode.f(3).some
    case 14 => KeyCode.f(4).some
    case 15 => KeyCode.f(5).some
    case 17 => KeyCode.f(6).some
    case 18 => KeyCode.f(7).some
    case 19 => KeyCode.f(8).some
    case 20 => KeyCode.f(9).some
    case 21 => KeyCode.f(10).some
    case 23 => KeyCode.f(11).some
    case 24 => KeyCode.f(12).some
    case 25 => KeyCode.f(13).some
    case 26 => KeyCode.f(14).some
    case 28 => KeyCode.f(15).some
    case 29 => KeyCode.f(16).some
    case 31 => KeyCode.f(17).some
    case 32 => KeyCode.f(18).some
    case 33 => KeyCode.f(19).some
    case 34 => KeyCode.f(20).some
    case 57427 => KeyCode.KeypadBegin.some
    case _ => none[KeyCode]
  }

  /** The event kind of the modifier parameter's sub-field: absent or 1 press, 2 repeat, 3 release, `None` for any other value. */
  private def kindOf(field: Option[Int]): Option[KeyEventKind] = field match {
    case None | Some(1) => KeyEventKind.Press.some
    case Some(2) => KeyEventKind.Repeat.some
    case Some(3) => KeyEventKind.Release.some
    case Some(_) => none[KeyEventKind]
  }

  /** The kind filter (design doc 6.2): repeats and releases pass only when the pushed flags contain `EventTypes`, otherwise a repeat is
    * delivered as a press and a release is dropped.
    */
  private def delivered(kind: KeyEventKind, state: DecoderState): Option[KeyEventKind] = kind match {
    case KeyEventKind.Press => KeyEventKind.Press.some
    case KeyEventKind.Repeat =>
      if (state.keyboard.contains(KittyFlags.EventTypes)) KeyEventKind.Repeat.some else KeyEventKind.Press.some
    case KeyEventKind.Release => Option.when(state.keyboard.contains(KittyFlags.EventTypes))(KeyEventKind.Release)
  }

  /** One key event of the code when the kind parses and passes the filter. */
  private def keyed(code: KeyCode, modifiers: KeyModifiers, kind: Option[KeyEventKind], state: DecoderState): Vector[Event] =
    kind.flatMap(k => delivered(k, state)).fold(Vector.empty[Event])(k => Vector(keyWith(code, modifiers, k)))

  private val CapsLockBit: Int = 64

  private def isSurrogate(cp: Int): Boolean = cp >= 0xd800 && cp <= 0xdfff

  /** A kitty key report (M3d): the associated text when the report carries some and is not a release, the key otherwise. */
  private def kittyKey(parsed: Parsed, state: DecoderState): Vector[Event] = {
    val bits      = math.max(0, param(parsed, 1).getOrElse(1) - 1)
    val modifiers = kittyModifiersOf(bits + 1)
    val text      = parsed.params.lift(2).fold(Vector.empty[Int])(_.flatten)
    val reported  = kindOf(sub(parsed, 1, 1))
    val release   = reported match {
      case Some(KeyEventKind.Release) => true
      case Some(KeyEventKind.Press) | Some(KeyEventKind.Repeat) | None => false
    }
    reported.flatMap(k => delivered(k, state)) match {
      case None => Vector.empty[Event]
      case Some(kind) =>
        if (text.nonEmpty && !release) {
          text.flatMap(cp => textEvents(cp, kind))
        } else {
          param(parsed, 0).fold(Vector.empty[Event])(code =>
            keyEvents(code, sub(parsed, 0, 1), modifiers, (bits & CapsLockBit) !== 0, kind)
          )
        }
    }
  }

  /** One code point of associated text, decoded as the same character arriving as UTF-8 would be: controls skipped (the specification
    * forbids them in text), an invalid scalar as U+FFFD.
    */
  private def textEvents(cp: Int, kind: KeyEventKind): Vector[Event] =
    if (cp < 0x20 || cp === Del) Vector.empty[Event]
    else if (cp > 0x10ffff || isSurrogate(cp)) Vector(printableWith(Replacement, kind))
    else scalarEventsWith(cp, kind)

  /** The key of a report without text (M3d): the named keys, the Private Use Area keys, a letter by Shift xor Caps Lock, another
    * character as its shifted key when Shift is held and the report names one.
    */
  private def keyEvents(code: Int, shifted: Option[Int], modifiers: KeyModifiers, caps: Boolean, kind: KeyEventKind): Vector[Event] = {
    val shift = modifiers.contains(KeyModifier.Shift)
    code match {
      case 9 => Vector(keyWith(if (shift) KeyCode.BackTab else KeyCode.Tab, modifiers, kind))
      case 13 => Vector(keyWith(KeyCode.Enter, modifiers, kind))
      case 27 => Vector(keyWith(KeyCode.Escape, modifiers, kind))
      case 127 => Vector(keyWith(KeyCode.Backspace, modifiers, kind))
      case c if c < 0x20 => Vector.empty[Event]
      case c if c >= KittyKeys.First && c <= KittyKeys.Last =>
        KittyKeys.keyCodeOf(c).fold(Vector.empty[Event])(k => Vector(keyWith(k, modifiers, kind)))
      case c if c > 0x10ffff || isSurrogate(c) => Vector.empty[Event]
      case c if c >= 'A'.toInt && c <= 'Z'.toInt =>
        Vector(keyWith(KeyCode.char(c.toChar), modifiers.add(KeyModifier.Shift), kind))
      case c if c >= 'a'.toInt && c <= 'z'.toInt =>
        if (shift ^ caps) Vector(keyWith(KeyCode.char((c - 0x20).toChar), modifiers.add(KeyModifier.Shift), kind))
        else Vector(keyWith(KeyCode.char(c.toChar), modifiers.remove(KeyModifier.Shift), kind))
      case c =>
        shifted.filter(s => s >= 0x20 && s =!= Del && s <= 0x10ffff && !isSurrogate(s)) match {
          case Some(s) if shift => scalarKeys(s, modifiers.remove(KeyModifier.Shift), kind)
          case Some(_) | None => scalarKeys(c, modifiers, kind)
        }
    }
  }

  /** A scalar as one `Char` key event, or two carrying the surrogate halves, with the modifiers and kind given. */
  private def scalarKeys(cp: Int, modifiers: KeyModifiers, kind: KeyEventKind): Vector[Event] =
    if (cp <= 0xffff) {
      Vector(keyWith(KeyCode.char(cp.toChar), modifiers, kind))
    } else {
      val offset = cp - 0x10000
      Vector(
        keyWith(KeyCode.char((0xd800 + (offset >> 10)).toChar), modifiers, kind),
        keyWith(KeyCode.char((0xdc00 + (offset & 0x3ff)).toChar), modifiers, kind),
      )
    }

  /** The kitty modifier parameter: `1 + Shift 1 + Alt 2 + Control 4 + Super 8 + Hyper 16 + Meta 32`, Caps Lock 64 and Num Lock 128
    * dropped (the event model has no lock state, design doc 6.2), a parameter below 1 meaning none.
    */
  def kittyModifiersOf(parameter: Int): KeyModifiers = {
    val bits = math.max(0, parameter - 1)
    KeyModifiers.of(
      List(
        Option.when((bits & 1) !== 0)(KeyModifier.Shift),
        Option.when((bits & 2) !== 0)(KeyModifier.Alt),
        Option.when((bits & 4) !== 0)(KeyModifier.Control),
        Option.when((bits & 8) !== 0)(KeyModifier.Super),
        Option.when((bits & 16) !== 0)(KeyModifier.Hyper),
        Option.when((bits & 32) !== 0)(KeyModifier.Meta),
      ).flatten
    )
  }

  /** The xterm modifier parameter: `1 + Shift 1 + Alt 2 + Control 4 + Meta 8`. */
  def modifiersOf(parameter: Int): KeyModifiers = {
    val bits = parameter - 1
    List(
      Option.when((bits & 1) !== 0)(KeyModifier.Shift),
      Option.when((bits & 2) !== 0)(KeyModifier.Alt),
      Option.when((bits & 4) !== 0)(KeyModifier.Control),
      Option.when((bits & 8) !== 0)(KeyModifier.Meta),
    ).flatten match {
      case modifiers => KeyModifiers.of(modifiers)
    }
  }

  /** One mouse report: the button field of xterm ctlseqs (button in the low two bits, Shift 4, Alt 8, Control 16, motion 32, wheel
    * 64), 1-based coordinates. `release` is the SGR `m` final, `legacy` marks the X10 and urxvt encodings whose button 3 means release.
    */
  private def mouse(b: Int, x: Int, y: Int, release: Boolean, legacy: Boolean): Vector[Event] =
    if (x < 1 || y < 1 || x > DecoderLimits.MaxCoordinate || y > DecoderLimits.MaxCoordinate) {
      Vector.empty[Event]
    } else {
      val button    = b & 3
      val modifiers = KeyModifiers.of(
        List(
          Option.when((b & 4) !== 0)(KeyModifier.Shift),
          Option.when((b & 8) !== 0)(KeyModifier.Alt),
          Option.when((b & 16) !== 0)(KeyModifier.Control),
        ).flatten
      )
      val kind      =
        if ((b & 64) !== 0) {
          button match {
            case 0 => MouseEventKind.ScrollUp
            case 1 => MouseEventKind.ScrollDown
            case 2 => MouseEventKind.ScrollLeft
            case _ => MouseEventKind.ScrollRight
          }
        } else if ((b & 32) !== 0) {
          buttonOf(button).fold(MouseEventKind.Moved)(MouseEventKind.drag)
        } else if (release) {
          buttonOf(button).fold(MouseEventKind.Moved)(MouseEventKind.up)
        } else {
          buttonOf(button).fold(if (legacy) MouseEventKind.up(MouseButton.Left) else MouseEventKind.Moved)(MouseEventKind.down)
        }
      val position  = Position(NonNegInts.clamp(x.toLong - 1L), NonNegInts.clamp(y.toLong - 1L))
      Vector(Event.mouse(MouseEvent(kind, position, modifiers)))
    }

  private def buttonOf(button: Int): Option[MouseButton] = button match {
    case 0 => MouseButton.Left.some
    case 1 => MouseButton.Middle.some
    case 2 => MouseButton.Right.some
    case _ => none[MouseButton]
  }

  private def key(code: KeyCode, modifiers: KeyModifiers): Event = Event.key(KeyEvent(code, modifiers, KeyEventKind.Press))

  private def keyWith(code: KeyCode, modifiers: KeyModifiers, kind: KeyEventKind): Event = Event.key(KeyEvent(code, modifiers, kind))

  /** A character of text with the given kind: Shift only for `A` to `Z`, never Alt (the UTF-8 path's convention). */
  private def printableWith(c: Char, kind: KeyEventKind): Event =
    keyWith(KeyCode.char(c), if (c >= 'A' && c <= 'Z') KeyModifiers(KeyModifier.Shift) else KeyModifiers.empty, kind)

  private def scalarEventsWith(cp: Int, kind: KeyEventKind): Vector[Event] =
    if (cp <= 0xffff) {
      Vector(printableWith(cp.toChar, kind))
    } else {
      val offset = cp - 0x10000
      Vector(printableWith((0xd800 + (offset >> 10)).toChar, kind), printableWith((0xdc00 + (offset & 0x3ff)).toChar, kind))
    }

  private def printable(c: Char, alt: Boolean): Event = {
    val shift     = c >= 'A' && c <= 'Z'
    val modifiers = KeyModifiers.of(List(Option.when(shift)(KeyModifier.Shift), Option.when(alt)(KeyModifier.Alt)).flatten)
    key(KeyCode.char(c), modifiers)
  }

  private def control(b: Int, alt: Boolean): Event = {
    val altSet   = if (alt) KeyModifiers(KeyModifier.Alt) else KeyModifiers.empty
    val withCtrl = altSet.add(KeyModifier.Control)
    b match {
      case 0x0d | 0x0a => key(KeyCode.Enter, altSet)
      case 0x09 => key(KeyCode.Tab, altSet)
      case 0x7f | 0x08 => key(KeyCode.Backspace, altSet)
      case 0x00 => key(KeyCode.char(' '), withCtrl)
      case n if n >= 0x01 && n <= 0x1a => key(KeyCode.char(('a'.toInt + n - 1).toChar), withCtrl)
      case n => key(KeyCode.char(('4'.toInt + n - 0x1c).toChar), withCtrl)
    }
  }

}
