package stui.terminal.decoder

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.event.*
import stui.core.geometry.Position
import stui.terminal.kitty.KittyFlags
import stui.testkit.Assertions

/** Byte sequences and the events they decode to (xterm ctlseqs, the DEC ANSI parser reference).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderFixturesSpec extends Properties {

  private val EscText: String = "\u001b"

  private val Replacement: Char = '�'

  private def decode(inputs: DecoderInput*): Vector[Event] = Decoder.stepAll(DecoderState.initial, inputs.toVector) match {
    case Decoded(_, events, _) => events
  }

  private def replies(inputs: DecoderInput*): Vector[Reply] = Decoder.stepAll(DecoderState.initial, inputs.toVector) match {
    case Decoded(_, _, answered) => answered
  }

  private def replyRow(name: String, expected: Vector[Reply], inputs: DecoderInput*): Test =
    example(name, Assertions.eqv(replies(inputs*), expected))

  private def bytes(values: Int*): DecoderInput = DecoderInput.bytesOfInts(values*)

  private def text(s: String): DecoderInput = DecoderInput.bytesOf(s)

  private def esc(s: String): DecoderInput = DecoderInput.bytesOf(EscText + s)

  private def key(code: KeyCode, modifiers: KeyModifier*): Event = Event.key(KeyEvent(code, KeyModifiers(modifiers*), KeyEventKind.Press))

  private def char(c: Char, modifiers: KeyModifier*): Event = key(KeyCode.char(c), modifiers*)

  private def mouse(kind: MouseEventKind, x: Int, y: Int, modifiers: KeyModifier*): Event =
    Event.mouse(MouseEvent(kind, Position(NonNegInt.unsafeFrom(x), NonNegInt.unsafeFrom(y)), KeyModifiers(modifiers*)))

  private def row(name: String, expected: Vector[Event], inputs: DecoderInput*): Test =
    example(name, Assertions.eqv(decode(inputs*), expected))

  private def decodeIn(state: DecoderState, inputs: DecoderInput*): Vector[Event] = Decoder.stepAll(state, inputs.toVector).events

  /** A row decoded in a state with the kitty flags pushed (M3d). */
  private def kittyRow(name: String, state: DecoderState, expected: Vector[Event], inputs: DecoderInput*): Test =
    example(name, Assertions.eqv(decodeIn(state, inputs*), expected))

  private def pushed(bits: Int): DecoderState = DecoderState.initial.withKeyboard(KittyFlags.fromInt(bits))

  private val k1: DecoderState = pushed(1)

  private val k3: DecoderState = pushed(3)

  private val k7: DecoderState = pushed(7)

  private val k27: DecoderState = pushed(27)

  private def keyOf(code: KeyCode, kind: KeyEventKind, modifiers: KeyModifier*): Event =
    Event.key(KeyEvent(code, KeyModifiers(modifiers*), kind))

  private def release(code: KeyCode, modifiers: KeyModifier*): Event = keyOf(code, KeyEventKind.Release, modifiers*)

  override def tests: List[Test] = List(
    row("a", Vector(char('a')), text("a")),
    row("A carries Shift", Vector(char('A', KeyModifier.Shift)), text("A")),
    row("CR is Enter", Vector(key(KeyCode.Enter)), bytes(0x0d)),
    row("LF is Enter", Vector(key(KeyCode.Enter)), bytes(0x0a)),
    row("TAB", Vector(key(KeyCode.Tab)), bytes(0x09)),
    row("DEL is Backspace", Vector(key(KeyCode.Backspace)), bytes(0x7f)),
    row("BS is Backspace", Vector(key(KeyCode.Backspace)), bytes(0x08)),
    row("0x01 is Control+a", Vector(char('a', KeyModifier.Control)), bytes(0x01)),
    row("0x00 is Control+Space", Vector(char(' ', KeyModifier.Control)), bytes(0x00)),
    row("0x1c is Control+4", Vector(char('4', KeyModifier.Control)), bytes(0x1c)),
    row("CSI A is Up", Vector(key(KeyCode.Up)), esc("[A")),
    row("SS3 A is Up", Vector(key(KeyCode.Up)), esc("OA")),
    row("CSI 1;5A is Control+Up", Vector(key(KeyCode.Up, KeyModifier.Control)), esc("[1;5A")),
    row("CSI 1;3A is Alt+Up", Vector(key(KeyCode.Up, KeyModifier.Alt)), esc("[1;3A")),
    row("CSI 1;2A is Shift+Up", Vector(key(KeyCode.Up, KeyModifier.Shift)), esc("[1;2A")),
    row("CSI 1;10A is Meta+Shift+Up", Vector(key(KeyCode.Up, KeyModifier.Shift, KeyModifier.Meta)), esc("[1;10A")),
    row("CSI H is Home", Vector(key(KeyCode.Home)), esc("[H")),
    row("CSI 1~ is Home", Vector(key(KeyCode.Home)), esc("[1~")),
    row("CSI 3~ is Delete", Vector(key(KeyCode.Delete)), esc("[3~")),
    row("CSI 3;2~ is Shift+Delete", Vector(key(KeyCode.Delete, KeyModifier.Shift)), esc("[3;2~")),
    row("CSI 15~ is F5", Vector(key(KeyCode.f(5))), esc("[15~")),
    row("CSI 24~ is F12", Vector(key(KeyCode.f(12))), esc("[24~")),
    row("CSI 34~ is F20", Vector(key(KeyCode.f(20))), esc("[34~")),
    row("SS3 P is F1", Vector(key(KeyCode.f(1))), esc("OP")),
    row("CSI 1;2P is Shift+F1", Vector(key(KeyCode.f(1), KeyModifier.Shift)), esc("[1;2P")),
    row("CSI 1;2R is Shift+F3 (a CPR needs a pending probe, M1f)", Vector(key(KeyCode.f(3), KeyModifier.Shift)), esc("[1;2R")),
    row("CSI Z is Shift+BackTab", Vector(key(KeyCode.BackTab, KeyModifier.Shift)), esc("[Z")),
    row("ESC x is Alt+x", Vector(char('x', KeyModifier.Alt)), esc("x")),
    row("ESC Y is Alt+Shift+Y", Vector(char('Y', KeyModifier.Shift, KeyModifier.Alt)), esc("Y")),
    row("ESC X starts a string sequence and is consumed", Vector(char('a')), esc("Xtitle\u0007"), text("a")),
    row("ESC then a tick is Escape", Vector(key(KeyCode.Escape)), esc(""), DecoderInput.Tick),
    row("ESC ESC then a tick is two Escapes", Vector(key(KeyCode.Escape), key(KeyCode.Escape)), esc(EscText), DecoderInput.Tick),
    row("ESC [ then a tick is Alt+[", Vector(char('[', KeyModifier.Alt)), esc("["), DecoderInput.Tick),
    row("ESC O then a tick is Alt+Shift+O", Vector(char('O', KeyModifier.Shift, KeyModifier.Alt)), esc("O"), DecoderInput.Tick),
    row("ESC [ 2 then a tick is Escape, [, 2", Vector(key(KeyCode.Escape), char('['), char('2')), esc("[2"), DecoderInput.Tick),
    row("ESC CR is Alt+Enter", Vector(key(KeyCode.Enter, KeyModifier.Alt)), bytes(0x1b, 0x0d)),
    row("a Hangul syllable", Vector(char('가')), text("가")),
    row("ESC before a multi-byte character is Alt", Vector(char('가', KeyModifier.Alt)), bytes(0x1b, 0xea, 0xb0, 0x80)),
    row("an astral scalar is two surrogate chars", Vector(char('\ud83d'), char('\ude00')), bytes(0xf0, 0x9f, 0x98, 0x80)),
    row("0xff is U+FFFD", Vector(char(Replacement)), bytes(0xff)),
    row("a truncated lead then a is U+FFFD then a", Vector(char(Replacement), char('a')), bytes(0xc3, 0x61)),
    row("a truncated lead then a tick is U+FFFD", Vector(char(Replacement)), bytes(0xc3), DecoderInput.Tick),
    row("an overlong C0 80 is two U+FFFD", Vector(char(Replacement), char(Replacement)), bytes(0xc0, 0x80)),
    row("a surrogate encoded in UTF-8 is U+FFFD", Vector(char(Replacement)), bytes(0xed, 0xa0, 0x80)),
    row("SGR mouse press", Vector(mouse(MouseEventKind.down(MouseButton.Left), 9, 4)), esc("[<0;10;5M")),
    row("SGR mouse release", Vector(mouse(MouseEventKind.up(MouseButton.Left), 9, 4)), esc("[<0;10;5m")),
    row("SGR mouse drag", Vector(mouse(MouseEventKind.drag(MouseButton.Left), 9, 4)), esc("[<32;10;5M")),
    row("SGR mouse move", Vector(mouse(MouseEventKind.Moved, 9, 4)), esc("[<35;10;5M")),
    row("SGR wheel up", Vector(mouse(MouseEventKind.ScrollUp, 0, 0)), esc("[<64;1;1M")),
    row("SGR wheel down", Vector(mouse(MouseEventKind.ScrollDown, 0, 0)), esc("[<65;1;1M")),
    row("SGR Control+press", Vector(mouse(MouseEventKind.down(MouseButton.Left), 1, 1, KeyModifier.Control)), esc("[<16;2;2M")),
    row(
      "SGR right button with Shift and Alt",
      Vector(mouse(MouseEventKind.down(MouseButton.Right), 0, 0, KeyModifier.Shift, KeyModifier.Alt)),
      esc("[<14;1;1M"),
    ),
    row("X10 press", Vector(mouse(MouseEventKind.down(MouseButton.Left), 10, 5)), bytes(0x1b, 0x5b, 0x4d, 32 + 0, 32 + 11, 32 + 6)),
    row(
      "X10 release is Up(Left)",
      Vector(mouse(MouseEventKind.up(MouseButton.Left), 10, 5)),
      bytes(0x1b, 0x5b, 0x4d, 32 + 3, 32 + 11, 32 + 6),
    ),
    row("urxvt press", Vector(mouse(MouseEventKind.down(MouseButton.Left), 10, 5)), esc("[32;11;6M")),
    row("a zero coordinate is dropped", Vector.empty[Event], esc("[<0;0;5M")),
    row("focus in and out", Vector(Event.FocusGained, Event.FocusLost), esc("[I"), esc("[O")),
    row("a paste in one chunk", Vector(Event.paste("hello")), esc("[200~hello" + EscText + "[201~")),
    row("a paste split across chunks", Vector(Event.paste("hello")), esc("[200~he"), text("l"), text("lo" + EscText + "[20"), text("1~")),
    row(
      "a paste with a partial terminator inside keeps it",
      Vector(Event.paste("a" + EscText + "[20b")),
      esc("[200~a" + EscText + "[20b" + EscText + "[201~"),
    ),
    row("an OSC reply is consumed", Vector(char('a')), esc("]11;rgb:0000/0000/0000\u0007"), text("a")),
    row("a DCS reply terminated by ST is consumed", Vector(char('a')), esc("P1+r524742=1" + EscText + "\\"), text("a")),
    row("a DA1 reply is consumed", Vector.empty[Event], esc("[?62;22c")),
    row("a DECRPM reply is consumed", Vector.empty[Event], esc("[?2026;2$y")),
    row("a kitty key report decodes in the legacy state", Vector(char('a')), esc("[97u")),
    row("a charset designation is consumed", Vector(char('a')), esc("(B"), text("a")),
    row("a 1 200-byte control sequence is consumed", Vector(char('a')), esc("[" + "1;" * 600 + "A"), text("a")),
    row("an ESC-terminated OSC followed by [A decodes as Up", Vector(key(KeyCode.Up)), esc("]0;title" + EscText + "[A")),
    example(
      "the state returns to ground after an over-long sequence",
      Assertions.eqv(Decoder.step(DecoderState.initial, esc("[" + "1;" * 600 + "A")).state, DecoderState.initial),
    ),
    example("awaiting after ESC", Result.assert(Decoder.step(DecoderState.initial, esc("")).state.awaiting)),
    example("not awaiting inside a paste", Result.assert(!Decoder.step(DecoderState.initial, esc("[200~he")).state.awaiting)),
    replyRow(
      "a DA1 reply is a primary device attributes report",
      Vector(Reply.PrimaryDeviceAttributes(Vector(62, 22))),
      esc("[?62;22c"),
    ),
    replyRow(
      "a DA2 reply is a secondary device attributes report",
      Vector(Reply.SecondaryDeviceAttributes(Vector(1, 10, 0))),
      esc("[>1;10;0c"),
    ),
    replyRow("a DECRPM reply reports the mode", Vector(Reply.PrivateModeReport(2026, 2)), esc("[?2026;2$y")),
    replyRow(
      "a valid XTGETTCAP reply decodes its hex entries",
      Vector(Reply.TermcapReply(true, Vector(Reply.TermcapEntry("RGB", Option("8"))))),
      esc("P1+r524742=38" + EscText + "\\"),
    ),
    replyRow(
      "an invalid XTGETTCAP reply keeps the names it names (the iTerm2 shape)",
      Vector(Reply.TermcapReply(false, Vector(Reply.TermcapEntry("Tc", Option.empty[String])))),
      esc("P0+r5463" + EscText + "\\"),
    ),
    replyRow(
      "a bare valid name has no value (the Ghostty shape)",
      Vector(Reply.TermcapReply(true, Vector(Reply.TermcapEntry("Tc", Option.empty[String])))),
      esc("P1+r5463" + EscText + "\\"),
    ),
    replyRow("an XTVERSION reply carries the text", Vector(Reply.VersionReply("fake 1.0")), esc("P>|fake 1.0" + EscText + "\\")),
    replyRow(
      "a DCS reply split across chunks still parses",
      Vector(Reply.TermcapReply(true, Vector(Reply.TermcapEntry("RGB", Option("8"))))),
      esc("P1+r5247"),
      text("42=38"),
      text(EscText + "\\"),
    ),
    example("a CPR is a reply while expecting and F3 with modifiers otherwise", testCpr),
    kittyRow("k1: CSI 27u is Escape", k1, Vector(key(KeyCode.Escape)), esc("[27u")),
    kittyRow("k1: CSI 99;5u is Control+c", k1, Vector(char('c', KeyModifier.Control)), esc("[99;5u")),
    kittyRow("k1: CSI 120;3u is Alt+x", k1, Vector(char('x', KeyModifier.Alt)), esc("[120;3u")),
    kittyRow("k1: CSI 13u is Enter", k1, Vector(key(KeyCode.Enter)), esc("[13u")),
    kittyRow("k1: CSI 9;2u is Shift+BackTab", k1, Vector(key(KeyCode.BackTab, KeyModifier.Shift)), esc("[9;2u")),
    kittyRow("k1: CSI 127;5u is Control+Backspace", k1, Vector(key(KeyCode.Backspace, KeyModifier.Control)), esc("[127;5u")),
    kittyRow("k1: CSI 1;9A is Super+Up", k1, Vector(key(KeyCode.Up, KeyModifier.Super)), esc("[1;9A")),
    row("legacy: CSI 1;9A is Meta+Up", Vector(key(KeyCode.Up, KeyModifier.Meta)), esc("[1;9A")),
    kittyRow("k1: CSI 1;33A is Meta+Up", k1, Vector(key(KeyCode.Up, KeyModifier.Meta)), esc("[1;33A")),
    kittyRow("k1: CSI 1;65A (Caps Lock) is Up", k1, Vector(key(KeyCode.Up)), esc("[1;65A")),
    kittyRow("k1: CSI 57399u (KP_0) is 0", k1, Vector(char('0')), esc("[57399u")),
    kittyRow("k1: CSI 57414u (KP_ENTER) is Enter", k1, Vector(key(KeyCode.Enter)), esc("[57414u")),
    kittyRow("k1: CSI 57427u is Keypad Begin", k1, Vector(key(KeyCode.KeypadBegin)), esc("[57427u")),
    kittyRow("k1: CSI E is Keypad Begin", k1, Vector(key(KeyCode.KeypadBegin)), esc("[E")),
    kittyRow("k1: CSI 1;2E is Shift+Keypad Begin", k1, Vector(key(KeyCode.KeypadBegin, KeyModifier.Shift)), esc("[1;2E")),
    kittyRow("k1: SS3 E is Keypad Begin", k1, Vector(key(KeyCode.KeypadBegin)), esc("OE")),
    kittyRow("k1: CSI 57427~ is Keypad Begin", k1, Vector(key(KeyCode.KeypadBegin)), esc("[57427~")),
    kittyRow("k1: CSI 57376u is F13", k1, Vector(key(KeyCode.f(13))), esc("[57376u")),
    kittyRow("k1: CSI 57398u is F35", k1, Vector(key(KeyCode.f(35))), esc("[57398u")),
    kittyRow("k1: CSI 57428u is Play", k1, Vector(key(KeyCode.media(MediaKey.Play))), esc("[57428u")),
    kittyRow(
      "k1: CSI 57441;2u is Left Shift with Shift",
      k1,
      Vector(key(KeyCode.modifier(ModifierKey.LeftShift), KeyModifier.Shift)),
      esc("[57441;2u"),
    ),
    kittyRow("k1: CSI 12615u is a jamo", k1, Vector(char('ㅇ')), esc("[12615u")),
    kittyRow("k1: CSI 128512u is two surrogate chars", k1, Vector(char('\ud83d'), char('\ude00')), esc("[128512u")),
    kittyRow("k1: CSI 65u gains Shift", k1, Vector(char('A', KeyModifier.Shift)), esc("[65u")),
    kittyRow("k1: a release without EventTypes is dropped", k1, Vector.empty[Event], esc("[97;2:3u")),
    kittyRow("k1: a repeat without EventTypes is a press", k1, Vector(char('a')), esc("[97;1:2u")),
    kittyRow("k1: control, zero, and unknown PUA codes are dropped", k1, Vector.empty[Event], esc("[1u"), esc("[0u"), esc("[60000u")),
    kittyRow("k1: a late cursor report is dropped", k1, Vector.empty[Event], esc("[24;80R")),
    kittyRow("k1: a lone ESC stalls without Escape", k1, Vector.empty[Event], esc(""), DecoderInput.Tick),
    kittyRow("k1: ESC [ stalls without Alt+[", k1, Vector.empty[Event], esc("["), DecoderInput.Tick),
    kittyRow("k1: ESC [ 2 7 stalls without keys", k1, Vector.empty[Event], esc("[27"), DecoderInput.Tick),
    kittyRow("k1: a truncated UTF-8 lead still ticks to U+FFFD", k1, Vector(char(Replacement)), bytes(0xc3), DecoderInput.Tick),
    kittyRow("k3: Escape release", k3, Vector(release(KeyCode.Escape)), esc("[27;1:3u")),
    kittyRow("k3: CSI 1;1:1A is an Up press", k3, Vector(key(KeyCode.Up)), esc("[1;1:1A")),
    kittyRow("k3: CSI 1;1:3A is an Up release", k3, Vector(release(KeyCode.Up)), esc("[1;1:3A")),
    kittyRow("k3: a release", k3, Vector(release(KeyCode.char('a'))), esc("[97;1:3u")),
    kittyRow("k3: Shift+a release is A with Shift", k3, Vector(release(KeyCode.char('A'), KeyModifier.Shift)), esc("[97;2:3u")),
    kittyRow("k3: Caps Lock a release is A with Shift", k3, Vector(release(KeyCode.char('A'), KeyModifier.Shift)), esc("[97;65:3u")),
    kittyRow("k3: Shift and Caps Lock a release is a", k3, Vector(release(KeyCode.char('a'))), esc("[97;66:3u")),
    kittyRow("k3: Delete release", k3, Vector(release(KeyCode.Delete)), esc("[3;1:3~")),
    kittyRow("k3: F3 release", k3, Vector(release(KeyCode.f(3))), esc("[13;1:3~")),
    kittyRow("k3: the iTerm2 Option+x release", k3, Vector(release(KeyCode.char('≈'))), esc("[8776;1:3u")),
    kittyRow("k3: the Ghostty Option+x release", k3, Vector(release(KeyCode.char('x'), KeyModifier.Alt)), esc("[120;3:3u")),
    kittyRow("k3: an input-method jamo release", k3, Vector(release(KeyCode.char('ㅇ'))), esc("[12615;1:3u")),
    kittyRow("k7: the shifted key names the release", k7, Vector(release(KeyCode.char('$'))), esc("[52:36;2:3u")),
    kittyRow("k7: the shifted key with Control", k7, Vector(char('$', KeyModifier.Control)), esc("[52:36;6u")),
    kittyRow("k27: a with its text", k27, Vector(char('a')), esc("[97;;97u")),
    kittyRow("k27: Shift+a with its text", k27, Vector(char('A', KeyModifier.Shift)), esc("[97;2;65u")),
    kittyRow("k27: Option+x text is the character without Alt", k27, Vector(char('≈')), esc("[120;3;8776u")),
    kittyRow("k27: a commit on o is its text", k27, Vector(char('私'), char('は')), esc("[111;;31169:12399u")),
    kittyRow("k27: a commit on Enter is its text, no Enter", k27, Vector(char('歌'), char('で')), esc("[13;;27468:12391u")),
    kittyRow("k27: a Hangul commit", k27, Vector(char('안')), esc("[12596;;50504u")),
    kittyRow("k27: astral text is two surrogate chars", k27, Vector(char('\ud83d'), char('\ude00')), esc("[13;;128512u")),
    kittyRow("k27: a pure text event", k27, Vector(char('å')), esc("[0;;229u")),
    kittyRow("k27: a control code point in text is skipped", k27, Vector(char('å')), esc("[13;;1:229u")),
    row("legacy: CSI 99;5u is Control+c", Vector(char('c', KeyModifier.Control)), esc("[99;5u")),
    row("legacy: a kitty release is dropped", Vector.empty[Event], esc("[97;1:3u")),
    row("legacy: a legacy-form release is dropped", Vector.empty[Event], esc("[1;5:3A")),
    replyRow("the kitty flags answer is a reply", Vector(Reply.KeyboardFlags(1)), esc("[?1u")),
    replyRow("an empty kitty flags answer is 0", Vector(Reply.KeyboardFlags(0)), esc("[?u")),
    replyRow("a kitty flags answer of 27", Vector(Reply.KeyboardFlags(27)), esc("[?27u")),
    example(
      "the kitty modifier parameter",
      Result.all(
        List(
          Assertions.eqv(Decoder.kittyModifiersOf(1), KeyModifiers.empty),
          Assertions.eqv(Decoder.kittyModifiersOf(9), KeyModifiers(KeyModifier.Super)),
          Assertions.eqv(Decoder.kittyModifiersOf(17), KeyModifiers(KeyModifier.Hyper)),
          Assertions.eqv(Decoder.kittyModifiersOf(33), KeyModifiers(KeyModifier.Meta)),
          Assertions.eqv(Decoder.kittyModifiersOf(65), KeyModifiers.empty),
          Assertions.eqv(
            Decoder.kittyModifiersOf(256),
            KeyModifiers(KeyModifier.Shift, KeyModifier.Alt, KeyModifier.Control, KeyModifier.Super, KeyModifier.Hyper, KeyModifier.Meta),
          ),
        )
      ),
    ),
    example("an out-of-range CPR is dropped while expecting", testCprOutOfRange),
    example(
      "the modifier parameter",
      Result.all(
        List(
          Assertions.eqv(Decoder.modifiersOf(1), KeyModifiers.empty),
          Assertions.eqv(Decoder.modifiersOf(16), KeyModifiers(KeyModifier.Shift, KeyModifier.Alt, KeyModifier.Control, KeyModifier.Meta)),
        )
      ),
    ),
  )

  def testCpr: Result = {
    val expecting = Decoder.step(DecoderState.initial.expecting(true), DecoderInput.bytesOf(EscText + "[24;80R"))
    val normal    = Decoder.step(DecoderState.initial, DecoderInput.bytesOf(EscText + "[24;80R"))
    Result.all(
      List(
        Assertions.eqv(expecting.replies, Vector(Reply.CursorPosition(Position(NonNegInt(79), NonNegInt(23))))),
        Assertions.eqv(expecting.events, Vector.empty[Event]),
        Assertions.eqv(normal.replies, Vector.empty[Reply]),
        Assertions.eqv(normal.events, Vector(Event.key(KeyEvent(KeyCode.f(3), Decoder.modifiersOf(80), KeyEventKind.Press)))),
      )
    )
  }

  def testCprOutOfRange: Result = {
    val zero = Decoder.step(DecoderState.initial.expecting(true), DecoderInput.bytesOf(EscText + "[0;5R"))
    Result.all(List(Assertions.eqv(zero.replies, Vector.empty[Reply]), Assertions.eqv(zero.events, Vector.empty[Event])))
  }

}
