package stui.terminal.decoder

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.event.*
import stui.core.geometry.Position
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
    case (_, events) => events
  }

  private def bytes(values: Int*): DecoderInput = DecoderInput.bytesOfInts(values*)

  private def text(s: String): DecoderInput = DecoderInput.bytesOf(s)

  private def esc(s: String): DecoderInput = DecoderInput.bytesOf(EscText + s)

  private def key(code: KeyCode, modifiers: KeyModifier*): Event = Event.key(KeyEvent(code, KeyModifiers(modifiers*), KeyEventKind.Press))

  private def char(c: Char, modifiers: KeyModifier*): Event = key(KeyCode.char(c), modifiers*)

  private def mouse(kind: MouseEventKind, x: Int, y: Int, modifiers: KeyModifier*): Event =
    Event.mouse(MouseEvent(kind, Position(NonNegInt.unsafeFrom(x), NonNegInt.unsafeFrom(y)), KeyModifiers(modifiers*)))

  private def row(name: String, expected: Vector[Event], inputs: DecoderInput*): Test =
    example(name, Assertions.eqv(decode(inputs*), expected))

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
    row("a kitty key report is consumed", Vector.empty[Event], esc("[97u")),
    row("a charset designation is consumed", Vector(char('a')), esc("(B"), text("a")),
    row("a 300-byte control sequence is consumed", Vector(char('a')), esc("[" + "1;" * 150 + "A"), text("a")),
    row("an ESC-terminated OSC followed by [A decodes as Up", Vector(key(KeyCode.Up)), esc("]0;title" + EscText + "[A")),
    example(
      "the state returns to ground after an over-long sequence",
      Assertions.eqv(Decoder.step(DecoderState.initial, esc("[" + "1;" * 150 + "A"))._1, DecoderState.initial),
    ),
    example("awaiting after ESC", Result.assert(Decoder.step(DecoderState.initial, esc(""))._1.awaiting)),
    example("not awaiting inside a paste", Result.assert(!Decoder.step(DecoderState.initial, esc("[200~he"))._1.awaiting)),
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

}
