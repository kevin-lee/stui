package stui.terminal.ansi

import stui.core.spi.{TerminalFeature, TerminalOptions}

/** The fixed control sequences the writer owns (design doc 7.1 and principle 9), in xterm ctlseqs notation. Everything the terminal
  * receives is either one of these or a glyph from a cell.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Sequences {

  /** ESC. */
  val Esc: String = "\u001b"

  /** The Control Sequence Introducer, `ESC [`. */
  val Csi: String = Esc + "["

  /** Cursor Position, `CSI row ; column H`, both 1-based. */
  def cup(row: Int, column: Int): String = s"$Csi${row.toString};${column.toString}H"

  /** `CSI 0 m`, every attribute and colour back to the default. */
  val SgrReset: String = Csi + "0m"

  /** DECTCEM set, the cursor becomes visible. */
  val CursorShow: String = Csi + "?25h"

  /** DECTCEM reset, the cursor becomes invisible. */
  val CursorHide: String = Csi + "?25l"

  /** Mode 1049: save the cursor and switch to the alternate screen. Some terminals do not clear it, so [[enter]] clears explicitly. */
  val AlternateScreenEnter: String = Csi + "?1049h"

  /** Mode 1049 reset: back to the normal screen with the saved cursor. */
  val AlternateScreenExit: String = Csi + "?1049l"

  /** Erase in Display, the whole screen. */
  val ClearScreen: String = Csi + "2J"

  /** Cursor Position without parameters, the home position. */
  val CursorHome: String = Csi + "H"

  /** Resets every mouse mode Stui never wants before enabling the wanted ones (xterm ctlseqs "Mouse Tracking"): all-motion tracking
    * 1003 (it floods slow links), the UTF-8 encoding 1005, the urxvt encoding 1015, and the pixel encoding 1016.
    */
  val MouseHygieneReset: String = Csi + "?1003l" + Csi + "?1005l" + Csi + "?1015l" + Csi + "?1016l"

  /** Button tracking 1000, button-motion tracking 1002, and the SGR encoding 1006. */
  val MouseTrackingEnable: String = Csi + "?1000h" + Csi + "?1002h" + Csi + "?1006h"

  /** [[MouseTrackingEnable]] undone in reverse order. */
  val MouseTrackingDisable: String = Csi + "?1006l" + Csi + "?1002l" + Csi + "?1000l"

  /** Bracketed paste on. */
  val BracketedPasteEnable: String = Csi + "?2004h"

  /** Bracketed paste off. */
  val BracketedPasteDisable: String = Csi + "?2004l"

  /** Focus reporting on. */
  val FocusEnable: String = Csi + "?1004h"

  /** Focus reporting off. */
  val FocusDisable: String = Csi + "?1004l"

  /** Carriage return and line feed, the row separator of printed rows. */
  val CrLf: String = "\r\n"

  /** The fixed reset every exit path emits, crash and signal paths included (rule R6): every optional mode off in reverse order, the
    * style reset, the cursor shown, and the normal screen restored. Disabling a mode that was never enabled is harmless, so one
    * sequence serves every feature set.
    */
  val SafeReset: String = FocusDisable + BracketedPasteDisable + MouseTrackingDisable + SgrReset + CursorShow + AlternateScreenExit

  /** The entry sequence for the options: the alternate screen, an explicit clear and home (fact 13 of the comparison report: a
    * terminal may not clear on mode 1049), the cursor hidden, then the enabled features in order (mouse with the hygiene reset first,
    * bracketed paste, focus). `KeyReleaseEvents` adds nothing until the kitty keyboard protocol (M3).
    */
  def enter(options: TerminalOptions): String = {
    val mouse = if (options.enabled(TerminalFeature.MouseCapture)) MouseHygieneReset + MouseTrackingEnable else ""
    val paste = if (options.enabled(TerminalFeature.BracketedPaste)) BracketedPasteEnable else ""
    val focus = if (options.enabled(TerminalFeature.FocusEvents)) FocusEnable else ""
    AlternateScreenEnter + ClearScreen + CursorHome + CursorHide + mouse + paste + focus
  }

}
