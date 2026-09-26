package stui.terminal.ansi

import stui.core.spi.{TerminalFeature, TerminalOptions}
import stui.terminal.kitty.KittyFlags

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

  /** Cursor Horizontal Absolute to the 1-based `column` of the current row: the join break of a printed row (rule R2a) and the
    * placements of a printed VS16 cluster (rule R3a).
    *
    * @since 2026-08-31
    */
  def cha(column: Int): String = s"$Csi${column.toString}G"

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

  /** Carriage return alone. */
  val Cr: String = "\r"

  /** Line feed alone: at the bottom margin of the scroll region (or of the screen without one) it scrolls up by one line (IND
    * semantics, the VT510 manual).
    */
  val Lf: String = "\n"

  /** Save Cursor (DECSC), `ESC 7`: cursor position, SGR attributes, character sets, the wrap flag, and origin mode (VT510 manual). */
  val SaveCursor: String = Esc + "7"

  /** Restore Cursor (DECRC), `ESC 8`: restores what DECSC saved, or homes with defaults when nothing was saved (VT510 manual). */
  val RestoreCursor: String = Esc + "8"

  /** Set Top and Bottom Margins (DECSTBM), 1-based inclusive rows, `top` strictly below `bottom`. DECSTBM homes the cursor, so the
    * writer only emits it inside [[armRegion]] (rule R7).
    */
  def scrollRegion(top: Int, bottom: Int): String = s"$Csi${top.toString};${bottom.toString}r"

  /** DECSTBM with no parameters: the full screen, and the cursor homed. */
  val ScrollRegionReset: String = Csi + "r"

  /** The region set with the cursor preserved (rule R7): DECSC, DECSTBM, DECRC. */
  def armRegion(top: Int, bottom: Int): String = SaveCursor + scrollRegion(top, bottom) + RestoreCursor

  /** The region reset with the cursor preserved (rule R7). */
  val resetRegion: String = SaveCursor + ScrollRegionReset + RestoreCursor

  /** Begin Synchronized Update, mode 2026 set (the synchronized-output specification). */
  val SyncBegin: String = Csi + "?2026h"

  /** End Synchronized Update, mode 2026 reset. */
  val SyncEnd: String = Csi + "?2026l"

  /** Erase in Display from the cursor to the end of the screen (ED 0), the overlay print's viewport erase. */
  val EraseBelow: String = Csi + "J"

  /** Erase in Line from the cursor to the end of the row (EL 0), emitted after every printed row so a short row leaves no residue. */
  val EraseToLineEnd: String = Csi + "K"

  /** The fixed part of the reset every exit path emits, crash and signal paths included (rule R6): every optional mode off in reverse
    * order, the style reset, the cursor shown, and the normal screen restored. Disabling a mode that was never enabled is harmless, so
    * this part serves every feature set. A kitty keyboard pop is not harmless without its push (it removes another program's entry),
    * so the exits add [[kittyPop]] of exactly what was pushed (M3d).
    */
  val SafeReset: String = FocusDisable + BracketedPasteDisable + MouseTrackingDisable + SgrReset + CursorShow + AlternateScreenExit

  /** The kitty keyboard push `CSI > flags u` (the kitty keyboard protocol specification, M3d), nothing for no flags. */
  def kittyPush(flags: KittyFlags): String = if (flags.isEmpty) "" else Csi + ">" + flags.bits.toString + "u"

  /** The kitty keyboard pop `CSI < u` of one pushed entry, nothing when no flags were pushed. */
  def kittyPop(flags: KittyFlags): String = if (flags.isEmpty) "" else Csi + "<u"

  /** The entry sequence for the options: the alternate screen, an explicit clear and home (fact 13 of the comparison report: a
    * terminal may not clear on mode 1049), the cursor hidden, the enabled features in order (mouse with the hygiene reset first,
    * bracketed paste, focus), then the kitty keyboard push of `flags` (after mode 1049, because the main and alternate screens keep
    * separate stacks, M3d).
    */
  def enter(options: TerminalOptions, flags: KittyFlags): String =
    AlternateScreenEnter + ClearScreen + CursorHome + CursorHide + features(options) + kittyPush(flags)

  /** The feature part of an entry sequence, in order: mouse (with the hygiene reset first), bracketed paste, focus.
    * `KeyReleaseEvents` is not a mode: it selects the kitty keyboard flags (`KittyKeyboard.flagsFor`, M3d).
    */
  def features(options: TerminalOptions): String = {
    val mouse = if (options.enabled(TerminalFeature.MouseCapture)) MouseHygieneReset + MouseTrackingEnable else ""
    val paste = if (options.enabled(TerminalFeature.BracketedPaste)) BracketedPasteEnable else ""
    val focus = if (options.enabled(TerminalFeature.FocusEvents)) FocusEnable else ""
    mouse + paste + focus
  }

}
