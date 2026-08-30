package stui.terminal.probe

import stui.terminal.ansi.Sequences

/** The startup probe's query batch (design doc 7.3, decision D15), one round trip: DECRQM for mode 2026, XTGETTCAP for the truecolour
  * capabilities, XTVERSION for the terminal's identity, DA2 (recorded, unused in M1f), the Cursor Position Report (the inline entry
  * anchor, harmless on the alternate screen), and DA1 last as the sentinel that ends the probe. A terminal ignores every query it
  * does not know, and every reply lands in the input stream for the decoder.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ProbeQueries {

  /** The termcap names asked for: either one in a valid answer means truecolour (xterm ctlseqs, terminfo(5)). */
  val TruecolorNames: List[String] = List("RGB", "Tc")

  /** DECRQM for the synchronised-output mode, answered by DECRPM (the synchronized-output specification). */
  val Decrqm2026: String = Sequences.Csi + "?2026$p"

  /** XTVERSION, answered as `DCS > | text ST` (xterm ctlseqs). */
  val Xtversion: String = Sequences.Csi + ">0q"

  /** DA2, answered as `CSI > Pp ; Pv ; Pc c` (xterm ctlseqs). */
  val Da2: String = Sequences.Csi + ">c"

  /** The Cursor Position Report request, answered as `CSI Pl ; Pc R`. */
  val CursorReport: String = Sequences.Csi + "6n"

  /** DA1, the sentinel: every xterm-compatible terminal answers `CSI ? ... c`. */
  val Da1: String = Sequences.Csi + "c"

  /** XTGETTCAP with the names hex-encoded two digits per character, joined with `;` (xterm ctlseqs). */
  def xtgettcap(names: List[String]): String =
    Sequences.Esc + "P+q" + names.map(hex).mkString(";") + Sequences.Esc + "\\"

  /** The whole batch in the order sent, the sentinel last. */
  val batch: String = Decrqm2026 + xtgettcap(TruecolorNames) + Xtversion + Da2 + CursorReport + Da1

  private def hex(name: String): String =
    name.map(c => f"${c.toInt}%02x").mkString

}
