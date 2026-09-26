package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Position

/** A terminal answer decoded from the input stream (design doc 7.3 and 7.5): not input, and never delivered as an `Event`. The probe
  * collects replies until the Primary Device Attributes sentinel; outside a probe window a late reply is dropped by the event source.
  *
  *   - `CursorPosition`: the Cursor Position Report `CSI Pl ; Pc R`, stored 0-based. Decoded only while `DecoderState.expectingReplies`
  *     holds, because the same bytes are F3 with modifiers otherwise (plan refinement R5).
  *   - `PrivateModeReport`: DECRPM `CSI ? Pd ; Ps $ y` (0 not recognised, 1 set, 2 reset, 3 permanently set, 4 permanently reset).
  *   - `TermcapReply`: XTGETTCAP `DCS 1 + r Pt ST` (valid) or `DCS 0 + r Pt ST` (invalid), names and values hex-decoded two digits per
  *     character (iTerm2 lists the unknown names in its `0 + r` reply, Ghostty answers a bare name with no value, both observed in the
  *     M1f spike).
  *   - `VersionReply`: XTVERSION `DCS > | text ST`.
  *   - `PrimaryDeviceAttributes`: DA1 `CSI ? Ps ; ... c`, the probe's sentinel.
  *   - `SecondaryDeviceAttributes`: DA2 `CSI > Pp ; Pv ; Pc c`, recorded and unused in M1f.
  *   - `KeyboardFlags`: the kitty keyboard protocol's answer `CSI ? flags u` to the query `CSI ? u` (M3d), recognised always, the flags
  *     currently set on the answering screen. A DA1 answer without it means the terminal does not support the protocol.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum Reply derives Eq, Show, Hash {
  case CursorPosition(position: Position)
  case PrivateModeReport(mode: Int, value: Int)
  case TermcapReply(valid: Boolean, entries: Vector[Reply.TermcapEntry])
  case VersionReply(text: String)
  case PrimaryDeviceAttributes(parameters: Vector[Int])
  case SecondaryDeviceAttributes(parameters: Vector[Int])
  case KeyboardFlags(flags: Int)
}

object Reply {

  /** One termcap name from an XTGETTCAP reply, with its value when the terminal sent one. */
  final case class TermcapEntry(name: String, value: Option[String]) derives Eq, Show, Hash

}
