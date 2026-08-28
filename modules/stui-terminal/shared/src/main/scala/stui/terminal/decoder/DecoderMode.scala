package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Where the decoder is in a sequence, following the DEC ANSI parser reference (Paul Flo Williams): ground, after ESC, ESC with
  * intermediates, inside a control sequence (its bytes so far, and whether it overflowed and is only consumed), after `ESC O` (SS3),
  * inside a string sequence (OSC, DCS, APC, PM, SOS: whether an ESC was just seen, and the length counted), collecting the three bytes of
  * an X10 mouse report, or inside a bracketed paste (the body so far and how many bytes of the terminator matched).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum DecoderMode derives Eq, Show, Hash {
  case Ground
  case Escape
  case EscapeIntermediate(bytes: Vector[Byte])
  case Csi(bytes: Vector[Byte], ignoring: Boolean)
  case Ss3
  case StringSeq(escPending: Boolean, length: Int)
  case X10Mouse(bytes: Vector[Byte])
  case Paste(body: Vector[Byte], matched: Int)
}
