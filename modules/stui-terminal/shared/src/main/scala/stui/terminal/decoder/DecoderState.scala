package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The whole decoder state (design doc 7.5): the parser mode, the bytes of an unfinished UTF-8 sequence with the number still expected,
  * and whether the next completed key carries Alt (an ESC preceded a multi-byte character).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class DecoderState(mode: DecoderMode, utf8: Vector[Byte], utf8Need: Int, altPending: Boolean) derives Eq, Show, Hash

object DecoderState {

  /** Ground, nothing pending. */
  val initial: DecoderState = DecoderState(DecoderMode.Ground, Vector.empty[Byte], 0, false)

  extension (state: DecoderState) {

    /** True when a clock tick would change the state: a lone ESC, a partial sequence, or an unfinished UTF-8 character is pending.
      * The event source arms the ESC timeout when this holds.
      */
    def awaiting: Boolean =
      state.utf8Need > 0 || (state.mode match {
        case DecoderMode.Ground | DecoderMode.Paste(_, _) => false
        case DecoderMode.Escape | DecoderMode.EscapeIntermediate(_) | DecoderMode.Csi(_, _) | DecoderMode.Ss3 |
            DecoderMode.StringSeq(_, _) | DecoderMode.X10Mouse(_) =>
          true
      })

    /** The bytes the state holds, bounded by `DecoderLimits.MaxPaste + MaxControlSequence + MaxUtf8Pending`. */
    def bufferedBytes: Int =
      state.utf8.length + (state.mode match {
        case DecoderMode.Ground => 0
        case DecoderMode.Escape => 1
        case DecoderMode.EscapeIntermediate(bytes) => 1 + bytes.length
        case DecoderMode.Csi(bytes, _) => 2 + bytes.length
        case DecoderMode.Ss3 => 2
        case DecoderMode.StringSeq(_, _) => 0
        case DecoderMode.X10Mouse(bytes) => 3 + bytes.length
        case DecoderMode.Paste(body, matched) => body.length + matched
      })

  }

}
