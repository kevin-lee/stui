package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.terminal.kitty.KittyFlags

/** The whole decoder state (design doc 7.5): the parser mode, the bytes of an unfinished UTF-8 sequence with the number still expected,
  * whether the next completed key carries Alt (an ESC preceded a multi-byte character), whether terminal replies are expected (set
  * by the probe loop, plan refinement R5 of M1f: only the Cursor Position Report needs the flag, because `CSI Pl ; Pc R` is F3 with
  * modifiers otherwise), and the kitty keyboard flags Stui pushed (M3d, decision D30), which drive the kind filter, the meaning of
  * the modifier parameter in the legacy forms, and the stall rule of a tick.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class DecoderState(
  mode: DecoderMode,
  utf8: Vector[Byte],
  utf8Need: Int,
  altPending: Boolean,
  expectingReplies: Boolean,
  keyboard: KittyFlags,
) derives Eq,
      Show,
      Hash

object DecoderState {

  /** Ground, nothing pending, no replies expected, no kitty flags pushed. */
  val initial: DecoderState = DecoderState(DecoderMode.Ground, Vector.empty[Byte], 0, false, false, KittyFlags.none)

  extension (state: DecoderState) {

    /** The state with the reply expectation replaced (the probe loop sets it, the event source starts with it cleared). */
    def expecting(flag: Boolean): DecoderState = state.copy(expectingReplies = flag)

    /** The state with the pushed kitty keyboard flags replaced (the platform sets them from `KittyKeyboard.flagsFor`, M3d). */
    def withKeyboard(flags: KittyFlags): DecoderState = state.copy(keyboard = flags)

    /** True when a clock tick would change the state: a lone ESC, a partial sequence, or an unfinished UTF-8 character is pending.
      * The event source arms the ESC timeout when this holds.
      */
    def awaiting: Boolean =
      state.utf8Need > 0 || (state.mode match {
        case DecoderMode.Ground | DecoderMode.Paste(_, _) => false
        case DecoderMode.Escape | DecoderMode.EscapeIntermediate(_) | DecoderMode.Csi(_, _) | DecoderMode.Ss3 |
            DecoderMode.StringSeq(_, _, _, _) | DecoderMode.X10Mouse(_) =>
          true
      })

    /** The bytes the state holds, bounded by `DecoderLimits.MaxPaste + MaxControlSequence + MaxStringSequence + MaxUtf8Pending`. */
    def bufferedBytes: Int =
      state.utf8.length + (state.mode match {
        case DecoderMode.Ground => 0
        case DecoderMode.Escape => 1
        case DecoderMode.EscapeIntermediate(bytes) => 1 + bytes.length
        case DecoderMode.Csi(bytes, _) => 2 + bytes.length
        case DecoderMode.Ss3 => 2
        case DecoderMode.StringSeq(_, _, _, bytes) => 2 + bytes.length
        case DecoderMode.X10Mouse(bytes) => 3 + bytes.length
        case DecoderMode.Paste(body, matched) => body.length + matched
      })

  }

}
