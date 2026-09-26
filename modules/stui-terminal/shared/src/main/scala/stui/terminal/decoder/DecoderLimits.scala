package stui.terminal.decoder

/** The bounds of the decoder's buffers (design doc 7.5, decision D17): Stui's own constants, so that no input can grow the state without
  * limit. No specification sets a maximum for any of these sequences. The control-sequence bound was measured in M3d (2026-09-26): a
  * kitty key report carrying associated text costs about 6 parameter bytes per five-digit code point (CJK and Hangul), the longest
  * report observed (an input-method commit of 14 code points in iTerm2 3.7.3) was 87 bytes, and 1 024 bytes hold about 170 such code
  * points, while SGR mouse reports stay under 40 bytes and XTGETTCAP replies under a few hundred. Overflow never corrupts the state: a
  * control sequence over the limit is consumed and dropped, a string sequence is counted and only a DCS body is buffered, and the bytes
  * of a paste beyond the limit are dropped while the terminator is still recognised.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderLimits {

  /** The longest control sequence (the bytes between `CSI` and the final byte) that is decoded, 128 until M3d. */
  val MaxControlSequence: Int = 1024

  /** The longest string sequence (OSC, DCS, APC, PM, SOS) counted before the counter stops, and the size up to which a DCS body is
    * buffered for the probe replies (XTGETTCAP, XTVERSION).
    */
  val MaxStringSequence: Int = 4096

  /** The most paste bytes kept, one mebibyte. */
  val MaxPaste: Int = 1048576

  /** The most bytes of an unfinished UTF-8 sequence kept. */
  val MaxUtf8Pending: Int = 4

  /** The largest mouse coordinate accepted, larger reports are dropped (fact 12 of the comparison report). */
  val MaxCoordinate: Int = 1000000

}
