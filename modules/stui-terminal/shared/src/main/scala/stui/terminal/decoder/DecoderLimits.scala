package stui.terminal.decoder

/** The bounds of the decoder's buffers (design doc 7.5, decision D17): Stui's own constants, chosen above every sequence a terminal is
  * specified to send (kitty key reports and SGR mouse reports stay under 40 bytes, XTGETTCAP replies under a few hundred), so that no
  * input can grow the state without limit. Overflow never corrupts the state: a control sequence over the limit is consumed and dropped,
  * a string sequence is counted but never buffered in M1e, and the bytes of a paste beyond the limit are dropped while the terminator is
  * still recognised.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderLimits {

  /** The longest control sequence (the bytes between `CSI` and the final byte) that is decoded. */
  val MaxControlSequence: Int = 128

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
