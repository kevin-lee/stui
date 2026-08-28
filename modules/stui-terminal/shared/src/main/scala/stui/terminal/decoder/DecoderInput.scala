package stui.terminal.decoder

import java.nio.charset.StandardCharsets

/** What the decoder consumes: a chunk of raw bytes, or a clock tick meaning the ESC timeout elapsed with no further byte (design doc
  * 7.5, plan refinement R5). Timing is data, the event source decides when a tick is due.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum DecoderInput {
  case Bytes(chunk: IArray[Byte])
  case Tick
}

object DecoderInput {

  /** A chunk of raw bytes. */
  def bytes(chunk: IArray[Byte]): DecoderInput = Bytes(chunk)

  /** The UTF-8 bytes of the string, for tests and fixtures. */
  def bytesOf(s: String): DecoderInput = Bytes(IArray.unsafeFromArray(s.getBytes(StandardCharsets.UTF_8)))

  /** The bytes given as ints (0 to 255), for fixtures. */
  def bytesOfInts(values: Int*): DecoderInput = Bytes(IArray.from(values.map(_.toByte)))

}
