package stui.terminal.decoder

import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.event.Event
import stui.terminal.kitty.KittyFlags
import stui.testkit.gen.EventGens
import stui.unicode.internal.IntOps.*

/** Generators for the decoder laws: random byte chunks, inputs with ticks, chunk splits, encodable events, printable text, and the
  * kitty keyboard protocol's flags, states, events, and text (M3d).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderGens {

  /** Random bytes, 0 to 40 of them. */
  val chunk: Gen[IArray[Byte]] = Gen.int(Range.linear(0, 255)).list(Range.linear(0, 40)).map(bytes => IArray.from(bytes.map(_.toByte)))

  /** Mostly byte chunks, sometimes a tick. */
  val input: Gen[DecoderInput] = Gen.frequency1(8 -> chunk.map(DecoderInput.bytes), 1 -> Gen.constant(DecoderInput.Tick))

  /** Input sequences of 0 to 12 inputs. */
  val inputs: Gen[Vector[DecoderInput]] = input.list(Range.linear(0, 12)).map(_.toVector)

  /** The bytes cut into consecutive chunks at random points. */
  def splits(bytes: IArray[Byte]): Gen[Vector[IArray[Byte]]] =
    Gen.int(Range.linear(0, bytes.length)).list(Range.linear(0, 4)).map { cuts =>
      val points = (0 +: cuts.sorted.distinct.toVector :+ bytes.length).distinct
      points.zip(points.drop(1)).map { case (from, until) => IArray.from(bytes.toVector.slice(from, until)) }
    }

  /** Events with a canonical byte sequence. */
  val encodable: Gen[Event] = EventGens.event(NonNegInt(200)).filter(event => Encoder.encode(event).isDefined)

  /** Printable text of BMP scalars (no controls, no DEL, no surrogates), 0 to 20 characters. */
  val printableText: Gen[String] =
    Gen.string(Gen.unicode.filter(c => c >= ' ' && (c.toInt !== 0x7f) && !Character.isSurrogate(c)), Range.linear(0, 20))

  /** The flag sets the laws push: disambiguate, with event types, with alternate keys, and the full report set. */
  val kittyFlags: Gen[KittyFlags] = Gen.element1(1, 3, 7, 27).map(KittyFlags.fromInt)

  /** The initial state with no flags or one of [[kittyFlags]] pushed. */
  val kittyState: Gen[DecoderState] =
    Gen.frequency1(1 -> Gen.constant(KittyFlags.none), 4 -> kittyFlags).map(DecoderState.initial.withKeyboard)

  /** Events with a canonical kitty form under the flags (plan refinement D7: the kitty encoder is nearly total, so few are discarded). */
  def kittyEncodable(flags: KittyFlags): Gen[Event] =
    EventGens.event(NonNegInt(200)).filter(event => Encoder.encodeKitty(flags, event).isDefined)

  /** Non-empty printable text, the associated text of a kitty report. */
  val reportText: Gen[String] = printableText.filter(_.nonEmpty)

  /** A lowercase ASCII letter. */
  val letter: Gen[Char] = Gen.char('a', 'z')

}
