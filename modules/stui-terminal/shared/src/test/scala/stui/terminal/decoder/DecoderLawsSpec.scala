package stui.terminal.decoder

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

/** The decoder laws (design doc 7.5 and 12): the round trip through the encoder, chunk-split invariance, determinism, the state-size
  * bound, fuzzing, ticks, and printable text.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderLawsSpec extends Properties {

  private val bound: Int = DecoderLimits.MaxPaste + DecoderLimits.MaxControlSequence + DecoderLimits.MaxUtf8Pending

  override def tests: List[Test] = List(
    property("an encoded event decodes back to itself", testRoundTrip),
    property("splitting the bytes into chunks does not change the events", testSplits),
    property("splitting with a trailing tick does not change the events", testSplitsWithTick),
    property("decoding is deterministic", testDeterministic),
    property("the state stays bounded", testBounded),
    property("any input sequence decodes without failure and keeps every coordinate in range", testFuzz),
    property("a tick leaves nothing awaiting outside a paste", testTick),
    property("printable text decodes to one Char per character", testText),
  )

  def testRoundTrip: Property =
    DecoderGens.encodable.forAll.map { event =>
      Encoder.encode(event) match {
        case Some(bytes) =>
          Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)) match {
            case (_, events) => Assertions.eqv(events, Vector(event))
          }
        case None => Result.failure.log("not encodable")
      }
    }

  def testSplits: Property =
    for {
      bytes  <- DecoderGens.chunk.forAll
      chunks <- DecoderGens.splits(bytes).forAll
    } yield Assertions.eqv(
      Decoder.stepAll(DecoderState.initial, chunks.map(DecoderInput.bytes)),
      Decoder.step(DecoderState.initial, DecoderInput.bytes(bytes)),
    )

  def testSplitsWithTick: Property =
    for {
      bytes  <- DecoderGens.chunk.forAll
      chunks <- DecoderGens.splits(bytes).forAll
    } yield Assertions.eqv(
      Decoder.stepAll(DecoderState.initial, chunks.map(DecoderInput.bytes) :+ DecoderInput.Tick),
      Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)),
    )

  def testDeterministic: Property =
    DecoderGens
      .inputs
      .forAll
      .map(inputs => Assertions.eqv(Decoder.stepAll(DecoderState.initial, inputs), Decoder.stepAll(DecoderState.initial, inputs)))

  def testBounded: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      val state = Decoder.stepAll(DecoderState.initial, inputs)._1
      Result.assert(state.bufferedBytes <= bound).log(s"buffered ${state.bufferedBytes.toString}")
    }

  def testFuzz: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      Decoder.stepAll(DecoderState.initial, inputs) match {
        case (_, events) =>
          Result.all(events.toList.map {
            case Event.Mouse(mouse) =>
              Result.assert(mouse.position.x.value < DecoderLimits.MaxCoordinate && mouse.position.y.value < DecoderLimits.MaxCoordinate)
            case Event.Key(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => Result.success
          })
      }
    }

  def testTick: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      val before = Decoder.stepAll(DecoderState.initial, inputs)._1
      val after  = Decoder.step(before, DecoderInput.Tick)._1
      after.mode match {
        case DecoderMode.Paste(_, _) => Result.success
        case DecoderMode.Ground | DecoderMode.Escape | DecoderMode.EscapeIntermediate(_) | DecoderMode.Csi(_, _) | DecoderMode.Ss3 |
            DecoderMode.StringSeq(_, _) | DecoderMode.X10Mouse(_) =>
          Result.assert(!after.awaiting).log(after.show)
      }
    }

  def testText: Property =
    DecoderGens.printableText.forAll.map { text =>
      val events = Decoder.step(DecoderState.initial, DecoderInput.bytesOf(text))._2
      val chars  = events.map {
        case Event.Key(KeyEvent(KeyCode.Char(c), _, _)) => c.toInt.some
        case Event.Key(_) | Event.Mouse(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => none[Int]
      }
      Result.all(
        List(
          Result.assert(events.length === text.length).log(s"${events.length.toString} events for ${text.length.toString} chars"),
          Assertions.eqv(chars, text.toVector.map(c => c.toInt.some)),
        )
      )
    }

}
