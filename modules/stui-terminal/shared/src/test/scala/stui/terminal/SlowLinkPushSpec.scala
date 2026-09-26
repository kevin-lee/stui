package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier, KeyModifiers}
import stui.core.geometry.Size
import stui.core.spi.EscTimeout
import stui.terminal.decoder.{Decoder, DecoderGens, DecoderInput, DecoderState, Encoder}
import stui.terminal.kitty.KittyFlags
import stui.testkit.{Assertions, ManualScheduler, SlowLink}
import stui.testkit.SlowLink.{Arrival, Timeline}
import stui.testkit.SlowLink.Timeline.*
import stui.testkit.gen.SlowLinkGens

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The push event source under ssh-like timing through the slow link (design doc 7.5 and 12, M3c), on every platform: the split arrow
  * key under both ESC timeouts and the gaps-below-the-timeout law, and under the kitty disambiguate flag (M3d) the split Escape report
  * kept whole, a stalled lone ESC dropped, and the same law over kitty scripts with the 200 ms stall timeout.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SlowLinkPushSpec extends Properties {

  private val esc: String = 0x1b.toChar.toString

  private val local: FiniteDuration = EscTimeout.localDefault

  private def key(code: KeyCode): Event = Event.key(KeyEvent.press(code))

  private def chunk(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes(StandardCharsets.UTF_8))

  private def arrival(at: FiniteDuration, text: String): Arrival = Arrival(at, chunk(text))

  /** An uppercase letter decodes with the Shift modifier (the decoder's convention). */
  private val shiftedA: Event = Event.key(KeyEvent.pressWith(KeyCode.Char('A'), KeyModifiers.of(List(KeyModifier.Shift))))

  private val splitArrow: Timeline = Timeline.of(arrival(Duration.Zero, esc), arrival(150.millis, "[A"))

  private def pushEvents(timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] =
    pushEventsIn(DecoderState.initial, timeline, escTimeout)

  private def pushEventsIn(state: DecoderState, timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] = {
    val scheduler = ManualScheduler.of(0.millis)
    val events    =
      new PushEventSource(identity, escTimeout, Schedules.of(scheduler), state, Vector.empty[Event], none[Size])
    val seen      = new AtomicReference(Vector.empty[Event])
    events.subscribe(event => seen.updateAndGet(_ :+ event): Unit): Unit
    SlowLink.push(timeline, scheduler, events.onChunk): Unit
    scheduler.advance(timeline.end + escTimeout * 2L)
    seen.get()
  }

  private def unsplit(bytes: IArray[Byte]): Vector[Event] =
    Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)).events

  private val script: Gen[IArray[Byte]] =
    DecoderGens
      .encodable
      .list(Range.linear(1, 5))
      .map(events => IArray.from(events.flatMap(event => Encoder.encode(event).fold(Vector.empty[Byte])(_.toVector))))

  override def tests: List[Test] = List(
    example("an arrow key split 150 ms after its ESC stays one key under the ssh timeout", testSshTimeout),
    example("the same split under the local timeout is Escape, a bracket, and a shifted A", testLocalTimeout),
    property("gaps below the ESC timeout leave the events equal to the unsplit decoding on the push source", testBelowTimeout),
    example("under flag 1 an Escape report split 150 ms after its ESC stays one Escape", testKittySplitEscape),
    example("under flag 1 a lone ESC followed by a 250 ms later gives only a", testKittyStall),
    property("under flag 1 gaps below the stall timeout leave kitty scripts equal to the unsplit decoding", testKittyBelowTimeout),
  )

  private val k1: DecoderState = DecoderState.initial.withKeyboard(KittyFlags.Disambiguate)

  /** The stall timeout under flag 1 (`KittyKeyboard.escTimeout` gives the ssh default there). */
  private val stall: FiniteDuration = EscTimeout.sshDefault

  private val kittyScript: Gen[IArray[Byte]] =
    DecoderGens
      .kittyEncodable(KittyFlags.Disambiguate)
      .list(Range.linear(1, 5))
      .map(events =>
        IArray.from(events.flatMap(event => Encoder.encodeKitty(KittyFlags.Disambiguate, event).fold(Vector.empty[Byte])(_.toVector)))
      )

  def testKittySplitEscape: Result =
    Assertions.eqv(
      pushEventsIn(k1, Timeline.of(arrival(Duration.Zero, esc), arrival(150.millis, "[27u")), stall),
      Vector(key(KeyCode.Escape)),
    )

  def testKittyStall: Result =
    Assertions.eqv(
      pushEventsIn(k1, Timeline.of(arrival(Duration.Zero, esc), arrival(250.millis, "a")), stall),
      Vector(key(KeyCode.Char('a'))),
    )

  def testKittyBelowTimeout: Property =
    for {
      bytes    <- kittyScript.forAll
      timeline <- SlowLinkGens.timeline(bytes, stall - 1.millis).forAll
    } yield Assertions.eqv(
      pushEventsIn(k1, timeline, stall),
      Decoder.stepAll(k1, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)).events,
    )

  def testSshTimeout: Result = Assertions.eqv(pushEvents(splitArrow, EscTimeout.sshDefault), Vector(key(KeyCode.Up)))

  def testLocalTimeout: Result =
    Assertions.eqv(pushEvents(splitArrow, local), Vector(key(KeyCode.Escape), key(KeyCode.Char('[')), shiftedA))

  def testBelowTimeout: Property =
    for {
      bytes    <- script.forAll
      timeline <- SlowLinkGens.timeline(bytes, local - 1.millis).forAll
    } yield Assertions.eqv(pushEvents(timeline, local), unsplit(bytes))

}
