package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier, KeyModifiers}
import stui.core.geometry.Size
import stui.core.spi.{EscTimeout, Probing, TerminalOptions}
import stui.terminal.decoder.{Decoder, DecoderGens, DecoderInput, DecoderState, Encoder, Reply}
import stui.terminal.kitty.KittyFlags
import stui.terminal.probe.ProbeResult
import stui.terminal.probe.ProbeResult.*
import stui.testkit.{Assertions, ManualClock, ManualScheduler, SlowLink}
import stui.testkit.ManualClock.*
import stui.testkit.SlowLink.{Arrival, Timeline}
import stui.testkit.SlowLink.Timeline.*
import stui.testkit.gen.{Gens, SlowLinkGens}

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The blocking event source and the probe under ssh-like timing through the slow link (design doc 7.5 and 12, M3c): the split arrow
  * key under both ESC timeouts, the probe over a split reply inside and past its deadline, and the timing laws - gaps below the
  * timeout leave the events equal to the unsplit decoding, a gap above it after a lone ESC yields Escape first, and the blocking and
  * the push source decode the same timeline identically, and under the kitty disambiguate flag (M3d) the split Escape report, the
  * stalled lone ESC, and the two sources agreeing over kitty scripts.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SlowLinkDecodingSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val esc: String = 0x1b.toChar.toString

  private val local: FiniteDuration = EscTimeout.localDefault

  private def key(code: KeyCode): Event = Event.key(KeyEvent.press(code))

  private def chunk(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes(StandardCharsets.UTF_8))

  private def arrival(at: FiniteDuration, text: String): Arrival = Arrival(at, chunk(text))

  /** An uppercase letter decodes with the Shift modifier (the decoder's convention). */
  private val shiftedA: Event = Event.key(KeyEvent.pressWith(KeyCode.Char('A'), KeyModifiers.of(List(KeyModifier.Shift))))

  /** The split arrow key: a lone ESC, then `[A` 150 ms later. */
  private val splitArrow: Timeline = Timeline.of(arrival(Duration.Zero, esc), arrival(150.millis, "[A"))

  private def sourceIn(
    state: DecoderState,
    player: SlowLink.PullPlayer,
    clock: ManualClock,
    escTimeout: FiniteDuration,
  ): DecodingEventSource =
    new DecodingEventSource(
      (timeout: FiniteDuration) => player.poll(timeout),
      new AtomicBoolean(false),
      () => sized(80, 24).some,
      (size: Size) => size,
      escTimeout,
      clock,
      state,
      Vector.empty[Event],
    )

  @tailrec
  private def drain(events: DecodingEventSource, clock: ManualClock, until: FiniteDuration, acc: Vector[Event]): Vector[Event] =
    events.poll(50.millis) match {
      case Some(event) => drain(events, clock, until, acc :+ event)
      case None => if (clock.now >= until) acc else drain(events, clock, until, acc)
    }

  private def blockingEvents(timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] =
    blockingEventsIn(DecoderState.initial, timeline, escTimeout)

  private def blockingEventsIn(state: DecoderState, timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] = {
    val clock = ManualClock.of(0.millis)
    drain(sourceIn(state, SlowLink.pull(timeline, clock), clock, escTimeout), clock, timeline.end + escTimeout * 2L, Vector.empty[Event])
  }

  private def pushEvents(timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] =
    pushEventsIn(DecoderState.initial, timeline, escTimeout)

  private def pushEventsIn(state: DecoderState, timeline: Timeline, escTimeout: FiniteDuration): Vector[Event] = {
    val scheduler = ManualScheduler.of(0.millis)
    val events    =
      new PushEventSource((size: Size) => size, escTimeout, Schedules.of(scheduler), state, Vector.empty[Event], none[Size])
    val seen      = new AtomicReference(Vector.empty[Event])
    events.subscribe(event => seen.updateAndGet(_ :+ event): Unit): Unit
    SlowLink.push(timeline, scheduler, events.onChunk): Unit
    scheduler.advance(timeline.end + escTimeout * 2L)
    seen.get()
  }

  private def unsplit(bytes: IArray[Byte]): Vector[Event] =
    Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)).events

  /** The reply of `ProbeRunSpec` cut at its reply boundaries. */
  private val replyChunks: Vector[String] =
    Vector(esc + "[?2026;2$y", esc + "P1+r524742=38" + esc + "\\", esc + "[14;1R" + esc + "[?62;22c")

  private def probeOver(timeline: Timeline, deadline: FiniteDuration): (ProbeResult, FiniteDuration) = {
    val clock  = ManualClock.of(0.millis)
    val player = SlowLink.pull(timeline, clock)
    val probe  = Probe.run(
      FakeTty.of(sized(80, 24)),
      (timeout: FiniteDuration) => player.poll(timeout),
      TerminalOptions.alternateScreen.withProbing(Probing.fixed(deadline)),
      Capabilities.conservative,
      clock,
    )
    (probe, clock.now)
  }

  /** One to five encodable events as one byte script. */
  private val script: Gen[IArray[Byte]] =
    DecoderGens
      .encodable
      .list(Range.linear(1, 5))
      .map(events => IArray.from(events.flatMap(event => Encoder.encode(event).fold(Vector.empty[Byte])(_.toVector))))

  override def tests: List[Test] = List(
    example("an arrow key split 150 ms after its ESC stays one key under the ssh timeout", testSshTimeout),
    example("the same split under the local timeout is Escape, a bracket, and a shifted A", testLocalTimeout),
    example("the probe reaches its sentinel over a reply split 100 ms apart", testProbeSplit),
    example("the probe fails open when the last chunk arrives after the deadline", testProbeLate),
    property("gaps below the ESC timeout leave the events equal to the unsplit decoding on the blocking source", testBelowTimeout),
    property("a gap above the timeout after a lone ESC yields Escape first", testEscapeFirst),
    property("the blocking and the push source decode the same timeline identically", testBothSources),
    example("under flag 1 an Escape report split 150 ms after its ESC stays one Escape", testKittySplitEscape),
    example("under flag 1 a lone ESC followed by a 250 ms later gives only a", testKittyStall),
    property("under flag 1 the blocking and the push source decode kitty scripts identically", testKittyBothSources),
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
      blockingEventsIn(k1, Timeline.of(arrival(Duration.Zero, esc), arrival(150.millis, "[27u")), stall),
      Vector(key(KeyCode.Escape)),
    )

  def testKittyStall: Result =
    Assertions.eqv(
      blockingEventsIn(k1, Timeline.of(arrival(Duration.Zero, esc), arrival(250.millis, "a")), stall),
      Vector(key(KeyCode.Char('a'))),
    )

  def testKittyBothSources: Property =
    for {
      bytes    <- kittyScript.forAll
      timeline <- SlowLinkGens.timeline(bytes, 250.millis).forAll
    } yield Assertions.eqv(blockingEventsIn(k1, timeline, stall), pushEventsIn(k1, timeline, stall))

  def testSshTimeout: Result = Assertions.eqv(blockingEvents(splitArrow, EscTimeout.sshDefault), Vector(key(KeyCode.Up)))

  def testLocalTimeout: Result =
    Assertions.eqv(blockingEvents(splitArrow, local), Vector(key(KeyCode.Escape), key(KeyCode.Char('[')), shiftedA))

  def testProbeSplit: Result = {
    val (probe, now) = probeOver(Timeline.spaced(100.millis, replyChunks.map(chunk)*), 2.seconds)
    Result.all(
      List(
        Result.assert(probe.sentinelSeen).log("sentinel"),
        Assertions.eqv(probe.syncOutputAnswer, true.some),
        Assertions.eqv(now, 200.millis),
      )
    )
  }

  def testProbeLate: Result = {
    val timeline     = Timeline.of(
      arrival(Duration.Zero, replyChunks.headOption.getOrElse("")),
      arrival(100.millis, replyChunks.lift(1).getOrElse("")),
      arrival(3.seconds, replyChunks.lift(2).getOrElse("")),
    )
    val (probe, now) = probeOver(timeline, 2.seconds)
    Result.all(
      List(
        Result.assert(!probe.sentinelSeen).log("no sentinel"),
        Result.assert(probe.replies.contains(Reply.PrivateModeReport(2026, 2))).log("the early reply kept"),
        Assertions.eqv(now, 2.seconds),
      )
    )
  }

  def testBelowTimeout: Property =
    for {
      bytes    <- script.forAll
      timeline <- SlowLinkGens.timeline(bytes, local - 1.millis).forAll
    } yield Assertions.eqv(blockingEvents(timeline, local), unsplit(bytes))

  def testEscapeFirst: Property =
    for {
      event <- DecoderGens.encodable.forAll
      gap   <- Gens.millis(Range.linear(1, 100)).forAll
    } yield {
      val rest     = Encoder.encode(event).getOrElse(IArray.empty[Byte])
      val timeline = Timeline.of(arrival(Duration.Zero, esc), Arrival(local + gap, rest))
      Assertions.eqv(blockingEvents(timeline, local).headOption, key(KeyCode.Escape).some)
    }

  def testBothSources: Property =
    for {
      bytes    <- script.forAll
      timeline <- SlowLinkGens.timeline(bytes, 120.millis).forAll
    } yield Assertions.eqv(blockingEvents(timeline, local), pushEvents(timeline, local))

}
