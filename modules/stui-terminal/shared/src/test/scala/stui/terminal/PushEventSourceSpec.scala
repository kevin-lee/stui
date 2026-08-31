package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.event.{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}
import stui.core.geometry.Size
import stui.core.internal.NonNegInts
import stui.terminal.decoder.DecoderState
import stui.testkit.Assertions

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The push-based event source (design doc 6.3 and 7.5): delivery order, the buffered pre-subscription events, the scheduled ESC
  * timeout ticks, the resize deduplication with the D12 effective size, and shutdown. The scheduler is a manual recorder, so the
  * tick timing is deterministic.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object PushEventSourceSpec extends Properties {

  /** One recorded schedule call: the delay, the action, and whether the cancel was invoked. */
  final private case class Scheduled(delay: FiniteDuration, action: () => Unit, cancelled: AtomicReference[Boolean])

  final private class ManualSchedule {
    val calls: AtomicReference[Vector[Scheduled]] = new AtomicReference(Vector.empty[Scheduled])

    val schedule: PushEventSource.Schedule = (delay, action) => {
      val cancelled = new AtomicReference(false)
      calls.updateAndGet(_ :+ Scheduled(delay, action, cancelled)): Unit
      () => cancelled.set(true)
    }
  }

  private val timeout: FiniteDuration = 50.millis

  private def sourceWith(
    schedule: PushEventSource.Schedule,
    viewportSize: Size => Size,
    initialEvents: Vector[Event],
    initialSize: Option[Size],
  ): PushEventSource =
    new PushEventSource(
      viewportSize,
      timeout,
      schedule,
      DecoderState.initial,
      initialEvents,
      initialSize,
    )

  private def source(schedule: PushEventSource.Schedule): PushEventSource =
    sourceWith(schedule, identity, Vector.empty[Event], none[Size])

  private def recording(events: PushEventSource): AtomicReference[Vector[Event]] = {
    val seen = new AtomicReference(Vector.empty[Event])
    events.subscribe(event => seen.updateAndGet(_ :+ event): Unit): Unit
    seen
  }

  private def char(c: Char): Event = Event.key(KeyEvent(KeyCode.Char(c), KeyModifiers.empty, KeyEventKind.Press))

  private def chunk(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes("UTF-8"))

  private val escChunk: IArray[Byte] = IArray(0x1b.toByte)

  override def tests: List[Test] = List(
    example("a printable chunk delivers one Char event per character, in order", testPrintable),
    example("events from a chunk before any subscription are delivered at subscribe", testBufferedChunk),
    example("the initial events are delivered at the first subscription", testInitialEvents),
    example("a lone ESC arms the tick and the tick delivers Escape", testEscTick),
    example("a completion chunk cancels the armed tick and no Escape is delivered", testTickCancelled),
    example("a resize is deduplicated on the terminal size and delivered as the effective size", testResize),
    example("a failed or zero-sized report delivers nothing", testResizeNoise),
    example("unsubscribing stops delivery to that listener only", testUnsubscribe),
    example("shutdown cancels the armed tick and later chunks deliver nothing", testShutdown),
  )

  def testPrintable: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val seen   = recording(events)
    events.onChunk(chunk("ab"))
    Result.all(
      List(
        Assertions.eqv(seen.get(), Vector(char('a'), char('b'))),
        Assertions.eqv(manual.calls.get().size, 0),
      )
    )
  }

  def testBufferedChunk: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    events.onChunk(chunk("x"))
    val seen   = recording(events)
    Assertions.eqv(seen.get(), Vector(char('x')))
  }

  def testInitialEvents: Result = {
    val manual = new ManualSchedule
    val events = sourceWith(manual.schedule, identity, Vector(char('k')), none[Size])
    val seen   = recording(events)
    Assertions.eqv(seen.get(), Vector(char('k')))
  }

  def testEscTick: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val seen   = recording(events)
    events.onChunk(escChunk)
    val armed  = manual.calls.get()
    armed.foreach(call => call.action())
    Result.all(
      List(
        Assertions.eqv(armed.size, 1),
        Assertions.eqv(armed.map(_.delay), Vector(timeout)),
        Assertions.eqv(
          seen.get(),
          Vector(Event.key(KeyEvent(KeyCode.Escape, KeyModifiers.empty, KeyEventKind.Press))),
        ),
      )
    )
  }

  def testTickCancelled: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val seen   = recording(events)
    events.onChunk(escChunk)
    events.onChunk(chunk("[A"))
    Result.all(
      List(
        Assertions.eqv(manual.calls.get().map(_.cancelled.get()), Vector(true)),
        Assertions.eqv(seen.get(), Vector(Event.key(KeyEvent(KeyCode.Up, KeyModifiers.empty, KeyEventKind.Press)))),
      )
    )
  }

  def testResize: Result = {
    val manual   = new ManualSchedule
    val clampTo9 = (size: Size) => Size(size.width, NonNegInts.min(NonNegInt(9), size.height))
    val events   = sourceWith(manual.schedule, clampTo9, Vector.empty[Event], Size(NonNegInt(80), NonNegInt(24)).some)
    val seen     = recording(events)
    events.onResize(Size(NonNegInt(80), NonNegInt(24)).some)
    events.onResize(Size(NonNegInt(90), NonNegInt(30)).some)
    events.onResize(Size(NonNegInt(90), NonNegInt(30)).some)
    Assertions.eqv(seen.get(), Vector(Event.resize(Size(NonNegInt(90), NonNegInt(9)))))
  }

  def testResizeNoise: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val seen   = recording(events)
    events.onResize(none[Size])
    events.onResize(Size(NonNegInt(0), NonNegInt(24)).some)
    Assertions.eqv(seen.get(), Vector.empty[Event])
  }

  def testUnsubscribe: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val first  = new AtomicReference(Vector.empty[Event])
    val second = new AtomicReference(Vector.empty[Event])
    val handle = events.subscribe(event => first.updateAndGet(_ :+ event): Unit)
    events.subscribe(event => second.updateAndGet(_ :+ event): Unit): Unit
    events.onChunk(chunk("a"))
    handle.cancel()
    events.onChunk(chunk("b"))
    Result.all(
      List(
        Assertions.eqv(first.get(), Vector(char('a'))),
        Assertions.eqv(second.get(), Vector(char('a'), char('b'))),
      )
    )
  }

  def testShutdown: Result = {
    val manual = new ManualSchedule
    val events = source(manual.schedule)
    val seen   = recording(events)
    events.onChunk(escChunk)
    events.shutdown()
    events.onChunk(chunk("q"))
    Result.all(
      List(
        Assertions.eqv(manual.calls.get().map(_.cancelled.get()), Vector(true)),
        Assertions.eqv(seen.get(), Vector.empty[Event]),
      )
    )
  }

}
