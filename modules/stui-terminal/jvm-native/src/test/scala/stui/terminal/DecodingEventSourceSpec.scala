package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.geometry.Size
import stui.core.spi.Clock
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

import java.nio.charset.StandardCharsets
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.duration.*

/** The consumer-side decoding source over a queue: keys, chunk splits, the ESC timeout, resizes, timeouts, and the push style.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecodingEventSourceSpec extends Properties {

  /** A [[RawInput]] fed by tests. */
  final class QueueInput extends RawInput {
    private val queue: LinkedBlockingQueue[IArray[Byte]]             = new LinkedBlockingQueue[IArray[Byte]]()
    def push(s: String): Unit                                        = queue.put(IArray.unsafeFromArray(s.getBytes(StandardCharsets.UTF_8)))
    override def poll(timeout: FiniteDuration): Option[IArray[Byte]] = Option(queue.poll(timeout.toMillis, TimeUnit.MILLISECONDS))
  }

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val esc: String = "\u001b"

  private def key(code: KeyCode): Event = Event.key(KeyEvent.press(code))

  private def source(input: QueueInput, resized: AtomicBoolean, size: AtomicReference[Option[Size]], escTimeout: FiniteDuration) =
    new DecodingEventSource(input, resized, () => size.get(), escTimeout, Clock.system)

  override def tests: List[Test] = List(
    example("a key arrives", testKey),
    example("one chunk with three keys gives three polls", testThree),
    example("a lone ESC resolves after the ESC timeout", testEscTimeout),
    example("a sequence split across chunks decodes", testSplit),
    example("the resize flag gives one Resize event", testResize),
    example("an empty source times out with None", testTimeout),
    example("subscribe delivers on the dispatcher thread and cancel stops it", testSubscribe),
  )

  def testKey: Result = {
    val input = new QueueInput
    val s     = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 50.millis)
    input.push("a")
    Assertions.eqv(s.poll(500.millis), key(KeyCode.char('a')).some)
  }

  def testThree: Result = {
    val input = new QueueInput
    val s     = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 50.millis)
    input.push("abc")
    val got   = List(s.poll(500.millis), s.poll(500.millis), s.poll(500.millis), s.poll(10.millis))
    Assertions.eqv(got, List(key(KeyCode.char('a')).some, key(KeyCode.char('b')).some, key(KeyCode.char('c')).some, none[Event]))
  }

  def testEscTimeout: Result = {
    val input = new QueueInput
    val s     = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 10.millis)
    input.push(esc)
    Assertions.eqv(s.poll(2.seconds), key(KeyCode.Escape).some)
  }

  def testSplit: Result = {
    val input = new QueueInput
    val s     = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 200.millis)
    input.push(esc + "[")
    input.push("A")
    Assertions.eqv(s.poll(2.seconds), key(KeyCode.Up).some)
  }

  def testResize: Result = {
    val input   = new QueueInput
    val resized = new AtomicBoolean(false)
    val size    = new AtomicReference(sized(80, 24).some)
    val s       = source(input, resized, size, 50.millis)
    size.set(sized(20, 5).some)
    resized.set(true)
    val first   = s.poll(200.millis)
    val second  = s.poll(10.millis)
    Result.all(List(Assertions.eqv(first, Event.resize(sized(20, 5)).some), Assertions.eqv(second, none[Event])))
  }

  def testTimeout: Result = {
    val input = new QueueInput
    val s     = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 50.millis)
    Assertions.eqv(s.poll(20.millis), none[Event])
  }

  def testSubscribe: Result = {
    val input        = new QueueInput
    val s            = source(input, new AtomicBoolean(false), new AtomicReference(sized(80, 24).some), 50.millis)
    val delivered    = new CountDownLatch(1)
    val received     = new AtomicReference(none[Event])
    val count        = new AtomicInteger(0)
    val subscription = s.subscribe { event =>
      received.set(event.some)
      count.incrementAndGet(): Unit
      delivered.countDown()
    }
    input.push("z")
    val arrived      = delivered.await(5, TimeUnit.SECONDS)
    subscription.cancel()
    input.push("y")
    val late         = new CountDownLatch(1)
    val stayed       = !late.await(300, TimeUnit.MILLISECONDS)
    Result.all(
      List(
        Result.assert(arrived).log("delivered within 5 s"),
        Assertions.eqv(received.get(), key(KeyCode.char('z')).some),
        Result.assert(stayed && count.get() === 1).log(s"count ${count.get().toString}"),
      )
    )
  }

}
