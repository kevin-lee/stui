package stui.core.spi

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.geometry.{Position, Size}
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.AtomicReference

/** The SPI traits exercised through in-memory test doubles.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object SpiSpec extends Properties {

  /** Records every call as a `String`. */
  final class RecordingBackend extends TerminalBackend {
    private val calls: AtomicReference[Vector[String]]   = new AtomicReference(Vector.empty[String])
    private def record(call: String): Unit               = calls.updateAndGet(_ :+ call): Unit
    def recorded: Vector[String]                         = calls.get()
    override def size(): Size                            = Size(NonNegInt(80), NonNegInt(24))
    override def draw(updates: Vector[CellUpdate]): Unit = record(s"draw(${updates.length.toString})")
    override def flush(): Unit                           = record("flush")
    override def moveCursor(position: Position): Unit    = record(s"moveCursor(${position.x.value.toString},${position.y.value.toString})")
    override def showCursor(): Unit                      = record("showCursor")
    override def hideCursor(): Unit                      = record("hideCursor")
    override def clear(): Unit                           = record("clear")
    override def enter(options: TerminalOptions): Unit   = record(s"enter(${options.features.size.toString})")
    override def exit(): Unit                            = record("exit")
  }

  /** Listeners are invoked synchronously by `publish`. */
  final class ManualEventSource extends EventSource {
    private val listeners: AtomicReference[Vector[(Int, Event => Unit)]] = new AtomicReference(Vector.empty[(Int, Event => Unit)])
    private val ids: AtomicReference[Int]                                = new AtomicReference(0)
    override def subscribe(listener: Event => Unit): Subscription        = {
      val id = ids.getAndUpdate(_ + 1)
      listeners.updateAndGet(_ :+ (id -> listener)): Unit
      () => listeners.updateAndGet(_.filterNot { case (listenerId, _) => listenerId === id }): Unit
    }
    def publish(event: Event): Unit                                      = listeners.get().foreach { case (_, listener) => listener(event) }
  }

  override def tests: List[Test] = List(
    example("a backend receives the diff of two buffers", testDraw),
    example("subscribe then publish delivers, cancel stops delivery, cancelling twice is harmless", testSubscribe),
    example("TerminalOptions.of enables exactly the given features", testOptions),
  )

  def testDraw: Result = {
    val backend = new RecordingBackend
    backend.enter(TerminalOptions.of(TerminalFeature.AlternateScreen))
    backend.draw(Buffer.diff(Buffer.fromLines(Vector("hello")), Buffer.fromLines(Vector("hallo"))))
    backend.flush()
    backend.exit()
    Assertions.eqv(backend.recorded, Vector("enter(1)", "draw(1)", "flush", "exit"))
  }

  def testSubscribe: Result = {
    val source       = new ManualEventSource
    val received     = new AtomicReference(Vector.empty[Event])
    val subscription = source.subscribe(event => received.updateAndGet(_ :+ event): Unit)
    val enter        = Event.key(KeyEvent.press(KeyCode.Enter))
    source.publish(enter)
    subscription.cancel()
    subscription.cancel()
    source.publish(Event.FocusLost)
    Assertions.eqv(received.get(), Vector(enter))
  }

  def testOptions: Result = {
    val options = TerminalOptions.of(TerminalFeature.MouseCapture, TerminalFeature.BracketedPaste)
    Result.all(
      List(
        Result.assert(options.enabled(TerminalFeature.MouseCapture)),
        Result.assert(options.enabled(TerminalFeature.BracketedPaste)),
        Result.assert(!options.enabled(TerminalFeature.AlternateScreen)),
        Result.assert(!TerminalOptions.none.enabled(TerminalFeature.AlternateScreen)),
      )
    )
  }

}
