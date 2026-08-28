package stui.core.spi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.geometry.{Position, Size}
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

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
    override def flush(): NonNegInt                      = {
      record("flush")
      NonNegInt(0)
    }
    override def moveCursor(position: Position): Unit    = record(s"moveCursor(${position.x.value.toString},${position.y.value.toString})")
    override def showCursor(): Unit                      = record("showCursor")
    override def hideCursor(): Unit                      = record("hideCursor")
    override def clear(): Unit                           = record("clear")
    override def print(rows: Buffer): Unit               = record(s"print(${rows.area.height.value.toString})")
    override def enter(options: TerminalOptions): Either[TerminalError, Unit] = {
      record(s"enter(${options.features.size.toString})")
      ().asRight[TerminalError]
    }
    override def exit(): Unit                                                 = record("exit")
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
    example("ScreenMode.inlineFrom rejects 0 and accepts 5", testInlineFrom),
    example("EscTimeout resolves 50 ms locally, 200 ms over ssh, and a fixed value as given", testEscTimeout),
  )

  def testDraw: Result = {
    val backend = new RecordingBackend
    val entered = backend.enter(TerminalOptions.alternateScreen.withFeature(TerminalFeature.MouseCapture))
    backend.draw(Buffer.diff(Buffer.fromLines(Vector("hello")), Buffer.fromLines(Vector("hallo"))))
    val flushed = backend.flush()
    backend.print(Buffer.fromLines(Vector("a", "b")))
    backend.exit()
    Result.all(
      List(
        Assertions.eqv(entered, ().asRight[TerminalError]),
        Assertions.eqv(flushed, NonNegInt(0)),
        Assertions.eqv(backend.recorded, Vector("enter(1)", "draw(1)", "flush", "print(2)", "exit")),
      )
    )
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
    val options = TerminalOptions.of(ScreenMode.AlternateScreen, TerminalFeature.MouseCapture, TerminalFeature.BracketedPaste)
    Result.all(
      List(
        Result.assert(options.enabled(TerminalFeature.MouseCapture)),
        Result.assert(options.enabled(TerminalFeature.BracketedPaste)),
        Result.assert(!options.enabled(TerminalFeature.FocusEvents)),
        Result.assert(!options.withoutFeature(TerminalFeature.MouseCapture).enabled(TerminalFeature.MouseCapture)),
        Result.assert(options.withFeature(TerminalFeature.FocusEvents).enabled(TerminalFeature.FocusEvents)),
        Result.assert(!TerminalOptions.alternateScreen.enabled(TerminalFeature.MouseCapture)),
        Assertions.eqv(options.screenMode, ScreenMode.AlternateScreen),
        Assertions.eqv(options.escTimeout, EscTimeout.Automatic),
        Assertions.eqv(options.withEscTimeout(EscTimeout.fixed(1.second)).escTimeout, EscTimeout.fixed(1.second)),
      )
    )
  }

  def testInlineFrom: Result =
    Result.all(
      List(
        Result.assert(ScreenMode.inlineFrom(0).isLeft),
        Result.assert(ScreenMode.inlineFrom(-1).isLeft),
        Assertions.eqv(ScreenMode.inlineFrom(5), Right(ScreenMode.Inline(PosInt(5)))),
        Assertions.eqv(ScreenMode.inlineOf(PosInt(5)), ScreenMode.Inline(PosInt(5))),
      )
    )

  def testEscTimeout: Result =
    Result.all(
      List(
        Assertions.eqv(EscTimeout.Automatic.resolve(false), 50.milliseconds),
        Assertions.eqv(EscTimeout.Automatic.resolve(true), 200.milliseconds),
        Assertions.eqv(EscTimeout.fixed(1.second).resolve(true), 1.second),
        Assertions.eqv(EscTimeout.fixed(1.second).resolve(false), 1.second),
      )
    )

}
