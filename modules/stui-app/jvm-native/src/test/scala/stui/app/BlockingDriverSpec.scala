package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.app.internal.{BlockingDriver, QueueScheduler}
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.frame.Frame
import stui.core.geometry.Size
import stui.core.spi.{BlockingEventSource, ScreenMode, Subscription, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.testkit.{Assertions, BackendCall, ManualClock, TestBackend}
import stui.testkit.ManualClock.*
import stui.testkit.TestBackend.*

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.duration.*
import scala.util.Try

/** The blocking driver over the in-memory backend, a clock-advancing fake source, and the production scheduler on a manual clock
  * (design doc 10 and 12, M3b): one present per batch, the last resize, prints before the draw, drift-free ticks bounding the poll
  * timeout, the termination flag, tasks (a posted result presented without waiting for the poll, M3c), the render correction, and
  * the restore path.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object BlockingDriverSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(20, 5))

  /** A blocking source the test feeds by hand: an empty poll advances the clock by the timeout (the wait) and calls the hook. */
  final class FakeBlockingSource(clock: ManualClock, onEmptyPoll: () => Unit) extends BlockingEventSource {
    private val queue: ConcurrentLinkedQueue[Event]               = new ConcurrentLinkedQueue[Event]()
    private val timeouts: AtomicReference[Vector[FiniteDuration]] = new AtomicReference(Vector.empty[FiniteDuration])
    def push(events: Event*): Unit                                = events.foreach(event => queue.add(event): Unit)
    def polled: Vector[FiniteDuration]                            = timeouts.get()
    override def poll(timeout: FiniteDuration): Option[Event]     = {
      timeouts.updateAndGet(_ :+ timeout): Unit
      Option(queue.poll()) match {
        case Some(event) => event.some
        case None =>
          if (timeout > Duration.Zero) {
            clock.advance(timeout)
            onEmptyPoll()
          } else {
            ()
          }
          none[Event]
      }
    }
    override def subscribe(listener: Event => Unit): Subscription = () => ()
  }

  /** One run's parts, the hook receiving the setup so a test can push, fire the deferred callback, or flip the flag from a poll. */
  final class Setup(exitAfterTicks: Option[Int], onEmptyPoll: Setup => Unit) {
    val clock: ManualClock                                                = ManualClock.of(0.millis)
    val backend: TestBackend                                              = TestBackend.of(sized(20, 5))
    val deferred: AtomicReference[Option[Either[Throwable, Int] => Unit]] = new AtomicReference(none[Either[Throwable, Int] => Unit])
    val terminating: AtomicBoolean                                        = new AtomicBoolean(false)
    val empties: AtomicInteger                                            = new AtomicInteger(0)
    val source: FakeBlockingSource                                        = new FakeBlockingSource(
      clock,
      () => {
        empties.incrementAndGet(): Unit
        onEmptyPoll(this)
      },
    )

    def run(options: TerminalOptions): Either[TerminalError, CounterModel] =
      Terminal.run(backend, options, Capabilities.conservative, clock) { terminal =>
        BlockingDriver.run(
          CounterApp.of(deferred, exitAfterTicks),
          env,
          terminal,
          source,
          QueueScheduler.unwoken(clock),
          () => terminating.get(),
          100.millis,
          (_: Frame) => (),
        )
      }
  }

  private def setup(): Setup = new Setup(none[Int], _ => ())

  private def draws(backend: TestBackend): Int = backend.calls.count {
    case BackendCall.Draw(_) => true
    case _ => false
  }

  private def key(c: Char): Event = Event.key(KeyEvent.press(KeyCode.Char(c)))

  override def tests: List[Test] = List(
    example("the first present happens before the first poll", testFirstPresent),
    example("one batch presents once", testOneDrawPerBatch),
    example("the last Resize wins", testLastResize),
    example("prints precede the draw", testPrints),
    example("ticks fire drift-free with the poll timeout bounded by the tick", testTicks),
    example("the termination flag ends the loop without exit", testTerminating),
    example("a synchronous task posts into a later batch", testSyncTask),
    example("a deferred task completes from outside", testDeferredTask),
    example("the render correction reaches the model", testCorrection),
    example("an exception from update propagates after the restore", testCrash),
  )

  def testFirstPresent: Result = {
    val s      = setup()
    s.source.push(key('q'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(
      List(
        Assertions.eqv(result.map(_.exit), true.asRight[TerminalError]),
        Assertions.eqv(draws(s.backend), 2),
        Assertions.eqv(s.source.polled.length, 2),
      )
    )
  }

  def testOneDrawPerBatch: Result = {
    val s      = setup()
    s.source.push(key('+'), key('+'), key('+'), key('q'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(List(Assertions.eqv(result.map(_.count), 3.asRight[TerminalError]), Assertions.eqv(draws(s.backend), 2)))
  }

  def testLastResize: Result = {
    val s      = setup()
    s.source.push(Event.resize(sized(3, 3)), Event.resize(sized(9, 9)), key('q'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(List(Assertions.eqv(result.map(_.size), sized(9, 9).asRight[TerminalError]), Assertions.eqv(draws(s.backend), 2)))
  }

  def testPrints: Result = {
    val s        = setup()
    s.source.push(key('p'), key('q'))
    val result   = s.run(TerminalOptions.of(ScreenMode.inlineOf(PosInt(5))))
    val calls    = s.backend.calls
    val printAt  = calls.indexWhere {
      case BackendCall.Print(_) => true
      case _ => false
    }
    val lastDraw = calls.lastIndexWhere {
      case BackendCall.Draw(_) => true
      case _ => false
    }
    Result.all(
      List(
        Assertions.eqv(result.map(_.prints), 1.asRight[TerminalError]),
        Assertions.eqv(s.backend.printed.length, 1),
        Result.assert(printAt >= 0 && printAt < lastDraw).log(s"print at ${printAt.toString}, last draw at ${lastDraw.toString}"),
        Assertions.eqv(draws(s.backend), 2),
      )
    )
  }

  def testTicks: Result = {
    val s      = new Setup(3.some, _ => ())
    s.source.push(key('t'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(
      List(
        Assertions.eqv(result.map(_.ticks), Vector(10.millis, 20.millis, 30.millis).asRight[TerminalError]),
        Result
          .assert(s.source.polled.drop(1).forall(_ <= 10.millis))
          .log(s"polled = ${s.source.polled.map(_.toMillis.toString).mkString(",")}"),
        Assertions.eqv(draws(s.backend), 5),
      )
    )
  }

  def testTerminating: Result = {
    val s      = new Setup(none[Int], setup => setup.terminating.set(true))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(
      List(
        Assertions.eqv(result.map(_.exit), false.asRight[TerminalError]),
        Assertions.eqv(s.empties.get(), 1),
        Assertions.eqv(draws(s.backend), 1),
      )
    )
  }

  def testSyncTask: Result = {
    val s      = new Setup(none[Int], setup => if (setup.empties.get() === 1) setup.source.push(key('q')) else ())
    s.source.push(key('s'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(
      List(
        Assertions.eqv(result.map(_.count), 10.asRight[TerminalError]),
        Assertions.eqv(s.source.polled.lift(1), (Duration.Zero: FiniteDuration).some),
        Assertions.eqv(draws(s.backend), 4),
      )
    )
  }

  def testDeferredTask: Result = {
    val s      = new Setup(
      none[Int],
      setup =>
        setup.empties.get() match {
          case 1 => setup.deferred.get().foreach(callback => callback(5.asRight[Throwable]))
          case 2 => setup.source.push(key('q'))
          case _ => ()
        },
    )
    s.source.push(key('d'))
    val result = s.run(TerminalOptions.alternateScreen)
    Result.all(List(Assertions.eqv(result.map(_.count), 5.asRight[TerminalError]), Assertions.eqv(s.deferred.get().isDefined, true)))
  }

  def testCorrection: Result = {
    val s = setup()
    s.source.push(Vector.fill(150)(key('+')) :+ key('q')*)
    Assertions.eqv(s.run(TerminalOptions.alternateScreen).map(_.count), CounterApp.maxCount.asRight[TerminalError])
  }

  def testCrash: Result = {
    val s      = setup()
    s.source.push(key('!'))
    val result = Try(s.run(TerminalOptions.alternateScreen))
    Result.all(
      List(
        Result
          .assert(result.failed.toOption.exists(_.getMessage === "counter crash"))
          .log(s"result = ${result.fold(_.getMessage, _ => "no failure")}"),
        Assertions.eqv(s.backend.calls.lastOption, (BackendCall.Exit: BackendCall).some),
      )
    )
  }

}
