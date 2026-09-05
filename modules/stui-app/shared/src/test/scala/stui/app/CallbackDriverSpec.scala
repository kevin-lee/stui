package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.app.internal.CallbackDriver
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind}
import stui.core.geometry.{Position, Size}
import stui.core.frame.Frame
import stui.core.spi.{ScreenMode, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.testkit.{Assertions, BackendCall, ManualScheduler, TestBackend}
import stui.testkit.TestBackend.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*
import scala.util.Try

/** The callback driver over the in-memory backend, a push source fake, and the deterministic scheduler (design doc 10 and 12, M3b):
  * the first present, one present per batch, the last resize, prints before the draw, exit, ticks, tasks, the render correction, mouse
  * routing, the batch order rule, and the restore path.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object CallbackDriverSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(20, 5))

  final private case class Harness(
    backend: TestBackend,
    source: FakePushSource,
    scheduler: ManualScheduler,
    deferred: AtomicReference[Option[Either[Throwable, Int] => Unit]],
    exited: AtomicReference[Option[CounterModel]],
  )

  private def harness(): Harness =
    Harness(
      TestBackend.of(sized(20, 5)),
      new FakePushSource,
      ManualScheduler.of(0.millis),
      new AtomicReference(none[Either[Throwable, Int] => Unit]),
      new AtomicReference(none[CounterModel]),
    )

  private def draws(backend: TestBackend): Int = backend.calls.count {
    case BackendCall.Draw(_) => true
    case _ => false
  }

  private def key(c: Char): Event = Event.key(KeyEvent.press(KeyCode.Char(c)))

  /** Runs the driver under the terminal bracket on the alternate screen and then the body, which drives the source and the
    * scheduler.
    */
  private def withDriver[A](h: Harness)(body: => A): Either[TerminalError, A] = withDriverIn(TerminalOptions.alternateScreen, h)(body)

  /** [[withDriver]] under the given options (inline mode makes a print reach the backend at once, design doc 7.2). */
  private def withDriverIn[A](options: TerminalOptions, h: Harness)(body: => A): Either[TerminalError, A] =
    Terminal.run(h.backend, options, Capabilities.conservative, h.scheduler) { terminal =>
      CallbackDriver.start(
        CounterApp.of(h.deferred, none[Int]),
        env,
        terminal,
        h.source,
        h.scheduler,
        (_: Frame) => (),
        model => h.exited.set(model.some),
      )
      body
    }

  private def flush(h: Harness): Unit = h.scheduler.advance(Duration.Zero)

  private def quit(h: Harness): Option[CounterModel] = {
    h.source.emit(key('q'))
    flush(h)
    h.exited.get()
  }

  override def tests: List[Test] = List(
    example("the first present happens before any event", testFirstPresent),
    example("five events then one flush give exactly one more present and the screen shows the count", testOneDrawPerBatch),
    example("the last Resize in a batch wins", testLastResize),
    example("prints precede the batch's draw", testPrints),
    example("Exit ends the loop after a final present", testExit),
    example("a tick subscription fires and presents", testTicks),
    example("a synchronous task posts its result into the next batch", testSyncTask),
    example("a deferred task posts when it completes", testDeferredTask),
    example("a batch folds its events before its posted messages", testOrder),
    example("the render correction reaches the model", testCorrection),
    example("a mouse event is routed with the last frame's regions", testMouse),
    example("an exception from update propagates from the flush after the restore", testCrash),
  )

  def testFirstPresent: Result = {
    val h = harness()
    Assertions.eqv(withDriver(h)(draws(h.backend)), 1.asRight[TerminalError])
  }

  def testOneDrawPerBatch: Result = {
    val h = harness()
    withDriver(h) {
      (1 to 5).foreach(_ => h.source.emit(key('+')))
      val before = draws(h.backend)
      flush(h)
      Result.all(
        List(
          Assertions.eqv(before, 1),
          Assertions.eqv(draws(h.backend), 2),
          Assertions.grid(h.backend.screen, Vector("count 5" + " " * 13, " " * 20, " " * 20, " " * 20, " " * 20)),
        )
      )
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testLastResize: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(Event.resize(sized(3, 3)))
      h.source.emit(Event.resize(sized(9, 9)))
      h.source.emit(key('q'))
      flush(h)
      Result.all(List(Assertions.eqv(h.exited.get().map(_.size), sized(9, 9).some), Assertions.eqv(draws(h.backend), 2)))
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testPrints: Result = {
    val h = harness()
    withDriverIn(TerminalOptions.of(ScreenMode.inlineOf(PosInt(5))), h) {
      h.source.emit(key('p'))
      flush(h)
      val calls    = h.backend.calls
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
          Assertions.eqv(h.backend.printed.length, 1),
          Result.assert(printAt >= 0 && printAt < lastDraw).log(s"print at ${printAt.toString}, last draw at ${lastDraw.toString}"),
          Assertions.eqv(draws(h.backend), 2),
        )
      )
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testExit: Result = {
    val h = harness()
    withDriver(h) {
      val exited = quit(h)
      val after  = draws(h.backend)
      h.source.emit(key('+'))
      flush(h)
      Result.all(
        List(
          Assertions.eqv(exited.map(_.exit), true.some),
          Assertions.eqv(after, 2),
          Assertions.eqv(draws(h.backend), 2),
          Assertions.eqv(h.source.listenerCount, 0),
        )
      )
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testTicks: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(key('t'))
      flush(h)
      h.scheduler.advance(10.millis)
      val afterOne = draws(h.backend)
      h.scheduler.advance(10.millis)
      h.scheduler.advance(10.millis)
      val exited   = quit(h)
      Result.all(
        List(
          Assertions.eqv(afterOne, 3),
          Assertions.eqv(exited.map(_.ticks), Vector(10.millis, 20.millis, 30.millis).some),
          Assertions.eqv(draws(h.backend), 6),
        )
      )
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testSyncTask: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(key('s'))
      flush(h)
      flush(h)
      Assertions.eqv(quit(h).map(_.count), 10.some)
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testDeferredTask: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(key('d'))
      flush(h)
      val captured = h.deferred.get().isDefined
      h.deferred.get().foreach(callback => callback(5.asRight[Throwable]))
      flush(h)
      Result.all(List(Assertions.eqv(captured, true), Assertions.eqv(quit(h).map(_.count), 5.some)))
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testOrder: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(key('+'))
      flush(h)
      h.source.emit(key('m'))
      flush(h)
      h.deferred.get().foreach(callback => callback(3.asRight[Throwable]))
      h.source.emit(key('+'))
      flush(h)
      Assertions.eqv(quit(h).map(_.count), 6.some)
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testCorrection: Result = {
    val h = harness()
    withDriver(h) {
      (1 to 150).foreach(_ => h.source.emit(key('+')))
      flush(h)
      Assertions.eqv(quit(h).map(_.count), CounterApp.maxCount.some)
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testMouse: Result = {
    val h = harness()
    withDriver(h) {
      h.source.emit(Event.mouse(MouseEvent(MouseEventKind.Down(MouseButton.Left), at(1, 0), KeyModifiers.empty)))
      flush(h)
      Assertions.eqv(quit(h).map(_.over), CounterApp.region.some.some)
    }.getOrElse(Result.failure.log("the terminal did not open"))
  }

  def testCrash: Result = {
    val h      = harness()
    val result = Try(withDriver(h) {
      h.source.emit(key('!'))
      flush(h)
    })
    Result.all(
      List(
        Result
          .assert(result.failed.toOption.exists(_.getMessage === "counter crash"))
          .log(s"result = ${result.fold(_.getMessage, _ => "no failure")}"),
        Assertions.eqv(h.backend.calls.lastOption, (BackendCall.Exit: BackendCall).some),
      )
    )
  }

}
