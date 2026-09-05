package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.app.internal.CallbackDriver
import stui.core.buffer.Buffer
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.frame.Frame
import stui.core.geometry.Size
import stui.core.spi.{ScreenMode, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.testkit.{Assertions, ManualScheduler, TestBackend}
import stui.testkit.TestBackend.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The driver-equivalence law (design doc 10 and 12, M3c) between the callback driver over `TestBackend` and the simulator: for the
  * same trace ending in exit, the presented frames, the final models, the printed rows, and the backend screen agree. The harness
  * members are public so the `jvm-native` blocking spec reuses them.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object DriverEquivalenceSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  /** The viewport every ignition starts at. */
  val viewport: Size = sized(20, 5)

  /** The environment every ignition starts with. */
  val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, viewport)

  /** A fresh counter application (the deferred reference is unused, `d` and `m` are outside the vocabulary). */
  def app: StuiApp[CounterModel, CounterMsg] = CounterApp.of(new AtomicReference(none[Either[Throwable, Int] => Unit]), none[Int])

  /** What a driver run leaves behind. */
  final case class Observed(frames: Vector[Frame], model: Option[CounterModel], screen: Buffer, printed: Vector[Buffer])

  private def key(c: Char): Event = Event.key(KeyEvent.press(KeyCode.Char(c)))

  /** The exit batch every law's trace ends with. */
  val quit: Simulator.Step = Simulator.Step.events(key('q'))

  /** A generated trace ending in exit. */
  val trace: Gen[Vector[Simulator.Step]] = SimulatorGens.trace.map(_ :+ quit)

  /** The coupling a real terminal has: a resize event arrives with the screen already at the new size. */
  def resizeIfNeeded(backend: TestBackend, event: Event): Unit = event match {
    case Event.Resize(size) => if (size =!= backend.size()) backend.resize(size) else ()
    case Event.Key(_) | Event.Mouse(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => ()
  }

  /** The simulator over the trace. */
  def simulated(trace: Vector[Simulator.Step]): Vector[Simulator.Snapshot[CounterModel]] =
    Simulator.run(app, env, trace, ManualScheduler.of(0.millis))

  /** The four agreements: frames, the final model, the screen, the printed rows. */
  def check(observed: Either[TerminalError, Observed], snapshots: Vector[Simulator.Snapshot[CounterModel]]): Result =
    observed match {
      case Left(error) => Result.failure.log(error.show)
      case Right(driver) =>
        Result.all(
          List(
            Assertions.eqv(driver.frames, snapshots.map(_.frame)),
            Assertions.eqv(driver.model, snapshots.lastOption.map(_.model)),
            Assertions.eqv(snapshots.lastOption.map(_.frame.buffer), driver.screen.some),
            Assertions.eqv(driver.printed, snapshots.flatMap(_.prints)),
          )
        )
    }

  private def callbackRun(trace: Vector[Simulator.Step]): Either[TerminalError, Observed] = {
    val backend   = TestBackend.of(viewport)
    val scheduler = ManualScheduler.of(0.millis)
    val source    = new FakePushSource
    val frames    = new AtomicReference(Vector.empty[Frame])
    val exited    = new AtomicReference(none[CounterModel])
    Terminal
      .run(backend, TerminalOptions.alternateScreen, Capabilities.conservative, scheduler) { terminal =>
        CallbackDriver.start(
          app,
          env,
          terminal,
          source,
          scheduler,
          frame => frames.updateAndGet(_ :+ frame): Unit,
          model => exited.set(model.some),
        )
        trace.foreach {
          case Simulator.Step.Events(events) =>
            events.foreach { event =>
              resizeIfNeeded(backend, event)
              source.emit(event)
            }
            scheduler.advance(Duration.Zero)
          case Simulator.Step.Advance(by) => scheduler.advance(by)
        }
      }
      .map(_ => Observed(frames.get(), exited.get(), backend.screen, backend.printed))
  }

  override def tests: List[Test] = List(
    property("the callback driver and the simulator present the same frames for the same trace", testEquivalence),
    example("a resize inside a batch agrees", testResize),
  )

  def testEquivalence: Property = trace.forAll.map(t => check(callbackRun(t), simulated(t)))

  def testResize: Result = {
    val t = Vector(Simulator.Step.events(Event.resize(sized(3, 3)), Event.resize(sized(9, 9)), key('+')), quit)
    check(callbackRun(t), simulated(t))
  }

}
