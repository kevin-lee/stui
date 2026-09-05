package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.ScreenMode
import stui.core.terminal.RedrawReason
import stui.testkit.{Assertions, ManualScheduler}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The headless simulator over the counter application (design doc 10 and 12, M3c): the first snapshot, one per batch, the last
  * resize, prints, the redraw reasons, exit, ticks under `Advance`, a synchronous task's own batch, mouse routing, the render
  * correction, an empty batch, and determinism.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SimulatorSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(20, 5))

  private def app: StuiApp[CounterModel, CounterMsg] =
    CounterApp.of(new AtomicReference(none[Either[Throwable, Int] => Unit]), none[Int])

  private def key(c: Char): Event = Event.key(KeyEvent.press(KeyCode.Char(c)))

  private def events(events: Event*): Simulator.Step = Simulator.Step.events(events*)

  private def run(steps: Simulator.Step*): Vector[Simulator.Snapshot[CounterModel]] =
    Simulator.run(app, env, steps.toVector, ManualScheduler.of(0.millis))

  private val blankRows: Vector[String] = Vector.fill(4)(" " * 20)

  override def tests: List[Test] = List(
    example("the first snapshot is init's present", testFirst),
    example("one snapshot per non-empty batch and the last resize sets the frame area", testBatches),
    example("prints are rendered over the viewport and trimmed", testPrints),
    example("Redraw records the requested reason", testRedraw),
    example("Exit ends the run and the rest of the trace is ignored", testExit),
    example("ticks fire under Advance at their dues", testTicks),
    example("a synchronous task's result is its own batch", testSyncTask),
    example("a mouse event routes through the previous frame's regions", testMouse),
    example("the render correction reaches the model", testCorrection),
    example("an empty Events step presents nothing", testEmpty),
    property("the simulator is deterministic", testDeterministic),
  )

  def testFirst: Result = {
    val snapshots = run()
    Result.all(
      List(
        Assertions.eqv(snapshots.length, 1),
        Assertions.eqv(snapshots.headOption.map(_.model.count), 0.some),
        Assertions.eqv(snapshots.headOption.map(_.exit), false.some),
        snapshots
          .headOption
          .fold(Result.failure.log("no snapshot"))(s => Assertions.grid(s.frame.buffer, ("count 0" + " " * 13) +: blankRows)),
      )
    )
  }

  def testBatches: Result = {
    val snapshots = run(events(Event.resize(sized(3, 3)), Event.resize(sized(9, 9)), key('+')))
    Result.all(
      List(
        Assertions.eqv(snapshots.length, 2),
        Assertions.eqv(snapshots.lastOption.map(_.frame.buffer.area), Rect.sized(sized(9, 9)).some),
        Assertions.eqv(snapshots.lastOption.map(_.model.size), sized(9, 9).some),
        Assertions.eqv(snapshots.lastOption.map(_.model.count), 1.some),
        Assertions.eqv(snapshots.lastOption.flatMap(_.redraw), RedrawReason.Resize.some),
      )
    )
  }

  def testPrints: Result = {
    val snapshots = run(events(key('p')))
    Result.all(
      List(
        Assertions.eqv(snapshots.lastOption.map(_.prints.length), 1.some),
        snapshots
          .lastOption
          .flatMap(_.prints.headOption)
          .fold(Result.failure.log("no print"))(print => Assertions.grid(print, Vector("printed" + " " * 13))),
      )
    )
  }

  def testRedraw: Result = Assertions.eqv(run(events(key('r'))).lastOption.flatMap(_.redraw), RedrawReason.Requested.some)

  def testExit: Result = {
    val snapshots = run(events(key('q')), events(key('+')))
    Result.all(
      List(
        Assertions.eqv(snapshots.length, 2),
        Assertions.eqv(snapshots.lastOption.map(_.exit), true.some),
        Assertions.eqv(snapshots.lastOption.map(_.model.count), 0.some),
      )
    )
  }

  def testTicks: Result = {
    val snapshots = run(events(key('t')), Simulator.Step.advance(35.millis))
    Result.all(
      List(
        Assertions.eqv(snapshots.lift(1).map(_.subscriptions), Set[SubKey](SubKey.Every(10.millis)).some),
        Assertions.eqv(snapshots.length, 5),
        Assertions.eqv(snapshots.lastOption.map(_.model.ticks), Vector(10.millis, 20.millis, 30.millis).some),
      )
    )
  }

  def testSyncTask: Result = {
    val snapshots = run(events(key('s')))
    Result.all(List(Assertions.eqv(snapshots.length, 3), Assertions.eqv(snapshots.lastOption.map(_.model.count), 10.some)))
  }

  def testMouse: Result = {
    val click = Event.mouse(MouseEvent(MouseEventKind.Down(MouseButton.Left), at(1, 0), KeyModifiers.empty))
    Assertions.eqv(run(events(click)).lastOption.map(_.model.over), CounterApp.region.some.some)
  }

  def testCorrection: Result =
    Assertions.eqv(run(events(Vector.fill(150)(key('+'))*)).lastOption.map(_.model.count), CounterApp.maxCount.some)

  def testEmpty: Result = Assertions.eqv(run(events()).length, 1)

  def testDeterministic: Property =
    SimulatorGens.trace.forAll.map { trace =>
      Assertions.eqv(
        Simulator.run(app, env, trace, ManualScheduler.of(0.millis)),
        Simulator.run(app, env, trace, ManualScheduler.of(0.millis)),
      )
    }

}
