package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.app.DriverEquivalenceSpec.{app, check, env, quit, resizeIfNeeded, simulated, trace, viewport, Observed}
import stui.app.internal.{BlockingDriver, QueueScheduler}
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.frame.Frame
import stui.core.spi.{BlockingEventSource, Subscription, TerminalError, TerminalOptions}
import stui.core.terminal.Terminal
import stui.testkit.{Assertions, ManualClock, TestBackend}
import stui.testkit.ManualClock.*
import stui.testkit.TestBackend.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.*

/** The driver-equivalence law (design doc 10 and 12, M3c) between the blocking driver over `TestBackend` and the simulator, through a
  * scripted source that replays a step trace: a step's events are delivered one per poll with no clock movement, a zero-timeout
  * poll never crosses a step and never moves the clock, an `Advance` step moves the clock by the smaller of the poll timeout and
  * what remains of it on every poll (so the driver wakes at each due exactly as production does), and an exhausted script raises the
  * termination flag.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object BlockingEquivalenceSpec extends Properties {

  final private case class Script(pending: Vector[Event], remaining: Option[FiniteDuration], rest: Vector[Simulator.Step])

  /** The scripted blocking source over a manual clock. */
  final class ScriptedBlockingSource(
    clock: ManualClock,
    backend: TestBackend,
    steps: Vector[Simulator.Step],
    terminating: AtomicBoolean,
  ) extends BlockingEventSource {

    private val script: AtomicReference[Script] = new AtomicReference(Script(Vector.empty[Event], none[FiniteDuration], steps))

    override def poll(timeout: FiniteDuration): Option[Event] = {
      val current = script.get()
      current.pending match {
        case event +: more =>
          script.set(current.copy(pending = more))
          deliver(event)
        case _ if timeout <= Duration.Zero => none[Event]
        case _ =>
          current.remaining match {
            case Some(remaining) =>
              elapse(current, remaining, timeout)
              none[Event]
            case None =>
              current.rest match {
                case Simulator.Step.Events(events) +: rest =>
                  events match {
                    case event +: more =>
                      script.set(Script(more, none[FiniteDuration], rest))
                      deliver(event)
                    case _ =>
                      script.set(Script(Vector.empty[Event], none[FiniteDuration], rest))
                      none[Event]
                  }
                case Simulator.Step.Advance(by) +: rest =>
                  val next = Script(Vector.empty[Event], none[FiniteDuration], rest)
                  script.set(next)
                  if (by > Duration.Zero) elapse(next, by, timeout) else ()
                  none[Event]
                case _ =>
                  terminating.set(true)
                  none[Event]
              }
          }
      }
    }

    override def subscribe(listener: Event => Unit): Subscription = () => ()

    private def deliver(event: Event): Option[Event] = {
      resizeIfNeeded(backend, event)
      event.some
    }

    private def elapse(current: Script, remaining: FiniteDuration, timeout: FiniteDuration): Unit = {
      val moved = if (timeout < remaining) timeout else remaining
      clock.advance(moved)
      val left  = remaining - moved
      script.set(current.copy(remaining = if (left <= Duration.Zero) none[FiniteDuration] else left.some))
    }

  }

  private def key(c: Char): Event = Event.key(KeyEvent.press(KeyCode.Char(c)))

  private def blockingRun(trace: Vector[Simulator.Step]): Either[TerminalError, Observed] = {
    val clock       = ManualClock.of(0.millis)
    val backend     = TestBackend.of(viewport)
    val terminating = new AtomicBoolean(false)
    val source      = new ScriptedBlockingSource(clock, backend, trace, terminating)
    val frames      = new AtomicReference(Vector.empty[Frame])
    Terminal
      .run(backend, TerminalOptions.alternateScreen, Capabilities.conservative, clock) { terminal =>
        BlockingDriver.run(
          app,
          env,
          terminal,
          source,
          QueueScheduler.unwoken(clock),
          () => terminating.get(),
          100.millis,
          frame => frames.updateAndGet(_ :+ frame): Unit,
        )
      }
      .map(model => Observed(frames.get(), model.some, backend.screen, backend.printed))
  }

  override def tests: List[Test] = List(
    property("the blocking driver and the simulator present the same frames for the same trace", testEquivalence),
    example("a tick trace agrees", testTicks),
  )

  def testEquivalence: Property = trace.forAll.map(t => check(blockingRun(t), simulated(t)))

  def testTicks: Result = {
    val t         = Vector(Simulator.Step.events(key('t')), Simulator.Step.advance(35.millis), quit)
    val snapshots = simulated(t)
    Result.all(List(check(blockingRun(t), snapshots), Assertions.eqv(snapshots.length, 6)))
  }

}
