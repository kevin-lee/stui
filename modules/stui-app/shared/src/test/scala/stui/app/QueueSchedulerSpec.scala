package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.QueueScheduler
import stui.app.internal.QueueScheduler.*
import stui.core.spi.Subscription
import stui.testkit.{Assertions, ManualClock}
import stui.testkit.ManualClock.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The production scheduler over a manual clock (design doc 6.3 and 12, M3b): drain order, the wake hook, the poll timeout, cancel,
  * scheduling inside a drain, and the clock.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object QueueSchedulerSpec extends Properties {

  final private case class Setup(
    clock: ManualClock,
    scheduler: QueueScheduler,
    wakes: AtomicReference[Vector[Option[Long]]],
    log: AtomicReference[Vector[String]],
  )

  private def setup(): Setup = {
    val clock = ManualClock.of(0.millis)
    val wakes = new AtomicReference(Vector.empty[Option[Long]])
    val log   = new AtomicReference(Vector.empty[String])
    Setup(clock, QueueScheduler.of(clock, due => wakes.updateAndGet(_ :+ due): Unit), wakes, log)
  }

  private def tick(s: Setup, name: String): () => Unit = () => s.log.updateAndGet(_ :+ name): Unit

  override def tests: List[Test] = List(
    example("drain runs due actions in due then schedule order and leaves the rest", testOrder),
    example("wake is called with the head after schedule, cancel, and drain", testWake),
    example("timeoutUntilNextDue is the cap when empty, zero when overdue, the remaining time otherwise", testTimeout),
    example("cancel is idempotent and a cancelled action never runs", testCancel),
    example("a zero-delay action scheduled inside drain runs in the same drain", testInside),
    example("a scheduler is a clock", testClock),
  )

  def testOrder: Result = {
    val s = setup()
    s.scheduler.schedule(20.millis, tick(s, "b")): Unit
    s.scheduler.schedule(10.millis, tick(s, "a")): Unit
    s.scheduler.schedule(10.millis, tick(s, "c")): Unit
    s.scheduler.schedule(30.millis, tick(s, "d")): Unit
    s.clock.advance(20.millis)
    s.scheduler.drain()
    Result.all(List(Assertions.eqv(s.log.get(), Vector("a", "c", "b")), Assertions.eqv(s.scheduler.nextDue, 30.millis.toNanos.some)))
  }

  def testWake: Result = {
    val s               = setup()
    s.scheduler.schedule(10.millis, tick(s, "a")): Unit
    val b: Subscription = s.scheduler.schedule(5.millis, tick(s, "b"))
    b.cancel()
    s.clock.advance(10.millis)
    s.scheduler.drain()
    Assertions.eqv(
      s.wakes.get(),
      Vector(10.millis.toNanos.some, 5.millis.toNanos.some, 10.millis.toNanos.some, none[Long]),
    )
  }

  def testTimeout: Result = {
    val s      = setup()
    val empty  = s.scheduler.timeoutUntilNextDue(100.millis)
    s.scheduler.schedule(30.millis, tick(s, "a")): Unit
    val ahead  = s.scheduler.timeoutUntilNextDue(100.millis)
    val capped = s.scheduler.timeoutUntilNextDue(20.millis)
    s.clock.advance(40.millis)
    Result.all(
      List(
        Assertions.eqv(empty, 100.millis),
        Assertions.eqv(ahead, 30.millis),
        Assertions.eqv(capped, 20.millis),
        Assertions.eqv(s.scheduler.timeoutUntilNextDue(100.millis), Duration.Zero),
      )
    )
  }

  def testCancel: Result = {
    val s      = setup()
    val handle = s.scheduler.schedule(10.millis, tick(s, "a"))
    handle.cancel()
    handle.cancel()
    s.clock.advance(10.millis)
    s.scheduler.drain()
    Result.all(List(Assertions.eqv(s.log.get(), Vector.empty[String]), Assertions.eqv(s.scheduler.nextDue, none[Long])))
  }

  def testInside: Result = {
    val s = setup()
    s.scheduler
      .schedule(
        10.millis,
        () => {
          s.scheduler.schedule(Duration.Zero, tick(s, "b")): Unit
          tick(s, "a")()
        },
      ): Unit
    s.clock.advance(10.millis)
    s.scheduler.drain()
    Result.all(List(Assertions.eqv(s.log.get(), Vector("a", "b")), Assertions.eqv(s.scheduler.nextDue, none[Long])))
  }

  def testClock: Result = {
    val s = setup()
    s.clock.advance(4.millis)
    Assertions.eqv(s.scheduler.monotonicNanos(), s.clock.now.toNanos)
  }

}
