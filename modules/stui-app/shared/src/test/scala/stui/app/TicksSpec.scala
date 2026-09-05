package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.{QueueScheduler, Reconcile, Ticks}
import stui.app.internal.QueueScheduler.*
import stui.app.internal.Ticks.*
import stui.testkit.{Assertions, ManualClock, ManualScheduler}
import stui.testkit.ManualClock.*
import stui.testkit.ManualScheduler.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The subscription timers (design doc 10 and 12, M3b): interval, kept due, stop, tagger refresh, shared keys, clear, and the
  * drift-free re-arm.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object TicksSpec extends Properties {

  private def leaf(interval: FiniteDuration, name: String): Sub.Every[String] =
    Sub.Every(interval, now => s"$name@${now.toMillis.toString}")

  private val key: SubKey = SubKey.Every(10.millis)

  final private case class Setup(scheduler: ManualScheduler, ticks: Ticks[String], log: AtomicReference[Vector[String]])

  private def setup(): Setup = {
    val scheduler = ManualScheduler.of(0.millis)
    val log       = new AtomicReference(Vector.empty[String])
    Setup(scheduler, Ticks.of[String](scheduler, msg => log.updateAndGet(_ :+ msg): Unit), log)
  }

  override def tests: List[Test] = List(
    example("a started key fires at its interval and re-arms", testFires),
    example("reconciling the same leaves twice changes nothing", testIdempotent),
    example("a kept key keeps its due across a reconcile with a new tagger, and the new tagger fires", testKept),
    example("a stopped key never fires", testStopped),
    example("two taggers on one key both fire in order", testShared),
    example("clear cancels everything", testClear),
    example("a late drain re-arms against the previous due, not now", testDriftFree),
  )

  def testFires: Result = {
    val s        = setup()
    s.ticks.reconcile(Vector(leaf(10.millis, "a"))): Unit
    s.scheduler.advance(10.millis)
    val afterOne = s.log.get()
    val pending  = s.scheduler.pending
    s.scheduler.advance(10.millis)
    Result.all(
      List(
        Assertions.eqv(afterOne, Vector("a@10")),
        Assertions.eqv(pending, Vector(20.millis)),
        Assertions.eqv(s.log.get(), Vector("a@10", "a@20")),
      )
    )
  }

  def testIdempotent: Result = {
    val s       = setup()
    val first   = s.ticks.reconcile(Vector(leaf(10.millis, "a"), leaf(30.millis, "b")))
    val pending = s.scheduler.pending
    val second  = s.ticks.reconcile(Vector(leaf(10.millis, "a"), leaf(30.millis, "b")))
    Result.all(
      List(
        Assertions.eqv(first, Reconcile.Diff(Set[SubKey](key, SubKey.Every(30.millis)), Set.empty[SubKey])),
        Assertions.eqv(second, Reconcile.nothing),
        Assertions.eqv(s.scheduler.pending, pending),
        Assertions.eqv(s.ticks.keys, Set[SubKey](key, SubKey.Every(30.millis))),
      )
    )
  }

  def testKept: Result = {
    val s       = setup()
    s.ticks.reconcile(Vector(leaf(10.millis, "a"))): Unit
    s.scheduler.advance(3.millis)
    val changes = s.ticks.reconcile(Vector(leaf(10.millis, "b")))
    val due     = s.ticks.dueOf(key)
    s.scheduler.advance(7.millis)
    Result.all(
      List(
        Assertions.eqv(changes, Reconcile.nothing),
        Assertions.eqv(due, 10.millis.toNanos.some),
        Assertions.eqv(s.log.get(), Vector("b@10")),
      )
    )
  }

  def testStopped: Result = {
    val s       = setup()
    s.ticks.reconcile(Vector(leaf(10.millis, "a"))): Unit
    val changes = s.ticks.reconcile(Vector.empty[Sub.Every[String]])
    s.scheduler.advance(50.millis)
    Result.all(
      List(
        Assertions.eqv(changes, Reconcile.Diff(Set.empty[SubKey], Set[SubKey](key))),
        Assertions.eqv(s.log.get(), Vector.empty[String]),
        Assertions.eqv(s.scheduler.pending, Vector.empty[FiniteDuration]),
        Assertions.eqv(s.ticks.keys, Set.empty[SubKey]),
      )
    )
  }

  def testShared: Result = {
    val s = setup()
    s.ticks.reconcile(Vector(leaf(10.millis, "a"), leaf(10.millis, "b"))): Unit
    s.scheduler.advance(10.millis)
    Result.all(List(Assertions.eqv(s.log.get(), Vector("a@10", "b@10")), Assertions.eqv(s.scheduler.pending, Vector(20.millis))))
  }

  def testClear: Result = {
    val s       = setup()
    s.ticks.reconcile(Vector(leaf(10.millis, "a"), leaf(30.millis, "b"))): Unit
    s.ticks.clear()
    val pending = s.scheduler.pending
    s.scheduler.advance(100.millis)
    Result.all(
      List(
        Assertions.eqv(pending, Vector.empty[FiniteDuration]),
        Assertions.eqv(s.log.get(), Vector.empty[String]),
        Assertions.eqv(s.ticks.keys, Set.empty[SubKey]),
      )
    )
  }

  def testDriftFree: Result = {
    val clock     = ManualClock.of(0.millis)
    val scheduler = QueueScheduler.unwoken(clock)
    val log       = new AtomicReference(Vector.empty[String])
    val ticks     = Ticks.of[String](scheduler, msg => log.updateAndGet(_ :+ msg): Unit)
    ticks.reconcile(Vector(leaf(10.millis, "a"))): Unit
    clock.advance(13.millis)
    scheduler.drain()
    Result.all(List(Assertions.eqv(log.get(), Vector("a@13")), Assertions.eqv(ticks.dueOf(key), 20.millis.toNanos.some)))
  }

}
