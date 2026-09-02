package stui.testkit

import cats.syntax.all.*
import stui.core.spi.{Scheduler, Subscription}
import stui.testkit.ManualClock.*

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The deterministic [[Scheduler]] (design doc 6.3 and 12, M3a) over a frozen [[ManualClock]]: time moves only under
  * [[ManualScheduler.advance]], which fires every tick due at or before the target in due order (ties in schedule order, ticks
  * scheduled during the advance included when due inside the window) with the clock at each tick's due time while its action runs,
  * so the same trace fires the same sequence on every platform. A cancelled tick never fires, cancel is idempotent and a no-op from
  * inside the action. `ManualClock.ticking` is deliberately not composed (a read that crosses a due time would have to decide whether
  * to fire).
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
final class ManualScheduler private (
  private val clock: ManualClock,
  private val entries: AtomicReference[Vector[ManualScheduler.Entry]],
  private val ids: AtomicLong,
) extends Scheduler {

  /* the methods live in the class body because they implement the Scheduler trait members */
  /** The frozen clock's value. */
  override def monotonicNanos(): Long = clock.monotonicNanos()

  /** Records the tick as due `delay` from now (a fresh id keeps schedule order among equal dues) and returns the cancel that removes
    * it, a no-op once it has fired or been cancelled.
    */
  override def schedule(delay: FiniteDuration, action: () => Unit): Subscription = {
    val id  = ids.getAndIncrement()
    val due = clock.now.toNanos + delay.toNanos
    entries.updateAndGet(_ :+ ManualScheduler.Entry(id, due, action)): Unit
    () => entries.updateAndGet(_.filterNot(_.id === id)): Unit
  }

}

object ManualScheduler {

  /** One scheduled tick: its schedule order, its due time in nanoseconds, and its action. */
  final private[testkit] case class Entry(id: Long, due: Long, action: () => Unit)

  /** A scheduler whose clock starts at `start` and moves only under [[advance]]. */
  def of(start: FiniteDuration): ManualScheduler =
    new ManualScheduler(ManualClock.of(start), new AtomicReference(Vector.empty[Entry]), new AtomicLong(0L))

  extension (scheduler: ManualScheduler) {

    /** The clock's value. */
    def now: FiniteDuration = scheduler.clock.now

    /** The due times of the not-yet-fired, not-cancelled ticks in firing order (due ascending, then schedule order). */
    def pending: Vector[FiniteDuration] = scheduler.entries.get().sortBy(entry => (entry.due, entry.id)).map(_.due.nanos)

    /** Moves the clock forward by `by` (a negative duration counts as zero, the clock never decreases), firing every tick due at or
      * before the target in due order with ties in schedule order, each action running with the clock at its due time, ticks
      * scheduled by an action included when due inside the window. The clock ends at the target.
      */
    def advance(by: FiniteDuration): Unit = {
      val target = scheduler.clock.now.toNanos + math.max(0L, by.toNanos)

      @tailrec
      def loop(): Unit =
        scheduler.entries.get().filter(_.due <= target).minByOption(entry => (entry.due, entry.id)) match {
          case Some(entry) =>
            scheduler.entries.updateAndGet(_.filterNot(_.id === entry.id)): Unit
            scheduler.clock.advance((entry.due - scheduler.clock.now.toNanos).nanos)
            entry.action()
            loop()
          case None =>
            scheduler.clock.advance((target - scheduler.clock.now.toNanos).nanos)
        }

      loop()
    }

  }

}
