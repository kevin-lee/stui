package stui.testkit

import stui.core.internal.TickQueue
import stui.core.internal.TickQueue.*
import stui.core.spi.{Subscription, VirtualScheduler}
import stui.testkit.ManualClock.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The deterministic scheduler (design doc 6.3 and 12, M3a) over a frozen [[ManualClock]]: time moves only under
  * [[ManualScheduler.advance]], which fires every tick due at or before the target in due order (ties in schedule order, ticks
  * scheduled during the advance included when due inside the window) with the clock at each tick's due time while its action runs,
  * so the same trace fires the same sequence on every platform. A cancelled tick never fires, cancel is idempotent and a no-op from
  * inside the action. `ManualClock.ticking` is deliberately not composed (a read that crosses a due time would have to decide whether
  * to fire). Built over the shared `TickQueue` (M3b), so its laws prove the production ordering too. It implements the
  * `VirtualScheduler` SPI (M3c), the form the `stui-app` simulator takes.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
final class ManualScheduler private (
  private val clock: ManualClock,
  private val queue: AtomicReference[TickQueue],
) extends VirtualScheduler {

  /* the methods live in the class body because they implement the VirtualScheduler trait members */
  /** The frozen clock's value. */
  override def monotonicNanos(): Long = clock.monotonicNanos()

  /** Records the tick as due `delay` from now (the queue's next id keeps schedule order among equal dues) and returns the cancel that
    * removes it, a no-op once it has fired or been cancelled.
    */
  override def schedule(delay: FiniteDuration, action: () => Unit): Subscription = {
    val due = clock.now.toNanos + delay.toNanos
    val id  = queue.getAndUpdate(_.add(due, action)).nextId
    () => queue.updateAndGet(_.remove(id)): Unit
  }

  /** Moves the clock forward by `by` (a negative duration counts as zero, the clock never decreases), firing every tick due at or
    * before the target in due order with ties in schedule order, each action running with the clock at its due time, ticks
    * scheduled by an action included when due inside the window. The clock ends at the target.
    */
  override def advance(by: FiniteDuration): Unit = {
    val target = clock.now.toNanos + math.max(0L, by.toNanos)

    @tailrec
    def loop(): Unit =
      queue.get().popDue(target) match {
        case Some((entry, rest)) =>
          queue.set(rest)
          clock.advance((entry.due - clock.now.toNanos).nanos)
          entry.action()
          loop()
        case None =>
          clock.advance((target - clock.now.toNanos).nanos)
      }

    loop()
  }

}

object ManualScheduler {

  /** A scheduler whose clock starts at `start` and moves only under [[ManualScheduler.advance]]. */
  def of(start: FiniteDuration): ManualScheduler =
    new ManualScheduler(ManualClock.of(start), new AtomicReference(TickQueue.empty))

  extension (scheduler: ManualScheduler) {

    /** The clock's value. */
    def now: FiniteDuration = scheduler.clock.now

    /** The due times of the not-yet-fired, not-cancelled ticks in firing order (due ascending, then schedule order). */
    def pending: Vector[FiniteDuration] = scheduler.queue.get().dues.map(_.nanos)

  }

}
