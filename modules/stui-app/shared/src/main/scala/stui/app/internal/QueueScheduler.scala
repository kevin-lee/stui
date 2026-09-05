package stui.app.internal

import stui.core.internal.TickQueue
import stui.core.internal.TickQueue.*
import stui.core.spi.{Clock, Scheduler, Subscription}

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The production [[Scheduler]] (design doc 6.3 and 8.1, M3b): the shared due-time queue over the platform's clock, one wake primitive
  * per platform - the blocking driver ignores `wake` and reads [[QueueScheduler.timeoutUntilNextDue]] before each poll, the Node
  * driver re-arms one timer from it. `wake` receives the earliest due (or `None`) after every change to the queue. `schedule` may be
  * called from a task callback thread on the JVM and Native (the queue is atomic, `wake` is a no-op there), and actions always run on
  * the loop thread inside [[QueueScheduler.drain]], which pops one due tick at a time so an action may schedule again (a zero-delay
  * tick scheduled inside a drain runs in that drain, as `ManualScheduler.advance` does).
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final private[stui] class QueueScheduler private (
  private val clock: Clock,
  private val queue: AtomicReference[TickQueue],
  private val wake: Option[Long] => Unit,
) extends Scheduler {

  /* the methods live in the class body because they implement the Scheduler trait members */
  /** The platform clock's value. */
  override def monotonicNanos(): Long = clock.monotonicNanos()

  /** Records the tick as due `delay` from now (a non-positive delay is due now), wakes, and returns the cancel that removes it. */
  override def schedule(delay: FiniteDuration, action: () => Unit): Subscription = {
    val due = clock.monotonicNanos() + delay.toNanos
    val id  = queue.getAndUpdate(_.add(due, action)).nextId
    wake(queue.get().nextDue)
    () => {
      queue.updateAndGet(_.remove(id)): Unit
      wake(queue.get().nextDue)
    }
  }

}

private[stui] object QueueScheduler {

  /** A scheduler over the clock whose `wake` is called with the earliest due after every change. */
  def of(clock: Clock, wake: Option[Long] => Unit): QueueScheduler =
    new QueueScheduler(clock, new AtomicReference(TickQueue.empty), wake)

  /** A scheduler nobody needs to wake (the blocking driver polls with [[timeoutUntilNextDue]]). */
  def unwoken(clock: Clock): QueueScheduler = of(clock, _ => ())

  extension (scheduler: QueueScheduler) {

    /** The earliest due on the scheduler's clock, `None` when nothing is scheduled. */
    def nextDue: Option[Long] = scheduler.queue.get().nextDue

    /** How long a poll may block: the time to the earliest due, at most `cap`, zero when a tick is overdue, `cap` when nothing is
      * scheduled.
      */
    def timeoutUntilNextDue(cap: FiniteDuration): FiniteDuration =
      scheduler.nextDue.fold(cap)(due => math.min(cap.toNanos, math.max(0L, due - scheduler.clock.monotonicNanos())).nanos)

    /** Runs every tick due now, one at a time in due then schedule order, then wakes with what remains. */
    def drain(): Unit = {
      @tailrec
      def loop(): Unit = {
        val before = scheduler.queue.get()
        before.popDue(scheduler.clock.monotonicNanos()) match {
          case Some((entry, rest)) =>
            if (scheduler.queue.compareAndSet(before, rest)) entry.action() else ()
            loop()
          case None => scheduler.wake(scheduler.queue.get().nextDue)
        }
      }

      loop()
    }

  }

}
