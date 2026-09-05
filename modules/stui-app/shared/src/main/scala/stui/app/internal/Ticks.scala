package stui.app.internal

import stui.app.{Sub, SubKey}
import stui.core.spi.{Scheduler, Subscription}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The subscription timers (design doc 10, decision D20, M3b): keys from the data, the newest tagger, and drift-free re-arming against
  * the previous due. Single-threaded by construction: [[Ticks.reconcile]] and the firing run on the loop thread.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final private[stui] class Ticks[Msg] private (
  private val scheduler: Scheduler,
  private val post: Msg => Unit,
  private val active: AtomicReference[Map[SubKey, Ticks.Active[Msg]]],
)

private[stui] object Ticks {

  /** One running tick: its cancel handle, its due on the scheduler's clock, and the taggers to fire. */
  final case class Active[Msg](handle: Subscription, due: Long, taggers: Vector[FiniteDuration => Msg])

  /** No tick running, messages posted through `post`. */
  def of[Msg](scheduler: Scheduler, post: Msg => Unit): Ticks[Msg] =
    new Ticks(scheduler, post, new AtomicReference(Map.empty[SubKey, Active[Msg]]))

  /** The interval of a key, floored at `Sub.minimumInterval`. */
  def period(key: SubKey): FiniteDuration = key match {
    case SubKey.Every(interval) => Sub.floored(interval)
  }

  extension [Msg](ticks: Ticks[Msg]) {

    /** The keys running. */
    def keys: Set[SubKey] = ticks.active.get().keySet

    /** The due of a running key. */
    def dueOf(key: SubKey): Option[Long] = ticks.active.get().get(key).map(_.due)

    /** Applies the leaves: stopped keys are cancelled, kept keys get the newest taggers and keep their due, new keys start one
      * interval from now. Returns the diff applied.
      */
    def reconcile(leaves: Vector[Sub.Every[Msg]]): Reconcile.Diff = {
      val next    = leaves.foldLeft(Map.empty[SubKey, Vector[FiniteDuration => Msg]]) { (acc, leaf) =>
        val key = Sub.key(leaf)
        acc.updated(key, acc.getOrElse(key, Vector.empty[FiniteDuration => Msg]) :+ leaf.tag)
      }
      val changes = Reconcile.diff(ticks.keys, next.keySet)
      changes.stops.foreach { key =>
        ticks.active.get().get(key).foreach(_.handle.cancel())
        ticks.active.updateAndGet(_ - key): Unit
      }
      next.foreachEntry { (key, taggers) =>
        if (changes.starts.contains(key)) {
          arm(key, ticks.scheduler.monotonicNanos() + period(key).toNanos, taggers)
        } else {
          ticks.active.updateAndGet(map => map.get(key).fold(map)(entry => map.updated(key, entry.copy(taggers = taggers)))): Unit
        }
      }
      changes
    }

    /** Cancels every running tick. */
    def clear(): Unit = ticks.active.getAndSet(Map.empty[SubKey, Active[Msg]]).values.foreach(_.handle.cancel())

    private def arm(key: SubKey, due: Long, taggers: Vector[FiniteDuration => Msg]): Unit = {
      val handle = ticks.scheduler.schedule(math.max(0L, due - ticks.scheduler.monotonicNanos()).nanos, () => fire(key))
      ticks.active.updateAndGet(_.updated(key, Active(handle, due, taggers))): Unit
    }

    private def fire(key: SubKey): Unit =
      ticks.active.get().get(key).foreach { entry =>
        val now = ticks.scheduler.monotonicNanos()
        entry.taggers.foreach(tag => ticks.post(tag(now.nanos)))
        arm(key, entry.due + period(key).toNanos, entry.taggers)
      }

  }

}
