package stui.core.internal

import cats.syntax.all.*

/** The due-time queue every [[stui.core.spi.Scheduler]] implementation shares (design doc 6.3, M3b): ticks fire in due order with
  * ties in schedule order, which the id encodes, so the `ManualScheduler` laws of testkit prove the ordering rule of the production
  * schedulers as well. A pure value, kept in an `AtomicReference` by its owners.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final private[stui] case class TickQueue(entries: Vector[TickQueue.Entry], nextId: Long)

private[stui] object TickQueue {

  /** One scheduled tick: its schedule order, its due time in nanoseconds on the owner's clock, and its action. */
  final case class Entry(id: Long, due: Long, action: () => Unit)

  /** No tick, the first id 0. */
  val empty: TickQueue = TickQueue(Vector.empty[Entry], 0L)

  extension (queue: TickQueue) {

    /** The queue with one more tick under the next id. */
    def add(due: Long, action: () => Unit): TickQueue =
      TickQueue(queue.entries :+ Entry(queue.nextId, due, action), queue.nextId + 1L)

    /** The queue without the tick of the id, unchanged for an absent id. */
    def remove(id: Long): TickQueue = queue.copy(entries = queue.entries.filterNot(_.id === id))

    /** The earliest due, `None` when empty. */
    def nextDue: Option[Long] = queue.entries.map(_.due).minOption

    /** The tick that fires first among those due at or before `limit` (the smallest due, then the smallest id) paired with the queue
      * without it, `None` when nothing is due by then.
      */
    def popDue(limit: Long): Option[(Entry, TickQueue)] =
      queue
        .entries
        .filter(_.due <= limit)
        .minByOption(entry => (entry.due, entry.id))
        .map(entry => (entry, queue.remove(entry.id)))

    /** Every due in firing order (due ascending, then schedule order). */
    def dues: Vector[Long] = queue.entries.sortBy(entry => (entry.due, entry.id)).map(_.due)

    /** True when no tick is queued. */
    def isEmpty: Boolean = queue.entries.isEmpty

  }

}
