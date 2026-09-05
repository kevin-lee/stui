package stui.testkit

import cats.syntax.all.*
import stui.core.spi.{Scheduler, Subscription}
import stui.testkit.ManualClock.*

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

/** The slow-link timing model (design doc 7.5 and 12, M3c): a pure [[SlowLink.Timeline]] of byte arrivals - when each chunk of a
  * terminal's output reaches the application - replayed by two players so that Escape (ESC) disambiguation and the startup probe can
  * be tested under ssh-like timing. The pull player answers a `poll(timeout)` against a [[ManualClock]] the way a device does (the
  * wait ends when the bytes arrive or the timeout elapses), the push player schedules every chunk through a [[Scheduler]]. testkit
  * cannot see `stui-terminal`, so the adapters live at the call sites: `RawInput` is a one-method trait a lambda over
  * [[SlowLink.PullPlayer.poll]] satisfies, `PushEventSource.onChunk` is a method reference for [[SlowLink.push]]. A chunk due at the
  * same instant as a decoder tick is delivered before the tick by both players (the pull player's wait ends at that instant with the
  * chunk, the push player scheduled the chunk before the tick was armed), and the laws keep gaps strictly away from the timeout
  * anyway.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SlowLink {

  /** When a chunk arrives, relative to the start of the timeline. */
  final case class Arrival(at: FiniteDuration, chunk: IArray[Byte])

  /** The arrivals in time order (ties in the order given to [[Timeline.of]]), a pure value the players replay. */
  final case class Timeline private (arrivals: Vector[Arrival])

  object Timeline {

    /** No arrival. */
    val empty: Timeline = new Timeline(Vector.empty[Arrival])

    /** The arrivals sorted by time, stably, so equal times keep the given order. */
    def of(arrivals: Arrival*): Timeline = new Timeline(arrivals.toVector.sortBy(_.at))

    /** Chunk `i` at `i` times the gap. */
    def spaced(gap: FiniteDuration, chunks: IArray[Byte]*): Timeline =
      new Timeline(chunks.toVector.zipWithIndex.map { case (chunk, i) => Arrival(gap * i.toLong, chunk) })

    extension (timeline: Timeline) {

      /** The chunks in arrival order. */
      def chunks: Vector[IArray[Byte]] = timeline.arrivals.map(_.chunk)

      /** The concatenation, what an instant link delivers. */
      def bytes: IArray[Byte] = IArray.from(timeline.arrivals.flatMap(_.chunk.toVector))

      /** The last arrival's time, zero when empty. */
      def end: FiniteDuration = timeline.arrivals.lastOption.fold(Duration.Zero)(_.at)

    }

  }

  /** A device over a timeline and a [[ManualClock]]: [[poll]] returns the next chunk once its time is within the wait, moving the
    * clock to it, and otherwise moves the clock by the whole wait and returns `None`. Times are relative to the clock's value when
    * the player was built.
    */
  final class PullPlayer private[SlowLink] (
    private val timeline: Timeline,
    private val clock: ManualClock,
    private val origin: FiniteDuration,
    private val index: AtomicInteger,
  ) {

    /** The next undelivered chunk when its time is at or before now (no clock movement) or within the wait (the clock moved to its
      * time), else `None` with the clock moved by the wait (a negative timeout counts as zero).
      */
    def poll(timeout: FiniteDuration): Option[IArray[Byte]] = {
      val wait    = math.max(0L, timeout.toNanos).nanos
      val elapsed = clock.now - origin
      timeline.arrivals.lift(index.get()) match {
        case Some(arrival) if arrival.at <= elapsed =>
          index.incrementAndGet(): Unit
          arrival.chunk.some
        case Some(arrival) if arrival.at <= elapsed + wait =>
          clock.advance(arrival.at - elapsed)
          index.incrementAndGet(): Unit
          arrival.chunk.some
        case Some(_) | None =>
          clock.advance(wait)
          none[IArray[Byte]]
      }
    }

    /** The number of chunks not yet delivered. */
    def remaining: Int = timeline.arrivals.length - index.get()

  }

  /** The pull player over the timeline with its origin at the clock's current value. */
  def pull(timeline: Timeline, clock: ManualClock): PullPlayer = new PullPlayer(timeline, clock, clock.now, new AtomicInteger(0))

  /** Schedules `onChunk` for every arrival at its time, in timeline order, and returns the handles: under a `VirtualScheduler`,
    * `advance(timeline.end)` delivers everything in order (ties in schedule order).
    */
  def push(timeline: Timeline, scheduler: Scheduler, onChunk: IArray[Byte] => Unit): Vector[Subscription] =
    timeline.arrivals.map(arrival => scheduler.schedule(arrival.at, () => onChunk(arrival.chunk)))

}
