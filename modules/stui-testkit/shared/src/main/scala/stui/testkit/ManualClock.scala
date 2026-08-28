package stui.testkit

import stui.core.spi.Clock

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.*

/** A deterministic [[Clock]] for tests: it reads a value that only [[ManualClock.advance]] or the per-read `step` of
  * [[ManualClock.ticking]] moves.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class ManualClock private (private val nanos: AtomicLong, private val step: Long) extends Clock {

  /* the method lives in the class body because it implements the Clock trait member */
  /** The current value, then the clock moves by the step (0 unless built with [[ManualClock.ticking]]). */
  override def monotonicNanos(): Long = nanos.getAndAdd(step)

}

object ManualClock {

  /** A clock that stays at `start` until advanced. */
  def of(start: FiniteDuration): ManualClock = new ManualClock(new AtomicLong(start.toNanos), 0L)

  /** A clock that advances by `step` after every read. */
  def ticking(start: FiniteDuration, step: FiniteDuration): ManualClock = new ManualClock(new AtomicLong(start.toNanos), step.toNanos)

  extension (clock: ManualClock) {

    /** Moves the clock forward. */
    def advance(by: FiniteDuration): Unit = clock.nanos.getAndAdd(by.toNanos): Unit

    /** The current value without moving the clock. */
    def now: FiniteDuration = clock.nanos.get().nanos

  }

}
