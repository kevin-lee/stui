package stui.core.spi

import scala.concurrent.duration.FiniteDuration

/** The tick source of the runtime and the ESC timeout (design doc 6.3 and 8.1, decision D20, M3a): a monotonic [[Clock]] plus
  * one-shot scheduling, the general form of the push event source's schedule function. `schedule` runs the action once, after at
  * least `delay` on this clock, on the implementation's own thread or loop, and returns the handle that prevents it. Cancel is
  * idempotent and a no-op once the action has run or from inside it. An action may schedule again (periodic ticks re-arm against the
  * previous due time, which the clock on the same trait makes drift-free). Thread safety is each implementation's documented business
  * (single-threaded drivers need none). No production implementation ships before the M3b drivers; testkit's `ManualScheduler` is
  * the deterministic one.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
trait Scheduler extends Clock {

  /** Runs the action once, after at least `delay` on this clock, and returns the idempotent handle that prevents it. */
  def schedule(delay: FiniteDuration, action: () => Unit): Subscription

}
