package stui.terminal

import stui.core.spi.Scheduler

/** The push source's schedule function over a [[Scheduler]] for tests: the cancel action cancels the tick's handle.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object Schedules {

  /** [[PushEventSource.Schedule]] over the scheduler (testkit's `ManualScheduler` in the specs). */
  def of(scheduler: Scheduler): PushEventSource.Schedule = (delay, action) => {
    val handle = scheduler.schedule(delay, action)
    () => handle.cancel()
  }

}
