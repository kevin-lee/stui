package stui.core.spi

import scala.concurrent.duration.FiniteDuration

/** The deterministic scheduler as a Service Provider Interface (design doc 6.3 and 10, M3c): a [[Scheduler]] whose time moves only
  * under [[advance]], which moves the clock to now plus the duration (a negative duration counts as zero, the clock never decreases)
  * and fires every tick due at or before the target in due order, ties in schedule order, ticks scheduled during the advance included
  * when due inside the window, the clock at each tick's due while its action runs, the clock ending at the target. This is the M3a
  * contract of testkit's `ManualScheduler`, which implements it, and what the `stui-app` simulator takes so that it holds no clock of
  * its own.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
trait VirtualScheduler extends Scheduler {

  /** Moves the clock forward by the duration (at least zero), firing every tick due at or before the target in due order, ties in
    * schedule order, with the clock at each tick's due while its action runs.
    */
  def advance(by: FiniteDuration): Unit

}
