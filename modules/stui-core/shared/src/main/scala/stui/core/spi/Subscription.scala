package stui.core.spi

/** A handle to stop receiving events from an [[EventSource]]. `cancel` is idempotent.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
trait Subscription {

  /** Stops delivery. Safe to call more than once. */
  def cancel(): Unit

}
