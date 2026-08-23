package stui.core.spi

import stui.core.event.Event

/** Push-based event delivery, the one shape every platform supports, JS included (design doc 6.3). Listeners are invoked on the
  * backend's thread, so they must hand off quickly.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
trait EventSource {

  def subscribe(listener: Event => Unit): Subscription

}
