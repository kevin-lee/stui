package stui.core.spi

import stui.core.event.Event

import scala.concurrent.duration.FiniteDuration

/** An [[EventSource]] that can also block the caller: JVM and Native only, absent on JS by construction (design doc 6.3), which is why this
  * file lives in the `jvm-native` source directory.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
trait BlockingEventSource extends EventSource {

  /** The next event, or `None` when the timeout elapses first. */
  def poll(timeout: FiniteDuration): Option[Event]

}
