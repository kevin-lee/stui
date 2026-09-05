package stui.app

import stui.core.event.Event
import stui.core.spi.{EventSource, Subscription}
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

/** A push source the tests feed by hand: `emit` calls every subscribed listener synchronously, the callback driver's shape on Node.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final class FakePushSource extends EventSource {

  private val listeners: AtomicReference[Vector[(Int, Event => Unit)]] = new AtomicReference(Vector.empty[(Int, Event => Unit)])

  private val ids: AtomicInteger = new AtomicInteger(0)

  /* the method lives in the class body because it implements the EventSource trait member */
  override def subscribe(listener: Event => Unit): Subscription = {
    val id = ids.getAndIncrement()
    listeners.updateAndGet(_ :+ (id -> listener)): Unit
    () => listeners.updateAndGet(_.filterNot { case (listenerId, _) => listenerId === id }): Unit
  }

  /** Delivers the event to every listener. */
  def emit(event: Event): Unit = listeners.get().foreach { case (_, listener) => listener(event) }

  /** How many listeners are subscribed. */
  def listenerCount: Int = listeners.get().size

}
