package stui.terminal

import cats.syntax.all.*
import stui.core.event.Event
import stui.core.geometry.Size
import stui.core.spi.{EventSource, Subscription}
import stui.terminal.decoder.{Decoded, Decoder, DecoderInput, DecoderState}
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

/** The push-based event source for callback platforms, Node first (design doc 6.3 and 7.5): chunks and resizes are fed in by the
  * platform's callbacks, go through the same pure decoder as everywhere else (no decoder branch exists for Node), and the decoded
  * events are delivered to the subscribed listeners. Because there is no `poll` to arm the ESC timeout, a tick is scheduled through
  * the `schedule` function whenever the decoder reports it is awaiting more input, and cancelled when the next chunk arrives first
  * (deterministic in tests through a manual scheduler). A resize is deduplicated on the terminal size while the delivered payload is
  * the effective viewport size (the D12 under-run rule, [[EffectiveSize]]). The probe hands over its final decoder state and the
  * events it decoded while waiting, and events decoded before any listener subscribes are buffered and delivered at the first
  * subscription, so nothing typed during startup is lost.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
final class PushEventSource(
  viewportSize: Size => Size,
  escTimeout: FiniteDuration,
  schedule: PushEventSource.Schedule,
  initialState: DecoderState,
  initialEvents: Vector[Event],
  initialSize: Option[Size],
) extends EventSource {

  private val decoder: AtomicReference[DecoderState] = new AtomicReference(initialState)

  private val pending: AtomicReference[Vector[Event]] = new AtomicReference(initialEvents)

  private val listeners: AtomicReference[Vector[(Int, Event => Unit)]] = new AtomicReference(Vector.empty[(Int, Event => Unit)])

  private val ids: AtomicInteger = new AtomicInteger(0)

  private val lastSize: AtomicReference[Option[Size]] = new AtomicReference(initialSize)

  private val tickCancel: AtomicReference[Option[() => Unit]] = new AtomicReference(none[() => Unit])

  private val closed: AtomicBoolean = new AtomicBoolean(false)

  /** Registers the listener, delivers anything buffered, and returns the handle that removes it. */
  override def subscribe(listener: Event => Unit): Subscription = {
    val id = ids.getAndIncrement()
    listeners.updateAndGet(_ :+ (id -> listener)): Unit
    drain()
    () => listeners.updateAndGet(_.filterNot { case (listenerId, _) => listenerId === id }): Unit
  }

  /** A raw chunk from the platform: decoded, the ESC timeout re-armed when the decoder awaits more, the events delivered. */
  def onChunk(chunk: IArray[Byte]): Unit =
    if (closed.get()) {
      ()
    } else {
      cancelTick()
      step(DecoderInput.bytes(chunk))
      drain()
    }

  /** A size report from the platform's resize callback: deduplicated on the terminal size, delivered as the effective viewport
    * size, nothing for a failed or zero-sized report.
    */
  def onResize(size: Option[Size]): Unit =
    if (closed.get()) {
      ()
    } else {
      size.filter(reported => !reported.isEmpty) match {
        case Some(reported) =>
          if (lastSize.getAndSet(reported.some) =!= reported.some) {
            pending.updateAndGet(_ :+ Event.resize(viewportSize(reported))): Unit
            drain()
          } else {
            ()
          }
        case None => ()
      }
    }

  /** Stops delivery and cancels an armed tick. Idempotent. */
  def shutdown(): Unit =
    if (closed.compareAndSet(false, true)) cancelTick() else ()

  private def onTick(): Unit = {
    tickCancel.set(none[() => Unit])
    if (closed.get() || !decoder.get().awaiting) {
      ()
    } else {
      step(DecoderInput.Tick)
      drain()
    }
  }

  /* replies outside a probe window are late answers and are dropped */
  private def step(input: DecoderInput): Unit =
    Decoder.step(decoder.get(), input) match {
      case Decoded(next, events, _) =>
        decoder.set(next)
        pending.updateAndGet(_ ++ events): Unit
        if (next.awaiting) tickCancel.set(schedule(escTimeout, () => onTick()).some) else ()
    }

  private def cancelTick(): Unit = tickCancel.getAndSet(none[() => Unit]).foreach(cancel => cancel())

  @tailrec
  private def drain(): Unit =
    if (listeners.get().isEmpty) {
      ()
    } else {
      pending.getAndUpdate(_.drop(1)).headOption match {
        case Some(event) =>
          listeners.get().foreach { case (_, listener) => listener(event) }
          drain()
        case None => ()
      }
    }

}

object PushEventSource {

  /** Schedules the action after the delay and returns the cancel action (`js.timers` on Node, a manual recorder in tests). */
  type Schedule = (FiniteDuration, () => Unit) => (() => Unit)

}
