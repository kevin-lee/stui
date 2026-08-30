package stui.terminal

import cats.syntax.all.*
import stui.core.event.Event
import stui.core.geometry.Size
import stui.core.spi.{BlockingEventSource, Clock, Subscription}
import stui.terminal.decoder.{Decoded, Decoder, DecoderInput, DecoderState}
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The JVM and Native event source (design doc 7 and 7.5): raw chunks from a [[RawInput]] go through the pure decoder on the consumer's
  * side, so `poll` can arm the ESC timeout itself (a tick is fed once the timeout has elapsed since the decoder started waiting), and a
  * resize flag set by the platform's WINCH handler becomes a `Resize` event when the size query reports a new non-zero terminal size,
  * delivered as the effective viewport size through `viewportSize` (the D12 under-run rule: the deduplication is on the terminal size,
  * so an origin move without a viewport size change still surfaces). The probe hands over its final decoder state and the events it
  * decoded while waiting, so nothing typed during the probe is lost. `subscribe` runs one daemon thread over `poll` for the push style.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class DecodingEventSource(
  raw: RawInput,
  resized: AtomicBoolean,
  sizeQuery: () => Option[Size],
  viewportSize: Size => Size,
  escTimeout: FiniteDuration,
  clock: Clock,
  initialState: DecoderState,
  initialEvents: Vector[Event],
) extends BlockingEventSource {

  private val decoder: AtomicReference[DecoderState] = new AtomicReference(initialState)

  private val pending: AtomicReference[Vector[Event]] = new AtomicReference(initialEvents)

  private val awaitingSince: AtomicLong = new AtomicLong(0L)

  private val lastSize: AtomicReference[Option[Size]] = new AtomicReference(sizeQuery())

  private val listeners: AtomicReference[Vector[(Int, Event => Unit)]] = new AtomicReference(Vector.empty[(Int, Event => Unit)])

  private val ids: AtomicInteger = new AtomicInteger(0)

  private val dispatching: AtomicBoolean = new AtomicBoolean(false)

  /* the methods live in the class body because they implement the BlockingEventSource trait members */

  /** The next event within the timeout: a pending decoded event, a resize, or whatever the next chunk (or the ESC timeout) yields. */
  override def poll(timeout: FiniteDuration): Option[Event] = pollUntil(clock.monotonicNanos() + timeout.toNanos)

  /** Registers the listener and starts the dispatcher thread if it is not running. */
  override def subscribe(listener: Event => Unit): Subscription = {
    val id = ids.getAndIncrement()
    listeners.updateAndGet(_ :+ (id -> listener)): Unit
    startDispatcher()
    () => listeners.updateAndGet(_.filterNot { case (listenerId, _) => listenerId === id }): Unit
  }

  @tailrec
  private def pollUntil(deadline: Long): Option[Event] =
    takePending() match {
      case Some(event) => event.some
      case None =>
        resizeEvent() match {
          case Some(event) => event.some
          case None =>
            val now       = clock.monotonicNanos()
            val remaining = deadline - now
            if (remaining <= 0L) {
              none[Event]
            } else {
              val awaiting = decoder.get().awaiting
              val tickAt   = awaitingSince.get() + escTimeout.toNanos
              if (awaiting && now >= tickAt) {
                step(DecoderInput.Tick)
                pollUntil(deadline)
              } else {
                val wait = if (awaiting) math.min(remaining, tickAt - now) else remaining
                raw.poll(wait.nanos) match {
                  case Some(chunk) => step(DecoderInput.bytes(chunk))
                  case None => ()
                }
                pollUntil(deadline)
              }
            }
        }
    }

  /* replies outside a probe window are late answers and are dropped */
  private def step(input: DecoderInput): Unit =
    Decoder.step(decoder.get(), input) match {
      case Decoded(next, events, _) =>
        decoder.set(next)
        if (next.awaiting) awaitingSince.set(clock.monotonicNanos()) else ()
        pending.updateAndGet(_ ++ events): Unit
    }

  private def takePending(): Option[Event] = pending.getAndUpdate(_.drop(1)).headOption

  private def resizeEvent(): Option[Event] =
    if (resized.getAndSet(false)) {
      sizeQuery().filter(size => !size.isEmpty) match {
        case Some(size) => if (lastSize.getAndSet(size.some) =!= size.some) Event.resize(viewportSize(size)).some else none[Event]
        case None => none[Event]
      }
    } else {
      none[Event]
    }

  private def startDispatcher(): Unit =
    if (dispatching.compareAndSet(false, true)) {
      val thread = new Thread(() => dispatchLoop(), "stui-events")
      thread.setDaemon(true)
      thread.start()
    } else {
      ()
    }

  @tailrec
  private def dispatchLoop(): Unit =
    if (listeners.get().isEmpty) {
      dispatching.set(false)
    } else {
      poll(100.millis) match {
        case Some(event) => listeners.get().foreach { case (_, listener) => listener(event) }
        case None => ()
      }
      dispatchLoop()
    }

}

object DecodingEventSource {

  /** The `Resize` payload mapping of the D12 under-run rule: the terminal size on the alternate screen, the width with the height
    * clamped to the requested inline height in inline mode.
    */
  def effectiveSize(options: stui.core.spi.TerminalOptions): Size => Size = options.screenMode match {
    case stui.core.spi.ScreenMode.AlternateScreen => identity
    case stui.core.spi.ScreenMode.Inline(height) =>
      size =>
        Size(
          size.width,
          stui.core.internal.NonNegInts.min(stui.core.internal.NonNegInts.clamp(height.value.toLong), size.height),
        )
  }

}
