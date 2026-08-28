package stui.core.spi

/** A monotonic time source for render statistics and timeouts (design doc 7.1 and 8.1): never wall-clock time, so a deterministic clock
  * can drive tests. The M3 Scheduler extends it.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
trait Clock {

  /** Nanoseconds from an arbitrary origin, never decreasing. */
  def monotonicNanos(): Long

}

object Clock {

  /** The platform's monotonic clock (`System.nanoTime`, available on JVM, Scala.js, and Scala Native). */
  val system: Clock = () => System.nanoTime()

}
