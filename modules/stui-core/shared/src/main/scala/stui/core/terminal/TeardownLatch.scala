package stui.core.terminal

import java.util.concurrent.atomic.AtomicInteger

/** The late-signal latch of design doc 7.4 (decision D23): a signal that lands during teardown is recorded here instead of aborting the
  * cleanup, and the platform bracket exits with 128 plus the signal number once the cleanup is complete. [[TeardownLatch.record]] is
  * one atomic store with no allocation, so a Scala Native signal handler may call it (signal-safety(7)). Only the Native path records: on
  * the JVM the default SIGTERM handling runs the shutdown hook, which closes the terminal, and the JVM exits 143 on its own.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class TeardownLatch private (private val signal: AtomicInteger)

object TeardownLatch {

  /** A latch with nothing recorded. */
  def apply(): TeardownLatch = new TeardownLatch(new AtomicInteger(0))

  extension (latch: TeardownLatch) {

    /** Records the signal number, the first one wins, numbers below 1 are ignored. Allocation-free. */
    def record(signalNumber: Int): Unit = if (signalNumber > 0) (latch.signal.compareAndSet(0, signalNumber): Unit) else ()

    /** The recorded signal number, if any. */
    def recorded: Option[Int] = {
      val n = latch.signal.get()
      Option.when(n > 0)(n)
    }

  }

}
