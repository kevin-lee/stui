package stui.terminal.internal

import java.util.concurrent.atomic.AtomicBoolean

/** The JVM resize signal (design doc 7, M0-verified on Java 21): a `WINCH` handler that only sets a flag, which the event source turns
  * into a `Resize` event on its next poll. Termination stays with the JVM's default handling, which runs the shutdown hook.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
private[stui] object JvmSignals {

  /** Set by the handler, cleared by the event source. */
  val resized: AtomicBoolean = new AtomicBoolean(false)

  private val installed: AtomicBoolean = new AtomicBoolean(false)

  /** Installs the `WINCH` handler once. */
  def installWinch(): Unit =
    if (installed.compareAndSet(false, true)) {
      sun.misc.Signal.handle(new sun.misc.Signal("WINCH"), _ => resized.set(true)): Unit
    } else {
      ()
    }

}
