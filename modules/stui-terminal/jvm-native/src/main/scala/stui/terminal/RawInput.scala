package stui.terminal

import scala.concurrent.duration.FiniteDuration

/** Where raw input bytes come from on the JVM and Native: a reader thread's queue or a `poll` on the device (design doc 7).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
trait RawInput {

  /** The next chunk, or `None` when the timeout elapses first (an interrupted wait counts as `None`). */
  def poll(timeout: FiniteDuration): Option[IArray[Byte]]

}
