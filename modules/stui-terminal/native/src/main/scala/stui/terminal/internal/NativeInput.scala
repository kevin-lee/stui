package stui.terminal.internal

import cats.syntax.all.*
import stui.terminal.RawInput
import stui.unicode.internal.IntOps.*

import scala.concurrent.duration.FiniteDuration
import scala.scalanative.posix.{poll, unistd}
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Standard input as a [[RawInput]] on Native (the M0 recipe): `poll` on file descriptor 0 with the timeout, then one `read` of up to
  * 4 096 bytes. A signal interrupting the wait counts as a timeout, so the caller re-checks its flags.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
private[stui] object NativeInput {

  private val BufferSize: Int = 4096

  /** The chunks of standard input. */
  val input: RawInput = timeout => read(timeout)

  private def read(timeout: FiniteDuration): Option[IArray[Byte]] = {
    val fds          = stackalloc[poll.struct_pollfd]()
    fds._1 = 0
    fds._2 = poll.POLLIN.toShort
    fds._3 = 0.toShort
    val milliseconds = math.max(0L, math.min(timeout.toMillis, Int.MaxValue.toLong)).toInt
    val ready        = poll.poll(fds, 1.toUInt, milliseconds)
    if (ready > 0 && ((fds._3.toInt & poll.POLLIN) !== 0)) {
      val buffer = stackalloc[Byte](BufferSize)
      val count  = unistd.read(0, buffer, BufferSize.toUSize)
      if (count > 0) IArray.tabulate(count)(i => buffer(i)).some else none[IArray[Byte]]
    } else {
      none[IArray[Byte]]
    }
  }

}
