package stui.terminal.internal

import stui.terminal.RawInput
import stui.unicode.internal.IntOps.*

import java.io.{FileDescriptor, FileInputStream}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec

/** Standard input as a [[RawInput]] (the M0 recipe): one daemon thread blocks in `read` and queues each chunk, `poll` waits on the
  * queue. The thread starts on the first poll and ends at the end of the stream.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
private[stui] object StdinReader {

  private val BufferSize: Int = 4096

  private val queue: LinkedBlockingQueue[IArray[Byte]] = new LinkedBlockingQueue[IArray[Byte]]()

  private val started: AtomicBoolean = new AtomicBoolean(false)

  /** The chunks of standard input. */
  val input: RawInput = timeout => {
    start()
    Option(queue.poll(timeout.toMillis, TimeUnit.MILLISECONDS))
  }

  private def start(): Unit =
    if (started.compareAndSet(false, true)) {
      /* the read buffer the thread fills (the documented mutation site) */
      val thread = new Thread(() => readLoop(new FileInputStream(FileDescriptor.in), new Array[Byte](BufferSize)), "stui-stdin")
      thread.setDaemon(true)
      thread.start()
    } else {
      ()
    }

  @tailrec
  private def readLoop(in: FileInputStream, buffer: Array[Byte]): Unit = {
    val n = in.read(buffer)
    if (n > 0) {
      queue.put(IArray.unsafeFromArray(java.util.Arrays.copyOf(buffer, n)))
      readLoop(in, buffer)
    } else if (n === 0) {
      readLoop(in, buffer)
    } else {
      ()
    }
  }

}
