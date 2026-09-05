package stui.app.internal

import stui.core.spi.Subscription

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** The starter of the task seam (design doc 8.3 and 10, decision D27, M3b): a task's result is posted as a message, the running
  * handles are kept for the best-effort cancel at exit, and a completed task leaves nothing behind (the callback removes its id before
  * posting, so a synchronous completion never stores a handle). A result arriving after [[Tasks.cancelAll]] is still posted and the
  * driver's ended flag drops it.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final private[stui] class Tasks[Msg] private (
  private val post: Msg => Unit,
  private val running: AtomicReference[Map[Long, Subscription]],
  private val ids: AtomicLong,
)

private[stui] object Tasks {

  private val placeholder: Subscription = () => ()

  /** No task running, results posted through `post`. */
  def of[Msg](post: Msg => Unit): Tasks[Msg] = new Tasks(post, new AtomicReference(Map.empty[Long, Subscription]), new AtomicLong(0L))

  extension [Msg](tasks: Tasks[Msg]) {

    /** Starts the task and keeps its handle while it runs. */
    def start(launch: Loop.Launch[Msg]): Unit = {
      val id     = tasks.ids.getAndIncrement()
      tasks.running.updateAndGet(_ + (id -> placeholder)): Unit
      val handle = launch { msg =>
        tasks.running.updateAndGet(_ - id): Unit
        tasks.post(msg)
      }
      tasks.running.updateAndGet(map => if (map.contains(id)) map.updated(id, handle) else map): Unit
    }

    /** Cancels every running task, best effort, and forgets them. */
    def cancelAll(): Unit = tasks.running.getAndSet(Map.empty[Long, Subscription]).values.foreach(_.cancel())

    /** The number of tasks still running. */
    def runningCount: Int = tasks.running.get().size

  }

}
