package stui.app.internal

import java.util.concurrent.atomic.AtomicReference

/** The messages and events waiting for the next batch (design doc 10, M3b): the one cross-thread structure of the blocking driver,
  * because a task callback may post from any thread, and single-threaded by construction on Node.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final private[stui] class Inbox[A] private (private val ref: AtomicReference[Vector[A]])

private[stui] object Inbox {

  /** Nothing waiting. */
  def empty[A]: Inbox[A] = new Inbox(new AtomicReference(Vector.empty[A]))

  extension [A](inbox: Inbox[A]) {

    /** Appends one item. */
    def post(a: A): Unit = inbox.ref.updateAndGet(_ :+ a): Unit

    /** Everything waiting, in posting order, and the inbox emptied. */
    def drain(): Vector[A] = inbox.ref.getAndSet(Vector.empty[A])

    /** True when nothing waits. */
    def isEmpty: Boolean = inbox.ref.get().isEmpty

  }

}
