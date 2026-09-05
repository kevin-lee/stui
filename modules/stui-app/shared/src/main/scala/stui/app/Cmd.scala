package stui.app

import stui.core.spi.Subscription
import stui.core.widget.Widget

/** What `init` and `update` ask the runtime to do besides keeping the model (design doc 10, decisions D13, D20, D27, M3b). `None` is
  * nothing. `Emit` queues a message after the batch's pending messages (first in, first out, processed before the present). `Batch`
  * runs its commands in order (nesting has no effect, the associativity law of 12). `Exit` lets the batch complete, presents once, and
  * ends the loop (in-flight tasks and ticks cancelled best-effort). `Print` renders rows above the UI through the cell pipeline,
  * buffered under the alternate screen (7.2, D13). `Redraw` forces `RedrawReason.Requested` at the present. `Task` is the effect seam
  * of D27: `start` is called once with the callback and returns the cancel handle, the result becomes a message through `onResult`
  * and joins a later batch, and results after `Exit` are dropped; stui-effectie builds it from an `F[A]` through `UnsafeRun[F]` (8.2),
  * so no `F` exists here (8.1).
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
enum Cmd[+Msg] {
  case None
  case Emit(msg: Msg)
  case Batch(cmds: Vector[Cmd[Msg]])
  case Exit
  case Print(content: Widget)
  case Redraw
  case Task[A, M](start: (Either[Throwable, A] => Unit) => Subscription, onResult: Either[Throwable, A] => M) extends Cmd[M]
}

object Cmd {

  /** Nothing. */
  val none: Cmd[Nothing] = None

  /** One message queued into the batch. */
  def emit[Msg](msg: Msg): Cmd[Msg] = Emit(msg)

  /** The commands in order. */
  def batch[Msg](cmds: Cmd[Msg]*): Cmd[Msg] = Batch(cmds.toVector)

  /** The loop ends after this batch's present. */
  val exit: Cmd[Nothing] = Exit

  /** The widget printed above the UI. */
  def print(content: Widget): Cmd[Nothing] = Print(content)

  /** A full redraw at this batch's present. */
  val redraw: Cmd[Nothing] = Redraw

  /** A task started through `start`, its result turned into a message by `onResult`. */
  def task[A, Msg](start: (Either[Throwable, A] => Unit) => Subscription)(onResult: Either[Throwable, A] => Msg): Cmd[Msg] =
    Task(start, onResult)

}
