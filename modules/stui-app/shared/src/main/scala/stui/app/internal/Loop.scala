package stui.app.internal

import cats.syntax.all.*
import stui.app.{AppEnv, Cmd, StuiApp, Sub}
import stui.core.event.Event
import stui.core.frame.Regions
import stui.core.spi.Subscription
import stui.core.terminal.RedrawReason
import stui.core.widget.Widget

import scala.annotation.tailrec

/** The one pure step of the runtime (design doc 10, decision D20, M3b), which every driver and the M3c simulator fold: the inputs of
  * a batch in order, each routed against the model the previous input's updates produced (D25's routing rule under batching), every
  * message to the fixed point of the messages it emits, and the effects as data - so at most one present happens per batch by
  * construction, the present being the driver's. `Event.Resize` records the `Resize` redraw reason and `Cmd.Redraw` the `Requested`
  * one, the later replacing the earlier. Nothing here catches an exception.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
private[stui] object Loop {

  /** How a driver starts a task: given the message poster, the cancel handle. */
  type Launch[Msg] = (Msg => Unit) => Subscription

  /** One input of a batch: an event from the terminal or a message posted by a tick or a task. */
  enum Input[+Msg] {
    case Received(event: Event)
    case Message(msg: Msg)
  }

  /** The outcome of a batch: the model to present, the effects in order, and the subscriptions the final model wants. */
  final case class Stepped[Model, Msg](
    model: Model,
    prints: Vector[Widget],
    redraw: Option[RedrawReason],
    tasks: Vector[Launch[Msg]],
    exit: Boolean,
    subscriptions: Vector[Sub.Every[Msg]],
  )

  final private case class Acc[Model, Msg](
    model: Model,
    prints: Vector[Widget],
    redraw: Option[RedrawReason],
    tasks: Vector[Launch[Msg]],
    exit: Boolean,
  )

  /** The batch order rule (design doc 10, M3c): the terminal events in arrival order, then the posted messages (tick and task
    * results) in arrival order, the same in every driver and in the simulator, so the fold of a batch does not depend on the ignition.
    */
  def eventsFirst[Msg](inputs: Vector[Input[Msg]]): Vector[Input[Msg]] = {
    val (events, messages) = inputs.partition {
      case Input.Received(_) => true
      case Input.Message(_) => false
    }
    events ++ messages
  }

  /** `init` and its command run to the fixed point. */
  def start[Model, Msg](app: StuiApp[Model, Msg], env: AppEnv): Stepped[Model, Msg] = {
    val (model, cmd)   = app.init(env)
    val (acc, emitted) = interpret(fresh(model), Vector(cmd), Vector.empty[Msg])
    finish(app, drainMessages(app, acc, emitted))
  }

  /** One batch folded in order over the model, the mouse inputs routed with `regions`. */
  def step[Model, Msg](app: StuiApp[Model, Msg], regions: Regions, model: Model, inputs: Vector[Input[Msg]]): Stepped[Model, Msg] =
    finish(app, inputs.foldLeft(fresh[Model, Msg](model))(route(app, regions)))

  private def fresh[Model, Msg](model: Model): Acc[Model, Msg] =
    Acc(model, Vector.empty[Widget], none[RedrawReason], Vector.empty[Launch[Msg]], false)

  private def route[Model, Msg](app: StuiApp[Model, Msg], regions: Regions)(acc: Acc[Model, Msg], input: Input[Msg]): Acc[Model, Msg] =
    input match {
      case Input.Received(Event.Mouse(mouse)) =>
        drainMessages(app, acc, app.onMouse(mouse, regions, acc.model).fold(Vector.empty[Msg])(Vector(_)))
      case Input.Received(event @ Event.Resize(_)) =>
        drainMessages(app, acc.copy(redraw = RedrawReason.Resize.some), app.onEvent(event, acc.model).fold(Vector.empty[Msg])(Vector(_)))
      case Input.Received(event) =>
        drainMessages(app, acc, app.onEvent(event, acc.model).fold(Vector.empty[Msg])(Vector(_)))
      case Input.Message(msg) => drainMessages(app, acc, Vector(msg))
    }

  @tailrec
  private def drainMessages[Model, Msg](app: StuiApp[Model, Msg], acc: Acc[Model, Msg], queue: Vector[Msg]): Acc[Model, Msg] =
    queue match {
      case msg +: rest =>
        val (model, cmd)    = app.update(acc.model, msg)
        val (next, emitted) = interpret(acc.copy(model = model), Vector(cmd), Vector.empty[Msg])
        drainMessages(app, next, rest ++ emitted)
      case _ => acc
    }

  @tailrec
  private def interpret[Model, Msg](
    acc: Acc[Model, Msg],
    work: Vector[Cmd[Msg]],
    emitted: Vector[Msg],
  ): (Acc[Model, Msg], Vector[Msg]) =
    work match {
      case cmd +: rest =>
        cmd match {
          case Cmd.None => interpret(acc, rest, emitted)
          case Cmd.Emit(msg) => interpret(acc, rest, emitted :+ msg)
          case Cmd.Batch(cmds) => interpret(acc, cmds ++ rest, emitted)
          case Cmd.Exit => interpret(acc.copy(exit = true), rest, emitted)
          case Cmd.Print(content) => interpret(acc.copy(prints = acc.prints :+ content), rest, emitted)
          case Cmd.Redraw => interpret(acc.copy(redraw = RedrawReason.Requested.some), rest, emitted)
          case Cmd.Task(start, onResult) =>
            val launch: Launch[Msg] = post => start(result => post(onResult(result)))
            interpret(acc.copy(tasks = acc.tasks :+ launch), rest, emitted)
        }
      case _ => (acc, emitted)
    }

  private def finish[Model, Msg](app: StuiApp[Model, Msg], acc: Acc[Model, Msg]): Stepped[Model, Msg] =
    Stepped(acc.model, acc.prints, acc.redraw, acc.tasks, acc.exit, app.subscriptions(acc.model).leaves)

}
