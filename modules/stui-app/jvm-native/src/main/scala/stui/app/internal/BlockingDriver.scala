package stui.app.internal

import stui.app.{AppEnv, StuiApp}
import stui.app.internal.Inbox.*
import stui.app.internal.Loop.Input
import stui.app.internal.QueueScheduler.*
import stui.app.internal.Tasks.*
import stui.app.internal.Ticks.*
import stui.core.event.Event
import stui.core.frame.Frame
import stui.core.spi.BlockingEventSource
import stui.core.terminal.Terminal

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, FiniteDuration}

/** The blocking ignition of the JVM and Native (design doc 6.3 and 10, M3b): a single-threaded loop that presents, then polls for the
  * first event with the time to the earliest due tick as its timeout (capped, so a termination signal on Native and a task result
  * posted from another thread are noticed within the cap - nothing else wakes a blocked poll before M5's event source), drains
  * every further pending event without waiting, fires the due ticks, drains the inbox, and steps the batch once: at most one present
  * per batch, the last resize winning, a slow flush skipping intermediates (D20). A posted message never waits for the poll: while
  * the inbox holds one the poll timeout is zero, so a synchronous task's result is presented in the next batch at once, as the
  * zero-delay flush does on Node (M3c). The batch is the events then the inbox, the order rule of design doc 10, and `onPresent`
  * receives every presented frame (`_ => ()` in production). An exception from the application propagates through `Terminal.run`'s
  * restore to the caller of `Stui.run`.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
private[stui] object BlockingDriver {

  /** Runs the application to `Cmd.Exit` or the termination flag and returns the model the loop ended with. */
  def run[Model, Msg](
    app: StuiApp[Model, Msg],
    env: AppEnv,
    terminal: Terminal,
    events: BlockingEventSource,
    scheduler: QueueScheduler,
    terminating: () => Boolean,
    cap: FiniteDuration,
    onPresent: Frame => Unit,
  ): Model = {
    val inbox = Inbox.empty[Input[Msg]]
    val ticks = Ticks.of[Msg](scheduler, msg => inbox.post(Input.Message(msg)))
    val tasks = Tasks.of[Msg](msg => inbox.post(Input.Message(msg)))

    def cleanup(): Unit = {
      ticks.clear()
      tasks.cancelAll()
    }

    @tailrec
    def loop(model: Model): Model =
      if (terminating()) {
        cleanup()
        model
      } else {
        val timeout  = if (inbox.isEmpty) scheduler.timeoutUntilNextDue(cap) else Duration.Zero
        val first    = events.poll(timeout)
        val received = first.fold(Vector.empty[Event])(event => event +: drainEvents(events, Vector.empty[Event]))
        scheduler.drain()
        val batch    = received.map(event => Input.Received(event): Input[Msg]) ++ inbox.drain()
        if (batch.isEmpty) {
          loop(model)
        } else {
          val stepped            = Loop.step(app, Effects.regions(terminal), model, batch)
          Effects.apply(terminal, ticks, tasks, stepped)
          val (presented, frame) = Effects.present(terminal, app, stepped.model)
          onPresent(frame)
          if (stepped.exit) {
            cleanup()
            presented
          } else {
            loop(presented)
          }
        }
      }

    val first              = Loop.start(app, env)
    Effects.apply(terminal, ticks, tasks, first)
    val (presented, frame) = Effects.present(terminal, app, first.model)
    onPresent(frame)
    if (first.exit) {
      cleanup()
      presented
    } else {
      loop(presented)
    }
  }

  @tailrec
  private def drainEvents(events: BlockingEventSource, acc: Vector[Event]): Vector[Event] =
    events.poll(Duration.Zero) match {
      case Some(event) => drainEvents(events, acc :+ event)
      case None => acc
    }

}
