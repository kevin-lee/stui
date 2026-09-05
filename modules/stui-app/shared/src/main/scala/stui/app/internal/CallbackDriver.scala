package stui.app.internal

import cats.syntax.all.*
import stui.app.{AppEnv, StuiApp}
import stui.app.internal.Inbox.*
import stui.app.internal.Loop.Input
import stui.app.internal.Tasks.*
import stui.app.internal.Ticks.*
import stui.core.spi.{EventSource, Scheduler, Subscription}
import stui.core.terminal.Terminal

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.Duration

/** The callback ignition (design doc 6.3 and 10, M3b), Node's and any push source's: every listener call posts into the inbox and
  * arms one zero-delay flush tick through the scheduler if none is armed, and the flush drains the inbox into one batch, steps,
  * applies the effects, and presents once - so Node batches like the blocking driver does and the D20 laws hold there. Under
  * `ManualScheduler` a test drives the flush with `advance(Duration.Zero)`, which makes this driver testable on every platform. An
  * exception from the application escapes the flush, that is the scheduler's callback, which on Node is the uncaught path the exit
  * hook restores from.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
private[stui] object CallbackDriver {

  /** Starts the application: the first present, then the subscription to the events. `onExit` receives the final model after
    * `Cmd.Exit` has been presented, with the event subscription cancelled and the ticks and tasks cancelled.
    */
  def start[Model, Msg](
    app: StuiApp[Model, Msg],
    env: AppEnv,
    terminal: Terminal,
    events: EventSource,
    scheduler: Scheduler,
    onExit: Model => Unit,
  ): Unit = {
    val ended        = new AtomicBoolean(false)
    val armed        = new AtomicBoolean(false)
    val inbox        = Inbox.empty[Input[Msg]]
    val subscription = new AtomicReference(none[Subscription])
    val first        = Loop.start(app, env)
    val current      = new AtomicReference(first.model)

    lazy val ticks: Ticks[Msg] = Ticks.of(scheduler, msg => post(Input.Message(msg)))
    lazy val tasks: Tasks[Msg] = Tasks.of(msg => post(Input.Message(msg)))

    def post(input: Input[Msg]): Unit =
      if (ended.get()) {
        ()
      } else {
        inbox.post(input)
        arm()
      }

    def arm(): Unit = if armed.compareAndSet(false, true) then scheduler.schedule(Duration.Zero, () => flush()): Unit else ()

    def flush(): Unit = {
      armed.set(false)
      if (ended.get()) {
        ()
      } else {
        val batch = inbox.drain()
        if (batch.isEmpty) {
          ()
        } else {
          val stepped   = Loop.step(app, Effects.regions(terminal), current.get(), batch)
          Effects.apply(terminal, ticks, tasks, stepped)
          val presented = Effects.present(terminal, app, stepped.model)
          current.set(presented)
          if (stepped.exit) finish(presented) else ()
        }
      }
    }

    def finish(model: Model): Unit = {
      ended.set(true)
      subscription.get().foreach(_.cancel())
      ticks.clear()
      tasks.cancelAll()
      onExit(model)
    }

    Effects.apply(terminal, ticks, tasks, first)
    val presented = Effects.present(terminal, app, first.model)
    current.set(presented)
    if (first.exit) finish(presented) else subscription.set(events.subscribe(event => post(Input.Received(event))).some)
  }

}
