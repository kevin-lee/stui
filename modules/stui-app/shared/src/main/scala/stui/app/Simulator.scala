package stui.app

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.app.internal.{Inbox, Loop, Tasks, Ticks}
import stui.app.internal.Inbox.*
import stui.app.internal.Loop.Input
import stui.app.internal.Tasks.*
import stui.app.internal.Ticks.*
import stui.core.buffer.Buffer
import stui.core.event.Event
import stui.core.frame.{Frame, Regions}
import stui.core.geometry.{Offset, Rect, Size}
import stui.core.geometry.Position.*
import stui.core.spi.VirtualScheduler
import stui.core.terminal.{RedrawReason, Terminal}
import stui.unicode.WidthPolicy

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.{Duration, FiniteDuration}

/** The headless ignition (design doc 10 and 12, decision D28, M3c): the same `Loop.step`, `Ticks`, and `Tasks` as the drivers, the
  * batches made by a trace through the callback driver's inbox-and-zero-delay-flush mechanism over the scheduler passed in (so ticks
  * group by due instant exactly as both drivers group them and a synchronous task's result becomes the next flush's batch), and the
  * present through `Frame.draw` with no terminal anywhere: the frame is viewport-relative (origin at zero), the last `Resize` of a
  * batch replaces the viewport, prints render over the viewport size. One [[Simulator.Snapshot]] is taken for `init`'s present (the
  * first present happens before the first wait) and one per non-empty batch, the exit batch is the last one and the rest of the
  * trace is ignored, as the drivers stop. Determinism: the same trace over a fresh scheduler at the same start gives the same
  * snapshots on every platform. An exception from the application propagates out of `run`. Time comes from the scheduler alone,
  * `stui-app` holds no clock (M3b's rule), so an application test passes testkit's `ManualScheduler`.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object Simulator {

  /** One step of a trace: a batch of terminal events, or time passing on the scheduler (ticks due inside it fire into batches of
    * their own, grouped by due instant as the drivers group them).
    */
  enum Step derives Eq, Show, Hash {
    case Events(events: Vector[Event])
    case Advance(by: FiniteDuration)
  }

  object Step {

    /** One batch of the events. */
    def events(events: Event*): Step = Events(events.toVector)

    /** Time passing on the scheduler. */
    def advance(by: FiniteDuration): Step = Advance(by)

  }

  /** What one present left behind: the model the root's render handed back (the one the next batch starts from), the frame
    * (viewport-relative, origin at zero), the prints rendered over the viewport size and trimmed (empty prints dropped, as
    * `Terminal.print` drops them), the redraw reason recorded, whether the batch asked to exit, and the keys of the subscriptions
    * the model wants.
    */
  final case class Snapshot[Model](
    model: Model,
    frame: Frame,
    prints: Vector[Buffer],
    redraw: Option[RedrawReason],
    exit: Boolean,
    subscriptions: Set[SubKey],
  ) derives Eq,
        Show,
        Hash

  /** [[runWith]] under `WidthPolicy.default`. */
  def run[Model, Msg](
    app: StuiApp[Model, Msg],
    env: AppEnv,
    trace: Vector[Step],
    scheduler: VirtualScheduler,
  ): Vector[Snapshot[Model]] = runWith(WidthPolicy.default, app, env, trace, scheduler)

  /** [[run]] with every event its own batch and no time passing. */
  def runEvents[Model, Msg](
    app: StuiApp[Model, Msg],
    env: AppEnv,
    events: Vector[Event],
    scheduler: VirtualScheduler,
  ): Vector[Snapshot[Model]] = run(app, env, events.map(event => Step.Events(Vector(event))), scheduler)

  /** Folds the trace through the runtime headless and returns the snapshots in order, `init`'s present first. */
  def runWith[Model, Msg](
    policy: WidthPolicy,
    app: StuiApp[Model, Msg],
    env: AppEnv,
    trace: Vector[Step],
    scheduler: VirtualScheduler,
  ): Vector[Snapshot[Model]] = {
    val engine = new Engine(policy, app, env, scheduler)
    engine.start()
    trace.foreach(step => engine.step(step))
    engine.snapshots
  }

  /** The engine: the callback driver's mechanism over a script and a frame value. */
  final private class Engine[Model, Msg](
    private val policy: WidthPolicy,
    private val app: StuiApp[Model, Msg],
    private val env: AppEnv,
    private val scheduler: VirtualScheduler,
  ) {

    private val first: Loop.Stepped[Model, Msg] = Loop.start(app, env)

    private val model: AtomicReference[Model] = new AtomicReference(first.model)

    private val viewport: AtomicReference[Size] = new AtomicReference(env.viewport)

    private val previous: AtomicReference[Option[Frame]] = new AtomicReference(none[Frame])

    private val taken: AtomicReference[Vector[Snapshot[Model]]] = new AtomicReference(Vector.empty[Snapshot[Model]])

    private val ended: AtomicBoolean = new AtomicBoolean(false)

    private val armed: AtomicBoolean = new AtomicBoolean(false)

    private val inbox: Inbox[Input[Msg]] = Inbox.empty[Input[Msg]]

    private lazy val ticks: Ticks[Msg] = Ticks.of(scheduler, msg => post(Input.Message(msg)))

    private lazy val tasks: Tasks[Msg] = Tasks.of(msg => post(Input.Message(msg)))

    /** The snapshots so far. */
    def snapshots: Vector[Snapshot[Model]] = taken.get()

    /** `init`'s present. */
    def start(): Unit = {
      apply(first)
      if (first.exit) finish() else ()
    }

    /** One trace step, nothing after the exit batch. */
    def step(step: Step): Unit =
      if (ended.get()) {
        ()
      } else {
        step match {
          case Step.Events(events) =>
            events.foreach(event => post(Input.Received(event)))
            scheduler.advance(Duration.Zero)
          case Step.Advance(by) => scheduler.advance(by)
        }
      }

    private def post(input: Input[Msg]): Unit =
      if (ended.get()) {
        ()
      } else {
        inbox.post(input)
        arm()
      }

    private def arm(): Unit = if armed.compareAndSet(false, true) then scheduler.schedule(Duration.Zero, () => flush()): Unit else ()

    private def flush(): Unit = {
      armed.set(false)
      if (ended.get()) {
        ()
      } else {
        val batch = Loop.eventsFirst(inbox.drain())
        if (batch.isEmpty) {
          ()
        } else {
          batch.collect { case Input.Received(Event.Resize(size)) => size }.lastOption.foreach(size => viewport.set(size))
          val stepped = Loop.step(app, regions, model.get(), batch)
          apply(stepped)
          if (stepped.exit) finish() else ()
        }
      }
    }

    private def regions: Regions = previous.get().fold(Regions.empty)(_.regions)

    /* the drivers' order: the prints, the tasks started, the subscriptions reconciled, then the one present of the batch */
    private def apply(stepped: Loop.Stepped[Model, Msg]): Unit = {
      val prints    = stepped
        .prints
        .map(widget => Terminal.printRows(policy, viewport.get(), widget))
        .filter(rows => rows.area.height.value > 0)
      stepped.tasks.foreach(launch => tasks.start(launch))
      ticks.reconcile(stepped.subscriptions): Unit
      val view      = app.view(stepped.model)
      val corrected = new AtomicReference(stepped.model)
      val frame     = Frame.draw(Buffer.emptyWith(policy, Rect.sized(viewport.get()))) { canvas =>
        corrected.set(view.root.render(canvas.area, canvas, stepped.model))
        view.cursor.foreach(cursor => canvas.cursor(cursor.offset(Offset(canvas.area.x.value, canvas.area.y.value))))
      }
      model.set(corrected.get())
      previous.set(frame.some)
      taken.updateAndGet(
        _ :+ Snapshot(
          corrected.get(),
          frame,
          prints,
          stepped.redraw,
          stepped.exit,
          stepped.subscriptions.map(every => Sub.key(every)).toSet,
        )
      ): Unit
    }

    private def finish(): Unit = {
      ended.set(true)
      ticks.clear()
      tasks.cancelAll()
    }

  }

}
