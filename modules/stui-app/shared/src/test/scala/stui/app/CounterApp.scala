package stui.app

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.event.{Event, KeyCode, KeyEvent, MouseEvent}
import stui.core.focus.FocusRing
import stui.core.focus.FocusRing.*
import stui.core.frame.{RegionId, Regions}
import stui.core.geometry.Size
import stui.core.style.Style
import stui.core.text.Line

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The model of the test application: a counter with the last size, the tick times, the print count, the last mouse hit, a focus
  * ring over two targets, the moves routed against it, and whether it asked to exit.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final case class CounterModel(
  count: Int,
  size: Size,
  ticks: Vector[FiniteDuration],
  ticking: Boolean,
  prints: Int,
  over: Option[RegionId],
  focus: FocusRing[Int],
  moved: Vector[Option[Int]],
  exit: Boolean,
) derives Eq,
      Show,
      Hash

/** The messages of the test application. */
enum CounterMsg derives Eq, Show, Hash {
  case Inc
  case Add(n: Int)
  case Quit
  case PrintIt
  case RedrawIt
  case ToggleTick
  case Ticked(now: FiniteDuration)
  case Resized(size: Size)
  case Crash
  case EmitTwice
  case AddTen
  case Later
  case Hit(over: Option[RegionId])
  case FocusNext
  case Move(target: Option[Int])
  case Scale(n: Int)
  case LaterScale
}

/** The test application shared by the loop and driver specs (design doc 10, M3b): `+` counts, `q` exits, `p` prints, `r` redraws,
  * `t` toggles a 10 ms tick, `!` throws, `e` emits two increments, `s` runs a task that completes at once, `d` runs a task whose
  * callback the test captures, `m` runs a deferred task whose result scales the count (the one message that does not commute with
  * `+`, for the order rule of M3c), Tab moves the focus ring, Down records the ring's current target, a resize records the size, a
  * mouse event records the region under it, and the root's render clamps the count at [[CounterApp.maxCount]] (the correction the
  * fixed-point contract allows).
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object CounterApp {

  /** The region the root records over its whole area. */
  val region: RegionId = RegionId("counter")

  /** The count the render clamps at. */
  val maxCount: Int = 100

  /* the deliberate throw for the restore-path examples */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def crash(): (CounterModel, Cmd[CounterMsg]) = throw new RuntimeException("counter crash") // scalafix:ok DisableSyntax.throw

  /** The application; `deferred` receives the callback of the `d` task, `exitAfterTicks` exits at that many ticks. */
  def of(
    deferred: AtomicReference[Option[Either[Throwable, Int] => Unit]],
    exitAfterTicks: Option[Int],
  ): StuiApp[CounterModel, CounterMsg] =
    new StuiApp[CounterModel, CounterMsg] {

      override def init(env: AppEnv): (CounterModel, Cmd[CounterMsg]) =
        (
          CounterModel(
            0,
            env.viewport,
            Vector.empty[FiniteDuration],
            false,
            0,
            none[RegionId],
            FocusRing.of(1, 2),
            Vector.empty[Option[Int]],
            false,
          ),
          Cmd.none,
        )

      override def onEvent(event: Event, model: CounterModel): Option[CounterMsg] = event match {
        case Event.Key(KeyEvent(KeyCode.Char('+'), _, _)) => CounterMsg.Inc.some
        case Event.Key(KeyEvent(KeyCode.Char('q'), _, _)) => CounterMsg.Quit.some
        case Event.Key(KeyEvent(KeyCode.Char('p'), _, _)) => CounterMsg.PrintIt.some
        case Event.Key(KeyEvent(KeyCode.Char('r'), _, _)) => CounterMsg.RedrawIt.some
        case Event.Key(KeyEvent(KeyCode.Char('t'), _, _)) => CounterMsg.ToggleTick.some
        case Event.Key(KeyEvent(KeyCode.Char('!'), _, _)) => CounterMsg.Crash.some
        case Event.Key(KeyEvent(KeyCode.Char('e'), _, _)) => CounterMsg.EmitTwice.some
        case Event.Key(KeyEvent(KeyCode.Char('s'), _, _)) => CounterMsg.AddTen.some
        case Event.Key(KeyEvent(KeyCode.Char('d'), _, _)) => CounterMsg.Later.some
        case Event.Key(KeyEvent(KeyCode.Char('m'), _, _)) => CounterMsg.LaterScale.some
        case Event.Key(KeyEvent(KeyCode.Tab, _, _)) => CounterMsg.FocusNext.some
        case Event.Key(KeyEvent(KeyCode.Down, _, _)) => CounterMsg.Move(model.focus.current).some
        case Event.Resize(size) => CounterMsg.Resized(size).some
        case Event.Key(_) | Event.Mouse(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => none[CounterMsg]
      }

      override def onMouse(mouse: MouseEvent, regions: Regions, model: CounterModel): Option[CounterMsg] =
        CounterMsg.Hit(regions.at(mouse.position)).some

      override def update(model: CounterModel, msg: CounterMsg): (CounterModel, Cmd[CounterMsg]) = msg match {
        case CounterMsg.Inc => (model.copy(count = model.count + 1), Cmd.none)
        case CounterMsg.Add(n) => (model.copy(count = model.count + n), Cmd.none)
        case CounterMsg.Quit => (model.copy(exit = true), Cmd.exit)
        case CounterMsg.PrintIt => (model.copy(prints = model.prints + 1), Cmd.print(Line.raw("printed")))
        case CounterMsg.RedrawIt => (model, Cmd.redraw)
        case CounterMsg.ToggleTick => (model.copy(ticking = !model.ticking), Cmd.none)
        case CounterMsg.Ticked(now) =>
          val ticked = model.copy(ticks = model.ticks :+ now)
          (ticked, if (exitAfterTicks.contains(ticked.ticks.length)) Cmd.exit else Cmd.none)
        case CounterMsg.Resized(size) => (model.copy(size = size), Cmd.none)
        case CounterMsg.Crash => crash()
        case CounterMsg.EmitTwice => (model, Cmd.batch(Cmd.emit(CounterMsg.Inc), Cmd.emit(CounterMsg.Inc)))
        case CounterMsg.AddTen =>
          (
            model,
            Cmd.task[Int, CounterMsg] { callback =>
              callback(10.asRight[Throwable])
              () => ()
            }(result => CounterMsg.Add(result.getOrElse(0))),
          )
        case CounterMsg.Later =>
          (
            model,
            Cmd.task[Int, CounterMsg] { callback =>
              deferred.set(callback.some)
              () => deferred.set(none[Either[Throwable, Int] => Unit])
            }(result => CounterMsg.Add(result.getOrElse(0))),
          )
        case CounterMsg.LaterScale =>
          (
            model,
            Cmd.task[Int, CounterMsg] { callback =>
              deferred.set(callback.some)
              () => deferred.set(none[Either[Throwable, Int] => Unit])
            }(result => CounterMsg.Scale(result.getOrElse(1))),
          )
        case CounterMsg.Scale(n) => (model.copy(count = model.count * n), Cmd.none)
        case CounterMsg.Hit(over) => (model.copy(over = over), Cmd.none)
        case CounterMsg.FocusNext => (model.copy(focus = model.focus.next), Cmd.none)
        case CounterMsg.Move(target) => (model.copy(moved = model.moved :+ target), Cmd.none)
      }

      override def view(model: CounterModel): View[CounterModel] =
        View.stateful { (area, canvas, current) =>
          canvas.putString(area.position, s"count ${current.count.toString}", Style.empty)
          canvas.region(region, area)
          current.copy(count = math.min(current.count, maxCount))
        }

      override def subscriptions(model: CounterModel): Sub[CounterMsg] =
        if (model.ticking) Sub.every(10.millis)(now => CounterMsg.Ticked(now)) else Sub.none

    }

}
