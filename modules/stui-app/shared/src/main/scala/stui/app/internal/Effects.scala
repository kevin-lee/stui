package stui.app.internal

import stui.app.{StuiApp, View}
import stui.app.internal.Tasks.*
import stui.app.internal.Ticks.*
import stui.core.frame.{Frame, Regions}
import stui.core.geometry.Offset
import stui.core.geometry.Position.*
import stui.core.terminal.Terminal
import stui.core.terminal.Terminal.*

import java.util.concurrent.atomic.AtomicReference

/** The effect interpretation every driver shares (design doc 10, M3b), in this order: the prints, the redraw reason, the tasks
  * started, the subscriptions reconciled, and then the one present of the batch, whose render returns the model the next batch
  * starts from and whose explicit cursor is recorded last (D8 over D18).
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
private[stui] object Effects {

  /** Applies a step's effects to the terminal, the tasks, and the ticks. */
  def apply[Model, Msg](terminal: Terminal, ticks: Ticks[Msg], tasks: Tasks[Msg], stepped: Loop.Stepped[Model, Msg]): Unit = {
    stepped.prints.foreach(content => terminal.print(content))
    stepped.redraw.foreach(reason => terminal.redraw(reason))
    stepped.tasks.foreach(launch => tasks.start(launch))
    ticks.reconcile(stepped.subscriptions): Unit
  }

  /** Presents the model once and returns the model its root's render handed back with the presented frame (the drivers' `onPresent`
    * observation, M3c).
    */
  def present[Model, Msg](terminal: Terminal, app: StuiApp[Model, Msg], model: Model): (Model, Frame) = {
    val view: View[Model] = app.view(model)
    val corrected         = new AtomicReference(model)
    val completed         = terminal.draw { canvas =>
      corrected.set(view.root.render(canvas.area, canvas, model))
      view.cursor.foreach(cursor => canvas.cursor(cursor.offset(Offset(canvas.area.x.value, canvas.area.y.value))))
    }
    (corrected.get(), completed.frame)
  }

  /** The last presented frame's hit map, empty before the first present. */
  def regions(terminal: Terminal): Regions = terminal.lastFrame.fold(Regions.empty)(_.regions)

}
