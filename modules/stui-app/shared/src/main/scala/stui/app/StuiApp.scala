package stui.app

import stui.core.event.{Event, MouseEvent}
import stui.core.frame.Regions

import scala.annotation.unused

/** The Elm Architecture (TEA) contract (design doc 10, decisions D8, D20, D25, D27, M3b): an application is these pure functions and
  * a subscription value, and never touches a terminal. The runtime rules: `update` and `view` never throw (an escaping exception ends
  * the program through the restore path and is rethrown after it, there is no error hook); the loop drains everything pending into
  * one batch, folds each input in order - the message derived from the model the previous input's updates produced, then `update` to
  * the fixed point of the emitted messages - and presents once (the last `Resize` wins, a slow flush skips intermediates); a batch
  * folds its terminal events before its posted messages, each kind in arrival order, on every driver and in the simulator (M3c);
  * mouse events reach [[onMouse]] with the last presented frame's hit map and every other event reaches [[onEvent]]; the model
  * `view`'s root returns from its render is the one the next batch starts from; the first present happens before the first wait.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
trait StuiApp[Model, Msg] {

  /** The first model and command, from the session's capabilities, screen mode, and viewport size. */
  def init(env: AppEnv): (Model, Cmd[Msg])

  /** The message an event means to this model, `None` to ignore it. The runtime routes mouse events through [[onMouse]] instead. */
  def onEvent(event: Event, model: Model): Option[Msg]

  /** The message a mouse event means to this model given the last presented frame's hit map (`Regions.at(mouse.position)` names the
    * region under the mouse, `Regions.empty` before the first present). The default hands the event to [[onEvent]].
    */
  def onMouse(mouse: MouseEvent, @unused regions: Regions, model: Model): Option[Msg] = onEvent(Event.mouse(mouse), model)

  /** The next model and command for a message. Pure, never throws. */
  def update(model: Model, msg: Msg): (Model, Cmd[Msg])

  /** The root to render and the optional viewport-relative cursor. Pure, never throws. */
  def view(model: Model): View[Model]

  /** The subscriptions this model wants, reconciled by key after every batch. Nothing by default. */
  def subscriptions(@unused model: Model): Sub[Msg] = Sub.none

}
