package stui.app

import cats.syntax.all.*
import stui.core.buffer.Canvas
import stui.core.geometry.{Position, Rect}
import stui.core.widget.{StatefulWidget, Widget}

/** The output of `StuiApp.view` (design doc 10, decisions D8 and D18, M3b): the root is rendered over the viewport and returns the
  * corrected model - the returned-state pattern of 6.4 lifted to the application, so a list's clamped selection reaches the model
  * without the application repeating its layout in `update` - and the next batch starts from that model (rendering again with it
  * must return an equal model, the fixed-point contract). The cursor is viewport-relative: the runtime adds the viewport's origin and
  * records it last, so it overrides a cursor a widget recorded during render, `None` keeps the recorded one, and neither means
  * hidden.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final case class View[Model](root: StatefulWidget[Model], cursor: Option[Position])

object View {

  /** A view over a stateless widget: the model comes back unchanged. */
  def of[Model](widget: Widget): View[Model] =
    View[Model](
      (area: Rect, canvas: Canvas, model: Model) => {
        widget.render(area, canvas)
        model
      },
      none[Position],
    )

  /** A view over a stateful root and no explicit cursor. */
  def stateful[Model](root: StatefulWidget[Model]): View[Model] = View[Model](root, none[Position])

  extension [Model](view: View[Model]) {

    /** The view with the explicit, viewport-relative cursor. */
    def withCursor(position: Position): View[Model] = view.copy(cursor = position.some)

    /** The view without an explicit cursor (the recorded one, if any, is used). */
    def withoutCursor: View[Model] = view.copy(cursor = none[Position])

  }

}
