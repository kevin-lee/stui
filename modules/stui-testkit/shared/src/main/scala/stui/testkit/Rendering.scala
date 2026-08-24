package stui.testkit

import stui.core.buffer.Buffer
import stui.core.geometry.{Rect, Size}
import stui.core.widget.{StatefulWidget, Widget}
import stui.unicode.WidthPolicy

import java.util.concurrent.atomic.AtomicReference

/** Golden renderers: one widget into one fresh buffer at the origin, so specs write
  * `Assertions.grid(Rendering.widget(w, Size(...)), expected)`.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object Rendering {

  /** The widget rendered into a blank buffer of the size with [[WidthPolicy.default]]. */
  def widget(widget: Widget, size: Size): Buffer = widgetWith(WidthPolicy.default, widget, size)

  /** [[widget]] with the given policy. */
  def widgetWith(policy: WidthPolicy, widget: Widget, size: Size): Buffer =
    Buffer.emptyWith(policy, Rect.sized(size)).draw(canvas => widget.render(Rect.sized(size), canvas))

  /** The stateful widget rendered once into a blank buffer of the size, with the corrected state. */
  def stateful[S](widget: StatefulWidget[S], size: Size, state: S): (Buffer, S) = statefulWith(WidthPolicy.default, widget, size, state)

  /** [[stateful]] with the given policy. The state is captured through an `AtomicReference`, the render runs exactly once. */
  def statefulWith[S](policy: WidthPolicy, widget: StatefulWidget[S], size: Size, state: S): (Buffer, S) = {
    val corrected = new AtomicReference[S](state)
    val buffer    = Buffer
      .emptyWith(policy, Rect.sized(size))
      .draw(canvas => corrected.updateAndGet(s => widget.render(Rect.sized(size), canvas, s)): Unit)
    (buffer, corrected.get())
  }

}
