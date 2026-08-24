package stui.core.widget

import stui.core.buffer.Canvas
import stui.core.geometry.Rect

/** What every drawable value implements (design doc 6.4): render into the one mutable scope of the pipeline, the [[Canvas]] that
  * [[stui.core.buffer.Buffer.draw]] opens. Widgets are immutable values built cheaply per frame and shared freely.
  *
  * Contract: draw only inside `area` intersected with `canvas.area`, and be total on every area, the empty and the off-canvas ones
  * included. One documented spill exists: a write on the area's edge over a straddling two-column glyph makes the canvas's invariant
  * repair blank the cell immediately outside that edge ([[stui.core.buffer.Canvas]] semantics). Implementations override `render` in
  * the class body because it is a trait member (the one carve-out from the extensions-in-companions rule).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
trait Widget {

  /** Draws this widget into the part of `area` that lies on the canvas. */
  def render(area: Rect, canvas: Canvas): Unit

}
