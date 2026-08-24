package stui.core.widget

import stui.core.buffer.Canvas
import stui.core.geometry.Rect

/** A [[Widget]] whose rendering also corrects a piece of state and hands it back functionally (design doc 6.4, fixing Ratatui's
  * `&mut`-threaded state, pain 3.1): a list that clamps its scroll offset during render returns the clamped offset.
  *
  * Contract, on top of [[Widget]]'s: rendering again with the returned state must return an equal state (state correction is
  * idempotent, the `WidgetLaws` fixed-point law in stui-testkit).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
trait StatefulWidget[S] {

  /** Draws this widget into the part of `area` that lies on the canvas and returns the corrected state. */
  def render(area: Rect, canvas: Canvas, state: S): S

}
