package stui.core.widget

import stui.core.geometry.Size
import stui.unicode.WidthPolicy

/** The optional intrinsic-size trait (design doc 6.4, decision D18), deliberately outside [[Widget]] so the four widget contract laws
  * are untouched by it. `measure` answers the size the widget needs, at most the constraints, under the given width policy (the
  * policy is an explicit parameter because a text widget cannot know cluster widths without one, refinement R1 of M2a).
  *
  * The laws (`MeasurableLaws` in stui-testkit): the answer fits the constraints, is deterministic, and rendering in any area at
  * least the measured size changes, per row, only cells within one measured-width span, over at most the measured height's rows
  * (modulo the one-cell wide-glyph edge repair of the canvas). Consumers: auto-sized status lines, dialog content, and the print
  * height. A widget that owns its whole area (a full-area style patch, perimeter borders - `Paragraph`, `Block`, the scroll-view
  * family) must not implement this trait, because its rendering cannot satisfy the row-span law.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
trait Measurable {

  /** The size this widget needs within `constraints` under `policy`: at most the constraints on both axes. */
  def measure(constraints: Size, policy: WidthPolicy): Size

}
