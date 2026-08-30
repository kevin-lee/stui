package stui.widgets

import cats.syntax.all.*
import stui.core.buffer.{Buffer, Canvas}
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.Style
import stui.core.widget.{Measurable, StatefulWidget, Widget}
import stui.unicode.WidthPolicy

/** The app-owned scrollable middle area (design doc 6.6, decision D21, M2a): `content` is rendered into an off-screen buffer of
  * `contentSize` under the canvas's width policy, and the window at the offset is copied into the visible area
  * ([[stui.core.buffer.Canvas.copyFrom]]), so any widget scrolls. The state is the cell offset ([[Scroll]]), corrected by the
  * [[Scrolling]] clamp during render and handed back (the returned-state pattern): the app owns it, tests hammer it. The off-screen
  * render costs `contentSize` cells every frame, so unbounded logs belong to [[LogView]], which renders only its visible rows. The
  * content's recorded cursor and hit regions are discarded in M2a ([[stui.core.buffer.Buffer.draw]] semantics); the scroll view
  * records its own region over the whole target when one is configured, which is what wheel routing resolves through
  * `Regions.at`.
  *
  * No `Eq` / `Show` / `Hash` instances exist: `content` is an arbitrary [[stui.core.widget.Widget]], which has none by design, so
  * scroll views are compared by rendering, exactly as the widget laws do.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class ScrollView(
  content: Widget,
  contentSize: Size,
  block: Option[Block],
  style: Style,
  region: Option[RegionId],
) extends StatefulWidget[Scroll] {

  /* the method lives in the class body because it implements the StatefulWidget trait member */
  /** Patches `style` over the target, renders the block, clamps the offset against the block's inner area, copies the content
    * window there, records the region (over the whole target, border included), and returns the clamped offset. An empty target or
    * inner area corrects nothing and returns the state unchanged.
    */
  override def render(area: Rect, canvas: Canvas, state: Scroll): Scroll = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      state
    } else {
      canvas.patchStyle(target, style)
      block.foreach(_.render(target, canvas))
      val inner     = block.fold(target)(_.inner(target))
      val corrected =
        if (inner.isEmpty) {
          state
        } else {
          val clamped     = Scrolling.clampScroll(state, contentSize, inner.size)
          val contentArea = Rect.sized(contentSize)
          /* the style is patched onto the off-screen base as well, so the copy (which replaces cells) carries the same appearance
           * the Paragraph pipeline would give (M2a refinement R5) */
          val rendered    = Buffer.emptyWith(canvas.policy, contentArea).draw { offScreen =>
            offScreen.patchStyle(contentArea, style)
            content.render(contentArea, offScreen)
          }
          canvas.copyFrom(rendered, Position(clamped.columns, clamped.rows), inner)
          clamped
        }
      region.foreach(id => canvas.region(id, target))
      corrected
    }
  }
}

object ScrollView {

  /** The content alone: no block, no style, no region. */
  def of(content: Widget, contentSize: Size): ScrollView =
    ScrollView(content, contentSize, none[Block], Style.empty, none[RegionId])

  /** [[of]] with the content size measured from the content itself (D18). */
  def measuring(content: Widget & Measurable, constraints: Size, policy: WidthPolicy): ScrollView =
    of(content, content.measure(constraints, policy))

  extension (scrollView: ScrollView) {

    /** The scroll view in the block (the offset clamps against the block's inner area). */
    def withBlock(block: Block): ScrollView = scrollView.copy(block = block.some)

    /** The scroll view with the area style replaced. */
    def withStyle(style: Style): ScrollView = scrollView.copy(style = style)

    /** The scroll view recording its hit region under the id (wheel routing resolves it through `Regions.at`). */
    def withRegion(id: RegionId): ScrollView = scrollView.copy(region = id.some)

  }

}
