package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.{NonNegInt, NonNegLong}
import stui.core.buffer.Canvas
import stui.core.frame.RegionId
import stui.core.geometry.Rect
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.core.widget.StatefulWidget
import stui.widgets.internal.WordWrap

/** What a [[LogView]] remembers between frames (design doc 6.6, M2a): `anchor` is the absolute index ([[LogRing.firstIndex]]-based)
  * of the first visible line, so the view stays still while the ring evicts under it, and `following` pins the view to the tail.
  * Render corrects the anchor (the returned-state pattern) and never flips `following`; only the [[LogView]] scroll helpers do -
  * scrolling up releases it, scrolling down to or past the bottom regains it.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class LogViewState(anchor: NonNegLong, following: Boolean) derives Eq, Show, Hash

object LogViewState {

  /** Following the tail (the fresh-log default): the anchor is corrected to the newest window at every render. */
  val following: LogViewState = LogViewState(NonNegLong(0L), true)

  /** Anchored at the absolute line index, not following. */
  def anchoredAt(anchor: NonNegLong): LogViewState = LogViewState(anchor, false)

}

/** The [[ScrollView]] specialisation for append-only lines (design doc 6.6, decision D21, M2a): the [[LogRing]] bounds the backing
  * store, only the visible rows are rendered (no off-screen buffer, so a large ring is cheap), each line truncated at the inner
  * width ([[Paragraph]]'s no-wrap semantics; wrapping is a later option). The absolute [[LogViewState]] anchor keeps the view still
  * while the ring evicts. Follow-the-tail is released by [[LogView.scrolledUp]] and regained by [[LogView.scrolledDown]] reaching
  * the bottom.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class LogView(
  ring: LogRing,
  block: Option[Block],
  style: Style,
  region: Option[RegionId],
) extends StatefulWidget[LogViewState] derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the StatefulWidget trait member */
  /** Patches `style` over the target, renders the block, slices the visible lines at the corrected anchor into the inner area (each
    * truncated at the inner width), records the region (over the whole target), and returns the corrected state: the anchor clamped
    * into the ring's kept range (an anchor evicted past pins to the oldest kept line), or the newest window when following. An
    * empty target or inner area corrects nothing and returns the state unchanged.
    */
  override def render(area: Rect, canvas: Canvas, state: LogViewState): LogViewState = {
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
          val visible = inner.height.value
          val relMax  = math.max(0, ring.size - visible)
          val rel     =
            if (state.following) relMax
            else math.min(relMax.toLong, math.max(0L, state.anchor.value - ring.firstIndex.value)).toInt
          ring.lines.slice(rel, rel + visible).zipWithIndex.foreach {
            case (line, i) =>
              val row = Rect(inner.x, NonNegInts.clamp(inner.y.value.toLong + i.toLong), inner.width, NonNegInt(1))
              WordWrap.truncate(line, inner.width.value, 0, canvas.policy).render(row, canvas)
          }
          LogViewState(LogRing.nonNegLong(ring.firstIndex.value + rel.toLong), state.following)
        }
      region.foreach(id => canvas.region(id, target))
      corrected
    }
  }
}

object LogView {

  /** The ring alone: no block, no style, no region. */
  def of(ring: LogRing): LogView = LogView(ring, none[Block], Style.empty, none[RegionId])

  /** The largest useful anchor: the absolute index of the top line of the newest window. */
  def maxAnchor(ring: LogRing, viewportRows: NonNegInt): NonNegLong =
    LogRing.nonNegLong(ring.firstIndex.value + math.max(0, ring.size - viewportRows.value).toLong)

  /** The state scrolled up by `rows` from its effective position (the bottom when following): follow is released, the anchor floors
    * at the oldest kept line. `rows` 0 changes nothing.
    */
  def scrolledUp(state: LogViewState, ring: LogRing, viewportRows: NonNegInt, rows: NonNegInt): LogViewState =
    if (rows.value === 0) {
      state
    } else {
      LogViewState.anchoredAt(LogRing.nonNegLong(math.max(ring.firstIndex.value, base(state, ring, viewportRows) - rows.value.toLong)))
    }

  /** The state scrolled down by `rows` from its effective position: reaching or passing the bottom regains follow (the overshoot
    * rule), anything short stays anchored. `rows` 0 changes nothing.
    */
  def scrolledDown(state: LogViewState, ring: LogRing, viewportRows: NonNegInt, rows: NonNegInt): LogViewState =
    if (rows.value === 0) {
      state
    } else {
      val max = maxAnchor(ring, viewportRows)
      val raw = base(state, ring, viewportRows) + rows.value.toLong
      if (raw >= max.value) LogViewState(max, true) else LogViewState.anchoredAt(LogRing.nonNegLong(raw))
    }

  /** Anchored at the oldest kept line, not following. */
  def toTop(ring: LogRing): LogViewState = LogViewState.anchoredAt(ring.firstIndex)

  /** Following the tail (the anchor is corrected at the next render). */
  val toBottom: LogViewState = LogViewState.following

  /** The effective anchor scrolling starts from: the bottom when following, else the stored anchor clamped into the kept range. */
  private def base(state: LogViewState, ring: LogRing, viewportRows: NonNegInt): Long = {
    val max = maxAnchor(ring, viewportRows).value
    if (state.following) max else math.min(max, math.max(ring.firstIndex.value, state.anchor.value))
  }

  extension (logView: LogView) {

    /** The log view in the block (the visible rows come from the block's inner area). */
    def withBlock(block: Block): LogView = logView.copy(block = block.some)

    /** The log view with the area style replaced. */
    def withStyle(style: Style): LogView = logView.copy(style = style)

    /** The log view recording its hit region under the id (wheel routing resolves it through `Regions.at`). */
    def withRegion(id: RegionId): LogView = logView.copy(region = id.some)

  }

}
