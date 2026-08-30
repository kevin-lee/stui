package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.style.Style
import stui.core.text.{Line, Text}
import stui.core.widget.StatefulWidget
import stui.widgets.internal.ItemWindow

import scala.annotation.tailrec

/** The selectable, scrollable list of the catalogue (design doc 6.6 and 6.7, M2b; the design doc's `List`, renamed because
  * `stui.widgets.List` would shadow `scala.List` under a wildcard import). The state is a [[Selection]] corrected during render (the
  * returned-state pattern): the selection clamps to the item count, the offset clamps to the largest useful one and follows the
  * selection with `scrollPadding` items of context. Item heights are their line counts (at least one row each). With a highlight
  * symbol and a [[HighlightSpacing]] that reserves it, the content shifts right by the symbol's width, the selected item's first row
  * (every row with `repeatHighlightSymbol`) carries the symbol, and `highlightStyle` is patched over the selected item's whole rows
  * after its content. [[ListDirection.BottomToTop]] stacks the items upward from the bottom edge. With a `region`, the base id covers
  * the whole target and every drawn item is tagged `base[index]` ([[ItemRegions]]). The list owns its area (style patch, block), so
  * it does not implement `Measurable` (design doc 6.4, R3).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class ListView(
  items: Vector[ListItem],
  block: Option[Block],
  style: Style,
  highlightStyle: Style,
  highlightSymbol: Option[Line],
  repeatHighlightSymbol: Boolean,
  highlightSpacing: HighlightSpacing,
  scrollPadding: NonNegInt,
  direction: ListDirection,
  region: Option[RegionId],
) extends StatefulWidget[Selection] derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the StatefulWidget trait member */
  /** Patches `style` over the target, renders the block, records the base region, corrects the state against the inner area, draws
    * the visible items from the corrected offset, and returns the corrected state. An empty target or inner area corrects nothing and
    * returns the state unchanged.
    */
  override def render(area: Rect, canvas: Canvas, state: Selection): Selection = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      state
    } else {
      canvas.patchStyle(target, style)
      block.foreach(_.render(target, canvas))
      val inner = block.fold(target)(_.inner(target))
      region.foreach(base => canvas.region(base, target))
      if (inner.isEmpty) {
        state
      } else {
        val corrected     = ItemWindow.correct(state, items.map(_.height), inner.height.value, scrollPadding.value)
        val symbolWidth   = highlightSymbol.fold(0)(_.width(canvas.policy))
        val reserve       = highlightSpacing match {
          case HighlightSpacing.Always => true
          case HighlightSpacing.WhenSelected => corrected.selected.isDefined
          case HighlightSpacing.Never => false
        }
        val symbolColumns = if (reserve) math.min(symbolWidth, inner.width.value) else 0
        ListView.renderItems(this, canvas, inner, corrected, symbolColumns, corrected.offset.value, 0)
        corrected
      }
    }
  }
}

object ListView {

  /** The items alone: no block, empty styles, no symbol, [[HighlightSpacing.WhenSelected]], no padding, top to bottom, no region. */
  def of(items: Vector[ListItem]): ListView =
    ListView(
      items,
      none[Block],
      Style.empty,
      Style.empty,
      none[Line],
      false,
      HighlightSpacing.WhenSelected,
      NonNegInt(0),
      ListDirection.TopToBottom,
      none[RegionId],
    )

  /** [[of]] over unstyled items. */
  def fromTexts(texts: Vector[Text]): ListView = of(texts.map(ListItem.of))

  /** [[of]] over [[ListItem.raw]] items. */
  def raw(items: String*): ListView = of(items.toVector.map(ListItem.raw))

  extension (view: ListView) {

    /** The list in the block (the items render in the block's inner area). */
    def withBlock(block: Block): ListView = view.copy(block = block.some)

    /** The list with the area style replaced. */
    def withStyle(style: Style): ListView = view.copy(style = style)

    /** The list with the style patched over the selected item replaced. */
    def withHighlightStyle(style: Style): ListView = view.copy(highlightStyle = style)

    /** The list with the symbol drawn in front of the selected item. */
    def withHighlightSymbol(symbol: Line): ListView = view.copy(highlightSymbol = symbol.some)

    /** Whether the symbol repeats on every row of a multi-line selected item. */
    def withRepeatHighlightSymbol(repeat: Boolean): ListView = view.copy(repeatHighlightSymbol = repeat)

    /** The list with the symbol-column rule replaced. */
    def withHighlightSpacing(spacing: HighlightSpacing): ListView = view.copy(highlightSpacing = spacing)

    /** The items of context kept visible around the selection where they fit. */
    def withScrollPadding(padding: NonNegInt): ListView = view.copy(scrollPadding = padding)

    /** The list with the stacking direction replaced. */
    def withDirection(direction: ListDirection): ListView = view.copy(direction = direction)

    /** The list recording its base region and one region per drawn item under the id ([[ItemRegions]]). */
    def withRegion(id: RegionId): ListView = view.copy(region = id.some)

    /** The number of items. */
    def size: Int = view.items.length

  }

  @tailrec
  private def renderItems(view: ListView, canvas: Canvas, inner: Rect, corrected: Selection, symbolColumns: Int, i: Int, row: Int): Unit =
    if (row >= inner.height.value) {
      ()
    } else {
      view.items.lift(i) match {
        case None => ()
        case Some(item) =>
          val visible  = math.min(item.height, inner.height.value - row)
          val y        = view.direction match {
            case ListDirection.TopToBottom => inner.y.value.toLong + row.toLong
            case ListDirection.BottomToTop => inner.bottom.value.toLong - row.toLong - visible.toLong
          }
          val itemRect = Rect(inner.x, NonNegInts.clamp(y), inner.width, NonNegInts.clamp(visible.toLong))
          val selected = corrected.selected.exists(_.value === i)
          canvas.patchStyle(itemRect, item.style)
          renderSymbol(view, canvas, itemRect, symbolColumns, selected, 0)
          item
            .content
            .render(
              Rect(
                NonNegInts.offset(inner.x, symbolColumns),
                itemRect.y,
                NonNegInts.clamp(inner.width.value.toLong - symbolColumns.toLong),
                itemRect.height,
              ),
              canvas,
            )
          if (selected) canvas.patchStyle(itemRect, view.highlightStyle) else ()
          view.region.foreach(base => canvas.region(ItemRegions.of(base, NonNegInts.clamp(i.toLong)), itemRect))
          renderItems(view, canvas, inner, corrected, symbolColumns, i + 1, row + visible)
      }
    }

  /** The symbol column of one item: the symbol on the first row (every row when repeating) of a selected item, blanks elsewhere. */
  @tailrec
  private def renderSymbol(view: ListView, canvas: Canvas, itemRect: Rect, symbolColumns: Int, selected: Boolean, r: Int): Unit =
    if (symbolColumns === 0 || r >= itemRect.height.value) {
      ()
    } else {
      val position = Position(itemRect.x, NonNegInts.offset(itemRect.y, r))
      val width    = NonNegInts.clamp(symbolColumns.toLong)
      view.highlightSymbol match {
        case Some(symbol) if selected && (r === 0 || view.repeatHighlightSymbol) => canvas.putLine(position, symbol, width)
        case Some(_) | None => canvas.putStringMax(position, " " * symbolColumns, Style.empty, width)
      }
      renderSymbol(view, canvas, itemRect, symbolColumns, selected, r + 1)
    }

}
