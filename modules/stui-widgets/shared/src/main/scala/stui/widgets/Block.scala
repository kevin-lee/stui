package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.geometry.{Position, Rect}
import stui.core.style.Style
import stui.core.text.{Alignment, Line}
import stui.core.widget.Widget

import scala.annotation.tailrec

/** A bordered, titled, padded box (Ratatui's `Block`): `style` patches the whole area first, the border sides and corners draw over it
  * with `borderStyle` (patch semantics keep the block's background), and the titles draw last, left group then centre group then right
  * group, one column between titles, later groups over earlier ones. Content belongs in [[Block.inner]].
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Block(
  titles: Vector[Title],
  titlesStyle: Style,
  titlesAlignment: Alignment,
  titlesPosition: TitlePosition,
  borders: Borders,
  borderSet: BorderSet,
  borderStyle: Style,
  style: Style,
  padding: Padding,
) extends Widget derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the Widget trait member */
  /** Patches the style over `area` intersected with the canvas, draws the border sides then the corners, then the titles. */
  override def render(area: Rect, canvas: Canvas): Unit = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      ()
    } else {
      canvas.patchStyle(target, style)
      Block.renderBorders(this, target, canvas)
      Block.renderTitles(this, target, canvas)
    }
  }
}

object Block {

  /** No titles, no borders, empty styles, left-aligned top titles, [[BorderSet.plain]], no padding. */
  val empty: Block =
    Block(
      Vector.empty[Title],
      Style.empty,
      Alignment.Left,
      TitlePosition.Top,
      Borders.none,
      BorderSet.plain,
      Style.empty,
      Style.empty,
      Padding.zero,
    )

  /** [[empty]] with every border side visible. */
  def bordered: Block = empty.withBorders(Borders.all)

  extension (block: Block) {

    /** The block with a title appended at the default position. */
    def withTitle(line: Line): Block = block.copy(titles = block.titles :+ Title.of(line))

    /** The block with a title appended on the first row. */
    def withTitleTop(line: Line): Block = block.copy(titles = block.titles :+ Title.top(line))

    /** The block with a title appended on the last row. */
    def withTitleBottom(line: Line): Block = block.copy(titles = block.titles :+ Title.bottom(line))

    /** The block with the titles replaced. */
    def withTitles(titles: Vector[Title]): Block = block.copy(titles = titles)

    /** The block with the style patched under every title replaced. */
    def withTitlesStyle(style: Style): Block = block.copy(titlesStyle = style)

    /** The block with the default title alignment replaced. */
    def withTitlesAlignment(alignment: Alignment): Block = block.copy(titlesAlignment = alignment)

    /** The block with the default title position replaced. */
    def withTitlesPosition(position: TitlePosition): Block = block.copy(titlesPosition = position)

    /** The block with the visible border sides replaced. */
    def withBorders(borders: Borders): Block = block.copy(borders = borders)

    /** The block with the border symbols replaced. */
    def withBorderSet(borderSet: BorderSet): Block = block.copy(borderSet = borderSet)

    /** The block with the border style replaced. */
    def withBorderStyle(style: Style): Block = block.copy(borderStyle = style)

    /** The block with the area style replaced. */
    def withStyle(style: Style): Block = block.copy(style = style)

    /** The block with the padding replaced. */
    def withPadding(padding: Padding): Block = block.copy(padding = padding)

    /** The content area of the block inside `area`: a visible border side takes one cell, a title reserves its row even without a
      * border (Ratatui block.rs, `inner` takes the titles into account), and the padding comes off last. Insets that meet or cross
      * give an empty rect ([[stui.core.geometry.Rect.inset]] semantics).
      */
    def inner(area: Rect): Rect = {
      val zero        = NonNegInt(0)
      val one         = NonNegInt(1)
      val leftInset   = if (block.borders.contains(Side.Left)) one else zero
      val topInset    = if (block.borders.contains(Side.Top) || hasTitleAt(block, TitlePosition.Top)) one else zero
      val rightInset  = if (block.borders.contains(Side.Right)) one else zero
      val bottomInset = if (block.borders.contains(Side.Bottom) || hasTitleAt(block, TitlePosition.Bottom)) one else zero
      area
        .inset(leftInset, topInset, rightInset, bottomInset)
        .inset(block.padding.left, block.padding.top, block.padding.right, block.padding.bottom)
    }

  }

  private def hasTitleAt(block: Block, position: TitlePosition): Boolean =
    block.titles.exists(title => title.position.getOrElse(block.titlesPosition) === position)

  /** `n` floored at 0 and saturated at `Int.MaxValue` (the `stui.core.internal` clamp is `private[core]`). */
  private def nonNeg(n: Long): NonNegInt = {
    val bounded = math.max(0L, math.min(n, Int.MaxValue.toLong)).toInt
    NonNegInt.from(bounded).fold(_ => NonNegInt.MinValue, identity)
  }

  private def renderBorders(block: Block, target: Rect, canvas: Canvas): Unit = {
    val left   = target.x.value.toLong
    val top    = target.y.value.toLong
    val right  = left + target.width.value.toLong - 1L
    val bottom = top + target.height.value.toLong - 1L
    if (block.borders.contains(Side.Left)) {
      drawColumn(canvas, left, top, bottom + 1L, block.borderSet.verticalLeft, block.borderStyle)
    } else {
      ()
    }
    if (block.borders.contains(Side.Top)) {
      drawRow(canvas, left, right + 1L, top, block.borderSet.horizontalTop, block.borderStyle)
    } else {
      ()
    }
    if (block.borders.contains(Side.Right)) {
      drawColumn(canvas, right, top, bottom + 1L, block.borderSet.verticalRight, block.borderStyle)
    } else {
      ()
    }
    if (block.borders.contains(Side.Bottom)) {
      drawRow(canvas, left, right + 1L, bottom, block.borderSet.horizontalBottom, block.borderStyle)
    } else {
      ()
    }
    corner(block, canvas, Side.Right, Side.Bottom, right, bottom, block.borderSet.bottomRight)
    corner(block, canvas, Side.Right, Side.Top, right, top, block.borderSet.topRight)
    corner(block, canvas, Side.Left, Side.Bottom, left, bottom, block.borderSet.bottomLeft)
    corner(block, canvas, Side.Left, Side.Top, left, top, block.borderSet.topLeft)
  }

  private def corner(block: Block, canvas: Canvas, a: Side, b: Side, x: Long, y: Long, symbol: String): Unit =
    if (block.borders.contains(a) && block.borders.contains(b)) {
      canvas.putString(Position(nonNeg(x), nonNeg(y)), symbol, block.borderStyle)
    } else {
      ()
    }

  @tailrec
  private def drawColumn(canvas: Canvas, x: Long, y: Long, endY: Long, symbol: String, style: Style): Unit =
    if (y >= endY) {
      ()
    } else {
      canvas.putString(Position(nonNeg(x), nonNeg(y)), symbol, style)
      drawColumn(canvas, x, y + 1L, endY, symbol, style)
    }

  @tailrec
  private def drawRow(canvas: Canvas, x: Long, endX: Long, y: Long, symbol: String, style: Style): Unit =
    if (x >= endX) {
      ()
    } else {
      canvas.putString(Position(nonNeg(x), nonNeg(y)), symbol, style)
      drawRow(canvas, x + 1L, endX, y, symbol, style)
    }

  private def renderTitles(block: Block, target: Rect, canvas: Canvas): Unit = {
    renderTitlesAt(block, target, canvas, TitlePosition.Top)
    renderTitlesAt(block, target, canvas, TitlePosition.Bottom)
  }

  private def renderTitlesAt(block: Block, target: Rect, canvas: Canvas, position: TitlePosition): Unit = {
    val titles = block.titles.filter(title => title.position.getOrElse(block.titlesPosition) === position)
    if (titles.isEmpty) {
      ()
    } else {
      val zero        = NonNegInt(0)
      val one         = NonNegInt(1)
      val leftInset   = if (block.borders.contains(Side.Left)) one else zero
      val rightInset  = if (block.borders.contains(Side.Right)) one else zero
      val rowInset    = nonNeg(target.height.value.toLong - 1L)
      val row         = position match {
        case TitlePosition.Top => target.inset(leftInset, zero, rightInset, rowInset)
        case TitlePosition.Bottom => target.inset(leftInset, rowInset, rightInset, zero)
      }
      val ofAlignment = (alignment: Alignment) =>
        titles.map(_.line).filter(line => line.alignment.getOrElse(block.titlesAlignment) === alignment)
      renderGroupFromLeft(canvas, row, ofAlignment(Alignment.Left).toList, block.titlesStyle)
      renderCentreGroup(canvas, row, ofAlignment(Alignment.Center).toList, block.titlesStyle)
      renderGroupFromRight(canvas, row, ofAlignment(Alignment.Right).reverse.toList, block.titlesStyle)
    }
  }

  /** Walks the strip left to right, one column between titles, each title truncated at the remaining width. */
  @tailrec
  private def renderGroupFromLeft(canvas: Canvas, strip: Rect, titles: List[Line], titlesStyle: Style): Unit = titles match {
    case Nil => ()
    case line :: rest =>
      if (strip.isEmpty) {
        ()
      } else {
        val lineWidth = line.width(canvas.policy).toLong
        val slotWidth = nonNeg(math.min(lineWidth, strip.width.value.toLong))
        val slot      = Rect(strip.x, strip.y, slotWidth, strip.height)
        canvas.patchStyle(slot, titlesStyle)
        line.render(slot, canvas)
        val advance   = lineWidth + 1L
        val next      = Rect(nonNeg(strip.x.value.toLong + advance), strip.y, nonNeg(strip.width.value.toLong - advance), strip.height)
        renderGroupFromLeft(canvas, next, rest, titlesStyle)
      }
  }

  /** Anchors the centred group at the middle of the strip (left-anchored and right-truncated when it overflows, R3). */
  private def renderCentreGroup(canvas: Canvas, strip: Rect, titles: List[Line], titlesStyle: Style): Unit =
    if (titles.isEmpty) {
      ()
    } else {
      val total  = titles.foldLeft(0L)((acc, line) => acc + line.width(canvas.policy).toLong + 1L) - 1L
      val offset = math.max(0L, (strip.width.value.toLong - total) / 2L)
      val anchor = Rect(nonNeg(strip.x.value.toLong + offset), strip.y, nonNeg(strip.width.value.toLong - offset), strip.height)
      renderGroupFromLeft(canvas, anchor, titles, titlesStyle)
    }

  /** Walks the strip right to left over the reversed titles, one column between titles, truncation on the right. */
  @tailrec
  private def renderGroupFromRight(canvas: Canvas, strip: Rect, reversedTitles: List[Line], titlesStyle: Style): Unit =
    reversedTitles match {
      case Nil => ()
      case line :: rest =>
        if (strip.isEmpty) {
          ()
        } else {
          val lineWidth = line.width(canvas.policy).toLong
          val stripEnd  = strip.x.value.toLong + strip.width.value.toLong
          val x         = math.max(strip.x.value.toLong, stripEnd - lineWidth)
          val slotWidth = nonNeg(math.min(lineWidth, strip.width.value.toLong))
          val slot      = Rect(nonNeg(x), strip.y, slotWidth, strip.height)
          canvas.patchStyle(slot, titlesStyle)
          line.render(slot, canvas)
          val next      = Rect(strip.x, strip.y, nonNeg(x - strip.x.value.toLong - 1L), strip.height)
          renderGroupFromRight(canvas, next, rest, titlesStyle)
        }
    }

}
