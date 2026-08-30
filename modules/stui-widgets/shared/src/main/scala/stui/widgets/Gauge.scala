package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Canvas
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect}
import stui.core.internal.NonNegInts
import stui.core.layout.Percent
import stui.core.style.{Color, Style}
import stui.core.text.Line
import stui.core.widget.Widget

import scala.annotation.tailrec

/** The ratio bar of the catalogue (design doc 6.7, M2b): `done` of `total` as an integer fraction (`done` above `total` clamps to
  * `total` at render, never a panic - a recorded divergence from Ratatui), filling the inner area's rows from the left. Under a
  * [[GaugeSet]] with fractional glyphs the fill is exact in eighths (`width * 8 * done / total` eighths, floored); without them it
  * is whole cells rounded half up. `style` patches the whole area, `gaugeStyle` patches the bar area (its `fg` colours the block
  * glyphs). The label - the percentage rounded half up as `N%` by default - is centred on the middle row, and its cells over the
  * filled part swap the gauge colours (`fg` becomes the gauge background, `bg` the gauge foreground, missing colours resolving to
  * `Color.Reset`) so it stays readable. The gauge owns its area, so it does not implement `Measurable` (design doc 6.4, R3).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Gauge(
  done: NonNegInt,
  total: PosInt,
  label: Option[Line],
  block: Option[Block],
  style: Style,
  gaugeStyle: Style,
  set: GaugeSet,
) extends Widget derives Eq, Show, Hash {

  /* the method lives in the class body because it implements the Widget trait member */
  /** Patches `style` over the target, renders the block, patches `gaugeStyle` over the inner area, draws the fill on every inner row,
    * and writes the label on the middle row. An empty target or inner area draws nothing.
    */
  override def render(area: Rect, canvas: Canvas): Unit = {
    val target = area.intersection(canvas.area)
    if (target.isEmpty) {
      ()
    } else {
      canvas.patchStyle(target, style)
      block.foreach(_.render(target, canvas))
      val inner = block.fold(target)(_.inner(target))
      if (inner.isEmpty) {
        ()
      } else {
        canvas.patchStyle(inner, gaugeStyle)
        val fill = Gauge.fillOf(this, inner.width.value)
        Gauge.fillRows(canvas, inner, Gauge.rowOf(this, inner.width.value, fill), inner.y.value)
        Gauge.renderLabel(this, canvas, inner, fill)
      }
    }
  }
}

object Gauge {

  /** `done` of `total`: the default percentage label, no block, empty styles, the Unicode set. */
  def fraction(done: NonNegInt, total: PosInt): Gauge =
    Gauge(done, total, none[Line], none[Block], Style.empty, Style.empty, GaugeSet.unicode)

  /** [[fraction]] of `p` in 100. */
  def percent(p: Percent): Gauge = fraction(NonNegInts.clamp(p.value.toLong), PosInt(100))

  /** The percentage the gauge shows: `done` (clamped to `total`) over `total`, rounded half up. */
  def percentOf(gauge: Gauge): Int = ((200L * clampedDone(gauge) + gauge.total.value.toLong) / (2L * gauge.total.value.toLong)).toInt

  extension (gauge: Gauge) {

    /** The gauge with the label replaced (the percentage otherwise). */
    def withLabel(label: Line): Gauge = gauge.copy(label = label.some)

    /** The gauge in the block. */
    def withBlock(block: Block): Gauge = gauge.copy(block = block.some)

    /** The gauge with the area style replaced. */
    def withStyle(style: Style): Gauge = gauge.copy(style = style)

    /** The gauge with the bar style replaced. */
    def withGaugeStyle(style: Style): Gauge = gauge.copy(gaugeStyle = style)

    /** The gauge with the glyph set replaced. */
    def withSet(set: GaugeSet): Gauge = gauge.copy(set = set)

    /** The gauge with the glyph set the capabilities select ([[GaugeSet.forCapabilities]]). */
    def withSetFor(capabilities: Capabilities): Gauge = gauge.copy(set = GaugeSet.forCapabilities(capabilities))

  }

  /** The filled whole cells and the eighths of the partial cell (0 when none). */
  final private case class Fill(full: Int, partial: Int)

  private def clampedDone(gauge: Gauge): Long = math.min(gauge.done.value.toLong, gauge.total.value.toLong)

  /** Exact in `BigInt` because `width * 8 * done` can exceed a `Long` at the extremes. */
  private def fillOf(gauge: Gauge, width: Int): Fill = {
    val done  = BigInt(clampedDone(gauge))
    val total = BigInt(gauge.total.value)
    if (gauge.set.eighths.isEmpty) {
      Fill(((BigInt(2) * width * done + total) / (BigInt(2) * total)).toInt, 0)
    } else {
      val eighths = BigInt(width) * 8 * done / total
      Fill((eighths / 8).toInt, (eighths % 8).toInt)
    }
  }

  /** One row of the bar: the full cells, the partial glyph when any, the empty cells. */
  private def rowOf(gauge: Gauge, width: Int, fill: Fill): String = {
    val partial = if (fill.partial > 0 && fill.full < width) gauge.set.eighths.lift(fill.partial - 1).getOrElse("") else ""
    val rest    = math.max(0, width - fill.full - (if (partial.isEmpty) 0 else 1))
    gauge.set.full * fill.full + partial + gauge.set.empty * rest
  }

  @tailrec
  private def fillRows(canvas: Canvas, inner: Rect, row: String, y: Int): Unit =
    if (y >= inner.bottom.value) {
      ()
    } else {
      canvas.putStringMax(Position(inner.x, NonNegInts.clamp(y.toLong)), row, Style.empty, inner.width)
      fillRows(canvas, inner, row, y + 1)
    }

  private def renderLabel(gauge: Gauge, canvas: Canvas, inner: Rect, fill: Fill): Unit = {
    val label       = gauge.label.getOrElse(Line.raw(s"${percentOf(gauge).toString}%"))
    val labelWidth  = label.width(canvas.policy).toLong
    val width       = inner.width.value.toLong
    val row         = NonNegInts.clamp(inner.y.value.toLong + (inner.height.value.toLong - 1L) / 2L)
    val x           = inner.x.value.toLong + math.max(0L, (width - labelWidth) / 2L)
    canvas.putLine(Position(NonNegInts.clamp(x), row), label, NonNegInts.clamp(inner.x.value.toLong + width - x))
    val filledUntil = inner.x.value.toLong + fill.full.toLong + (if (fill.partial > 0) 1L else 0L)
    val swapUntil   = math.min(x + labelWidth, math.min(filledUntil, inner.x.value.toLong + width))
    if (swapUntil > x) {
      val swapped = Style.empty.withFg(gauge.gaugeStyle.bg.getOrElse(Color.Reset)).withBg(gauge.gaugeStyle.fg.getOrElse(Color.Reset))
      canvas.patchStyle(Rect(NonNegInts.clamp(x), row, NonNegInts.clamp(swapUntil - x), NonNegInt(1)), swapped)
    } else {
      ()
    }
  }

}
