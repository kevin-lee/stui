package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Range, Result}
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Cell
import stui.core.geometry.{Position, Rect}
import stui.core.widget.{Measurable, Widget}
import stui.testkit.gen.{BufferGens, GeometryGens}
import stui.unicode.WidthPolicy

/** The [[stui.core.widget.Measurable]] laws (design doc 6.4, decision D18, M2a): the measured size fits the constraints, measuring is
  * deterministic, and rendering in any area at least the measured size changes, per row, only cells within one measured-width span,
  * over at most the measured height's rows. The row-wise form exists because alignment places content away from the area origin (a
  * text mixing left- and right-aligned lines spreads across the area), and the spans allow the canvas's one-cell wide-glyph edge
  * repair: the span over cells that changed to something other than a blank fits the measured width exactly, while the span over all
  * changed cells may be one column wider on each side (the repair writes blanks). Run these on small `outer` rects only.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object MeasurableLaws {

  private def positionsOf(outer: Rect): Vector[Position] = {
    val xs = Vector.tabulate(outer.width.value)(dx => GeometryGens.nonNegOrZero(outer.x.value.toLong + dx.toLong))
    val ys = Vector.tabulate(outer.height.value)(dy => GeometryGens.nonNegOrZero(outer.y.value.toLong + dy.toLong))
    ys.flatMap(y => xs.map(x => Position(x, y)))
  }

  private def spanOf(xs: Vector[Int]): Int =
    (for {
      max <- xs.maxOption
      min <- xs.minOption
    } yield max - min + 1).getOrElse(0)

  /** The three laws for any measurable-widget generator over a small `outer` rect, measured with [[WidthPolicy.default]]. */
  def laws(name: String, widgets: Gen[Widget & Measurable], outer: Rect): List[Test] = List(
    property(
      s"[$name] measure fits its constraints",
      for {
        widget      <- widgets.forAll
        constraints <- GeometryGens.size(NonNegInt(30)).forAll
      } yield {
        val measured = widget.measure(constraints, WidthPolicy.default)
        Result.all(
          List(
            Result
              .assert(measured.width.value <= constraints.width.value)
              .log(s"measured width ${measured.width.value.toString} exceeds ${constraints.width.value.toString}"),
            Result
              .assert(measured.height.value <= constraints.height.value)
              .log(s"measured height ${measured.height.value.toString} exceeds ${constraints.height.value.toString}"),
          )
        )
      },
    ),
    property(
      s"[$name] measure is deterministic",
      for {
        widget      <- widgets.forAll
        constraints <- GeometryGens.size(NonNegInt(30)).forAll
      } yield {
        val first  = widget.measure(constraints, WidthPolicy.default)
        val second = widget.measure(constraints, WidthPolicy.default)
        Result.assert(first === second).log(s"measure answered ${first.show} then ${second.show}")
      },
    ),
    property(
      s"[$name] rendering fits the measured size row-wise (modulo the one-cell edge repair)",
      for {
        widget <- widgets.forAll
        area   <- GeometryGens.rectWithin(outer).forAll
        ops    <- BufferGens.ops(outer, Range.linear(0, 5)).forAll
      } yield {
        val measured = widget.measure(area.size, WidthPolicy.default)
        val base     = BufferGens.bufferFrom(outer, ops)
        val next     = base.draw(canvas => widget.render(area, canvas))
        val changed  = positionsOf(outer).filter(position => next.cell(position) =!= base.cell(position))
        val byRow    = changed.groupBy(_.y.value)
        Result.all(
          Result
            .assert(byRow.sizeIs <= measured.height.value)
            .log(s"${byRow.size.toString} rows changed, measured height ${measured.height.value.toString}") ::
            byRow
              .toList
              .map {
                case (row, positions) =>
                  val all   = positions.map(_.x.value)
                  val solid = positions.withFilter(position => next.cell(position).exists(_ =!= Cell.blank)).map(_.x.value)
                  Result.all(
                    List(
                      Result
                        .assert(spanOf(solid) <= measured.width.value)
                        .log(s"row ${row.toString}: non-blank span ${spanOf(solid).toString} exceeds ${measured.width.value.toString}"),
                      Result
                        .assert(spanOf(all) <= measured.width.value + 2)
                        .log(s"row ${row.toString}: changed span ${spanOf(all).toString} exceeds ${(measured.width.value + 2).toString}"),
                    )
                  )
              }
        )
      },
    ),
  )

}
