package stui.testkit.laws

import cats.{Eq, Show}
import cats.syntax.all.*
import hedgehog.{Gen, Range, Result}
import hedgehog.runner.*
import stui.core.geometry.{Position, Rect, Size}
import stui.core.widget.{StatefulWidget, Widget}
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.{BufferGens, GeometryGens}

/** The widget contract laws (design doc 6.4): a widget draws only inside its area, deterministically, and totally, and a stateful
  * widget's returned state is a fixed point. Run these on small `outer` rects only, the inside-area law walks every cell.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object WidgetLaws {

  /** The area widened by one column on each side (its rows only): the canvas's wide-glyph invariant repair may blank the cell
    * immediately outside a shared vertical edge, so the inside-area law excludes that zone.
    */
  private def repairZone(area: Rect): Rect =
    if (area.isEmpty) {
      area
    } else {
      val left  = math.max(0L, area.x.value.toLong - 1L)
      val width = area.x.value.toLong + area.width.value.toLong + 1L - left
      Rect(GeometryGens.nonNegOrZero(left), area.y, GeometryGens.nonNegOrZero(width), area.height)
    }

  private def positionsOf(outer: Rect): Vector[Position] = {
    val xs = Vector.tabulate(outer.width.value)(dx => GeometryGens.nonNegOrZero(outer.x.value.toLong + dx.toLong))
    val ys = Vector.tabulate(outer.height.value)(dy => GeometryGens.nonNegOrZero(outer.y.value.toLong + dy.toLong))
    ys.flatMap(y => xs.map(x => Position(x, y)))
  }

  /** The stateless laws for any widget generator over a small `outer` rect. */
  def laws(name: String, widgets: Gen[Widget], outer: Rect): List[Test] = List(
    property(
      s"[$name] a widget renders only inside its area (modulo the one-cell edge repair)",
      for {
        widget <- widgets.forAll
        area   <- GeometryGens.rectWithin(outer).forAll
        ops    <- BufferGens.ops(outer, Range.linear(0, 5)).forAll
      } yield {
        val base = BufferGens.bufferFrom(outer, ops)
        val next = base.draw(canvas => widget.render(area, canvas))
        val zone = repairZone(area)
        Result.all(
          positionsOf(outer)
            .filterNot(position => zone.contains(position))
            .toList
            .map(position =>
              Result
                .assert(next.cell(position) === base.cell(position))
                .log(s"cell changed outside the area at ${position.show}")
            )
        )
      },
    ),
    property(
      s"[$name] rendering is deterministic",
      for {
        widget <- widgets.forAll
        area   <- GeometryGens.rectWithin(outer).forAll
        ops    <- BufferGens.ops(outer, Range.linear(0, 5)).forAll
      } yield {
        val base = BufferGens.bufferFrom(outer, ops)
        Assertions.eqv(base.draw(canvas => widget.render(area, canvas)), base.draw(canvas => widget.render(area, canvas)))
      },
    ),
    property(
      s"[$name] an empty or off-canvas area changes nothing",
      for {
        widget  <- widgets.forAll
        inside  <- GeometryGens.rectWithin(outer).forAll
        outside <- BufferGens.rectOutside(outer).forAll
        ops     <- BufferGens.ops(outer, Range.linear(0, 5)).forAll
      } yield {
        val base  = BufferGens.bufferFrom(outer, ops)
        val empty = inside.resize(Size(GeometryGens.nonNegOrZero(0L), inside.height))
        Result.all(
          List(
            Assertions.eqv(base.draw(canvas => widget.render(empty, canvas)), base),
            Assertions.eqv(base.draw(canvas => widget.render(outside, canvas)), base),
          )
        )
      },
    ),
  )

  /** The fixed-point law: rendering again with the returned state returns an equal state. */
  def stateLaw[S: Eq: Show](name: String, gen: Gen[(StatefulWidget[S], S)], outer: Rect): List[Test] = List(
    property(
      s"[$name] the returned state is a fixed point",
      gen.forAll.map {
        case (widget, state) =>
          Rendering.stateful(widget, outer.size, state) match {
            case (_, corrected) =>
              Rendering.stateful(widget, outer.size, corrected) match {
                case (_, again) => Assertions.eqv(again, corrected)
              }
          }
      },
    )
  )

}
