package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.{Buffer, Cell}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.{Color, Style}
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.GeometryGens
import stui.testkit.laws.WidgetLaws
import stui.widgets.gen.WidgetGens

/** The [[Gauge]] contract laws, the percentage rounding and clamping, the label placement and colour swap, and the fill
  * monotonicity (design doc 6.7, M2b).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object GaugeSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def fraction(inline done: Int, inline total: Int): Gauge = Gauge.fraction(NonNegInt(done), PosInt(total))

  private def at(buffer: Buffer, x: Int, y: Int): Option[Cell] =
    buffer.cell(Position(GeometryGens.nonNegOrZero(x.toLong), GeometryGens.nonNegOrZero(y.toLong)))

  private def symbolAt(buffer: Buffer, x: Int): Option[String] = at(buffer, x, 0).flatMap(_.symbolOption).map(_.value)

  /** The cells of the first row holding the set's full glyph, the label and block stripped so every cell is the bar's. */
  private def fullCells(gauge: Gauge, width: Int): Int =
    Buffer
      .renderRows(
        Rendering.widget(gauge.withLabel(Line.raw("")).copy(block = none[Block]), Size(GeometryGens.nonNegOrZero(width.toLong), nn(1)))
      )
      .headOption
      .fold(0)(row => row.count(c => c.toString === gauge.set.full))

  override def tests: List[Test] =
    WidgetLaws.laws("gauge", WidgetGens.gaugeWidget, outer) ++ List(
      example("a third rounds to 33", Assertions.eqv(Gauge.percentOf(fraction(1, 3)), 33)),
      example("two thirds round to 67", Assertions.eqv(Gauge.percentOf(fraction(2, 3)), 67)),
      example("five eighths round half up to 63", Assertions.eqv(Gauge.percentOf(fraction(5, 8)), 63)),
      example("seven twentieths are 35", Assertions.eqv(Gauge.percentOf(fraction(7, 20)), 35)),
      example("nothing done is 0", Assertions.eqv(Gauge.percentOf(fraction(0, 1)), 0)),
      example("more than the total clamps to 100", Assertions.eqv(Gauge.percentOf(fraction(9, 4)), 100)),
      example("percent is the fraction of 100", Assertions.eqv(Gauge.percent(stui.core.layout.Percent(42)), fraction(42, 100))),
      example("the label is centred", testCentred),
      example("the label cells over the filled part swap the gauge colours", testSwap),
      property(
        "the filled cells are monotone in the work done",
        for {
          gauge <- WidgetGens.gauge.forAll
          width <- Gen.int(Range.linear(1, 20)).forAll
        } yield {
          val more = gauge.copy(done = GeometryGens.nonNegOrZero(gauge.done.value.toLong + 1L))
          Result.assert(fullCells(gauge, width) <= fullCells(more, width)).log("more work filled fewer cells")
        },
      ),
      property(
        "nothing done fills no cell and everything done fills every cell",
        for {
          gauge <- WidgetGens.gauge.forAll
          width <- Gen.int(Range.linear(1, 20)).forAll
        } yield Result.all(
          List(
            Assertions.eqv(fullCells(gauge.copy(done = nn(0)), width), 0),
            Assertions.eqv(fullCells(gauge.copy(done = GeometryGens.nonNegOrZero(gauge.total.value.toLong)), width), width),
          )
        ),
      ),
      property(
        "the default label shows the rounded percentage",
        for {
          gauge <- WidgetGens.gauge.forAll
          width <- Gen.int(Range.linear(4, 20)).forAll
        } yield {
          val plain = gauge.copy(label = none[Line], block = none[Block])
          val row   = Buffer.renderRows(Rendering.widget(plain, Size(GeometryGens.nonNegOrZero(width.toLong), nn(1)))).mkString
          Result.assert(row.contains(s"${Gauge.percentOf(plain).toString}%")).log(s"row '$row' lacks the percentage")
        },
      ),
    )

  def testCentred: Result = {
    val buffer = Rendering.widget(fraction(0, 1), size(10, 1))
    Result.all(
      List(
        Assertions.eqv(symbolAt(buffer, 4), Option("0")),
        Assertions.eqv(symbolAt(buffer, 5), Option("%")),
        Assertions.eqv(symbolAt(buffer, 3), Option(" ")),
      )
    )
  }

  def testSwap: Result = {
    val buffer = Rendering.widget(fraction(1, 2).withGaugeStyle(Style.empty.withFg(Color.Green)), size(10, 1))
    Result.all(
      List(
        Assertions.eqv(at(buffer, 3, 0).map(_.style.bg), Option[Color](Color.Green)),
        Assertions.eqv(at(buffer, 3, 0).map(_.style.fg), Option[Color](Color.Reset)),
        Assertions.eqv(at(buffer, 5, 0).map(_.style.fg), Option[Color](Color.Green)),
        Assertions.eqv(at(buffer, 5, 0).map(_.style.bg), Option[Color](Color.Reset)),
        Assertions.eqv(at(buffer, 0, 0).map(_.style.fg), Option[Color](Color.Green)),
      )
    )
  }

}
