package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, NonNegLong, PosInt}
import stui.core.geometry.{Rect, Size}
import stui.core.text.Line
import stui.testkit.Assertions
import stui.testkit.gen.GeometryGens
import stui.testkit.laws.{MeasurableLaws, WidgetLaws}
import stui.unicode.WidthPolicy
import stui.widgets.gen.WidgetGens

/** The [[Scrollbar]] contract and Measurable laws, the thumb rule, and the constructors over the scroll states (design doc 6.7,
  * M2b).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ScrollbarSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def thumb(inline start: Int, inline length: Int): Thumb = Thumb(NonNegInt(start), NonNegInt(length))

  private inline def thumbOf(inline track: Int, inline content: Int, inline viewport: Int, inline position: Int): Thumb =
    Scrollbar.thumbOf(NonNegInt(track), NonNegInt(content), NonNegInt(viewport), NonNegInt(position))

  private def nnOf(n: Int): NonNegInt = GeometryGens.nonNegOrZero(n.toLong)

  override def tests: List[Test] =
    WidgetLaws.laws("scrollbar", WidgetGens.scrollbarWidget, outer) ++
      MeasurableLaws.laws("scrollbar", WidgetGens.scrollbarMeasurable, outer) ++
      List(
        example("the thumb at the top", Assertions.eqv(thumbOf(10, 100, 10, 0), thumb(0, 1))),
        example("the thumb at the bottom", Assertions.eqv(thumbOf(10, 100, 10, 90), thumb(9, 1))),
        example("the thumb rounds half up", Assertions.eqv(thumbOf(10, 100, 10, 45), thumb(5, 1))),
        example("the thumb is proportional", Assertions.eqv(thumbOf(4, 12, 6, 0), thumb(0, 2))),
        example("the thumb reaches the end at the largest offset", Assertions.eqv(thumbOf(4, 12, 6, 6), thumb(2, 2))),
        example("the thumb halfway", Assertions.eqv(thumbOf(4, 12, 6, 3), thumb(1, 2))),
        example("content that fits fills the track", Assertions.eqv(thumbOf(5, 3, 5, 0), thumb(0, 5))),
        example("empty content fills the track", Assertions.eqv(thumbOf(5, 0, 0, 0), thumb(0, 5))),
        example("a position beyond the content clamps", Assertions.eqv(thumbOf(4, 12, 6, 99), thumb(2, 2))),
        example("an empty track has no thumb", Assertions.eqv(thumbOf(0, 12, 6, 0), thumb(0, 0))),
        example(
          "a vertical bar measures one column by the height",
          Assertions.eqv(Scrollbar.vertical(nn(9), nn(3), nn(0)).measure(size(7, 9), WidthPolicy.default), size(1, 9)),
        ),
        example(
          "a horizontal bar measures the width by one row",
          Assertions.eqv(Scrollbar.horizontal(nn(9), nn(3), nn(0)).measure(size(7, 9), WidthPolicy.default), size(7, 1)),
        ),
        example("ofScroll reads the rows", testOfScroll),
        example("ofLogView reads the anchor relative to the ring start", testOfLogView),
        example("ofSelection reads the offset", testOfSelection),
        property(
          "the thumb lies inside the track and covers at least one cell",
          for {
            track    <- Gen.int(Range.linear(0, 20)).forAll
            content  <- Gen.int(Range.linear(0, 40)).forAll
            viewport <- Gen.int(Range.linear(0, 20)).forAll
            position <- Gen.int(Range.linear(0, 60)).forAll
          } yield {
            val t = Scrollbar.thumbOf(nnOf(track), nnOf(content), nnOf(viewport), nnOf(position))
            Result.all(
              List(
                Result.assert(t.start.value + t.length.value <= track).log("the thumb overflows the track"),
                Result.assert(track === 0 || t.length.value >= 1).log("the thumb vanished"),
              )
            )
          },
        ),
        property(
          "the thumb start is monotone in the position and reaches both ends",
          for {
            track    <- Gen.int(Range.linear(1, 20)).forAll
            viewport <- Gen.int(Range.linear(0, 20)).forAll
            extra    <- Gen.int(Range.linear(1, 20)).forAll
            position <- Gen.int(Range.linear(0, 40)).forAll
          } yield {
            val content = viewport + extra
            val first   = Scrollbar.thumbOf(nnOf(track), nnOf(content), nnOf(viewport), nnOf(0))
            val at      = Scrollbar.thumbOf(nnOf(track), nnOf(content), nnOf(viewport), nnOf(position))
            val next    = Scrollbar.thumbOf(nnOf(track), nnOf(content), nnOf(viewport), nnOf(position + 1))
            val last    = Scrollbar.thumbOf(nnOf(track), nnOf(content), nnOf(viewport), nnOf(extra))
            Result.all(
              List(
                Assertions.eqv(first.start, nn(0)),
                Result.assert(at.start.value <= next.start.value).log("the thumb moved backwards"),
                Assertions.eqv(last.start.value + last.length.value, track),
              )
            )
          },
        ),
      )

  def testOfScroll: Result =
    Assertions.eqv(
      Scrollbar.ofScroll(Scroll(nn(4), nn(1)), size(20, 30), size(10, 6)),
      Scrollbar.vertical(nn(30), nn(6), nn(4)),
    )

  def testOfLogView: Result = {
    val ring = LogRing.empty(PosInt(4)).appendAll(Vector("a", "b", "c", "d", "e", "f").map(Line.raw))
    Result.all(
      List(
        Assertions.eqv(Scrollbar.ofLogView(ring, LogViewState.following, nn(2)), Scrollbar.vertical(nn(4), nn(2), nn(2))),
        Assertions.eqv(Scrollbar.ofLogView(ring, LogViewState.anchoredAt(NonNegLong(3L)), nn(2)), Scrollbar.vertical(nn(4), nn(2), nn(1))),
        Assertions.eqv(Scrollbar.ofLogView(ring, LogViewState.anchoredAt(NonNegLong(0L)), nn(2)), Scrollbar.vertical(nn(4), nn(2), nn(0))),
      )
    )
  }

  def testOfSelection: Result =
    Assertions.eqv(Scrollbar.ofSelection(Selection(nn(7), Some(nn(9))), nn(30), nn(5)), Scrollbar.vertical(nn(30), nn(5), nn(7)))

}
