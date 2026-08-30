package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegLong, PosInt}
import stui.core.text.Line
import stui.testkit.Assertions
import stui.testkit.gen.TextGens
import stui.widgets.gen.WidgetGens

/** The [[LogRing]] bound, eviction, and `firstIndex` accounting (design doc 6.6, M2a).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object LogRingSpec extends Properties {

  private def raw(contents: String*): Vector[Line] = contents.toVector.map(Line.raw)

  override def tests: List[Test] = List(
    example(
      "appends below the bound accumulate in order",
      Assertions.eqv(
        LogRing.empty(PosInt(3)).append(Line.raw("a")).append(Line.raw("b")),
        LogRing(PosInt(3), raw("a", "b"), NonNegLong(0L)),
      ),
    ),
    example(
      "overflow evicts the oldest and advances firstIndex",
      Assertions.eqv(
        LogRing.empty(PosInt(3)).appendAll(raw("a", "b", "c")).append(Line.raw("d")),
        LogRing(PosInt(3), raw("b", "c", "d"), NonNegLong(1L)),
      ),
    ),
    example(
      "a bulk larger than the bound keeps its tail",
      Assertions.eqv(
        LogRing.empty(PosInt(2)).appendAll(raw("a", "b", "c", "d", "e")),
        LogRing(PosInt(2), raw("d", "e"), NonNegLong(3L)),
      ),
    ),
    example(
      "nextIndex tracks firstIndex plus size",
      Assertions.eqv(LogRing.empty(PosInt(3)).appendAll(raw("a", "b", "c", "d")).nextIndex, NonNegLong(4L)),
    ),
    property(
      "any append sequence keeps the bound and the count",
      for {
        ring  <- WidgetGens.logRing.forAll
        lines <- TextGens.line(Range.linear(0, 2), Range.linear(0, 4)).list(Range.linear(0, 8)).forAll
      } yield {
        val appended = ring.appendAll(lines.toVector)
        Result.all(
          List(
            Result.assert(appended.size <= appended.bound.value).log("size above the bound"),
            Result
              .assert(appended.firstIndex.value + appended.size.toLong === ring.firstIndex.value + ring.size.toLong + lines.length.toLong)
              .log("firstIndex + size is not the total appended count"),
          )
        )
      },
    ),
    property(
      "appendAll equals sequential appends",
      for {
        ring  <- WidgetGens.logRing.forAll
        lines <- TextGens.line(Range.linear(0, 2), Range.linear(0, 4)).list(Range.linear(0, 8)).forAll
      } yield Assertions.eqv(ring.appendAll(lines.toVector), lines.foldLeft(ring)((r, line) => r.append(line))),
    ),
  )

}
