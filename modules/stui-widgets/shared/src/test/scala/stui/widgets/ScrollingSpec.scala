package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.Assertions
import stui.testkit.gen.GeometryGens

/** The [[Scrolling]] clamp semantics (design doc 6.6, M2a): fixture rows and the clamp laws.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ScrollingSpec extends Properties {

  private val small: Gen[NonNegInt] = GeometryGens.nonNegInt(NonNegInt(40))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  override def tests: List[Test] = List(
    example("maxOffset is the overflow", Assertions.eqv(Scrolling.maxOffset(nn(10), nn(3)), nn(7))),
    example("maxOffset floors at zero when the content fits", Assertions.eqv(Scrolling.maxOffset(nn(3), nn(10)), nn(0))),
    example("clampOffset cuts at the max", Assertions.eqv(Scrolling.clampOffset(nn(9), nn(10), nn(3)), nn(7))),
    example("scrolledBy floors at zero", Assertions.eqv(Scrolling.scrolledBy(Scroll.none, -5, -5), Scroll.none)),
    property(
      "clampOffset is at most maxOffset and idempotent",
      for {
        offset   <- small.forAll
        content  <- small.forAll
        viewport <- small.forAll
      } yield {
        val clamped = Scrolling.clampOffset(offset, content, viewport)
        Result.all(
          List(
            Result.assert(clamped.value <= Scrolling.maxOffset(content, viewport).value).log("above maxOffset"),
            Assertions.eqv(Scrolling.clampOffset(clamped, content, viewport), clamped),
          )
        )
      },
    ),
    property(
      "content that fits clamps to zero",
      for {
        offset   <- small.forAll
        viewport <- small.forAll
        content  <- GeometryGens.nonNegInt(viewport).forAll
      } yield Assertions.eqv(Scrolling.clampOffset(offset, content, viewport), nn(0)),
    ),
    property(
      "clampScroll acts per axis",
      for {
        rows     <- small.forAll
        columns  <- small.forAll
        content  <- GeometryGens.size(NonNegInt(40)).forAll
        viewport <- GeometryGens.size(NonNegInt(40)).forAll
      } yield Assertions.eqv(
        Scrolling.clampScroll(Scroll(rows, columns), content, viewport),
        Scroll(
          Scrolling.clampOffset(rows, content.height, viewport.height),
          Scrolling.clampOffset(columns, content.width, viewport.width),
        ),
      ),
    ),
    property(
      "scrolledBy is the saturating per-axis sum",
      for {
        rows         <- small.forAll
        columns      <- small.forAll
        deltaRows    <- Gen.int(Range.linear(Int.MinValue, Int.MaxValue)).forAll
        deltaColumns <- Gen.int(Range.linear(Int.MinValue, Int.MaxValue)).forAll
      } yield {
        val moved                                    = Scrolling.scrolledBy(Scroll(rows, columns), deltaRows, deltaColumns)
        def expected(v: NonNegInt, delta: Int): Long = math.max(0L, math.min(Int.MaxValue.toLong, v.value.toLong + delta.toLong))
        Result.all(
          List(
            Result.assert(moved.rows.value.toLong === expected(rows, deltaRows)).log("rows"),
            Result.assert(moved.columns.value.toLong === expected(columns, deltaColumns)).log("columns"),
          )
        )
      },
    ),
  )

}
