package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.frame.RegionId
import stui.testkit.Assertions
import stui.testkit.gen.GeometryGens

/** The `base[index]` region scheme (design doc 6.6, M2b): the round trip and the rejections.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ItemRegionsSpec extends Properties {

  private val base: RegionId = RegionId("list")

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  override def tests: List[Test] = List(
    property(
      "indexOf inverts of",
      Gen.int(Range.linear(0, 100000)).forAll.map { n =>
        val index = GeometryGens.nonNegOrZero(n.toLong)
        Assertions.eqv(ItemRegions.indexOf(base, ItemRegions.of(base, index)), Option(index))
      },
    ),
    example("the base itself is not an item", Assertions.eqv(ItemRegions.indexOf(base, base), Option.empty[NonNegInt])),
    example("a foreign id is not an item", Assertions.eqv(ItemRegions.indexOf(base, RegionId("other[1]")), Option.empty[NonNegInt])),
    example("a non-numeric index is rejected", Assertions.eqv(ItemRegions.indexOf(base, RegionId("list[x]")), Option.empty[NonNegInt])),
    example("a negative index is rejected", Assertions.eqv(ItemRegions.indexOf(base, RegionId("list[-1]")), Option.empty[NonNegInt])),
    example("an unterminated id is rejected", Assertions.eqv(ItemRegions.indexOf(base, RegionId("list[")), Option.empty[NonNegInt])),
    example("owns accepts the base", Result.assert(ItemRegions.owns(base, base))),
    example("owns accepts an item", Result.assert(ItemRegions.owns(base, ItemRegions.of(base, nn(3))))),
    example("owns rejects another base", Result.assert(!ItemRegions.owns(base, RegionId("log")))),
    example("owns rejects a malformed id", Result.assert(!ItemRegions.owns(base, RegionId("list[")))),
  )

}
