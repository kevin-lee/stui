package stui.core.geometry

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.gen.GeometryGens
import stui.testkit.laws.GeometryLaws

/** The geometry algebra laws on small rects and on extreme ones (components up to `Int.MaxValue`).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object GeometryLawsSpec extends Properties {

  private val smallMargins: Gen[Margin] = GeometryGens.margin(NonNegInt(20))

  private val anyMargins: Gen[Margin] = Gen.frequency1(
    5 -> smallMargins,
    1 -> Gen.element1(Margin.zero, Margin.uniform(NonNegInt.MaxValue), Margin(NonNegInt(1000000000), NonNegInt(5))),
  )

  private val smallOffsets: Gen[Offset] = GeometryGens.offset(Range.linear(-50, 50))

  private val anyOffsets: Gen[Offset] =
    Gen.frequency1(5 -> smallOffsets, 1 -> GeometryGens.offset(Range.linear(-1000000000, 1000000000)))

  override def tests: List[Test] =
    GeometryLaws.algebraLaws("small", GeometryGens.rect(NonNegInt(200)), smallMargins, smallOffsets) ++
      GeometryLaws.algebraLaws("any", GeometryGens.anyRect, anyMargins, anyOffsets) ++
      GeometryLaws.partitionLaws("small", GeometryGens.rect(NonNegInt(30)), GeometryGens.position(NonNegInt(40)))

}
