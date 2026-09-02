package stui.core.layout

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.geometry.Rect
import stui.testkit.gen.{GeometryGens, LayoutGens}
import stui.testkit.laws.LayoutLaws

/** The layout laws on small inputs and on extreme ones.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object LayoutLawsSpec extends Properties {

  private val smallInputs: Gen[(Rect, Layout)] =
    for {
      area   <- GeometryGens.rect(NonNegInt(60))
      layout <- LayoutGens.layout(Range.linear(1, 6), NonNegInt(30))
    } yield (area, layout)

  private val anyInputs: Gen[(Rect, Layout)] =
    for {
      area   <- GeometryGens.anyRect
      layout <- LayoutGens.anyLayout
    } yield (area, layout)

  private val smallAreas: Gen[Rect] = GeometryGens.rect(NonNegInt(40))

  override def tests: List[Test] =
    LayoutLaws.laws("small", smallInputs) ++
      LayoutLaws.laws("any", anyInputs) ++
      LayoutLaws.fillLaws(
        "small",
        smallAreas,
        LayoutGens.fillOnly(Range.linear(1, 6), PosInt(5)),
        LayoutGens.equalFills(Range.linear(1, 6)),
      ) ++
      LayoutLaws.fixedArityLaws("small", smallAreas, LayoutGens.axis, LayoutGens.constraint(NonNegInt(30))) ++
      LayoutLaws.fixedArityLaws(
        "any",
        GeometryGens.anyRect,
        LayoutGens.axis,
        Gen.frequency1(9 -> LayoutGens.constraint(NonNegInt(30)), 1 -> LayoutGens.extremeConstraint),
      )

}
