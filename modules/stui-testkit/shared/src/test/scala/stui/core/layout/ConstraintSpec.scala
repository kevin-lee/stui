package stui.core.layout

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.testkit.Assertions
import stui.testkit.gen.LayoutGens

/** The constraint constructor triples and the runtime validation edges.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object ConstraintSpec extends Properties {

  override def tests: List[Test] = List(
    example("length(10) equals lengthOf(NonNegInt(10))", Assertions.eqv(Constraint.length(10), Constraint.lengthOf(NonNegInt(10)))),
    example(
      "percentage(50) equals percentageOf(Percent(50))",
      Assertions.eqv(Constraint.percentage(50), Constraint.percentageOf(Percent(50))),
    ),
    example(
      "ratio(1, 3) equals ratioOf(NonNegInt(1), PosInt(3))",
      Assertions.eqv(Constraint.ratio(1, 3), Constraint.ratioOf(NonNegInt(1), PosInt(3))),
    ),
    example("min(4) equals minOf(NonNegInt(4))", Assertions.eqv(Constraint.min(4), Constraint.minOf(NonNegInt(4)))),
    example("max(4) equals maxOf(NonNegInt(4))", Assertions.eqv(Constraint.max(4), Constraint.maxOf(NonNegInt(4)))),
    example("fill(2) equals fillOf(PosInt(2))", Assertions.eqv(Constraint.fill(2), Constraint.fillOf(PosInt(2)))),
    example(
      "percentage(50) round trips through percentageFrom",
      Assertions.eqv(Constraint.percentageFrom(50), Right(Constraint.percentage(50))),
    ),
    example(
      "percentageFrom rejects -1 and 101",
      Result.all(List(Result.assert(Constraint.percentageFrom(-1).isLeft), Result.assert(Constraint.percentageFrom(101).isLeft))),
    ),
    example("ratioFrom accepts 1 over 3", Assertions.eqv(Constraint.ratioFrom(1, 3), Right(Constraint.ratio(1, 3)))),
    example(
      "ratioFrom rejects a zero denominator and a negative numerator",
      Result.all(List(Result.assert(Constraint.ratioFrom(1, 0).isLeft), Result.assert(Constraint.ratioFrom(-1, 3).isLeft))),
    ),
    property(
      "constraints are equal to themselves",
      LayoutGens.constraint(NonNegInt(100)).forAll.map(constraint => Assertions.eqv(constraint, constraint)),
    ),
    property(
      "constraints have a non-empty Show",
      LayoutGens.constraint(NonNegInt(100)).forAll.map(constraint => Result.assert(constraint.show.nonEmpty)),
    ),
    property("layouts are equal to themselves", LayoutGens.anyLayout.forAll.map(layout => Assertions.eqv(layout, layout))),
    property("layouts have a non-empty Show", LayoutGens.anyLayout.forAll.map(layout => Result.assert(layout.show.nonEmpty))),
  )

}
