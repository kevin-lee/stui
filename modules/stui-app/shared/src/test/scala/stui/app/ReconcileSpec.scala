package stui.app

import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.Reconcile
import stui.testkit.Assertions

import scala.concurrent.duration.*

/** The reconciliation laws (design doc 10 and 12, M3b): deterministic, idempotent, exact, and applying the diff gives the next set.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object ReconcileSpec extends Properties {

  private val keySet: Gen[Set[SubKey]] =
    Gen.int(Range.linear(1, 5)).map(n => SubKey.Every(n.millis): SubKey).list(Range.linear(0, 6)).map(_.toSet)

  override def tests: List[Test] = List(
    property(
      "deterministic",
      for {
        previous <- keySet.forAll
        next     <- keySet.forAll
      } yield Assertions.eqv(Reconcile.diff(previous, next), Reconcile.diff(previous, next)),
    ),
    property(
      "idempotent: diff(next, next) is nothing",
      keySet.forAll.map(next => Assertions.eqv(Reconcile.diff(next, next), Reconcile.nothing)),
    ),
    property(
      "exact: starts is next minus previous, stops is previous minus next",
      for {
        previous <- keySet.forAll
        next     <- keySet.forAll
      } yield {
        val changes = Reconcile.diff(previous, next)
        Result.all(List(Assertions.eqv(changes.starts, next -- previous), Assertions.eqv(changes.stops, previous -- next)))
      },
    ),
    property(
      "applying the diff to previous gives next",
      for {
        previous <- keySet.forAll
        next     <- keySet.forAll
      } yield {
        val changes = Reconcile.diff(previous, next)
        Assertions.eqv(previous -- changes.stops ++ changes.starts, next)
      },
    ),
  )

}
