package stui.testkit.gen

import cats.Eq
import hedgehog.{Gen, Range}
import stui.core.focus.FocusRing
import stui.core.focus.FocusRing.*

/** Generators for focus rings.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
object FocusGens {

  /** A ring built by `fromVector` over `count` generated items (duplicates in the generator exercise distinctness). */
  def ring[A: Eq](items: Gen[A], count: Range[Int]): Gen[FocusRing[A]] = items.list(count).map(list => FocusRing.fromVector(list.toVector))

  /** A generated ring with 0 to 6 random transitions applied (`next`, `previous`, `first`, `last`, `blur`, `focus`, `withItems`), so
    * blurred, empty, and retargeted rings are all produced.
    */
  def walked[A: Eq](rings: Gen[FocusRing[A]], targets: Gen[A]): Gen[FocusRing[A]] =
    for {
      ring  <- rings
      steps <- step(targets).list(Range.linear(0, 6))
    } yield steps.foldLeft(ring)((current, move) => move(current))

  private def step[A: Eq](targets: Gen[A]): Gen[FocusRing[A] => FocusRing[A]] = Gen.choice1(
    Gen.constant((ring: FocusRing[A]) => ring.next),
    Gen.constant((ring: FocusRing[A]) => ring.previous),
    Gen.constant((ring: FocusRing[A]) => ring.first),
    Gen.constant((ring: FocusRing[A]) => ring.last),
    Gen.constant((ring: FocusRing[A]) => ring.blur),
    targets.map(target => (ring: FocusRing[A]) => ring.focus(target)),
    targets.list(Range.linear(0, 6)).map(items => (ring: FocusRing[A]) => ring.withItems(items.toVector)),
  )

}
