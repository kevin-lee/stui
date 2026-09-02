package stui.core.focus

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.focus.FocusRing.*
import stui.testkit.Assertions

/** The [[FocusRing]] transitions by example (design doc 10, decision D25): distinctness, wrap-around, focus on a non-item, and the
  * `withItems` correction.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
object FocusRingSpec extends Properties {

  private val two: FocusRing[Int] = FocusRing.of(1, 2)

  override def tests: List[Test] = List(
    example("of dedupes and focuses the first", Assertions.eqv(FocusRing.of(1, 2, 1), FocusRing(Vector(1, 2), 1.some))),
    example("empty has no item and no current", Assertions.eqv(FocusRing.empty[Int], FocusRing(Vector.empty[Int], none[Int]))),
    example("next wraps to the first after the last", Assertions.eqv(two.next.next.current, 1.some)),
    example("previous from the first goes to the last", Assertions.eqv(two.previous.current, 2.some)),
    example("focus on a non-item is unchanged", Assertions.eqv(two.focus(3), two)),
    example("withItems replaces an absent current by the first", Assertions.eqv(two.withItems(Vector(2)), FocusRing(Vector(2), 2.some))),
    example("withItems keeps a present current", Assertions.eqv(two.focus(2).withItems(Vector(2, 3)), FocusRing(Vector(2, 3), 2.some))),
    example("withItems with no items is empty", Assertions.eqv(two.withItems(Vector.empty[Int]), FocusRing.empty[Int])),
    example("withItems keeps a blurred ring blurred", Assertions.eqv(two.blur.withItems(Vector(1, 2)), FocusRing(Vector(1, 2), none[Int]))),
    example("next on a blurred ring focuses the first", Assertions.eqv(two.blur.next.current, 1.some)),
    example(
      "has and contains",
      Result.all(
        List(
          Assertions.eqv(two.has(1), true),
          Assertions.eqv(two.has(2), false),
          Assertions.eqv(two.contains(2), true),
          Assertions.eqv(two.contains(3), false),
        )
      ),
    ),
  )

}
