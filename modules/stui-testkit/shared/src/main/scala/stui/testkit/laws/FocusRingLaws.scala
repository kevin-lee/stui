package stui.testkit.laws

import cats.{Eq, Show}
import cats.syntax.all.*
import hedgehog.{Gen, Result}
import hedgehog.runner.*
import stui.core.focus.FocusRing
import stui.core.focus.FocusRing.*
import stui.testkit.Assertions

import scala.annotation.tailrec

/** The focus ring laws (design doc 10 and 12, decision D25).
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
object FocusRingLaws {

  private def check(condition: Boolean, message: => String): Result = Result.assert(condition).log(message)

  @tailrec
  private def times[A: Eq](ring: FocusRing[A], n: Int): FocusRing[A] = if (n <= 0) ring else times(ring.next, n - 1)

  private def distinct[A: Eq](items: Vector[A]): Boolean =
    items.zipWithIndex.forall { case (item, i) => items.indexWhere(_ === item) === i }

  /** Laws that hold for any ring and target, each test prefixed with `name`. */
  def laws[A: Eq: Show](name: String, rings: Gen[FocusRing[A]], targets: Gen[A]): List[Test] = List(
    property(
      s"[$name] the current target is one of the items or nothing",
      rings.forAll.map(ring => check(ring.current.forall(ring.contains), s"ring = ${ring.show}")),
    ),
    property(
      s"[$name] next then previous is the identity on a focused ring",
      rings.forAll.map { ring =>
        /* a blurred ring is not a fixed point of next then previous by design (next focuses the first, previous the last) */
        if (ring.isFocused) Assertions.eqv(ring.next.previous, ring) else Result.success
      },
    ),
    property(
      s"[$name] previous then next is the identity on a focused ring",
      rings.forAll.map(ring => if (ring.isFocused) Assertions.eqv(ring.previous.next, ring) else Result.success),
    ),
    property(
      s"[$name] next applied once per item is the identity on a focused ring",
      rings.forAll.map(ring => if (ring.isFocused) Assertions.eqv(times(ring, ring.items.length), ring) else Result.success),
    ),
    property(
      s"[$name] next and previous on an empty ring stay empty",
      rings.forAll.map { ring =>
        val emptied = ring.withItems(Vector.empty[A])
        Result.all(
          List(Assertions.eqv(emptied.next, emptied), Assertions.eqv(emptied.previous, emptied), Assertions.eqv(emptied.isEmpty, true))
        )
      },
    ),
    property(
      s"[$name] next on a blurred ring focuses the first and previous the last",
      rings.forAll.map { ring =>
        if (ring.isEmpty) Result.success
        else {
          Result.all(
            List(
              Assertions.eqv(ring.blur.next.current, ring.items.headOption),
              Assertions.eqv(ring.blur.previous.current, ring.items.lastOption),
            )
          )
        }
      },
    ),
    property(
      s"[$name] focus is idempotent and focuses exactly the items",
      for {
        ring   <- rings.forAll
        target <- targets.forAll
      } yield {
        val focused = ring.focus(target)
        Result.all(
          List(
            Assertions.eqv(focused.focus(target), focused),
            if (ring.contains(target)) Assertions.eqv(focused.current, target.some) else Assertions.eqv(focused, ring),
          )
        )
      },
    ),
    property(
      s"[$name] blur clears the current and keeps the items",
      rings.forAll.map(ring => Result.all(List(Assertions.eqv(ring.blur.current, none[A]), Assertions.eqv(ring.blur.items, ring.items)))),
    ),
    property(
      s"[$name] next, previous, first, last, focus, and blur keep the items",
      for {
        ring   <- rings.forAll
        target <- targets.forAll
      } yield Result.all(
        List(ring.next, ring.previous, ring.first, ring.last, ring.focus(target), ring.blur)
          .map(moved => Assertions.eqv(moved.items, ring.items))
      ),
    ),
    property(
      s"[$name] withItems keeps a present current, replaces an absent one by the first, keeps a blurred ring blurred, and is distinct",
      for {
        ring  <- rings.forAll
        items <- targets.list(hedgehog.Range.linear(0, 6)).map(_.toVector).forAll
      } yield {
        val retargeted = ring.withItems(items)
        val expected   = ring.current.flatMap { target =>
          if items.exists(_ === target)
          then target.some
          else retargeted.items.headOption
        }
        Result.all(
          List(
            Assertions.eqv(retargeted.current, expected),
            check(distinct(retargeted.items), s"items = ${retargeted.items.show}"),
            check(retargeted.items.forall(item => items.exists(_ === item)), "an item appeared from nowhere"),
            check(items.forall(item => retargeted.contains(item)), "an item was dropped"),
          )
        )
      },
    ),
    property(
      s"[$name] withItems with the ring's own items is the identity",
      rings.forAll.map(ring => Assertions.eqv(ring.withItems(ring.items), ring)),
    ),
  )

}
