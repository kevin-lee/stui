package stui.testkit.laws

import cats.{Eq, Monoid, Show}
import cats.syntax.all.*
import hedgehog.Gen
import hedgehog.runner.*
import stui.testkit.Assertions

/** The monoid laws as hedgehog tests, reusable for any `Monoid`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object MonoidLaws {

  /** Left identity, right identity, and associativity on the generator, each test prefixed with `name`. */
  def laws[A: Monoid: Eq: Show](name: String, gen: Gen[A]): List[Test] = List(
    property(s"[$name] left identity", gen.forAll.map(a => Assertions.eqv(Monoid[A].empty |+| a, a))),
    property(s"[$name] right identity", gen.forAll.map(a => Assertions.eqv(a |+| Monoid[A].empty, a))),
    property(
      s"[$name] associativity",
      for {
        a <- gen.forAll
        b <- gen.forAll
        c <- gen.forAll
      } yield Assertions.eqv((a |+| b) |+| c, a |+| (b |+| c)),
    ),
  )

}
