package stui.core.style

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.StyleGens

/** The bitset laws of `Modifiers`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object ModifiersSpec extends Properties {

  override def tests: List[Test] = List(
    property("contains after add, not after remove", testAddRemove),
    property("union is commutative and associative", testUnion),
    property("diff removes every bit of the other", testDiff),
    property("of(toList) is the identity", StyleGens.modifiers.forAll.map(m => Assertions.eqv(Modifiers.of(m.toList), m))),
    property("toList is in ordinal order", StyleGens.modifiers.forAll.map(m => Result.assert(m.toList === m.toList.sortBy(_.ordinal)))),
    example("empty is empty", Result.assert(Modifiers.empty.isEmpty)),
    example("show renders the members", Modifiers(Modifier.Bold, Modifier.Italic).show ==== "Modifiers(Bold, Italic)"),
  )

  def testAddRemove: Property =
    for {
      m        <- StyleGens.modifiers.forAll
      modifier <- StyleGens.modifier.forAll
    } yield Result.all(List(Result.assert(m.add(modifier).contains(modifier)), Result.assert(!m.remove(modifier).contains(modifier))))

  def testUnion: Property =
    for {
      a <- StyleGens.modifiers.forAll
      b <- StyleGens.modifiers.forAll
      c <- StyleGens.modifiers.forAll
    } yield Result.all(List(Assertions.eqv(a.union(b), b.union(a)), Assertions.eqv(a.union(b).union(c), a.union(b.union(c)))))

  def testDiff: Property =
    for {
      a <- StyleGens.modifiers.forAll
      b <- StyleGens.modifiers.forAll
    } yield Result.assert(!a.diff(b).intersects(b))

}
