package stui.core.event

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.Assertions
import stui.testkit.gen.EventGens

/** The `KeyModifiers` bitset laws, generator sanity, and the function-key constructors.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object EventSpec extends Properties {

  override def tests: List[Test] = List(
    property("contains after add, not after remove", testAddRemove),
    property("union is commutative", testUnion),
    property("union is associative", testUnionAssociative),
    property(
      "toList is in ordinal order",
      EventGens.keyModifiers.forAll.map(m => Result.assert(m.toList === m.toList.sortBy(_.ordinal))),
    ),
    property("of(toList) is the identity", EventGens.keyModifiers.forAll.map(m => Assertions.eqv(KeyModifiers.of(m.toList), m))),
    example("empty is empty", Result.assert(KeyModifiers.empty.isEmpty)),
    example(
      "show renders the members",
      KeyModifiers(KeyModifier.Shift, KeyModifier.Control).show ==== "KeyModifiers(Shift, Control)",
    ),
    property("events are equal to themselves", EventGens.event(NonNegInt(100)).forAll.map(event => Assertions.eqv(event, event))),
    property("key events are equal to themselves", EventGens.keyEvent.forAll.map(event => Assertions.eqv(event, event))),
    property("events have a non-empty Show", EventGens.event(NonNegInt(100)).forAll.map(event => Result.assert(event.show.nonEmpty))),
    example("f(12) equals fOf(FunctionKeyNumber(12))", Assertions.eqv(KeyCode.f(12), KeyCode.fOf(FunctionKeyNumber(12)))),
    example("fFrom rejects 0 and 36", Result.all(List(Result.assert(KeyCode.fFrom(0).isLeft), Result.assert(KeyCode.fFrom(36).isLeft)))),
    example("fFrom accepts 1", Assertions.eqv(KeyCode.fFrom(1), Right(KeyCode.f(1)))),
    example(
      "press has no modifiers",
      Assertions.eqv(KeyEvent.press(KeyCode.Enter), KeyEvent(KeyCode.Enter, KeyModifiers.empty, KeyEventKind.Press)),
    ),
  )

  def testAddRemove: Property =
    for {
      m        <- EventGens.keyModifiers.forAll
      modifier <- EventGens.keyModifier.forAll
    } yield Result.all(List(Result.assert(m.add(modifier).contains(modifier)), Result.assert(!m.remove(modifier).contains(modifier))))

  def testUnion: Property =
    for {
      a <- EventGens.keyModifiers.forAll
      b <- EventGens.keyModifiers.forAll
    } yield Assertions.eqv(a.union(b), b.union(a))

  def testUnionAssociative: Property =
    for {
      a <- EventGens.keyModifiers.forAll
      b <- EventGens.keyModifiers.forAll
      c <- EventGens.keyModifiers.forAll
    } yield Assertions.eqv(a.union(b).union(c), a.union(b.union(c)))

}
