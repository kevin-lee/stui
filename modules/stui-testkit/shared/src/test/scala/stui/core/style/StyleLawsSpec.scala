package stui.core.style

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.StyleGens
import stui.testkit.laws.MonoidLaws

/** `Monoid[Style]`, patch idempotence, and the `CellStyle` action laws.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object StyleLawsSpec extends Properties {

  override def tests: List[Test] = MonoidLaws.laws("Style", StyleGens.style) ++ List(
    property("patch is idempotent", StyleGens.style.forAll.map(s => Assertions.eqv(s.patch(s), s))),
    property("CellStyle.patch is a monoid action", testAction),
    property(
      "patching with the empty style changes nothing",
      StyleGens.cellStyle.forAll.map(cs => Assertions.eqv(cs.patch(Style.empty), cs)),
    ),
    property("patching with toStyle changes nothing", StyleGens.cellStyle.forAll.map(cs => Assertions.eqv(cs.patch(cs.toStyle), cs))),
    property(
      "toStyle applied to the default gives the cell style back",
      StyleGens.cellStyle.forAll.map(cs => Assertions.eqv(CellStyle.default.patch(cs.toStyle), cs)),
    ),
  )

  def testAction: Property =
    for {
      cs <- StyleGens.cellStyle.forAll
      a  <- StyleGens.style.forAll
      b  <- StyleGens.style.forAll
    } yield Assertions.eqv(cs.patch(a).patch(b), cs.patch(a.patch(b)))

}
