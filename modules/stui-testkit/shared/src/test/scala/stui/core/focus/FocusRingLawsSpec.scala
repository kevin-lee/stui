package stui.core.focus

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.gen.{FocusGens, Gens}
import stui.testkit.laws.FocusRingLaws

/** The focus ring laws over small integer rings and short string rings, both walked through random transitions.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
object FocusRingLawsSpec extends Properties {

  private val smallInt: Gen[Int] = Gen.int(Range.linear(0, 9))

  private val shortString: Gen[String] = Gens.asciiPrintable(Range.linear(0, 3))

  override def tests: List[Test] =
    FocusRingLaws.laws("small", FocusGens.walked(FocusGens.ring(smallInt, Range.linear(0, 6)), smallInt), smallInt) ++
      FocusRingLaws.laws("strings", FocusGens.walked(FocusGens.ring(shortString, Range.linear(0, 6)), shortString), shortString)

}
