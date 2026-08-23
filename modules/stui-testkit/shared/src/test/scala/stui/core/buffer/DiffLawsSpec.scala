package stui.core.buffer

import hedgehog.runner.*
import stui.testkit.gen.BufferGens
import stui.testkit.laws.DiffLaws

/** The diff laws on random buffer pairs.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object DiffLawsSpec extends Properties {

  override def tests: List[Test] = DiffLaws.laws("random", BufferGens.bufferPair, BufferGens.mismatchedPair)

}
