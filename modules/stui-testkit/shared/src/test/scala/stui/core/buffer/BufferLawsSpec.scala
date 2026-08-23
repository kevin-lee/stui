package stui.core.buffer

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.{BufferGens, TextGens}
import stui.testkit.laws.BufferLaws

/** The buffer laws on buffers built by random canvas operations, and the `fromLines` round trip.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object BufferLawsSpec extends Properties {

  override def tests: List[Test] = BufferLaws.laws("random", BufferGens.buffer, BufferGens.bufferWithOutsideOps) ++ List(
    property(
      "both buffers of a pair are well-formed",
      BufferGens.bufferPair.forAll.map { case (prev, next) => Result.all(List(BufferLaws.wellFormed(prev), BufferLaws.wellFormed(next))) },
    ),
    property("fromLines renders back to the lines padded to the widest", testFromLines),
  )

  def testFromLines: Property =
    TextGens.plainLines(Range.linear(0, 5), Range.linear(0, 10)).forAll.map { lines =>
      val width = lines.map(_.length).maxOption.getOrElse(0)
      Assertions.eqv(Buffer.renderRows(Buffer.fromLines(lines)), lines.map(_.padTo(width, ' ')))
    }

}
