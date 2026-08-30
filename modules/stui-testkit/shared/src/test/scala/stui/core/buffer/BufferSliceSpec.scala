package stui.core.buffer

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.Assertions
import stui.testkit.gen.BufferGens

/** The whole-row slices `firstRows` and `lastRows` (the print channel's trim and ring cut, design doc 7.2).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object BufferSliceSpec extends Properties {

  private val abc: Buffer = Buffer.fromLines(Vector("aa", "bb", "cc"))

  override def tests: List[Test] = List(
    example(
      "firstRows keeps the top rows",
      Assertions.eqv(Buffer.renderRows(abc.firstRows(NonNegInt(2))), Vector("aa", "bb")),
    ),
    example(
      "lastRows keeps the bottom rows",
      Assertions.eqv(Buffer.renderRows(abc.lastRows(NonNegInt(2))), Vector("bb", "cc")),
    ),
    example("a zero count gives an empty buffer", Assertions.eqv(abc.firstRows(NonNegInt(0)).area.height, NonNegInt(0))),
    example("a count above the height gives the buffer itself", Assertions.eqv(abc.lastRows(NonNegInt(9)), abc)),
    property("firstRows is a prefix of the rows", testPrefix),
    property("lastRows is a suffix of the rows", testSuffix),
  )

  def testPrefix: Property =
    for {
      buffer <- BufferGens.buffer.forAll
      n      <- Gen.int(Range.linear(0, 8)).forAll
    } yield {
      val count = NonNegInt.unsafeFrom(n)
      Assertions.eqv(buffer.firstRows(count).rows, buffer.rows.take(n))
    }

  def testSuffix: Property =
    for {
      buffer <- BufferGens.buffer.forAll
      n      <- Gen.int(Range.linear(0, 8)).forAll
    } yield {
      val count = NonNegInt.unsafeFrom(n)
      Assertions.eqv(buffer.lastRows(count).rows, buffer.rows.takeRight(n))
    }

}
