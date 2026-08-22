package stui.testkit.gen

import hedgehog.*
import hedgehog.runner.*

/** @author Kevin Lee
  * @since 2026-08-22
  */
object GensSpec extends Properties {

  override def tests: List[Test] = List(
    property("asciiPrintable yields only characters in 0x20..0x7e", testAsciiPrintable),
    property("unicodeString keeps its code point count within the range", testUnicodeStringLength),
  )

  def testAsciiPrintable: Property =
    Gens.asciiPrintable(Range.linear(0, 50)).forAll.map { s =>
      Result.assert(s.forall(c => c.toInt >= 0x20 && c.toInt <= 0x7e))
    }

  def testUnicodeStringLength: Property =
    Gens.unicodeString(Range.linear(0, 20)).forAll.map { s =>
      val codePoints = s.codePointCount(0, s.length)
      Result.assert(codePoints >= 0 && codePoints <= 20)
    }

}
