package stui.testkit.gen

import hedgehog.{Gen, Range}

/** Generators that depend on no stui type. Geometry, style, text, and nasty-Unicode generators arrive with later phases.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Gens {

  val asciiPrintableChar: Gen[Char] = Gen.char(' ', '~')

  def asciiPrintable(range: Range[Int]): Gen[String] = Gen.string(asciiPrintableChar, range)

  /** Valid scalar values only (no surrogates). The nasty-Unicode corpus generator is a stui-unicode deliverable. */
  def unicodeString(range: Range[Int]): Gen[String] = Gen.string(Gen.unicode, range)

}
