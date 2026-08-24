package stui.testkit.gen

import hedgehog.{Gen, Range}

/** Generators that depend on no stui type. The stui-aware generators live beside them in this package (geometry, style, text,
  * buffer, event, and layout).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Gens {

  /** One printable ASCII character (space to tilde). */
  val asciiPrintableChar: Gen[Char] = Gen.char(' ', '~')

  /** Printable ASCII strings of a length in the range. */
  def asciiPrintable(range: Range[Int]): Gen[String] = Gen.string(asciiPrintableChar, range)

  /** Valid scalar values only (no surrogates). The nasty-Unicode corpus generator is a stui-unicode deliverable. */
  def unicodeString(range: Range[Int]): Gen[String] = Gen.string(Gen.unicode, range)

}
