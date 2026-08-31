package stui.core.buffer

import refined4s.InlinedRefined
import refined4s.modules.cats.derivation.{CatsEq, CatsHash, CatsShow}
import stui.unicode.internal.IntOps.*

/** The symbol of a [[Cell.Glyph]]: exactly one extended grapheme cluster of valid UTF-16 with a non-zero display width under the
  * bundled tables, so a control, a format character, a lone combining mark, a lone surrogate, the empty string, and several clusters are
  * not symbols (decision D14, design doc 6.5 and principle 9). No literal constructor exists because the validator runs the segmenter,
  * which the compiler cannot fold: construct with [[GlyphSymbol.from]] (a compile-time macro over the real tables is a later addition).
  */
/** @author Kevin Lee
  * @since 2026-08-29
  */
type GlyphSymbol = GlyphSymbol.Type
object GlyphSymbol extends InlinedRefined[String], CatsHash[String], CatsEq[String], CatsShow[String] {

  private val Replacement = apply("\ufffd")

  private val VariationSelector16: Int = 0xfe0f

  override inline val inlinedExpectedValue =
    "exactly one extended grapheme cluster of valid UTF-16 with a non-zero display width (no control, format, or zero-width cluster)"

  override inline def inlinedPredicate(inline a: String): Boolean = GlyphSymbolValidator.Macros.isValidGlyphSymbol(a)

  /** The message for a rejected string. */
  override def invalidReason(a: String): String =
    expectedMessage(
      inlinedExpectedValue
    )

  /** One cluster, valid UTF-16, non-zero width under the bundled tables. */
  override def predicate(a: String): Boolean = GlyphSymbolValidator.isValidGlyphSymbol(a)

  /* the two constants are valid by inspection (a space and the replacement character) and `GlyphSymbolSpec` asserts `from` accepts
   * them, which is why `unsafeFrom` is acceptable here */
  /** The space, the symbol of a blank cell. */
  val space: GlyphSymbol = unsafeFrom(" ")

  /** U+FFFD, the symbol written for invalid input. */
  val replacement: GlyphSymbol = Replacement

  /** The symbol of a cluster the canvas has already measured. The canvas only passes valid clusters, so the fallback to [[replacement]]
    * is unreachable. The cost is one extra segmentation per written cell (measured later).
    */
  private[core] def trusted(cluster: String): GlyphSymbol = from(cluster).fold(_ => replacement, identity)

  extension (symbol: GlyphSymbol) {

    /** True when the code point right after the base is U+FE0F (VS16), the clusters whose width terminals disagree on (design doc 7.1). */
    def isVs16Sequence: Boolean = {
      val s    = symbol.value
      val base = s.codePointAt(0)
      val next = Character.charCount(base)
      next < s.length && s.codePointAt(next) === VariationSelector16
    }

  }

}
