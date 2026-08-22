package stui.unicode.internal

import stui.unicode.WidthPolicy
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The built-in width policy: the cluster width is `min(2, sum of the baked per-code-point widths)`, then a variation selector right after
  * the base code point overrides it (VS16 forces 2, VS15 forces 1 when the base has Emoji_Presentation and is outside the Enclosed
  * Ideographic Supplement block).
  *
  * Consequences: flags are 2 (1 + 1), a lone regional indicator is 1, Hangul syllables are 2 (2 + 0 + 0), a prepend plus its base has the
  * base's width, halfwidth katakana with a dakuten is 2 (1 + 1), emoji modifier sequences and any emoji ZWJ sequence are 2 (clamped),
  * controls and CR LF are 0, a lone surrogate is 1, the empty cluster is 0, and U+17D8 is 2 (unicode-width says 3, the law clamps it).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
private[unicode] object TableWidthPolicy extends WidthPolicy {

  private val VariationSelector15: Int = 0xfe0e

  private val VariationSelector16: Int = 0xfe0f

  private val EnclosedIdeographicSupplementFirst: Int = 0x1f200

  private val EnclosedIdeographicSupplementLast: Int = 0x1f2ff

  private val MaxClusterWidth: Int = 2

  def clusterWidth(s: String, start: Int, end: Int): Int =
    if (start < 0 || end > s.length || start >= end) {
      0
    } else {
      val base       = s.codePointAt(start)
      val baseRecord = CodePointTable.record(base)
      val next       = start + Character.charCount(base)
      val total      = math.min(MaxClusterWidth, sumWidths(s, start, end, 0))
      if (next < end) {
        val selector = s.codePointAt(next)
        if (selector === VariationSelector16) MaxClusterWidth
        else if (
          selector === VariationSelector15 && CodePointTable.isEmojiPresentation(baseRecord) && !isEnclosedIdeographicSupplement(base)
        ) 1
        else total
      } else {
        total
      }
    }

  private def isEnclosedIdeographicSupplement(cp: Int): Boolean =
    cp >= EnclosedIdeographicSupplementFirst && cp <= EnclosedIdeographicSupplementLast

  @tailrec
  private def sumWidths(s: String, i: Int, end: Int, acc: Int): Int =
    if (i >= end) {
      acc
    } else {
      val cp = s.codePointAt(i)
      sumWidths(s, i + Character.charCount(cp), end, acc + CodePointTable.width(CodePointTable.record(cp)))
    }

}
