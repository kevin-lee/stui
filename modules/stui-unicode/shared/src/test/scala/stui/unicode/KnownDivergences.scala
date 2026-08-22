package stui.unicode

import stui.unicode.corpus.GraphemeBreakCase

/** Documented differences between stui-unicode v0 and its references, so no gap is silent.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object KnownDivergences {

  /** `stuiWidth` is what [[WidthPolicy.default]] returns today (asserted by KnownDivergencesSpec), `unicodeWidthWidth` is unicode-width 0.2.2's. */
  final case class UnicodeWidthDivergence(input: String, stuiWidth: Int, unicodeWidthWidth: Int, reason: String)

  private def cps(codePoints: Int*): String = GraphemeBreakCase.render(codePoints)

  val unicodeWidth: List[UnicodeWidthDivergence] = List(
    UnicodeWidthDivergence(cps(0x17d8), 2, 3, "law clamp: every cluster is 0, 1, or 2 (KHMER SIGN BEYYAL)"),
    UnicodeWidthDivergence(cps(0x3042, 0xff9e), 2, 3, "law clamp: hiragana (2) + halfwidth dakuten (1) in one cluster"),
    UnicodeWidthDivergence("\r\n", 0, 1, "controls are 0"),
    UnicodeWidthDivergence(cps(0x1b), 0, 1, "controls are 0 (ESC)"),
    UnicodeWidthDivergence(
      cps(0x1f468, 0x200d, 0x1f468),
      2,
      4,
      "law clamp: non-RGI emoji ZWJ sequence (man ZWJ man), the RGI set is not consulted",
    ),
    UnicodeWidthDivergence(cps(0x61, 0x1f3fd), 2, 3, "law clamp: a non-emoji base followed by a skin tone modifier"),
    UnicodeWidthDivergence(cps(0x644, 0x627), 2, 1, "deferred to M4: Arabic lam-alef ligature"),
    UnicodeWidthDivergence(cps(0xa4f8, 0xa4fc), 2, 1, "deferred to M4: Lisu tone letter combination"),
    UnicodeWidthDivergence(cps(0x10c32, 0x200d, 0x10c03), 2, 1, "deferred to M4: Old Turkic ligature"),
    UnicodeWidthDivergence(cps(0x5d0, 0x200d, 0x5dc), 2, 1, "deferred to M4: Hebrew Alef-Lamed ligature"),
    UnicodeWidthDivergence(cps(0x2d31, 0x2d7f, 0x2d32), 3, 1, "deferred to M4: Tifinagh consonant joiner"),
    UnicodeWidthDivergence(cps(0x1a15, 0x1a17, 0x200d, 0x1a10), 2, 1, "deferred to M4: Buginese <a, -i> ya ligature"),
    UnicodeWidthDivergence(cps(0x1780, 0x17d2, 0x1780), 2, 1, "deferred to M4: Khmer coeng sign"),
    UnicodeWidthDivergence(
      cps(0x16d63, 0x16d67),
      0,
      1,
      "deferred to M4: Kirat Rai canonical equivalence (16D63 16D67 is U+16D69, both signs are zero-width marks here)",
    ),
  )

  /** emoji-test.txt fully-qualified entries excluded from EmojiConformanceSpec (space-separated hex code points). Expected to stay empty. */
  val emojiTest: Set[String] = Set.empty[String]

  /* Corpus line numbers (1-based, GraphemeBreakTestCorpus) where a platform oracle at a given Unicode version may disagree with us. */
  private val oracles: Map[(String, String), Set[Int]] = Map(
    /* JDK 21 ships Unicode 15.0 data: GB9c (Indic conjuncts, Unicode 15.1) is not implemented, so the conjunct lines 748-753 and 757-766
     * (Devanagari, Gujarati, Myanmar, Balinese, Khmer) break at every virama, and line 745 (U+2701 ZWJ U+2701) differs on GB11 because
     * the JDK's Extended_Pictographic data for U+2701 UPPER BLADE SCISSORS predates ours. */
    ("BreakIterator", "15.0") -> Set(745, 748, 749, 750, 751, 752, 753, 757, 758, 759, 760, 761, 762, 763, 764, 765, 766)
  )

  def oracle(name: String, unicodeVersion: String): Set[Int] = oracles.getOrElse((name, unicodeVersion), Set.empty[Int])

}
