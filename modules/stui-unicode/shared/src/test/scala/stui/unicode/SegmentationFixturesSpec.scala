package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.GraphemeBreakCase

/** Hand-written segmentation fixtures for the edges the corpus does not spell out (empty input, lone surrogates, mixed sequences).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object SegmentationFixturesSpec extends Properties {

  private def cps(codePoints: Int*): String = GraphemeBreakCase.render(codePoints)

  private val fixtures: List[(String, String, Vector[String])] = List(
    ("empty", "", Vector.empty[String]),
    ("CR LF", "\r\n", Vector("\r\n")),
    ("CR LF CR", "\r\n\r", Vector("\r\n", "\r")),
    ("lone high surrogate (D800)", cps(0xd800), Vector(cps(0xd800))),
    ("a + lone low surrogate + b (61 DC00 62)", cps(0x61, 0xdc00, 0x62), Vector("a", cps(0xdc00), "b")),
    (
      "two flags + lone regional indicator",
      cps(0x1f1f0, 0x1f1f7, 0x1f1f0, 0x1f1f7, 0x1f1f0),
      Vector(cps(0x1f1f0, 0x1f1f7), cps(0x1f1f0, 0x1f1f7), cps(0x1f1f0)),
    ),
    ("devanagari kshi (915 94D 937 93F)", cps(0x915, 0x94d, 0x937, 0x93f), Vector(cps(0x915, 0x94d, 0x937, 0x93f))),
    (
      "devanagari conjunct with ZWJ, InCB Extend (915 94D 200D 937)",
      cps(0x915, 0x94d, 0x200d, 0x937),
      Vector(cps(0x915, 0x94d, 0x200d, 0x937)),
    ),
    ("man ZWJ woman (1F468 200D 1F469)", cps(0x1f468, 0x200d, 0x1f469), Vector(cps(0x1f468, 0x200d, 0x1f469))),
    ("a ZWJ woman, no pictograph before the ZWJ (61 200D 1F469)", cps(0x61, 0x200d, 0x1f469), Vector(cps(0x61, 0x200d), cps(0x1f469))),
    ("hangul jamo L V T (1112 1161 11AB)", cps(0x1112, 0x1161, 0x11ab), Vector(cps(0x1112, 0x1161, 0x11ab))),
    ("arabic number sign + digit one (600 661)", cps(0x600, 0x661), Vector(cps(0x600, 0x661))),
    ("a + devanagari visarga, SpacingMark (61 903)", cps(0x61, 0x903), Vector(cps(0x61, 0x903))),
    ("ESC [ (1B 5B)", cps(0x1b, 0x5b), Vector(cps(0x1b), "[")),
  )

  override def tests: List[Test] =
    fixtures.map { case (name, input, expected) => example(s"clusters($name)", Graphemes.clusters(input) ==== expected) } ++ List(
      example("boundaries of the empty string is Array(0)", Graphemes.boundaries("").toList ==== List(0)),
      example("count of the empty string is 0", Graphemes.count("") ==== 0),
    )

}
