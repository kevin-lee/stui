package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.GraphemeBreakCase

/** Hand-written width fixtures covering every rule of the default policy. Inputs are given as code points so every fixture is readable.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object WidthFixturesSpec extends Properties {

  private def cps(codePoints: Int*): String = GraphemeBreakCase.render(codePoints)

  private val fixtures: List[(String, String, Int)] = List(
    ("empty", "", 0),
    ("ascii letter", "a", 1),
    ("hiragana A (3042)", cps(0x3042), 2),
    ("two kanji (6F22 5B57)", cps(0x6f22, 0x5b57), 4),
    ("halfwidth katakana KA (FF76)", cps(0xff76), 1),
    ("halfwidth katakana KA + dakuten (FF76 FF9E)", cps(0xff76, 0xff9e), 2),
    ("halfwidth katakana HA + handakuten (FF8A FF9F)", cps(0xff8a, 0xff9f), 2),
    ("ascii + halfwidth dakuten (61 FF9E)", cps(0x61, 0xff9e), 2),
    ("lone halfwidth dakuten (FF9E)", cps(0xff9e), 1),
    ("katakana KA + combining dakuten (30AB 3099)", cps(0x30ab, 0x3099), 2),
    ("precomposed e acute (E9)", cps(0xe9), 1),
    ("e + combining acute (65 301)", cps(0x65, 0x301), 1),
    ("e + three combining marks (65 301 302 303)", cps(0x65, 0x301, 0x302, 0x303), 1),
    ("precomposed hangul syllable HAN (D55C)", cps(0xd55c), 2),
    ("hangul jamo L V T (1112 1161 11AB)", cps(0x1112, 0x1161, 0x11ab), 2),
    ("lone hangul V jamo (1161)", cps(0x1161), 0),
    ("hangul choseong filler (115F)", cps(0x115f), 2),
    ("khmer sign beyyal (17D8, clamped)", cps(0x17d8), 2),
    ("arabic number sign + digit one (600 661, 0600 is a prepended concatenation mark of width 1)", cps(0x600, 0x661), 2),
    ("kaithi number sign alone (110BD)", cps(0x110bd), 1),
    ("grinning face (1F600)", cps(0x1f600), 2),
    ("grinning face + VS15 (1F600 FE0E)", cps(0x1f600, 0xfe0e), 1),
    ("white smiling face, text default (263A)", cps(0x263a), 1),
    ("white smiling face + VS16 (263A FE0F)", cps(0x263a, 0xfe0f), 2),
    ("keycap number sign (23 FE0F 20E3)", cps(0x23, 0xfe0f, 0x20e3), 2),
    ("copyright + VS16 (A9 FE0F)", cps(0xa9, 0xfe0f), 2),
    ("flag KR (1F1F0 1F1F7)", cps(0x1f1f0, 0x1f1f7), 2),
    ("lone regional indicator (1F1F0)", cps(0x1f1f0), 1),
    ("flag + lone regional indicator (1F1F0 1F1F7 1F1F0)", cps(0x1f1f0, 0x1f1f7, 0x1f1f0), 3),
    ("thumbs up + medium skin tone (1F44D 1F3FD)", cps(0x1f44d, 0x1f3fd), 2),
    ("family ZWJ sequence (1F468 200D 1F469 200D 1F467 200D 1F466)", cps(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467, 0x200d, 0x1f466), 2),
    ("heart on fire, VS16 inside a ZWJ sequence (2764 FE0F 200D 1F525)", cps(0x2764, 0xfe0f, 0x200d, 0x1f525), 2),
    (
      "flag England, tag sequence (1F3F4 E0067 E0062 E0065 E006E E0067 E007F)",
      cps(0x1f3f4, 0xe0067, 0xe0062, 0xe0065, 0xe006e, 0xe0067, 0xe007f),
      2,
    ),
    ("CR", "\r", 0),
    ("LF", "\n", 0),
    ("CR LF", "\r\n", 0),
    ("ESC [ 3 1 m", cps(0x1b, 0x5b, 0x33, 0x31, 0x6d), 4),
    ("NUL", cps(0x0), 0),
    ("DEL (7F)", cps(0x7f), 0),
    ("NEL (85)", cps(0x85), 0),
    ("zero width space (200B)", cps(0x200b), 0),
    ("line separator (2028)", cps(0x2028), 0),
    ("byte order mark (FEFF)", cps(0xfeff), 0),
    ("lone high surrogate (D800)", cps(0xd800), 1),
    ("a + lone low surrogate + b (61 DC00 62)", cps(0x61, 0xdc00, 0x62), 3),
    ("devanagari conjunct ksha (915 94D 937)", cps(0x915, 0x94d, 0x937), 2),
    ("lone combining acute (301)", cps(0x301), 0),
    ("left single quotation mark (2018)", cps(0x2018), 1),
    ("plus-minus sign, ambiguous width (B1)", cps(0xb1), 1),
    ("ideographic space (3000)", cps(0x3000), 2),
    ("fullwidth exclamation mark (FF01)", cps(0xff01), 2),
  )

  override def tests: List[Test] = fixtures.map {
    case (name, input, expected) =>
      example(s"width($name) is ${expected.toString}", WidthPolicy.default.width(input) ==== expected)
  }

}
