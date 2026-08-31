package stui.core.buffer

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.{BufferGens, NastyGens}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** `GlyphSymbol.from` accepts exactly the clusters the canvas produces: one cluster, valid UTF-16, non-zero width.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object GlyphSymbolSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  @tailrec
  private def hasUnpairedSurrogate(s: String, i: Int): Boolean =
    if (i >= s.length) {
      false
    } else {
      val cp = s.codePointAt(i)
      if (cp >= 0xd800 && cp <= 0xdfff) true else hasUnpairedSurrogate(s, i + Character.charCount(cp))
    }

  def expectedInvalidValueMessage(value: String): String =
    raw"""Invalid value: ["$value"]. It must be exactly one extended grapheme cluster of valid UTF-16 with a non-zero display width (no control, format, or zero-width cluster)."""

  def expectedInvalidValueMessageWithoutQuotes(value: String): String =
    raw"""Invalid value: [$value]. It must be exactly one extended grapheme cluster of valid UTF-16 with a non-zero display width (no control, format, or zero-width cluster)."""

  override def tests: List[Test] = List(
    property("every glyph of a generated buffer round-trips through from", testBufferGlyphs),
    property("from agrees with the definition on nasty clusters", testDefinition),
    example("apply() with a valid literal compiles", testApply),
    property("an accepted string is kept as is", testKept),
    example("""apply("") should not compile""", testApplyEmptyString),
    example("""from("")(an empty string) should return Left(error)""", testFromEmptyString),
    example("""apply("ab") (two clusters) should not compile""", testApplyTwoClusters),
    example("""from("ab") (two clusters) should return Left(error)""", testFromTwoClusters),
    example("a control is rejected", Result.assert(GlyphSymbol.from(cps(0x1b)).isLeft)),
    example("a tab is rejected", Result.assert(GlyphSymbol.from("\t").isLeft)),
    example("a lone combining mark is rejected", Result.assert(GlyphSymbol.from(cps(0x301)).isLeft)),
    example("a lone surrogate is rejected", Result.assert(GlyphSymbol.from(cps(0xd800)).isLeft)),
    example("a Hangul syllable is accepted", Result.assert(GlyphSymbol.from(cps(0xac00)).isRight)),
    example("a VS16 sequence is detected", Assertions.eqv(GlyphSymbol.from(cps(0x2328, 0xfe0f)).map(_.isVs16Sequence), Right(true))),
    example("a plain letter is not a VS16 sequence", Assertions.eqv(GlyphSymbol.from("a").map(_.isVs16Sequence), Right(false))),
    example("space is the space", GlyphSymbol.space.value ==== " "),
    example("replacement is U+FFFD", GlyphSymbol.replacement.value ==== cps(0xfffd)),
    example(
      "the constants pass from",
      Result.all(
        List(
          Result.assert(GlyphSymbol.from(" ").isRight),
          Result.assert(GlyphSymbol.from(cps(0xfffd)).isRight),
        )
      ),
    ),
  )

  def testBufferGlyphs: Property =
    for {
      buffer <- BufferGens.buffer.log("buffer")
    } yield {
      Result.all(
        buffer.cells.toList.map {
          case Cell.Glyph(symbol, _, _) => Result.assert(GlyphSymbol.from(symbol.value).isRight).log(s"rejected: ${symbol.value}")
          case Cell.Continuation(_) => Result.success
        }
      )
    }

  def testDefinition: Property =
    NastyGens.cluster.forAll.map { cluster =>
      val expected = Graphemes.count(cluster) === 1 && WidthPolicy.default.clusterWidth(cluster) > 0 && !hasUnpairedSurrogate(cluster, 0)
      Result.assert(GlyphSymbol.from(cluster).isRight === expected).log(s"cluster ${cluster.map(c => c.toInt.toHexString).mkString(",")}")
    }

  def testApply: Result = {
    import scala.compiletime.testing.typeCheckErrors

    val actual = typeCheckErrors(
      """
        GlyphSymbol("가")
        GlyphSymbol("0️⃣")
        GlyphSymbol("ហ্𑎳")
        GlyphSymbol("ഛ꧀ᮻ")
        GlyphSymbol("𐭸")
        GlyphSymbol("🆰‍⚛")
        GlyphSymbol("ｹﾞ")
        GlyphSymbol("򹆞𑵂")
        GlyphSymbol("𐘂")
        GlyphSymbol("ｹﾟ")
        GlyphSymbol("🇮")
        GlyphSymbol("˻")
        GlyphSymbol("𑼇꫶𐨦")
        GlyphSymbol("ન꧀𑨣")
        GlyphSymbol("񻻏̪")
        GlyphSymbol("🧞‍🰺")
        GlyphSymbol("ᩆ𐨿ဧ")
        GlyphSymbol("♟‍🶨")
        GlyphSymbol("ឤ𑩇𑎎")
        GlyphSymbol("📣‍🺖")
        GlyphSymbol("𑵆񪿄")
        GlyphSymbol("2️⃣")
        GlyphSymbol("ｾﾞ")
        GlyphSymbol("🇭")
        GlyphSymbol("🇼🇦")
        GlyphSymbol("🤜‍🫯")
      """
    ).map(_.message).mkString("\n")

    val expected = ""

    actual ==== expected
  }

  def testKept: Property =
    NastyGens.cluster.forAll.map { cluster =>
      GlyphSymbol.from(cluster) match {
        case Right(symbol) => Assertions.eqv(symbol.value, cluster)
        case Left(_) => Result.success
      }
    }

  def testApplyEmptyString: Result = {
    import scala.compiletime.testing.typeCheckErrors

    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("")
      """
    ).map(_.message).mkString("\n")

    val expected =
      expectedInvalidValueMessage("")

    actual ==== expected
  }

  def testFromEmptyString: Result = {
    val actual = GlyphSymbol.from("")

    val expected = expectedInvalidValueMessageWithoutQuotes("").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyTwoClusters: Result = {
    import scala.compiletime.testing.typeCheckErrors

    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("ab")
      """
    ).map(_.message).mkString("\n")

    val expected =
      expectedInvalidValueMessage("ab")

    actual ==== expected
  }

  def testFromTwoClusters: Result = {
    val actual = GlyphSymbol.from("ab")

    val expected = expectedInvalidValueMessageWithoutQuotes("ab").asLeft[GlyphSymbol]

    actual ==== expected
  }

}
