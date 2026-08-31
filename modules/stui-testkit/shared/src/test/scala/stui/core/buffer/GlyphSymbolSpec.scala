package stui.core.buffer

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.{BufferGens, NastyGens}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec
import scala.compiletime.testing.typeCheckErrors

/** `GlyphSymbol.from` accepts exactly the clusters the canvas produces: one cluster, valid UTF-16, non-zero width.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object GlyphSymbolSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  /* deliberately duplicates `GlyphSymbolValidator.hasUnpairedSurrogate` rather than calling it, so `testDefinition` restates the
   * specification independently of the implementation and any change to the production code is detected */
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
    property("an accepted string is kept as is", testKept),
    example("apply() with a valid literal compiles", testApply),
    example("""apply("") (an empty string) should not compile""", testApplyEmptyString),
    example("""from("") (an empty string) should return Left(error)""", testFromEmptyString),
    example("""apply("ab") (two clusters) should not compile""", testApplyTwoClusters),
    example("""from("ab") (two clusters) should return Left(error)""", testFromTwoClusters),
    example("""apply("가나") (two clusters) should not compile""", testApplyTwoHangulClusters),
    example("""from("가나") (two clusters) should return Left(error)""", testFromTwoHangulClusters),
    example("""apply("\t") (a tab) should not compile""", testApplyTab),
    example("""from("\t") (a tab) should return Left(error)""", testFromTab),
    example("""apply("\n") (a newline) should not compile""", testApplyNewline),
    example("""from("\n") (a newline) should return Left(error)""", testFromNewline),
    example("""apply("\u0000") (NUL) should not compile""", testApplyNul),
    example("""from("\u0000") (NUL) should return Left(error)""", testFromNul),
    example("""apply("\u001b") (a control) should not compile""", testApplyControl),
    example("""from("\u001b") (a control) should return Left(error)""", testFromControl),
    example("""apply("\u007f") (DEL) should not compile""", testApplyDel),
    example("""from("\u007f") (DEL) should return Left(error)""", testFromDel),
    example("""apply("\u0301") (a lone combining mark) should not compile""", testApplyLoneCombiningMark),
    example("""from("\u0301") (a lone combining mark) should return Left(error)""", testFromLoneCombiningMark),
    example("""apply("\u200b") (a zero-width space) should not compile""", testApplyZeroWidthSpace),
    example("""from("\u200b") (a zero-width space) should return Left(error)""", testFromZeroWidthSpace),
    example("""apply("\u200d") (a lone ZWJ) should not compile""", testApplyLoneZwj),
    example("""from("\u200d") (a lone ZWJ) should return Left(error)""", testFromLoneZwj),
    example("""apply("\ufe0f") (a lone VS16) should not compile""", testApplyLoneVs16),
    example("""from("\ufe0f") (a lone VS16) should return Left(error)""", testFromLoneVs16),
    example("""from("\ud800") (a lone surrogate) should return Left(error)""", testFromLoneSurrogate),
    example("""apply("가") should compile and keep the value""", testApplyHangul),
    example("""from("가") should return Right(value)""", testFromHangul),
    example("""apply("コ") should compile and keep the value""", testApplyWideGlyph),
    example("""from("コ") should return Right(value)""", testFromWideGlyph),
    example("""apply("⌨\ufe0f") should compile and keep the value""", testApplyVs16Sequence),
    example("""from("⌨\ufe0f") should return Right(value)""", testFromVs16Sequence),
    example("""apply("🇺🇸") should compile and keep the value""", testApplyFlagPair),
    example("""from("🇺🇸") should return Right(value)""", testFromFlagPair),
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

  def testApplyTwoHangulClusters: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("가나")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("가나")

    actual ==== expected
  }

  def testFromTwoHangulClusters: Result = {
    val actual = GlyphSymbol.from("가나")

    val expected = expectedInvalidValueMessageWithoutQuotes("가나").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyTab: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\t")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\\t")

    actual ==== expected
  }

  def testFromTab: Result = {
    val actual = GlyphSymbol.from("\t")

    val expected = expectedInvalidValueMessageWithoutQuotes("\t").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyNewline: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\n")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\\n")

    actual ==== expected
  }

  def testFromNewline: Result = {
    val actual = GlyphSymbol.from("\n")

    val expected = expectedInvalidValueMessageWithoutQuotes("\n").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyNul: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u0000")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\\u0000")

    actual ==== expected
  }

  def testFromNul: Result = {
    val actual = GlyphSymbol.from("\u0000")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u0000").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyControl: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u001b")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\\u001b")

    actual ==== expected
  }

  def testFromControl: Result = {
    val actual = GlyphSymbol.from("\u001b")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u001b").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyDel: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u007f")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\\u007f")

    actual ==== expected
  }

  def testFromDel: Result = {
    val actual = GlyphSymbol.from("\u007f")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u007f").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyLoneCombiningMark: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u0301")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\u0301")

    actual ==== expected
  }

  def testFromLoneCombiningMark: Result = {
    val actual = GlyphSymbol.from("\u0301")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u0301").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyZeroWidthSpace: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u200b")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\u200b")

    actual ==== expected
  }

  def testFromZeroWidthSpace: Result = {
    val actual = GlyphSymbol.from("\u200b")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u200b").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyLoneZwj: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\u200d")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\u200d")

    actual ==== expected
  }

  def testFromLoneZwj: Result = {
    val actual = GlyphSymbol.from("\u200d")

    val expected = expectedInvalidValueMessageWithoutQuotes("\u200d").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyLoneVs16: Result = {
    val actual = typeCheckErrors(
      """
      val _ = GlyphSymbol("\ufe0f")
      """
    ).map(_.message).mkString("\n")

    val expected = expectedInvalidValueMessage("\ufe0f")

    actual ==== expected
  }

  def testFromLoneVs16: Result = {
    val actual = GlyphSymbol.from("\ufe0f")

    val expected = expectedInvalidValueMessageWithoutQuotes("\ufe0f").asLeft[GlyphSymbol]

    actual ==== expected
  }

  /* no `apply` counterpart: `Expr.asTerm.show` renders a lone surrogate as `?`, so a compile-time test would assert a Dotty
   * printer artefact rather than `GlyphSymbol` behaviour */
  def testFromLoneSurrogate: Result = {
    val actual = GlyphSymbol.from("\ud800")

    val expected = expectedInvalidValueMessageWithoutQuotes("\ud800").asLeft[GlyphSymbol]

    actual ==== expected
  }

  def testApplyHangul: Result = GlyphSymbol("가").value ==== "가"

  def testApplyWideGlyph: Result = GlyphSymbol("コ").value ==== "コ"

  def testApplyVs16Sequence: Result = GlyphSymbol("⌨\ufe0f").value ==== "⌨\ufe0f"

  def testApplyFlagPair: Result = GlyphSymbol("🇺🇸").value ==== "🇺🇸"

  def testFromHangul: Result =
    GlyphSymbol.from("가").map(_.value) ==== "가".asRight[String]

  def testFromWideGlyph: Result =
    GlyphSymbol.from("コ").map(_.value) ==== "コ".asRight[String]

  def testFromVs16Sequence: Result =
    GlyphSymbol.from("⌨\ufe0f").map(_.value) ==== "⌨\ufe0f".asRight[String]

  def testFromFlagPair: Result =
    GlyphSymbol.from("🇺🇸").map(_.value) ==== "🇺🇸".asRight[String]

}
