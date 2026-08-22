package stui.unicode.oracle

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.{GraphemeBreakCase, GraphemeBreakTestCorpus}
import stui.unicode.gen.NastyGens
import stui.unicode.{Graphemes, KnownDivergences, UnicodeVersion}

import scala.scalajs.js

/** Differential test against Node's `Intl.Segmenter`. The corpus comparison tolerates the recorded divergences for the runtime's Unicode
  * version, and the random comparison is a hard gate only when that version matches the tables.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object IntlSegmenterOracleSpec extends Properties {

  val oracleName: String = "Intl.Segmenter"

  val oracleUnicodeVersion: String = Process.versions.unicode

  private val segmenter: IntlSegmenter = new IntlSegmenter("en", js.Dynamic.literal(granularity = "grapheme"))

  override def tests: List[Test] = List(
    example(
      s"corpus: disagreements with $oracleName (Unicode $oracleUnicodeVersion, Node ${Process.versions.node}) are all recorded",
      testCorpus,
    ),
    property(s"random: identical to $oracleName when the Unicode versions match", testRandom),
  )

  def oracleBoundaries(s: String): List[Int] =
    if (s.isEmpty) List(0)
    else 0 :: (segmenter.segment(s).iterator.map(_.index).toList.drop(1) ++ List(s.length))

  def testCorpus: Result = {
    val cases         = GraphemeBreakTestCorpus.lines.zipWithIndex.map { case (line, index) => GraphemeBreakCase.parse(index + 1, line) }
    val disagreements = cases.collect {
      case Right(testCase) if !Graphemes.boundaries(testCase.input).toList.sameElements(oracleBoundaries(testCase.input)) =>
        testCase.lineNumber
    }.toSet
    val allowed       = KnownDivergences.oracle(oracleName, oracleUnicodeVersion)
    val unrecorded    = disagreements.diff(allowed)
    Result
      .assert(unrecorded.isEmpty)
      .log(
        s"$oracleName Unicode $oracleUnicodeVersion disagrees on ${disagreements.size.toString} corpus lines: ${disagreements.toList.sorted.mkString(", ")}"
      )
      .log(s"unrecorded: ${unrecorded.toList.sorted.mkString(", ")}")
  }

  def testRandom: Property =
    UnicodeVersion.current.renderShort match {
      case `oracleUnicodeVersion` =>
        NastyGens.text(Range.linear(0, 30)).forAll.map(s => Graphemes.boundaries(s).toList ==== oracleBoundaries(s))
      case ours =>
        Gen.constant(()).forAll.map(_ => Result.success.log(s"skipped: $oracleName is Unicode $oracleUnicodeVersion, the tables are $ours"))
    }

}
