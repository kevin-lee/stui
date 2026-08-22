package stui.unicode.oracle

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.{GraphemeBreakCase, GraphemeBreakTestCorpus}
import stui.unicode.gen.NastyGens
import stui.unicode.internal.IntOps.*
import stui.unicode.{Graphemes, KnownDivergences, UnicodeVersion}

import java.text.BreakIterator
import java.util.Locale

/** Differential test against java.text.BreakIterator (extended grapheme clusters since JDK 20). The oracle's Unicode version usually
  * lags ours, so the corpus comparison tolerates the recorded divergences and the random comparison is a hard gate only when the versions
  * match.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object BreakIteratorOracleSpec extends Properties {

  val oracleName: String = "BreakIterator"

  /** Probed through characters first assigned in each Unicode version (DerivedAge.txt). */
  val oracleUnicodeVersion: String =
    if (Character.isDefined(0x1faea)) "17.0"
    else if (Character.isDefined(0x1fae9)) "16.0"
    else if (Character.isDefined(0x2ebf0)) "15.1"
    else "15.0"

  override def tests: List[Test] = List(
    example(s"corpus: disagreements with $oracleName (Unicode $oracleUnicodeVersion) are all recorded", testCorpus),
    property(s"random: identical to $oracleName when the Unicode versions match", testRandom),
  )

  def oracleBoundaries(s: String): List[Int] = {
    val iterator = BreakIterator.getCharacterInstance(Locale.ROOT)
    iterator.setText(s)
    iterator.first() :: Iterator.continually(iterator.next()).takeWhile(offset => offset !== BreakIterator.DONE).toList
  }

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
