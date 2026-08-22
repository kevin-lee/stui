package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.{GraphemeBreakCase, GraphemeBreakTestCorpus}

/** The UCD GraphemeBreakTest.txt conformance corpus, a hard gate on every platform.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object GraphemeBreakConformanceSpec extends Properties {

  override def tests: List[Test] = List(
    example("GraphemeBreakTest 17.0.0: every line segments as specified", testClusters),
    example("boundaries agree with clusters on every line", testBoundaries),
  )

  private val cases: List[Either[String, GraphemeBreakCase]] =
    GraphemeBreakTestCorpus.lines.zipWithIndex.map { case (line, index) => GraphemeBreakCase.parse(index + 1, line) }.toList

  private def forEachCase(check: GraphemeBreakCase => Result): Result =
    Result.all(cases.map {
      case Left(error) => Result.failure.log(error)
      case Right(testCase) => check(testCase)
    })

  def testClusters: Result = forEachCase { testCase =>
    (Graphemes.clusters(testCase.input) ==== testCase.expected)
      .log(s"line ${testCase.lineNumber.toString}: ${GraphemeBreakTestCorpus.lines.lift(testCase.lineNumber - 1).getOrElse("")}")
  }

  def testBoundaries: Result = forEachCase { testCase =>
    val expected = testCase.expected.scanLeft(0)((offset, cluster) => offset + cluster.length)
    (Graphemes.boundaries(testCase.input).toVector ==== expected).log(s"line ${testCase.lineNumber.toString}")
  }

}
