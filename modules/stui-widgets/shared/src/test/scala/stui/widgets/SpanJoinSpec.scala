package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.style.Style
import stui.core.text.{Alignment, Line, Span}
import stui.testkit.Assertions
import stui.testkit.gen.{NastyGens, StyleGens}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.widgets.internal.WordWrap

/** The span join break (issue 27): the wrapper tokenises per span, so two clusters that would form one grapheme cluster must never be
  * merged back into one span. Without the break a merged span re-segments into different clusters, and VS16 forces the joined one to
  * width 2, which produces a row wider than the width.
  *
  * The three shapes found on 2026-08-31 are pinned as examples - the CI counterexample of run 90290543213 (`ParagraphSpec` seed
  * 1489482538958), the VS16 row-width case through both `wrap` and `truncate`, and the trimmed whitespace case - together with the
  * single-span control that must not change, and a property placing joinable symbols one per span.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object SpanJoinSpec extends Properties {

  private val policy: WidthPolicy = WidthPolicy.default

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  /** Hangul choseong kiyeok and jungseong filler (U+1100 U+1160), one cluster of width 2. */
  private val hangulLv: String = cps(0x1100, 0x1160)

  /** Arabic small high Farsi yeh (U+08CA), a standalone mark of width 0 - the tail of the CI counterexample. */
  private val arabicMark: String = cps(0x08ca)

  /** Variation selector 16 (U+FE0F), which forces the cluster it joins to width 2. */
  private val vs16: String = cps(0xfe0f)

  /** Combining acute accent (U+0301), a standalone mark of width 0. */
  private val acute: String = cps(0x0301)

  /** The standalone Tamil vowel sign I (U+0BBF, a spacing mark of width 1). */
  private val tamilMark: String = cps(0x0bbf)

  private val widths: Gen[Int] = Gen.int(Range.linear(1, 12))

  private val joinable: Gen[String] = Gen.frequency1(
    1 -> NastyGens.regionalIndicators(Range.linear(1, 1)),
    1 -> Gen.constant(tamilMark),
    1 -> Gen.constant(vs16),
  )

  private def lineOf(contents: Vector[String], style: Style): Line =
    Line(contents.map(content => Span(content, style)), Style.empty, none[Alignment])

  private def wrapped(contents: Vector[String], width: Int, trim: Boolean): Vector[Line] =
    WordWrap.wrap(Vector(lineOf(contents, Style.empty)), width, trim, policy)

  private def contentsOf(rows: Vector[Line]): Vector[Vector[String]] = rows.map(_.spans.map(_.content))

  private def widthsOf(rows: Vector[Line]): Vector[Int] = rows.map(_.width(policy))

  private def hex(s: String): String = s.map(unit => f"${unit.toInt}%04X").mkString(" ")

  override def tests: List[Test] = List(
    example("the CI counterexample keeps the mark in its own span", testCiCounterexample),
    example("a span join is not merged into an over-wide cluster", testVs16Wrap),
    example("truncate does not merge a span join either", testVs16Truncate),
    example("an untrimmed whitespace span stays apart from a following mark", testWhitespaceUntrimmed),
    example("a trimmed whitespace span leaves the mark alone", testWhitespaceTrimmed),
    example("a cluster already inside one span is untouched", testSingleSpanControl),
    property("joinable symbols one per span keep every row within the width", testJoinableSymbols),
  )

  def testCiCounterexample: Result = {
    val rows = wrapped(Vector(hangulLv, arabicMark), 1, false)
    Result.all(
      List(
        Assertions.eqv(contentsOf(rows), Vector(Vector(arabicMark))),
        Assertions.eqv(widthsOf(rows), Vector(0)),
      )
    )
  }

  def testVs16Wrap: Result = {
    val rows = wrapped(Vector("x", vs16), 1, false)
    Result.all(
      List(
        Assertions.eqv(contentsOf(rows), Vector(Vector("x", vs16))),
        Assertions.eqv(widthsOf(rows), Vector(1)),
      )
    )
  }

  def testVs16Truncate: Result = {
    val row = WordWrap.truncate(lineOf(Vector("x", vs16), Style.empty), 1, 0, policy)
    Result.all(
      List(
        Assertions.eqv(row.spans.map(_.content), Vector("x", vs16)),
        Assertions.eqv(row.width(policy), 1),
      )
    )
  }

  def testWhitespaceUntrimmed: Result = {
    val rows = wrapped(Vector(" ", acute), 4, false)
    Result.all(
      List(
        Assertions.eqv(contentsOf(rows), Vector(Vector(" ", acute))),
        Assertions.eqv(widthsOf(rows), Vector(1)),
      )
    )
  }

  def testWhitespaceTrimmed: Result = {
    val rows = wrapped(Vector(" ", acute), 4, true)
    Result.all(
      List(
        Assertions.eqv(contentsOf(rows), Vector(Vector(acute))),
        Assertions.eqv(widthsOf(rows), Vector(0)),
      )
    )
  }

  def testSingleSpanControl: Result = {
    val rows = wrapped(Vector("x" + vs16), 2, false)
    Result.all(
      List(
        Assertions.eqv(contentsOf(rows), Vector(Vector("x" + vs16))),
        Assertions.eqv(widthsOf(rows), Vector(2)),
      )
    )
  }

  /** One style for the whole line, so every neighbouring pair is a merge candidate and the break is the only thing keeping them apart. */
  def testJoinableSymbols: Property =
    for {
      style   <- StyleGens.style.forAll
      symbols <- joinable.list(Range.linear(2, 6)).forAll
      width   <- widths.forAll
      trim    <- Gen.boolean.forAll
    } yield {
      val rows = WordWrap.wrap(Vector(lineOf(symbols.toVector, style)), width, trim, policy)
      Result.all(rows.toList.flatMap(row => rowResults(row, width)))
    }

  private def rowResults(row: Line, width: Int): List[Result] =
    Result.assert(row.width(policy) <= width).log(s"row too wide: ${hex(row.spans.map(_.content).mkString)}") ::
      row
        .spans
        .toList
        .sliding(2)
        .collect { case List(left, right) => (left, right) }
        .map { (left, right) =>
          Result
            .assert(left.style =!= right.style || Graphemes.joins(lastCluster(left.content), firstCluster(right.content)))
            .log(s"two spans with equal styles that do not join: ${hex(left.content)} and ${hex(right.content)}")
        }
        .toList

  private def lastCluster(s: String): String = Graphemes.clusters(s).lastOption.getOrElse("")

  private def firstCluster(s: String): String = Graphemes.clusters(s).headOption.getOrElse("")

}
