package stui.widgets

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Rect
import stui.core.text.Line
import stui.testkit.Assertions
import stui.testkit.gen.{GeometryGens, TextGens}
import stui.testkit.laws.WidgetLaws
import stui.unicode.{Graphemes, WidthPolicy}
import stui.widgets.gen.WidgetGens
import stui.widgets.internal.WordWrap

/** The widget laws for Paragraph and the word-wrap invariants of the M1d plan: row widths within the limit, no leading whitespace
  * under trimming, and non-whitespace cluster preservation.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object ParagraphSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private val policy: WidthPolicy = WidthPolicy.default

  private val widths: Gen[Int] = Gen.int(Range.linear(1, 12))

  private val lines: Gen[Line] = TextGens.line(Range.linear(0, 3), Range.linear(0, 10))

  private def clusterWidth(cluster: String): Int = math.min(2, policy.clusterWidth(cluster))

  private def isWhitespaceCluster(cluster: String): Boolean =
    cluster === "\u200b" || (cluster.nonEmpty && allWhitespace(cluster, 0))

  @scala.annotation.tailrec
  private def allWhitespace(s: String, i: Int): Boolean =
    if (i >= s.length) {
      true
    } else {
      val cp = s.codePointAt(i)
      if (Character.isWhitespace(cp)) allWhitespace(s, i + Character.charCount(cp)) else false
    }

  private def contentOf(line: Line): String = line.spans.map(_.content).mkString

  override def tests: List[Test] =
    WidgetLaws.laws("paragraph", WidgetGens.paragraphWidget, outer) ++ List(
      property("no wrapped row is wider than the width", testRowWidths),
      property("trimming leaves no leading whitespace", testTrimmed),
      property("non-whitespace clusters survive wrapping in order", testPreservation),
      property("lineCount matches the wrapped row count", testLineCount),
      property("lineWidth is the text width", testLineWidth),
    )

  def testRowWidths: Property =
    for {
      line  <- lines.forAll
      width <- widths.forAll
      trim  <- Gen.boolean.forAll
    } yield {
      val rows = WordWrap.wrap(Vector(line), width, trim, policy)
      Result.all(rows.toList.map(row => Result.assert(row.width(policy) <= width).log(s"row too wide: ${contentOf(row)}")))
    }

  def testTrimmed: Property =
    for {
      line  <- lines.forAll
      width <- widths.forAll
    } yield {
      val rows = WordWrap.wrap(Vector(line), width, true, policy)
      Result.all(
        rows.toList.map { row =>
          val leading = Graphemes.clusters(contentOf(row)).headOption
          Result.assert(!leading.exists(isWhitespaceCluster)).log(s"leading whitespace on: ${contentOf(row)}")
        }
      )
    }

  def testPreservation: Property =
    for {
      line  <- lines.forAll
      width <- widths.forAll
      trim  <- Gen.boolean.forAll
    } yield {
      val expected = Graphemes
        .clusters(contentOf(line))
        .filter(cluster => clusterWidth(cluster) <= width && !isWhitespaceCluster(cluster))
      val actual   = WordWrap
        .wrap(Vector(line), width, trim, policy)
        .flatMap(row => Graphemes.clusters(contentOf(row)))
        .filter(cluster => !isWhitespaceCluster(cluster))
      Assertions.eqv(actual, expected)
    }

  def testLineCount: Property =
    for {
      paragraph <- WidgetGens.paragraph.forAll
      width     <- widths.forAll
    } yield {
      val nonNegWidth = GeometryGens.nonNegOrZero(width.toLong)
      val expected    = paragraph.wrap match {
        case Some(Wrap.Word) => WordWrap.wrap(paragraph.text.resolvedLines, width, false, policy).length
        case Some(Wrap.WordTrimmed) => WordWrap.wrap(paragraph.text.resolvedLines, width, true, policy).length
        case None => paragraph.text.height
      }
      Assertions.eqv(paragraph.lineCount(nonNegWidth, policy), expected)
    }

  def testLineWidth: Property =
    WidgetGens.paragraph.forAll.map(paragraph => Assertions.eqv(paragraph.lineWidth(policy), paragraph.text.width(policy)))

}
