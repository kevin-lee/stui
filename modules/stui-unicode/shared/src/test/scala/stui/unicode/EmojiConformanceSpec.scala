package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.corpus.{EmojiTestCorpus, GraphemeBreakCase}

import scala.util.Try

/** Every fully-qualified emoji of emoji-test.txt is one grapheme cluster of width 2, a hard gate on every platform.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object EmojiConformanceSpec extends Properties {

  override def tests: List[Test] = List(
    example("every fully-qualified emoji is one grapheme cluster", testSingleCluster),
    example("every fully-qualified emoji has width 2", testWidth),
  )

  private val sequences: List[(String, String)] =
    EmojiTestCorpus.lines.filterNot(KnownDivergences.emojiTest.contains).map(line => (line, render(line))).toList

  private def render(line: String): String =
    GraphemeBreakCase.render(line.split(' ').toList.flatMap(hex => Try(Integer.parseInt(hex, 16)).toOption))

  def testSingleCluster: Result = Result.all(sequences.map { case (line, s) => (Graphemes.count(s) ==== 1).log(line) })

  def testWidth: Result = Result.all(sequences.map { case (line, s) => (WidthPolicy.default.width(s) ==== 2).log(line) })

}
