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

  override def tests: List[Test] = List(
    property("every glyph of a generated buffer round-trips through from", testBufferGlyphs),
    property("from agrees with the definition on nasty clusters", testDefinition),
    property("an accepted string is kept as is", testKept),
    example("the empty string is rejected", Result.assert(GlyphSymbol.from("").isLeft)),
    example("two clusters are rejected", Result.assert(GlyphSymbol.from("ab").isLeft)),
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
      Result.all(List(Result.assert(GlyphSymbol.from(" ").isRight), Result.assert(GlyphSymbol.from(cps(0xfffd)).isRight))),
    ),
  )

  def testBufferGlyphs: Property =
    BufferGens.buffer.forAll.map { buffer =>
      Result.all(buffer.cells.toList.map {
        case Cell.Glyph(symbol, _, _) => Result.assert(GlyphSymbol.from(symbol.value).isRight).log(s"rejected: ${symbol.value}")
        case Cell.Continuation(_) => Result.success
      })
    }

  def testDefinition: Property =
    NastyGens.cluster.forAll.map { cluster =>
      val expected = Graphemes.count(cluster) === 1 && WidthPolicy.default.clusterWidth(cluster) > 0 && !hasUnpairedSurrogate(cluster, 0)
      Result.assert(GlyphSymbol.from(cluster).isRight === expected).log(s"cluster ${cluster.map(c => c.toInt.toHexString).mkString(",")}")
    }

  def testKept: Property =
    NastyGens.cluster.forAll.map { cluster =>
      GlyphSymbol.from(cluster) match {
        case Right(symbol) => Assertions.eqv(symbol.value, cluster)
        case Left(_) => Result.success
      }
    }

}
