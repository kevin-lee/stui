package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.gen.NastyGens
import stui.unicode.internal.IntOps.*

/** [[Graphemes.joins]], the shared question "would `next` extend `last`'s cluster into one cluster". Two callers keep separate model
  * units apart with it: the writer's rule R2a between two cells, and the word wrapper's span join break between two spans (issue 27).
  *
  * The units pin the shapes both rules exist for, and the law says the ASCII fast path is a pure optimisation.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object GraphemeJoinSpec extends Properties {

  /** A lone regional indicator (U+1F1E6). */
  private val ri: String = NastyGens.render(List(0x1f1e6))

  /** The standalone Tamil vowel sign I (U+0BBF, a spacing mark of width 1). */
  private val mark: String = NastyGens.render(List(0x0bbf))

  /** Variation selector 16 (U+FE0F), which forces the cluster it joins to width 2. */
  private val vs16: String = NastyGens.render(List(0xfe0f))

  override def tests: List[Test] = List(
    example("two regional indicators join", Result.assert(Graphemes.joins(ri, ri))),
    example("a letter then a regional indicator does not join", Result.assert(!Graphemes.joins("a", ri))),
    example("a spacing mark joins a letter", Result.assert(Graphemes.joins("a", mark))),
    example("a spacing mark joins a space", Result.assert(Graphemes.joins(" ", mark))),
    example("an empty last never joins", Result.assert(!Graphemes.joins("", mark))),
    example("two letters do not join", Result.assert(!Graphemes.joins("a", "b"))),
    example("VS16 joins a printable ASCII character", Result.assert(Graphemes.joins("x", vs16))),
    property("joins agrees with the segmenter", testAgreesWithSegmenter),
  )

  def testAgreesWithSegmenter: Property =
    for {
      last <- NastyGens.cluster.forAll
      next <- NastyGens.cluster.forAll
    } yield Graphemes.joins(last, next) ==== (last.nonEmpty && Graphemes.count(last + next) === 1)

}
