package stui.unicode

import hedgehog.*
import hedgehog.runner.*

/** Keeps KnownDivergences honest: every recorded stui width is what the default policy returns today.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object KnownDivergencesSpec extends Properties {

  override def tests: List[Test] = List(
    example("every recorded unicode-width divergence states stui's current width", testUnicodeWidth)
  )

  def testUnicodeWidth: Result =
    Result.all(KnownDivergences.unicodeWidth.map { divergence =>
      (WidthPolicy.default.width(divergence.input) ==== divergence.stuiWidth).log(divergence.reason)
    })

}
