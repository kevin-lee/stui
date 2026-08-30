package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import stui.core.capability.{Capabilities, GlyphSet}
import stui.testkit.Assertions
import stui.testkit.gen.CapabilityGens

/** The capability-driven border set selection (design doc 7.3, M2a): [[BorderSet.forCapabilities]] and [[BorderSet.orAscii]] answer
  * from a [[stui.core.capability.Capabilities]] value alone.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object BorderSetSpec extends Properties {

  private val unicodeCaps: Capabilities = Capabilities.conservative

  private val asciiCaps: Capabilities = Capabilities.conservative.copy(glyphs = GlyphSet.Ascii)

  private val ambiguousCaps: Capabilities = Capabilities.conservative.copy(ambiguousWide = true)

  override def tests: List[Test] = List(
    example("Unicode glyphs select the plain set", Assertions.eqv(BorderSet.forCapabilities(unicodeCaps), BorderSet.plain)),
    example("ASCII glyphs select the ascii set", Assertions.eqv(BorderSet.forCapabilities(asciiCaps), BorderSet.ascii)),
    example("an ambiguous-wide terminal selects the ascii set", Assertions.eqv(BorderSet.forCapabilities(ambiguousCaps), BorderSet.ascii)),
    example(
      "orAscii keeps the preferred set under Unicode",
      Assertions.eqv(BorderSet.orAscii(BorderSet.rounded, unicodeCaps), BorderSet.rounded),
    ),
    example(
      "orAscii falls back under ambiguous-wide",
      Assertions.eqv(BorderSet.orAscii(BorderSet.rounded, ambiguousCaps), BorderSet.ascii),
    ),
    property(
      "forCapabilities follows effectiveGlyphs",
      CapabilityGens.capabilities.forAll.map { capabilities =>
        val expected = capabilities.effectiveGlyphs match {
          case GlyphSet.Unicode => BorderSet.plain
          case GlyphSet.Ascii => BorderSet.ascii
        }
        Assertions.eqv(BorderSet.forCapabilities(capabilities), expected)
      },
    ),
  )

}
