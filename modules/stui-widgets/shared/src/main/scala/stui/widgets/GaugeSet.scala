package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.capability.{Capabilities, GlyphSet}

/** The symbols a [[Gauge]] bar is drawn with, each exactly one width-1 grapheme cluster: `full` for a filled cell, `eighths` for
  * the partial cell (one eighth to seven eighths, in that order; empty when the set has no fractional glyphs), `empty` for the rest.
  * [[GaugeSet.forCapabilities]] selects a set from a [[stui.core.capability.Capabilities]] value (design doc 7.3, M2b) - never from
  * an environment read.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class GaugeSet(full: String, eighths: Vector[String], empty: String) derives Eq, Show, Hash

object GaugeSet {

  /** The full block (U+2588) with the seven left-block eighths (U+258F down to U+2589) and a space. */
  val unicode: GaugeSet = GaugeSet("█", Vector("▏", "▎", "▍", "▌", "▋", "▊", "▉"), " ")

  /** `#` for a filled cell, no fractions, a space for the rest. */
  val ascii: GaugeSet = GaugeSet("#", Vector.empty[String], " ")

  /** The set the capabilities select ([[stui.core.capability.Capabilities.effectiveGlyphs]]): [[unicode]] under Unicode glyphs,
    * [[ascii]] otherwise.
    */
  def forCapabilities(capabilities: Capabilities): GaugeSet = capabilities.effectiveGlyphs match {
    case GlyphSet.Unicode => unicode
    case GlyphSet.Ascii => ascii
  }

}
