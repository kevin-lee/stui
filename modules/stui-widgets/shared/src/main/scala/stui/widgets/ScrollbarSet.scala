package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.capability.{Capabilities, GlyphSet}

/** The symbols a [[Scrollbar]] is drawn with, each exactly one width-1 grapheme cluster: the track, the thumb, and the optional
  * begin and end glyphs (the arrows). [[ScrollbarSet.forCapabilities]] selects a set from a [[stui.core.capability.Capabilities]]
  * value (design doc 7.3, M2b) - never from an environment read.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class ScrollbarSet(track: String, thumb: String, begin: Option[String], end: Option[String]) derives Eq, Show, Hash

object ScrollbarSet {

  /** A light vertical line (U+2502), the full block, and the up and down triangles (U+25B2, U+25BC). */
  val vertical: ScrollbarSet = ScrollbarSet("│", "█", "▲".some, "▼".some)

  /** A double vertical line (U+2551) with the same thumb and arrows. */
  val doubleVertical: ScrollbarSet = ScrollbarSet("║", "█", "▲".some, "▼".some)

  /** A light horizontal line (U+2500), the full block, and the left and right pointers (U+25C4, U+25BA). */
  val horizontal: ScrollbarSet = ScrollbarSet("─", "█", "◄".some, "►".some)

  /** A double horizontal line (U+2550) with the same thumb and pointers. */
  val doubleHorizontal: ScrollbarSet = ScrollbarSet("═", "█", "◄".some, "►".some)

  /** `|`, `#`, `^`, and `v`. */
  val asciiVertical: ScrollbarSet = ScrollbarSet("|", "#", "^".some, "v".some)

  /** `-`, `#`, `<`, and `>`. */
  val asciiHorizontal: ScrollbarSet = ScrollbarSet("-", "#", "<".some, ">".some)

  /** The single-line Unicode set of the orientation. */
  def forOrientation(orientation: ScrollbarOrientation): ScrollbarSet =
    if (orientation.isVertical) vertical else horizontal

  /** The set the capabilities select ([[stui.core.capability.Capabilities.effectiveGlyphs]]): the single-line Unicode set of the
    * orientation, or its ASCII twin.
    */
  def forCapabilities(orientation: ScrollbarOrientation, capabilities: Capabilities): ScrollbarSet =
    capabilities.effectiveGlyphs match {
      case GlyphSet.Unicode => forOrientation(orientation)
      case GlyphSet.Ascii => if (orientation.isVertical) asciiVertical else asciiHorizontal
    }

  extension (set: ScrollbarSet) {

    /** The set without begin and end glyphs (the track takes the whole lane). */
    def withoutArrows: ScrollbarSet = set.copy(begin = none[String], end = none[String])

  }

}
