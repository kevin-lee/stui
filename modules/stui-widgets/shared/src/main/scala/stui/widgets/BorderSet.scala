package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.capability.{Capabilities, GlyphSet}

/** The eight symbols a [[Block]] border is drawn with, each exactly one width-1 grapheme cluster. The dashed and quadrant sets of
  * Ratatui arrive with later widgets. [[BorderSet.forCapabilities]] and [[BorderSet.orAscii]] select a set from a
  * [[stui.core.capability.Capabilities]] value (design doc 7.3, M2a) - never from an environment read.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class BorderSet(
  topLeft: String,
  topRight: String,
  bottomLeft: String,
  bottomRight: String,
  verticalLeft: String,
  verticalRight: String,
  horizontalTop: String,
  horizontalBottom: String,
) derives Eq,
      Show,
      Hash

object BorderSet {

  /** Light box drawing: ┌ ┐ └ ┘ │ ─. */
  val plain: BorderSet = BorderSet("┌", "┐", "└", "┘", "│", "│", "─", "─")

  /** Light box drawing with arcs: ╭ ╮ ╰ ╯ │ ─. */
  val rounded: BorderSet = BorderSet("╭", "╮", "╰", "╯", "│", "│", "─", "─")

  /** Double box drawing: ╔ ╗ ╚ ╝ ║ ═. */
  val double: BorderSet = BorderSet("╔", "╗", "╚", "╝", "║", "║", "═", "═")

  /** Heavy box drawing: ┏ ┓ ┗ ┛ ┃ ━. */
  val thick: BorderSet = BorderSet("┏", "┓", "┗", "┛", "┃", "┃", "━", "━")

  /** Eight spaces, a border that only takes room. */
  val empty: BorderSet = BorderSet(" ", " ", " ", " ", " ", " ", " ", " ")

  /** ASCII borders: `+`, `-`, and `|`, the safe set for ambiguous-wide and non-UTF-8 terminals (design doc 7.3, M2a). */
  val ascii: BorderSet = BorderSet("+", "+", "+", "+", "|", "|", "-", "-")

  /** The set the capabilities select ([[stui.core.capability.Capabilities.effectiveGlyphs]]): [[plain]] under Unicode glyphs,
    * [[ascii]] otherwise.
    */
  def forCapabilities(capabilities: Capabilities): BorderSet = capabilities.effectiveGlyphs match {
    case GlyphSet.Unicode => plain
    case GlyphSet.Ascii => ascii
  }

  /** `preferred` when the effective glyph set is Unicode, [[ascii]] otherwise - for an app that wants, say, rounded-or-ascii. */
  def orAscii(preferred: BorderSet, capabilities: Capabilities): BorderSet = capabilities.effectiveGlyphs match {
    case GlyphSet.Unicode => preferred
    case GlyphSet.Ascii => ascii
  }

}
