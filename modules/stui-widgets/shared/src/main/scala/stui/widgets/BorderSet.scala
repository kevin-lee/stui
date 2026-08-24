package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The eight symbols a [[Block]] border is drawn with, each exactly one width-1 grapheme cluster. The dashed and quadrant sets of
  * Ratatui arrive with later widgets.
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

}
