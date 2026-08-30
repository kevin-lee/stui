package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.style.Style
import stui.core.text.Text

/** One entry of a [[ListView]]: a [[stui.core.text.Text]] (one or more lines, each aligned through the text model) and a style
  * patched over the item's rows before the content is drawn.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class ListItem(content: Text, style: Style) derives Eq, Show, Hash

object ListItem {

  /** The text with no item style. */
  def of(content: Text): ListItem = ListItem(content, Style.empty)

  /** [[of]] over [[stui.core.text.Text.raw]]. */
  def raw(content: String): ListItem = of(Text.raw(content))

  /** [[of]] over [[stui.core.text.Text.styled]]. */
  def styled(content: String, style: Style): ListItem = of(Text.styled(content, style))

  extension (item: ListItem) {

    /** The item with the style replaced. */
    def withStyle(style: Style): ListItem = item.copy(style = style)

    /** The rows the item occupies: its line count, at least 1 (an empty text still takes a row, so a selection is always visible). */
    def height: Int = math.max(1, item.content.height)

  }

}
