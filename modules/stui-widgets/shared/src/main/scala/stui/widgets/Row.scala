package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.style.Style
import stui.core.text.Text

/** One row of a [[Table]]: its cells as [[stui.core.text.Text]] values (one per column, missing cells stay blank, alignment through
  * the text model), a style patched over the row before the cells are drawn, the row's height in rows, and blank margins above and
  * below it. [[Row.of]] takes the tallest cell as the height (Ratatui's documented default is one row, a recorded divergence).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Row(cells: Vector[Text], style: Style, height: PosInt, topMargin: NonNegInt, bottomMargin: NonNegInt)
    derives Eq,
      Show,
      Hash

object Row {

  /** The cells with no style, the tallest cell's line count as the height (at least 1), and no margins. */
  def of(cells: Text*): Row = fromTexts(cells.toVector)

  /** [[of]] over a vector. */
  def fromTexts(cells: Vector[Text]): Row = Row(cells, Style.empty, tallest(cells), NonNegInt(0), NonNegInt(0))

  /** [[of]] over [[stui.core.text.Text.raw]] cells. */
  def raw(cells: String*): Row = fromTexts(cells.toVector.map(Text.raw))

  /** The tallest cell's line count, at least 1, so the `PosInt` fallback is unreachable. */
  private def tallest(cells: Vector[Text]): PosInt =
    PosInt.from(math.max(1, cells.map(_.height).maxOption.getOrElse(1))).fold(_ => PosInt(1), identity)

  extension (row: Row) {

    /** The row with the style replaced. */
    def withStyle(style: Style): Row = row.copy(style = style)

    /** The row with the height replaced (cells taller than it are cut). */
    def withHeight(height: PosInt): Row = row.copy(height = height)

    /** The row with the blank rows above it replaced. */
    def withTopMargin(margin: NonNegInt): Row = row.copy(topMargin = margin)

    /** The row with the blank rows below it replaced. */
    def withBottomMargin(margin: NonNegInt): Row = row.copy(bottomMargin = margin)

    /** The rows the row occupies: the height plus both margins. */
    def extent: Int = row.height.value + row.topMargin.value + row.bottomMargin.value

    /** The number of cells. */
    def columnCount: Int = row.cells.length

  }

}
