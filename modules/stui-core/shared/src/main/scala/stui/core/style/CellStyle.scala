package stui.core.style

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** The resolved appearance of a cell, exactly what the terminal shows: concrete colours (`Reset` for the terminal default) and one set of
  * attributes. A [[Style]] is a patch over it.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class CellStyle(fg: Color, bg: Color, underlineColor: Color, modifiers: Modifiers) derives Eq, Show, Hash

object CellStyle {

  /** The terminal's own colours with no attributes. */
  val default: CellStyle = CellStyle(Color.Reset, Color.Reset, Color.Reset, Modifiers.empty)

  extension (cellStyle: CellStyle) {

    /** Colours replaced where the patch has one, modifiers gain `addModifiers` and then lose `subModifiers` (so a modifier in both sets ends
      * up off, Ratatui's order). Law: `patch(a).patch(b) == patch(a.patch(b))`.
      */
    def patch(style: Style): CellStyle =
      CellStyle(
        style.fg.getOrElse(cellStyle.fg),
        style.bg.getOrElse(cellStyle.bg),
        style.underlineColor.getOrElse(cellStyle.underlineColor),
        cellStyle.modifiers.union(style.addModifiers).diff(style.subModifiers),
      )

    /** The patch that turns any cell into this appearance. */
    def toStyle: Style =
      Style(cellStyle.fg.some, cellStyle.bg.some, cellStyle.underlineColor.some, cellStyle.modifiers, Modifiers.empty)

  }

}
