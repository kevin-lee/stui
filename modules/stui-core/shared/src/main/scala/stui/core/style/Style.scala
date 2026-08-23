package stui.core.style

import cats.{Eq, Hash, Monoid, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** A style patch: what a widget wants to change about a cell. `None` leaves the colour as it is, `addModifiers` turns attributes on,
  * `subModifiers` turns them off. [[Style.patch]] composes patches (a monoid with [[Style.empty]] as identity), [[CellStyle.patch]]
  * applies one to a cell's resolved appearance.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Style(
  fg: Option[Color],
  bg: Option[Color],
  underlineColor: Option[Color],
  addModifiers: Modifiers,
  subModifiers: Modifiers,
) derives Eq,
      Show,
      Hash

object Style {

  val empty: Style = Style(none[Color], none[Color], none[Color], Modifiers.empty, Modifiers.empty)

  /** `empty` and `patch`: associative because, per colour and per modifier bit, the last operand that says something wins. */
  given monoid: Monoid[Style] = Monoid.instance(empty, (a, b) => a.patch(b))

  extension (style: Style) {

    /** `other` applied on top of this patch: its colours win when present, its `subModifiers` cancel earlier additions, its `addModifiers`
      * cancel earlier removals (Ratatui's algebra).
      */
    def patch(other: Style): Style =
      Style(
        other.fg.orElse(style.fg),
        other.bg.orElse(style.bg),
        other.underlineColor.orElse(style.underlineColor),
        style.addModifiers.diff(other.subModifiers).union(other.addModifiers),
        style.subModifiers.diff(other.addModifiers).union(other.subModifiers),
      )

    def withFg(color: Color): Style = style.copy(fg = color.some)

    def withBg(color: Color): Style = style.copy(bg = color.some)

    def withUnderlineColor(color: Color): Style = style.copy(underlineColor = color.some)

    /** Turns the modifier on (and forgets an earlier removal). */
    def addModifier(modifier: Modifier): Style =
      style.copy(addModifiers = style.addModifiers.add(modifier), subModifiers = style.subModifiers.remove(modifier))

    /** Turns the modifier off (and forgets an earlier addition). */
    def removeModifier(modifier: Modifier): Style =
      style.copy(addModifiers = style.addModifiers.remove(modifier), subModifiers = style.subModifiers.add(modifier))

    def bold: Style = style.addModifier(Modifier.Bold)

    def dim: Style = style.addModifier(Modifier.Dim)

    def italic: Style = style.addModifier(Modifier.Italic)

    def underlined: Style = style.addModifier(Modifier.Underlined)

    def slowBlink: Style = style.addModifier(Modifier.SlowBlink)

    def rapidBlink: Style = style.addModifier(Modifier.RapidBlink)

    def reversed: Style = style.addModifier(Modifier.Reversed)

    def hidden: Style = style.addModifier(Modifier.Hidden)

    def crossedOut: Style = style.addModifier(Modifier.CrossedOut)

  }

}
