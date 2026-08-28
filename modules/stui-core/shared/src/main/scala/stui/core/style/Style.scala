package stui.core.style

import cats.{Eq, Hash, Monoid, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** A style patch: what a widget wants to change about a cell. `None` leaves the colour or the underline as it is, `addModifiers` turns
  * attributes on, `subModifiers` turns them off. [[Style.patch]] composes patches (a monoid with [[Style.empty]] as identity),
  * [[CellStyle.patch]] applies one to a cell's resolved appearance.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Style(
  fg: Option[Color],
  bg: Option[Color],
  underlineColor: Option[Color],
  underline: Option[UnderlineStyle],
  addModifiers: Modifiers,
  subModifiers: Modifiers,
) derives Eq,
      Show,
      Hash

object Style {

  /** The patch that changes nothing, the monoid identity. */
  val empty: Style = Style(none[Color], none[Color], none[Color], none[UnderlineStyle], Modifiers.empty, Modifiers.empty)

  /** `empty` and `patch`: associative because, per colour, per underline, and per modifier bit, the last operand that says something
    * wins.
    */
  given monoid: Monoid[Style] = Monoid.instance(empty, (a, b) => a.patch(b))

  extension (style: Style) {

    /** `other` applied on top of this patch: its colours and underline win when present, its `subModifiers` cancel earlier additions,
      * its `addModifiers` cancel earlier removals (Ratatui's algebra).
      */
    def patch(other: Style): Style =
      Style(
        other.fg.orElse(style.fg),
        other.bg.orElse(style.bg),
        other.underlineColor.orElse(style.underlineColor),
        other.underline.orElse(style.underline),
        style.addModifiers.diff(other.subModifiers).union(other.addModifiers),
        style.subModifiers.diff(other.addModifiers).union(other.subModifiers),
      )

    /** The patch with the foreground set. */
    def withFg(color: Color): Style = style.copy(fg = color.some)

    /** The patch with the background set. */
    def withBg(color: Color): Style = style.copy(bg = color.some)

    /** The patch with the underline colour set. */
    def withUnderlineColor(color: Color): Style = style.copy(underlineColor = color.some)

    /** The patch with the underline style set (`UnderlineStyle.None` turns the underline off). */
    def withUnderline(underline: UnderlineStyle): Style = style.copy(underline = underline.some)

    /** Turns the modifier on (and forgets an earlier removal). */
    def addModifier(modifier: Modifier): Style =
      style.copy(addModifiers = style.addModifiers.add(modifier), subModifiers = style.subModifiers.remove(modifier))

    /** Turns the modifier off (and forgets an earlier addition). */
    def removeModifier(modifier: Modifier): Style =
      style.copy(addModifiers = style.addModifiers.remove(modifier), subModifiers = style.subModifiers.add(modifier))

    /** [[addModifier]] with [[Modifier.Bold]]. */
    def bold: Style = style.addModifier(Modifier.Bold)

    /** [[addModifier]] with [[Modifier.Dim]]. */
    def dim: Style = style.addModifier(Modifier.Dim)

    /** [[addModifier]] with [[Modifier.Italic]]. */
    def italic: Style = style.addModifier(Modifier.Italic)

    /** [[withUnderline]] with [[UnderlineStyle.Single]] (decision D22). */
    def underlined: Style = style.withUnderline(UnderlineStyle.Single)

    /** [[withUnderline]] with [[UnderlineStyle.None]]. */
    def notUnderlined: Style = style.withUnderline(UnderlineStyle.None)

    /** [[addModifier]] with [[Modifier.SlowBlink]]. */
    def slowBlink: Style = style.addModifier(Modifier.SlowBlink)

    /** [[addModifier]] with [[Modifier.RapidBlink]]. */
    def rapidBlink: Style = style.addModifier(Modifier.RapidBlink)

    /** [[addModifier]] with [[Modifier.Reversed]]. */
    def reversed: Style = style.addModifier(Modifier.Reversed)

    /** [[addModifier]] with [[Modifier.Hidden]]. */
    def hidden: Style = style.addModifier(Modifier.Hidden)

    /** [[addModifier]] with [[Modifier.CrossedOut]]. */
    def crossedOut: Style = style.addModifier(Modifier.CrossedOut)

  }

}
