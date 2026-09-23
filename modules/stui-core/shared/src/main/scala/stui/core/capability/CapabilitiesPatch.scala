package stui.core.capability

import cats.{Eq, Hash, Monoid, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** A partial [[Capabilities]] (design doc 7.3, decision D15): probe results and application overrides are patches merged over the
  * environment layer by [[Capabilities.merge]]. Every field is optional, and the monoid combines patches with the `Style.patch` law:
  * per field the last operand that says something wins.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class CapabilitiesPatch(
  colors: Option[ColorProfile],
  syncOutput: Option[Boolean],
  kittyKeyboard: Option[Boolean],
  sgrMouse: Option[Boolean],
  focusEvents: Option[Boolean],
  scrollRegionsSafe: Option[Boolean],
  extendedUnderline: Option[Boolean],
  multiplexer: Option[Multiplexer],
  ssh: Option[Boolean],
  glyphs: Option[GlyphSet],
  ambiguousWide: Option[Boolean],
) derives Eq,
      Show,
      Hash

object CapabilitiesPatch {

  /** The patch that says nothing. */
  val empty: CapabilitiesPatch =
    CapabilitiesPatch(
      none[ColorProfile],
      none[Boolean],
      none[Boolean],
      none[Boolean],
      none[Boolean],
      none[Boolean],
      none[Boolean],
      none[Multiplexer],
      none[Boolean],
      none[GlyphSet],
      none[Boolean],
    )

  /** Per field the last operand that says something wins (the `Style.patch` law). */
  given monoid: Monoid[CapabilitiesPatch] = new Monoid[CapabilitiesPatch] {
    override val empty: CapabilitiesPatch                                               = CapabilitiesPatch.empty
    override def combine(x: CapabilitiesPatch, y: CapabilitiesPatch): CapabilitiesPatch =
      CapabilitiesPatch(
        y.colors.orElse(x.colors),
        y.syncOutput.orElse(x.syncOutput),
        y.kittyKeyboard.orElse(x.kittyKeyboard),
        y.sgrMouse.orElse(x.sgrMouse),
        y.focusEvents.orElse(x.focusEvents),
        y.scrollRegionsSafe.orElse(x.scrollRegionsSafe),
        y.extendedUnderline.orElse(x.extendedUnderline),
        y.multiplexer.orElse(x.multiplexer),
        y.ssh.orElse(x.ssh),
        y.glyphs.orElse(x.glyphs),
        y.ambiguousWide.orElse(x.ambiguousWide),
      )
  }

  extension (patch: CapabilitiesPatch) {

    /** The patch with the colour profile said. */
    def withColors(profile: ColorProfile): CapabilitiesPatch = patch.copy(colors = profile.some)

    /** The patch with synchronised output said. */
    def withSyncOutput(flag: Boolean): CapabilitiesPatch = patch.copy(syncOutput = flag.some)

    /** The patch with the kitty keyboard protocol said. */
    def withKittyKeyboard(flag: Boolean): CapabilitiesPatch = patch.copy(kittyKeyboard = flag.some)

    /** The patch with SGR mouse reporting said. */
    def withSgrMouse(flag: Boolean): CapabilitiesPatch = patch.copy(sgrMouse = flag.some)

    /** The patch with focus events said. */
    def withFocusEvents(flag: Boolean): CapabilitiesPatch = patch.copy(focusEvents = flag.some)

    /** The patch with scroll-region safety said. */
    def withScrollRegionsSafe(flag: Boolean): CapabilitiesPatch = patch.copy(scrollRegionsSafe = flag.some)

    /** The patch with extended underlines said. */
    def withExtendedUnderline(flag: Boolean): CapabilitiesPatch = patch.copy(extendedUnderline = flag.some)

    /** The patch with the multiplexer said. */
    def withMultiplexer(multiplexer: Multiplexer): CapabilitiesPatch = patch.copy(multiplexer = multiplexer.some)

    /** The patch with the ssh flag said. */
    def withSsh(flag: Boolean): CapabilitiesPatch = patch.copy(ssh = flag.some)

    /** The patch with the glyph set said. */
    def withGlyphs(glyphs: GlyphSet): CapabilitiesPatch = patch.copy(glyphs = glyphs.some)

    /** The patch with the ambiguous-width flag said. */
    def withAmbiguousWide(flag: Boolean): CapabilitiesPatch = patch.copy(ambiguousWide = flag.some)

  }

}
