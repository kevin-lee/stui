package stui.terminal.probe

import cats.syntax.all.*
import stui.core.capability.{Capabilities, CapabilitiesPatch, ColorProfile}

/** The pure policy that turns a probe result into a capabilities patch (design doc 7.3, decision D15), fail-open: an empty result is
  * the empty patch, and nothing upgrades out of `Mono` (`NO_COLOR` and `TERM=dumb` stay respected).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ProbePolicy {

  private val TruecolorIdentities: Set[String] = Set("kitty", "foot", "wezterm", "ghostty", "contour", "iterm2", "alacritty", "rio")

  private val ExtendedUnderlineIdentities: Set[String] = Set("kitty", "foot", "wezterm", "ghostty", "contour", "iterm2")

  /** The patch the result justifies over the environment value: the DECRPM 2026 answer sets `syncOutput`, a truecolour XTGETTCAP
    * answer or a known XTVERSION identity sets `colors` (unless the base is `Mono`), a known identity sets `extendedUnderline`, and
    * [[MultiplexerPolicy.restrict]] drops what the base's multiplexer forbids.
    */
  def patch(base: Capabilities, result: ProbeResult): CapabilitiesPatch = {
    val sync      = result.syncOutputAnswer.fold(CapabilitiesPatch.empty)(CapabilitiesPatch.empty.withSyncOutput)
    val mono      = base.colors === ColorProfile.Mono
    val identity  = result.identity
    val truecolor =
      if (!mono && (result.truecolorAnswered || identity.exists(TruecolorIdentities.contains))) {
        CapabilitiesPatch.empty.withColors(ColorProfile.Truecolor)
      } else {
        CapabilitiesPatch.empty
      }
    val underline =
      if (identity.exists(ExtendedUnderlineIdentities.contains)) CapabilitiesPatch.empty.withExtendedUnderline(true)
      else CapabilitiesPatch.empty
    MultiplexerPolicy.restrict(base.multiplexer, sync |+| truecolor |+| underline)
  }

}
