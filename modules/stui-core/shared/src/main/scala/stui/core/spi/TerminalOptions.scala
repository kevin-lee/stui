package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** What to enter: the screen mode, the optional features, and the ESC timeout policy (design doc 6.3, decision D12). Raw mode is
  * always entered.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class TerminalOptions(screenMode: ScreenMode, features: Set[TerminalFeature], escTimeout: EscTimeout) derives Eq, Show, Hash

object TerminalOptions {

  /** The alternate screen with no optional feature and the automatic ESC timeout. */
  val alternateScreen: TerminalOptions = TerminalOptions(ScreenMode.AlternateScreen, Set.empty[TerminalFeature], EscTimeout.Automatic)

  /** The options for the screen mode enabling exactly the given features, with the automatic ESC timeout. */
  def of(screenMode: ScreenMode, features: TerminalFeature*): TerminalOptions =
    TerminalOptions(screenMode, features.toSet, EscTimeout.Automatic)

  extension (options: TerminalOptions) {

    /** True when the feature is requested. */
    def enabled(feature: TerminalFeature): Boolean = options.features.contains(feature)

    /** The options with the feature added. */
    def withFeature(feature: TerminalFeature): TerminalOptions = options.copy(features = options.features + feature)

    /** The options with the feature removed. */
    def withoutFeature(feature: TerminalFeature): TerminalOptions = options.copy(features = options.features - feature)

    /** The options with the screen mode replaced. */
    def withScreenMode(screenMode: ScreenMode): TerminalOptions = options.copy(screenMode = screenMode)

    /** The options with the ESC timeout policy replaced. */
    def withEscTimeout(escTimeout: EscTimeout): TerminalOptions = options.copy(escTimeout = escTimeout)

  }

}
