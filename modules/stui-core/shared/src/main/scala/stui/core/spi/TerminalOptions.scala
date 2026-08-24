package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The features to enable when entering the terminal. Raw mode is always entered.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class TerminalOptions(features: Set[TerminalFeature]) derives Eq, Show, Hash

object TerminalOptions {

  /** Raw mode only, no optional feature. */
  val none: TerminalOptions = TerminalOptions(Set.empty[TerminalFeature])

  /** The options enabling exactly the given features. */
  def of(features: TerminalFeature*): TerminalOptions = TerminalOptions(features.toSet)

  extension (options: TerminalOptions) {

    /** True when the feature is requested. */
    def enabled(feature: TerminalFeature): Boolean = options.features.contains(feature)

  }

}
