package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.PosInt

/** What to enter: the screen mode, the optional features, the ESC timeout policy, the print-ring bound, the probing policy, and the
  * kitty keyboard protocol policy (design doc 6.3, 7.2, 7.3, 7.5, decisions D12, D13, D15, D30). Raw mode is always entered.
  * `printBufferRows` bounds the transcript kept under the alternate screen (rows, oldest prints dropped first), `probing` says whether
  * the startup probe runs and how long it waits, and `keyboard` whether the kitty keyboard protocol is pushed when supported.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class TerminalOptions(
  screenMode: ScreenMode,
  features: Set[TerminalFeature],
  escTimeout: EscTimeout,
  printBufferRows: PosInt,
  probing: Probing,
  keyboard: KeyboardProtocol,
) derives Eq,
      Show,
      Hash

object TerminalOptions {

  /** The default bound of the print ring under the alternate screen, in rows (design doc 7.2, decision D13). */
  val defaultPrintBufferRows: PosInt = PosInt(1000)

  /** The alternate screen with no optional feature, the automatic ESC timeout, the default print bound, automatic probing, and the
    * automatic kitty keyboard protocol.
    */
  val alternateScreen: TerminalOptions =
    TerminalOptions(
      ScreenMode.AlternateScreen,
      Set.empty[TerminalFeature],
      EscTimeout.Automatic,
      defaultPrintBufferRows,
      Probing.Automatic,
      KeyboardProtocol.Automatic,
    )

  /** The options for the screen mode enabling exactly the given features, with the automatic ESC timeout, the default print bound,
    * automatic probing, and the automatic kitty keyboard protocol.
    */
  def of(screenMode: ScreenMode, features: TerminalFeature*): TerminalOptions =
    TerminalOptions(
      screenMode,
      features.toSet,
      EscTimeout.Automatic,
      defaultPrintBufferRows,
      Probing.Automatic,
      KeyboardProtocol.Automatic,
    )

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

    /** The options with the print-ring bound replaced. */
    def withPrintBufferRows(rows: PosInt): TerminalOptions = options.copy(printBufferRows = rows)

    /** The options with the probing policy replaced. */
    def withProbing(probing: Probing): TerminalOptions = options.copy(probing = probing)

    /** The options with the kitty keyboard protocol policy replaced (M3d). */
    def withKeyboard(keyboard: KeyboardProtocol): TerminalOptions = options.copy(keyboard = keyboard)

  }

}
