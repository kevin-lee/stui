package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A terminal mode a backend enables on `enter` and disables on `exit`. The screen mode is not a feature but a [[ScreenMode]] (decision
  * D12). `KeyReleaseEvents` asks for the kitty keyboard protocol's event-type and alternate-key flags on top of the disambiguate flag
  * (M3d, decision D30), and takes effect only when the terminal supports the protocol and [[KeyboardProtocol]] is `Automatic`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum TerminalFeature derives Eq, Show, Hash {
  case MouseCapture
  case BracketedPaste
  case FocusEvents
  case KeyReleaseEvents
}
