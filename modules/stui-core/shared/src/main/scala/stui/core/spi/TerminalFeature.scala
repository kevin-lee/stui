package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A terminal mode a backend enables on `enter` and disables on `exit`. The screen mode is not a feature but a [[ScreenMode]] (decision
  * D12). `KeyReleaseEvents` needs the kitty keyboard protocol (M3), the M1e backends enable nothing for it.
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
