package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A terminal mode a backend enables on `enter` and disables on `exit`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum TerminalFeature derives Eq, Show, Hash {
  case AlternateScreen
  case MouseCapture
  case BracketedPaste
  case FocusEvents
  case KeyReleaseEvents
}
