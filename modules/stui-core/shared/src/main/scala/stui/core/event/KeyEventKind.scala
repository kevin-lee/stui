package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Backends deliver `Press` only unless [[stui.core.spi.TerminalFeature.KeyReleaseEvents]] is enabled (design doc 6.2: Windows and the
  * kitty keyboard protocol report repeats and releases, the double-event footgun is opt-in).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum KeyEventKind derives Eq, Show, Hash {
  case Press
  case Repeat
  case Release
}
