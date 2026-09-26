package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Backends deliver `Press` only unless [[stui.core.spi.TerminalFeature.KeyReleaseEvents]] is enabled (design doc 6.2: Windows and the
  * kitty keyboard protocol report repeats and releases, the double-event footgun is opt-in). The source is the kitty keyboard protocol
  * (M3d): without the feature the decoder drops a release and delivers a repeat as a press. A release can arrive without a press: an
  * input method consumes the presses of the keys it composes from but not their releases, and the release of the key that launched
  * the program can arrive after the push.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum KeyEventKind derives Eq, Show, Hash {
  case Press
  case Repeat
  case Release
}
