package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Whether the platform runner enables the kitty keyboard protocol (design doc 7.5, decision D30). `Automatic` pushes it when the
  * capabilities report support (the startup probe's flags answer, or an override): the disambiguate flag, which removes the Escape
  * guess and separates Control and Alt combinations, plus the event-type and alternate-key flags when
  * [[TerminalFeature.KeyReleaseEvents]] is requested, and pops exactly that on every exit path. `Disabled` never pushes: the
  * application's switch for a terminal whose kitty mode is defective (Cursor 3.22.7 splits Korean input into loose jamo under the
  * disambiguate flag, verified 2026-09-26).
  *
  * @author Kevin Lee
  * @since 2026-09-27
  */
enum KeyboardProtocol derives Eq, Show, Hash {
  case Automatic
  case Disabled
}
