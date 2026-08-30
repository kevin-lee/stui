package stui.core.terminal

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Why the next `Terminal.draw` must emit every cell instead of the diff (design doc 6.3, decision D12): the terminal was resized (its
  * content may have been reflowed or cleared), rows were printed above the UI and the viewport was overwritten or moved (the overlay
  * print strategy of design doc 7.2), the application asked for it, the capabilities changed, or the application suspects the screen
  * no longer matches the previous frame.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum RedrawReason derives Eq, Show, Hash {
  case Resize
  case Printed
  case Requested
  case CapabilitiesChanged
  case SuspectedCorruption
}
