package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** What a `TerminalBackend.print` did to the UI area (design doc 7.2, decision D13): `ViewportKept` means the printed rows went above
  * the viewport (or into a buffer) and the UI area was not touched, `ViewportLost` means the print overwrote or moved the viewport, so
  * the orchestration must force the next draw to emit every cell (`RedrawReason.Printed`).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum PrintEffect derives Eq, Show, Hash {
  case ViewportKept
  case ViewportLost
}
