package stui.terminal.ansi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** The active DECSTBM scroll region as inclusive screen rows (design doc 7.1 and 7.2). Set by the inline mode of M1f, always absent in
  * M1e.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class ScrollRegion(top: NonNegInt, bottom: NonNegInt) derives Eq, Show, Hash
