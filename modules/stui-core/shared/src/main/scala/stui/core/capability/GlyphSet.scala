package stui.core.capability

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Which symbols widgets may draw with (design doc 7.3): Unicode box drawing and arrows, or ASCII only. Selected from the locale and
  * consumed by the border sets from M2.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum GlyphSet derives Eq, Show, Hash {
  case Unicode
  case Ascii
}
