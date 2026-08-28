package stui.core.capability

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** How many colours the terminal can show (design doc 7.3, fixing 3.6): 24-bit truecolour, the 256-colour table, the 16 named colours,
  * or none. Widgets author in rich colour, the writer degrades at emission time.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum ColorProfile derives Eq, Show, Hash {
  case Truecolor
  case Ansi256
  case Ansi16
  case Mono
}

object ColorProfile {

  /** Every profile, richest first. */
  val all: List[ColorProfile] = values.toList

}
