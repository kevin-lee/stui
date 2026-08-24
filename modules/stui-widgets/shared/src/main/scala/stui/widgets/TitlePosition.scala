package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The row a [[Title]] sits on: the block's first or last row.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum TitlePosition derives Eq, Show, Hash {
  case Top
  case Bottom
}
