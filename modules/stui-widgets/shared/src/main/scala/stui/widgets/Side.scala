package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** One side of a [[Block]]'s border.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Side derives Eq, Show, Hash {
  case Top
  case Right
  case Bottom
  case Left
}

object Side {

  /** Every side in declaration order. */
  val all: List[Side] = values.toList

}
