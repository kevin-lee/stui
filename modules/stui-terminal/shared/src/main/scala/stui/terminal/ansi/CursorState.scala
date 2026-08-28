package stui.terminal.ansi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Position

/** Where the writer believes the terminal cursor is (design doc 7.1, decision D16): unknown (the next write moves absolutely), at a
  * known viewport position, or in the last column of `row` with a pending wrap (the DECAWM last-column state after a printable was
  * written there, rule R1: the next move is absolute because the next printable would wrap).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum CursorState derives Eq, Show, Hash {
  case Unknown
  case Known(position: Position)
  case PendingWrap(row: NonNegInt)
}
