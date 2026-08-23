package stui.core.buffer

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Position

/** One changed cell of a [[Buffer.diff]]. A `Continuation` cell means "this column is now the shadow of the glyph to its left", the
  * writer decides what, if anything, to emit for it.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class CellUpdate(position: Position, cell: Cell) derives Eq, Show, Hash
