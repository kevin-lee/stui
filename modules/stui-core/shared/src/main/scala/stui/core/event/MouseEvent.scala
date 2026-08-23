package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Position

/** A mouse action at a cell position with the modifiers held.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class MouseEvent(kind: MouseEventKind, position: Position, modifiers: KeyModifiers) derives Eq, Show, Hash
