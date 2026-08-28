package stui.core.frame

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Rect

/** One recorded hit region: the rect a widget tagged with the id, already clipped to the canvas area by `Canvas.region`.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class Region(id: RegionId, rect: Rect) derives Eq, Show, Hash
