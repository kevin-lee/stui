package stui.core.terminal

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

import scala.concurrent.duration.FiniteDuration

/** What one present cost (design doc 7.1, decision D16): the bytes the backend's `flush` pushed, the cells in the update vector, and
  * the time between the two clock reads around the present. A value returned by `Terminal.draw`, there is no global counter.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class RenderStats(bytes: NonNegInt, cells: NonNegInt, duration: FiniteDuration) derives Eq, Show, Hash
