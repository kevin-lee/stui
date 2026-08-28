package stui.core.terminal

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.frame.Frame

/** The result of `Terminal.draw`: the frame that was presented and its render statistics (design doc 6.3).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class CompletedFrame(frame: Frame, stats: RenderStats) derives Eq, Show, Hash
