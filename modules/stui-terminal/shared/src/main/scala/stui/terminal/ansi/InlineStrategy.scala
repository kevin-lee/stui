package stui.terminal.ansi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.capability.Capabilities

/** How printed rows get above an inline viewport (design doc 7.2, decision D12), a pure function of the capabilities:
  * `OverlayRedraw` is the always-correct baseline (a print erases the viewport, writes the rows, scrolls by line feeds, and the next
  * present redraws everything), `ScrollRegion` keeps the viewport still (a DECSTBM region from row 1 to the row above the viewport,
  * printed rows scrolled in at its bottom) and is chosen when the terminal supports safe scroll regions and synchronised output. The
  * writer additionally needs at least two rows above the viewport for a region to exist (DECSTBM requires top strictly below bottom,
  * plan refinement R3), and falls back to overlay per print otherwise.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum InlineStrategy derives Eq, Show, Hash {
  case OverlayRedraw
  case ScrollRegion
}

object InlineStrategy {

  /** [[ScrollRegion]] when the capabilities allow it, [[OverlayRedraw]] otherwise. */
  def of(capabilities: Capabilities): InlineStrategy =
    if (capabilities.scrollRegionsSafe && capabilities.syncOutput) ScrollRegion else OverlayRedraw

}
