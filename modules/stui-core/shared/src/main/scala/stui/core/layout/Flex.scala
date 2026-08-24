package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** How [[Layout.split]] places the excess space left after the constraints are satisfied. Flex only places segments, it never resizes
  * them (Ratatui's `Legacy` mode, where the last segment absorbs the excess, is intentionally absent: a trailing `Constraint.Fill`
  * expresses that on purpose).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Flex derives Eq, Show, Hash {

  /** Segments at the start, the excess trails. The default of [[Layout.horizontal]] and [[Layout.vertical]]. */
  case Start

  /** Segments at the end, the excess leads. */
  case End

  /** The excess splits around the group, half before (rounded down) and the rest after. */
  case Center

  /** The excess distributes into the inner gaps, none before the first segment or after the last. A single segment is placed as
    * [[Start]].
    */
  case SpaceBetween

  /** The excess distributes around every segment, the edge gaps half the inner ones (weights 1, 2, ..., 2, 1). */
  case SpaceAround

  /** The excess distributes evenly into all gaps, edges included (`n + 1` equal gaps). */
  case SpaceEvenly
}
