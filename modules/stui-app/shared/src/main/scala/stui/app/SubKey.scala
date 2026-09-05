package stui.app

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

import scala.concurrent.duration.FiniteDuration

/** The identity of a subscription: its data, never its function (design doc 10, M3b). Reconciliation diffs these between updates, so
  * a tick whose interval is unchanged keeps its timer and only its tagger is refreshed.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
enum SubKey derives Eq, Show, Hash {
  case Every(interval: FiniteDuration)
}
