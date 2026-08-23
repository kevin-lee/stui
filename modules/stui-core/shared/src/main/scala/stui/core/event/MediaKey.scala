package stui.core.event

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** A media key, reported by the kitty keyboard protocol.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum MediaKey derives Eq, Show, Hash {
  case Play
  case Pause
  case PlayPause
  case Reverse
  case Stop
  case FastForward
  case Rewind
  case TrackNext
  case TrackPrevious
  case Record
  case LowerVolume
  case RaiseVolume
  case MuteVolume
}
