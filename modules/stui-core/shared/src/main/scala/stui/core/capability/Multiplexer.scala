package stui.core.capability

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The terminal multiplexer the session runs under, if any (design doc 7.3): the multiplexer policy keys on it. `Other` is a
  * `screen` or `tmux` `TERM` without the identifying variable. The case is named `None` for the absence of a multiplexer and is always
  * written qualified as `Multiplexer.None`.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum Multiplexer derives Eq, Show, Hash {
  case None
  case Tmux
  case Screen
  case Zellij
  case Other
}
