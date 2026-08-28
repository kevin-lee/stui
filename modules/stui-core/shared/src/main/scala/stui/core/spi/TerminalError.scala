package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Why a terminal could not be entered (design doc 6.3): standard input is not a terminal, the screen mode is not implemented by the
  * backend or the milestone, or a platform call failed (the operation name such as `tcsetattr` and its detail such as a return code).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum TerminalError derives Eq, Show, Hash {
  case NotATerminal
  case UnsupportedScreenMode(mode: ScreenMode)
  case PlatformFailure(operation: String, detail: String)
}

object TerminalError {

  /** An [[UnsupportedScreenMode]] error. */
  def unsupportedScreenMode(mode: ScreenMode): TerminalError = UnsupportedScreenMode(mode)

  /** A [[PlatformFailure]] error. */
  def platformFailure(operation: String, detail: String): TerminalError = PlatformFailure(operation, detail)

}
