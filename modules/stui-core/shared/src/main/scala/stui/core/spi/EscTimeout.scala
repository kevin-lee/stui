package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

import scala.concurrent.duration.*

/** How long the decoder waits after a lone ESC byte before deciding it was the Escape key rather than the start of a sequence (design
  * doc 7.5, decision D17). `Automatic` is 50 ms locally and 200 ms when the capabilities say the session runs over ssh (vim's
  * `ttimeoutlen` and tmux's `escape-time` are the prior art), `Fixed` is an explicit value.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
enum EscTimeout derives Eq, Show, Hash {
  case Automatic
  case Fixed(duration: FiniteDuration)
}

object EscTimeout {

  /** The automatic timeout on a local terminal. */
  val localDefault: FiniteDuration = 50.milliseconds

  /** The automatic timeout when the session runs over ssh. */
  val sshDefault: FiniteDuration = 200.milliseconds

  /** A [[Fixed]] timeout. */
  def fixed(duration: FiniteDuration): EscTimeout = Fixed(duration)

  extension (escTimeout: EscTimeout) {

    /** The effective duration: [[sshDefault]] or [[localDefault]] for `Automatic` depending on `ssh`, the given duration for `Fixed`. */
    def resolve(ssh: Boolean): FiniteDuration = escTimeout match {
      case Automatic => if (ssh) sshDefault else localDefault
      case Fixed(duration) => duration
    }

  }

}
