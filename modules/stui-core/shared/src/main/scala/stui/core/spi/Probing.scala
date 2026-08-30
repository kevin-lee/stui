package stui.core.spi

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*

import scala.concurrent.duration.*

/** Whether and how long the platform runner probes the terminal at startup (design doc 7.3, decision D15). `Automatic` waits 100 ms
  * locally and 1 second when the capabilities say the session runs over ssh, `Fixed` waits the given time, and `Disabled` skips the
  * probe entirely (tests, and environments known not to answer). The probe always fails open: a timeout leaves the environment
  * capabilities untouched.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum Probing derives Eq, Show, Hash {
  case Automatic
  case Fixed(timeout: FiniteDuration)
  case Disabled
}

object Probing {

  /** The automatic probe timeout on a local terminal. */
  val localDefault: FiniteDuration = 100.milliseconds

  /** The automatic probe timeout when the session runs over ssh. */
  val sshDefault: FiniteDuration = 1.second

  /** A [[Fixed]] probing policy. */
  def fixed(timeout: FiniteDuration): Probing = Fixed(timeout)

  extension (probing: Probing) {

    /** The effective timeout: [[sshDefault]] or [[localDefault]] for `Automatic` depending on `ssh`, the given duration for `Fixed`,
      * and `None` for `Disabled` (no probe runs, nothing is written to the terminal).
      */
    def resolve(ssh: Boolean): Option[FiniteDuration] = probing match {
      case Automatic => (if (ssh) sshDefault else localDefault).some
      case Fixed(timeout) => timeout.some
      case Disabled => none[FiniteDuration]
    }

  }

}
