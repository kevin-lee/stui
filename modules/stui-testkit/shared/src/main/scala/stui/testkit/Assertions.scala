package stui.testkit

import cats.{Eq, Show}
import cats.syntax.all.*
import hedgehog.Result

/** hedgehog results that compare with cats `Eq` and log with `Show` (logging is the one use of `Show` in stui).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object Assertions {

  /** Success when `actual === expected`, otherwise a failure logging both sides rendered with `Show`. */
  def eqv[A: Eq: Show](actual: A, expected: A): Result =
    if (actual === expected) Result.success
    else Result.failure.log(s"--- actual ---\n${actual.show}\n--- expected ---\n${expected.show}")

}
