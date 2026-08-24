package stui.core.geometry

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.unicode.internal.IntOps.*

/** A signed displacement. Applying it to a position or a rect floors at 0 and saturates at `Int.MaxValue`.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Offset(dx: Int, dy: Int) derives Eq, Show, Hash

object Offset {

  /** No displacement. */
  val zero: Offset = Offset(0, 0)

  extension (offset: Offset) {

    /** Both components negated. `Int.MinValue` has no negation in `Int`, it becomes `Int.MaxValue`. */
    def negate: Offset = Offset(negateInt(offset.dx), negateInt(offset.dy))

  }

  private def negateInt(n: Int): Int = if (n === Int.MinValue) Int.MaxValue else -n

}
