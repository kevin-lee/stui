package stui.core.internal

import refined4s.types.numeric.NonNegInt

/** Total arithmetic on `NonNegInt` for the geometry algebra: every operation is evaluated in `Long`, then clamped into
  * `0..Int.MaxValue` (floored at 0, saturated at `Int.MaxValue`). Shared by every stui module for counts and coordinates that are
  * non-negative by construction.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
private[stui] object NonNegInts {

  private val MaxValue: Long = Int.MaxValue.toLong

  /** `n` floored at 0 and saturated at `Int.MaxValue`. */
  def clamp(n: Long): NonNegInt = {
    val bounded = if (n < 0L) 0 else if (n > MaxValue) Int.MaxValue else n.toInt
    /* `bounded` is within 0..Int.MaxValue, so `from` cannot fail and the Left branch is unreachable. A total clamp constructor is
     * proposed upstream for refined4s, this helper disappears when it lands. */
    NonNegInt.from(bounded).fold(_ => NonNegInt.MinValue, identity)
  }

  /** Saturating sum. */
  def plus(a: NonNegInt, b: NonNegInt): NonNegInt = clamp(a.value.toLong + b.value.toLong)

  /** `a + delta`, floored at 0 and saturated. */
  def offset(a: NonNegInt, delta: Int): NonNegInt = clamp(a.value.toLong + delta.toLong)

  /** `a - b`, floored at 0. */
  def minus(a: NonNegInt, b: NonNegInt): NonNegInt = clamp(a.value.toLong - b.value.toLong)

  /** Exact product. */
  def times(a: NonNegInt, b: NonNegInt): Long = a.value.toLong * b.value.toLong

  /** The smaller of the two. */
  def min(a: NonNegInt, b: NonNegInt): NonNegInt = if (a.value <= b.value) a else b

  /** The larger of the two. */
  def max(a: NonNegInt, b: NonNegInt): NonNegInt = if (a.value >= b.value) a else b

}
