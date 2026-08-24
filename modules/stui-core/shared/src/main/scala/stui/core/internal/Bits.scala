package stui.core.internal

import stui.unicode.internal.IntOps.*

/** Bit operations for the opaque bitsets (`Modifiers`, `KeyModifiers`), indexed by enum ordinal.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
private[core] object Bits {

  /** The single-bit mask for the ordinal. */
  def bit(ordinal: Int): Int = 1 << ordinal

  /** True when the ordinal's bit is set. */
  def has(bits: Int, ordinal: Int): Boolean = (bits & bit(ordinal)) !== 0

  /** The bitset with the ordinal's bit set. */
  def set(bits: Int, ordinal: Int): Int = bits | bit(ordinal)

  /** The bitset with the ordinal's bit cleared. */
  def clear(bits: Int, ordinal: Int): Int = bits & ~bit(ordinal)

}
