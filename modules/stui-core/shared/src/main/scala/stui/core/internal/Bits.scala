package stui.core.internal

import stui.unicode.internal.IntOps.*

/** Bit operations for the opaque bitsets (`Modifiers`, `KeyModifiers`), indexed by enum ordinal.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
private[core] object Bits {

  def bit(ordinal: Int): Int = 1 << ordinal

  def has(bits: Int, ordinal: Int): Boolean = (bits & bit(ordinal)) !== 0

  def set(bits: Int, ordinal: Int): Int = bits | bit(ordinal)

  def clear(bits: Int, ordinal: Int): Int = bits & ~bit(ordinal)

}
