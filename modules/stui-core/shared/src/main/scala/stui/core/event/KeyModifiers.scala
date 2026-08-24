package stui.core.event

import cats.{Hash, Show}
import stui.core.internal.Bits
import stui.unicode.internal.IntOps.*

/** A set of [[KeyModifier]]s packed into an `Int`, one bit per ordinal. */
type KeyModifiers = KeyModifiers.Type

/** @author Kevin Lee
  * @since 2026-08-23
  */
object KeyModifiers {

  opaque type Type = Int

  /** No modifiers. */
  val empty: Type = 0

  /** The set of the given modifiers. */
  def apply(modifiers: KeyModifier*): Type = of(modifiers)

  /** The set of the given modifiers. */
  def of(modifiers: Iterable[KeyModifier]): Type = modifiers.foldLeft(0)((bits, modifier) => Bits.set(bits, modifier.ordinal))

  extension (modifiers: Type) {

    /** True when the modifier is in the set. */
    def contains(modifier: KeyModifier): Boolean = Bits.has(modifiers, modifier.ordinal)

    /** The set with the modifier added. */
    def add(modifier: KeyModifier): Type = Bits.set(modifiers, modifier.ordinal)

    /** The set with the modifier removed. */
    def remove(modifier: KeyModifier): Type = Bits.clear(modifiers, modifier.ordinal)

    /** Every modifier of either set. */
    def union(other: Type): Type = modifiers | other

    /** True when no modifier is set. */
    def isEmpty: Boolean = modifiers === 0

    /** The members in ordinal order. */
    def toList: List[KeyModifier] = KeyModifier.all.filter(modifier => Bits.has(modifiers, modifier.ordinal))

    /** The raw bitset, one bit per [[KeyModifier]] ordinal. */
    def bits: Int = modifiers

  }

  /** Also the `Eq`. */
  given hash: Hash[Type] = Hash.fromUniversalHashCode

  /** Renders the members in ordinal order, for logs. */
  given show: Show[Type] = Show.show(modifiers => modifiers.toList.mkString("KeyModifiers(", ", ", ")"))

}
