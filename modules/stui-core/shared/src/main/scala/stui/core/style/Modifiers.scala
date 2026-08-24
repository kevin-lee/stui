package stui.core.style

import cats.{Hash, Show}
import stui.core.internal.Bits
import stui.unicode.internal.IntOps.*

/** A set of [[Modifier]]s packed into an `Int`, one bit per ordinal. Allocation-free, so the diff and the writer can compare and
  * intersect sets in their loops.
  */
type Modifiers = Modifiers.Type

/** @author Kevin Lee
  * @since 2026-08-23
  */
object Modifiers {

  opaque type Type = Int

  /** No modifiers. */
  val empty: Type = 0

  /** The set of the given modifiers. */
  def apply(modifiers: Modifier*): Type = of(modifiers)

  /** The set of the given modifiers. */
  def of(modifiers: Iterable[Modifier]): Type = modifiers.foldLeft(0)((bits, modifier) => Bits.set(bits, modifier.ordinal))

  extension (modifiers: Type) {

    /** True when the modifier is in the set. */
    def contains(modifier: Modifier): Boolean = Bits.has(modifiers, modifier.ordinal)

    /** The set with the modifier added. */
    def add(modifier: Modifier): Type = Bits.set(modifiers, modifier.ordinal)

    /** The set with the modifier removed. */
    def remove(modifier: Modifier): Type = Bits.clear(modifiers, modifier.ordinal)

    /** Every modifier of either set. */
    def union(other: Type): Type = modifiers | other

    /** Every bit of `other` removed. */
    def diff(other: Type): Type = modifiers & ~other

    /** True when the sets share a modifier. */
    def intersects(other: Type): Boolean = (modifiers & other) !== 0

    /** True when no modifier is set. */
    def isEmpty: Boolean = modifiers === 0

    /** The members in ordinal order. */
    def toList: List[Modifier] = Modifier.all.filter(modifier => Bits.has(modifiers, modifier.ordinal))

    /** The raw bitset, one bit per [[Modifier]] ordinal. */
    def bits: Int = modifiers

  }

  /** Also the `Eq`. */
  given hash: Hash[Type] = Hash.fromUniversalHashCode

  /** Renders the members in ordinal order, for logs. */
  given show: Show[Type] = Show.show(modifiers => modifiers.toList.mkString("Modifiers(", ", ", ")"))

}
