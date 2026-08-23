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

  val empty: Type = 0

  def apply(modifiers: Modifier*): Type = of(modifiers)

  def of(modifiers: Iterable[Modifier]): Type = modifiers.foldLeft(0)((bits, modifier) => Bits.set(bits, modifier.ordinal))

  extension (modifiers: Type) {

    def contains(modifier: Modifier): Boolean = Bits.has(modifiers, modifier.ordinal)

    def add(modifier: Modifier): Type = Bits.set(modifiers, modifier.ordinal)

    def remove(modifier: Modifier): Type = Bits.clear(modifiers, modifier.ordinal)

    def union(other: Type): Type = modifiers | other

    /** Every bit of `other` removed. */
    def diff(other: Type): Type = modifiers & ~other

    def intersects(other: Type): Boolean = (modifiers & other) !== 0

    def isEmpty: Boolean = modifiers === 0

    /** The members in ordinal order. */
    def toList: List[Modifier] = Modifier.all.filter(modifier => Bits.has(modifiers, modifier.ordinal))

    def bits: Int = modifiers

  }

  /** Also the `Eq`. */
  given hash: Hash[Type] = Hash.fromUniversalHashCode

  given show: Show[Type] = Show.show(modifiers => modifiers.toList.mkString("Modifiers(", ", ", ")"))

}
