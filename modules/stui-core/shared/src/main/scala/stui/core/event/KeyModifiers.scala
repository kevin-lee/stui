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

  val empty: Type = 0

  def apply(modifiers: KeyModifier*): Type = of(modifiers)

  def of(modifiers: Iterable[KeyModifier]): Type = modifiers.foldLeft(0)((bits, modifier) => Bits.set(bits, modifier.ordinal))

  extension (modifiers: Type) {

    def contains(modifier: KeyModifier): Boolean = Bits.has(modifiers, modifier.ordinal)

    def add(modifier: KeyModifier): Type = Bits.set(modifiers, modifier.ordinal)

    def remove(modifier: KeyModifier): Type = Bits.clear(modifiers, modifier.ordinal)

    def union(other: Type): Type = modifiers | other

    def isEmpty: Boolean = modifiers === 0

    /** The members in ordinal order. */
    def toList: List[KeyModifier] = KeyModifier.all.filter(modifier => Bits.has(modifiers, modifier.ordinal))

    def bits: Int = modifiers

  }

  /** Also the `Eq`. */
  given hash: Hash[Type] = Hash.fromUniversalHashCode

  given show: Show[Type] = Show.show(modifiers => modifiers.toList.mkString("KeyModifiers(", ", ", ")"))

}
