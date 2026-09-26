package stui.terminal.kitty

import cats.{Hash, Show}
import stui.unicode.internal.IntOps.*

/** The progressive-enhancement flags of the kitty keyboard protocol, packed into an `Int`. */
type KittyFlags = KittyFlags.Type

/** The kitty keyboard protocol's progressive-enhancement flags (design doc 7.5, decision D30, the kitty keyboard protocol
  * specification): `Disambiguate` (1), `EventTypes` (2), `AlternateKeys` (4), `AllKeys` (8), and `AssociatedText` (16). The value
  * Stui pushed drives the decoder's kind filter, its modifier meaning, and its stall rule, and the backend pops exactly what it
  * pushed.
  *
  * @author Kevin Lee
  * @since 2026-09-27
  */
object KittyFlags {

  opaque type Type = Int

  /** No flag: nothing pushed, the legacy encoding. */
  val none: Type = 0

  /** Report Escape, Alt, and Control combinations as `CSI u` so no key needs the ESC timeout. */
  val Disambiguate: Type = 1

  /** Report repeat and release events. */
  val EventTypes: Type = 2

  /** Report the shifted key and the base-layout key beside the key code. */
  val AlternateKeys: Type = 4

  /** Report every key as an escape code, text-producing keys included. */
  val AllKeys: Type = 8

  /** Embed the text a key produces in its report. */
  val AssociatedText: Type = 16

  /** The flags of the low five bits, the others dropped. */
  def fromInt(bits: Int): Type = bits & 31

  extension (flags: Type) {

    /** True when every bit of `flag` is set. */
    def contains(flag: Type): Boolean = (flags & flag) === flag

    /** Every flag of either value. */
    def union(other: Type): Type = flags | other

    /** True when no flag is set. */
    def isEmpty: Boolean = flags === 0

    /** The raw bits, as written in `CSI > flags u`. */
    def bits: Int = flags

  }

  /** Also the `Eq`. */
  given hash: Hash[Type] = Hash.fromUniversalHashCode

  /** Renders the bits, for logs. */
  given show: Show[Type] = Show.show(flags => "KittyFlags(" + flags.toString + ")")

}
