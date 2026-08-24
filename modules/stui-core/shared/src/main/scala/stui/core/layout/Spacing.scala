package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt

/** The gap between neighbouring segments of a [[Layout.split]]: `Space` puts cells between neighbours, `Overlap` shares cells (negative
  * spacing, for shared borders). Under the `Space*` flex modes the gap is the minimum, the distributed excess adds on top.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Spacing derives Eq, Show, Hash {
  case Space(n: NonNegInt)
  case Overlap(n: NonNegInt)
}

object Spacing {

  /** No gap and no overlap. */
  val none: Spacing = Space(NonNegInt(0))

  /** A gap from a literal, validated at compile time. */
  inline def space(inline n: Int): Spacing = Space(NonNegInt(n))

  /** A gap from an already-refined count. */
  def spaceOf(n: NonNegInt): Spacing = Space(n)

  /** An overlap from a literal, validated at compile time. */
  inline def overlap(inline n: Int): Spacing = Overlap(NonNegInt(n))

  /** An overlap from an already-refined count. */
  def overlapOf(n: NonNegInt): Spacing = Overlap(n)

}
