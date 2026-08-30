package stui.unicode

import stui.unicode.internal.IntOps.*
import stui.unicode.internal.Segmenter

/** Extended grapheme cluster segmentation (UAX #29) with the bundled Unicode tables, identical on JVM, Scala.js, and Scala Native.
  *
  * Every function is total on any `String`: lone surrogates, unpaired combining marks, embedded controls, and the empty string all
  * produce a result and never throw.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object Graphemes {

  /** The cluster boundaries of `s` as strictly increasing UTF-16 offsets, starting at 0 and ending at `s.length`, so the cluster count is
    * `boundaries(s).length - 1` and `boundaries("")` is `Array(0)`.
    *
    * The array is freshly allocated on every call and is returned as a plain `Array[Int]` for the rendering hot path (design principle 3).
    * Callers must not mutate it.
    */
  def boundaries(s: String): Array[Int] = Segmenter.boundaries(s)

  /** The clusters of `s` in order. `clusters(s).mkString` is `s`. */
  def clusters(s: String): Vector[String] = {
    val b = boundaries(s)
    Vector.tabulate(b.length - 1)(i => s.substring(b(i), b(i + 1)))
  }

  /** The number of clusters in `s`. */
  def count(s: String): Int = boundaries(s).length - 1

  /** True when `next` would extend `last`'s cluster into a single cluster: a lone regional indicator after another, a standalone mark
    * after any glyph. `last` is expected to be exactly one cluster, which is what both callers hold.
    *
    * False for an empty `last` (nothing to extend) and on the ASCII fast path (both a single printable ASCII character, which the
    * segmenter always breaks between), else exactly one cluster by the segmenter.
    *
    * Two callers keep separate model units apart with it: the writer's rule R2a places the cursor explicitly between two cells whose
    * symbols would join on the wire, and the word wrapper's span join break refuses to merge two spans whose contents would join into
    * one cluster.
    */
  def joins(last: String, next: String): Boolean =
    if (last.isEmpty) false
    else if (isAsciiPrintable(last) && isAsciiPrintable(next)) false
    else count(last + next) === 1

  private def isAsciiPrintable(s: String): Boolean = s.length === 1 && s.charAt(0) >= ' ' && s.charAt(0) <= '~'

}
