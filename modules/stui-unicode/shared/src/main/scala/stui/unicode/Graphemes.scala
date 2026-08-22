package stui.unicode

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

}
