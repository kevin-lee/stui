package stui.unicode

import stui.unicode.internal.TableWidthPolicy

import scala.annotation.tailrec

/** How many terminal cells a grapheme cluster occupies. Width is defined per cluster, so `width(s)` is the sum of the cluster widths by
  * construction, and every cluster width is 0, 1, or 2.
  *
  * The built-in [[WidthPolicy.default]] is table-driven (East Asian Width plus emoji rules). A backend that has probed the real terminal can
  * provide its own policy.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
@SuppressWarnings(
  Array("org.wartremover.warts.Overloading")
) // reason: the offset form is the allocation-free primitive, the String form is the same operation on a whole cluster
trait WidthPolicy {

  /** The width of the cluster `s.substring(start, end)` without allocating it. Returns 0 when the offsets do not describe a non-empty slice. */
  def clusterWidth(s: String, start: Int, end: Int): Int

  /** The width of a whole cluster. */
  def clusterWidth(cluster: String): Int = clusterWidth(cluster, 0, cluster.length)

  /** The width of any string: the sum of its cluster widths. */
  def width(s: String): Int = sum(s, Graphemes.boundaries(s), 0, 0)

  @tailrec
  private def sum(s: String, boundaries: Array[Int], i: Int, acc: Int): Int =
    if (i + 1 >= boundaries.length) acc
    else sum(s, boundaries, i + 1, acc + clusterWidth(s, boundaries(i), boundaries(i + 1)))

}

object WidthPolicy {

  /** The bundled table-driven policy. */
  val default: WidthPolicy = TableWidthPolicy

}
