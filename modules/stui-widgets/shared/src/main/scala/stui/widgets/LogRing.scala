package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.{NonNegLong, PosInt}
import stui.core.text.Line

/** The bounded append-only backing store of a [[LogView]] (design doc 6.6, M2a): the newest `bound` lines are kept, the oldest are
  * evicted first, and `firstIndex` counts the evicted lines monotonically, so an absolute line index (a [[LogViewState]] anchor)
  * keeps naming the same line while the ring rolls.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class LogRing(bound: PosInt, lines: Vector[Line], firstIndex: NonNegLong) derives Eq, Show, Hash

object LogRing {

  /** An empty ring keeping at most `bound` lines. */
  def empty(bound: PosInt): LogRing = LogRing(bound, Vector.empty[Line], NonNegLong(0L))

  /** `n` floored at 0. The scroll-view family only ever adds non-negative counts to non-negative indexes, so the fallback is
    * unreachable.
    */
  private[widgets] def nonNegLong(n: Long): NonNegLong = NonNegLong.from(math.max(0L, n)).fold(_ => NonNegLong.MinValue, identity)

  extension (ring: LogRing) {

    /** The ring with the line appended, the oldest line evicted when the bound was full. */
    def append(line: Line): LogRing = ring.appendAll(Vector(line))

    /** The ring with the lines appended in order. When more than `bound` lines result, the oldest are dropped and `firstIndex`
      * advances by the dropped count (a bulk larger than the bound keeps its tail).
      */
    def appendAll(newLines: Vector[Line]): LogRing = {
      val appended = ring.lines ++ newLines
      val overflow = math.max(0, appended.length - ring.bound.value)
      LogRing(ring.bound, appended.drop(overflow), nonNegLong(ring.firstIndex.value + overflow.toLong))
    }

    /** The number of kept lines, at most `bound`. */
    def size: Int = ring.lines.length

    /** True when nothing is kept. */
    def isEmpty: Boolean = ring.lines.isEmpty

    /** The absolute index the next appended line receives (`firstIndex + size`). */
    def nextIndex: NonNegLong = nonNegLong(ring.firstIndex.value + ring.lines.length.toLong)

  }

}
