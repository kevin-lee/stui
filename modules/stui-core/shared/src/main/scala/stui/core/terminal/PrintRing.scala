package stui.core.terminal

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Buffer
import stui.core.internal.NonNegInts
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The bounded transcript of prints under the alternate screen (design doc 7.2, decision D13): whole prints kept in order, the total
  * row count bounded by `bound`, the oldest prints dropped first when a new one overflows it, and a single print taller than the
  * bound cut to its newest rows. The orchestration flushes the ring to the normal screen after restore at exit, so the transcript
  * survives.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class PrintRing(bound: PosInt, prints: Vector[Buffer], totalRows: NonNegInt) derives Eq, Show, Hash

object PrintRing {

  /** A ring with nothing printed. */
  def empty(bound: PosInt): PrintRing = PrintRing(bound, Vector.empty[Buffer], NonNegInt(0))

  @tailrec
  private def evict(ring: PrintRing): PrintRing =
    if (ring.totalRows.value <= ring.bound.value) {
      ring
    } else {
      ring.prints match {
        case head +: tail =>
          evict(PrintRing(ring.bound, tail, NonNegInts.minus(ring.totalRows, NonNegInts.clamp(head.area.height.value.toLong))))
        case _ => ring
      }
    }

  extension (ring: PrintRing) {

    /** The ring with the print appended: a zero-height print changes nothing, one taller than the bound keeps only its last `bound`
      * rows, and the oldest prints are dropped while the total exceeds the bound.
      */
    def append(rows: Buffer): PrintRing = {
      val height = rows.area.height.value
      if (height === 0) {
        ring
      } else {
        val cut = if (height > ring.bound.value) rows.lastRows(NonNegInts.clamp(ring.bound.value.toLong)) else rows
        evict(
          PrintRing(
            ring.bound,
            ring.prints :+ cut,
            NonNegInts.plus(ring.totalRows, NonNegInts.clamp(cut.area.height.value.toLong)),
          )
        )
      }
    }

    /** The prints in order, oldest first. */
    def entries: Vector[Buffer] = ring.prints

    /** True when nothing is buffered. */
    def isEmpty: Boolean = ring.prints.isEmpty

  }

}
