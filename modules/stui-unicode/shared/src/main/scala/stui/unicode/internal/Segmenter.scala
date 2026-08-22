package stui.unicode.internal

import stui.unicode.internal.CodePointTable.{Gcb, InCB}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** Extended grapheme cluster boundaries per Unicode Standard Annex #29 (revision 47, Unicode 17.0.0): rules GB1-GB13 and GB999, including
  * GB9c (Indic conjuncts over Indic_Conjunct_Break), GB11 (emoji ZWJ sequences), and GB12/GB13 (regional indicator pairs).
  *
  * Total on any `String`: a lone surrogate decodes to itself (Grapheme_Cluster_Break Other), so it forms its own cluster.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
private[unicode] object Segmenter {

  private val StartOfText: Int = -1

  /* GB11 state: 1 while an "Extended_Pictographic Extend*" run is open, 2 right after the ZWJ that follows such a run. */
  private val EpRunOpen: Int = 1

  private val EpZwjArmed: Int = 2

  /* GB9c state: 1 after an InCB Consonant followed only by InCB Extend/Linker without a Linker yet, 2 once a Linker was seen. */
  private val ConjunctOpen: Int = 1

  private val ConjunctLinked: Int = 2

  /** Strictly increasing UTF-16 offsets from 0 to `s.length` (so `Array(0)` for the empty string). */
  def boundaries(s: String): Array[Int] = {
    /* Boundary buffer in the segmentation hot path: one slot per code point plus the end, trimmed on return. */
    val out   = new Array[Int](s.length + 1)
    out(0) = 0
    val count = loop(s, out, 0, 1, StartOfText, 0, 0, 0)
    java.util.Arrays.copyOf(out, count)
  }

  @tailrec
  private def loop(s: String, out: Array[Int], i: Int, n: Int, prevGcb: Int, riRun: Int, epZwj: Int, incbPhase: Int): Int =
    if (i >= s.length) {
      if (s.isEmpty) {
        n
      } else {
        out(n) = s.length
        n + 1
      }
    } else {
      val cp      = s.codePointAt(i)
      val record  = CodePointTable.record(cp)
      val cur     = CodePointTable.gcb(record)
      val curIncb = CodePointTable.incb(record)
      val curEp   = CodePointTable.isExtendedPictographic(record)
      val written =
        if (i > 0 && breakBefore(prevGcb, cur, curIncb, curEp, riRun, epZwj, incbPhase)) {
          out(n) = i
          n + 1
        } else {
          n
        }
      loop(
        s,
        out,
        i + Character.charCount(cp),
        written,
        cur,
        nextRiRun(cur, riRun),
        nextEpZwj(cur, curEp, epZwj),
        nextIncbPhase(curIncb, incbPhase),
      )
    }

  /** The rules in order, for the position between the previous code point and the current one. */
  private def breakBefore(prev: Int, cur: Int, curIncb: Int, curEp: Boolean, riRun: Int, epZwj: Int, incbPhase: Int): Boolean =
    if (prev === Gcb.CR && cur === Gcb.LF) false // GB3
    else if (prev === Gcb.Control || prev === Gcb.CR || prev === Gcb.LF) true // GB4
    else if (cur === Gcb.Control || cur === Gcb.CR || cur === Gcb.LF) true // GB5
    else if (prev === Gcb.L && (cur === Gcb.L || cur === Gcb.V || cur === Gcb.LV || cur === Gcb.LVT)) false // GB6
    else if ((prev === Gcb.LV || prev === Gcb.V) && (cur === Gcb.V || cur === Gcb.T)) false // GB7
    else if ((prev === Gcb.LVT || prev === Gcb.T) && cur === Gcb.T) false // GB8
    else if (cur === Gcb.Extend || cur === Gcb.ZWJ) false // GB9
    else if (cur === Gcb.SpacingMark) false // GB9a
    else if (prev === Gcb.Prepend) false // GB9b
    else if (curIncb === InCB.Consonant && incbPhase === ConjunctLinked) false // GB9c
    else if (prev === Gcb.ZWJ && epZwj === EpZwjArmed && curEp) false // GB11
    else if (prev === Gcb.RegionalIndicator && cur === Gcb.RegionalIndicator && riRun % 2 === 1) false // GB12, GB13
    else true // GB999

  private def nextRiRun(cur: Int, riRun: Int): Int = if (cur === Gcb.RegionalIndicator) riRun + 1 else 0

  private def nextEpZwj(cur: Int, curEp: Boolean, epZwj: Int): Int =
    if (curEp) EpRunOpen
    else if (cur === Gcb.Extend && epZwj === EpRunOpen) EpRunOpen
    else if (cur === Gcb.ZWJ && epZwj === EpRunOpen) EpZwjArmed
    else 0

  private def nextIncbPhase(curIncb: Int, incbPhase: Int): Int =
    if (curIncb === InCB.Consonant) ConjunctOpen
    else if (incbPhase > 0 && curIncb === InCB.Linker) ConjunctLinked
    else if (incbPhase > 0 && curIncb === InCB.Extend) incbPhase
    else 0

}
