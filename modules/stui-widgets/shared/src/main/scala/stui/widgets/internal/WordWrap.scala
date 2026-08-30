package stui.widgets.internal

import cats.syntax.all.*
import stui.core.style.Style
import stui.core.text.{Alignment, Line, Span}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The line composers of [[stui.widgets.Paragraph]]: the greedy word wrapper and the truncator, both grapheme-aware through
  * stui-unicode.
  *
  * The wrap machine follows Ratatui's `WordWrapper` with four documented fixes (M1d plan R2, the fourth added for issue 27): a
  * produced row is never wider than the width, pending whitespace at a row break is dropped entirely, a whitespace run wider than the
  * row is chunked like a word when not trimming, and the span join break - two clusters that would form a single grapheme cluster are
  * never merged into one span, because a span boundary is a cluster boundary throughout the stack (`Line.width` sums per-span widths,
  * `Canvas` segments per span, and the writer's rule R2a keeps such cells apart on the wire). Without it a merged span re-segments into
  * different clusters and VS16 forces the joined one to width 2, which produces a row wider than the width.
  * Words are maximal runs of non-whitespace clusters, a cluster is whitespace when all its code points satisfy
  * `Character.isWhitespace` or it is U+200B (zero width space, a free break), U+00A0 (no-break space) is not whitespace, a word wider
  * than the row breaks at cluster boundaries, a cluster wider than the row is skipped, zero-width clusters ride along at no cost, and
  * every logical line yields at least one row (an empty or fully trimmed line yields one empty row).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
private[widgets] object WordWrap {

  final private case class Token(symbol: String, width: Int, style: Style, isWhitespace: Boolean)

  final private case class Merge(spans: Vector[Span], previous: String)

  final private case class MachineState(
    rows: Vector[Vector[Token]],
    row: Vector[Token],
    rowWidth: Int,
    whitespace: Vector[Token],
    whitespaceWidth: Int,
    word: Vector[Token],
    wordWidth: Int,
    previousWasWord: Boolean,
  )

  private val emptyState: MachineState =
    MachineState(Vector.empty, Vector.empty, 0, Vector.empty, 0, Vector.empty, 0, false)

  /** Wraps every line at `width` columns. A width of 0 or below produces no rows at all. Each produced line carries its source line's
    * alignment and spans whose adjacent equal styles are merged back together.
    */
  def wrap(lines: Vector[Line], width: Int, trim: Boolean, policy: WidthPolicy): Vector[Line] =
    if (width <= 0) Vector.empty[Line] else lines.flatMap(line => wrapLine(line, width, trim, policy))

  /** Truncates the line at `width` columns, first dropping `skipColumns` display columns from a left-aligned line (a cluster
    * straddling the boundary is dropped whole, the lost columns are not compensated). Clusters wider than `width` are skipped, and
    * writing stops before the first cluster that would exceed the width.
    */
  def truncate(line: Line, width: Int, skipColumns: Int, policy: WidthPolicy): Line =
    if (width <= 0) {
      Line(Vector.empty[Span], Style.empty, line.alignment)
    } else {
      val skip      = if (line.alignment.getOrElse(Alignment.Left) === Alignment.Left) skipColumns else 0
      val tokens    = tokenise(line, width, policy)
      val afterSkip = dropColumns(tokens, skip)
      val kept      = takeWithin(afterSkip, width, Vector.empty[Token], 0)
      Line(mergeSpans(kept), Style.empty, line.alignment)
    }

  @tailrec
  private def dropColumns(tokens: Vector[Token], remaining: Int): Vector[Token] =
    if (remaining <= 0) {
      tokens
    } else {
      tokens.headOption match {
        case Some(token) => dropColumns(tokens.drop(1), remaining - token.width)
        case None => tokens
      }
    }

  @tailrec
  private def takeWithin(tokens: Vector[Token], remaining: Int, acc: Vector[Token], used: Int): Vector[Token] =
    tokens.headOption match {
      case Some(token) if used + token.width <= remaining => takeWithin(tokens.drop(1), remaining, acc :+ token, used + token.width)
      case Some(_) => acc
      case None => acc
    }

  private def wrapLine(line: Line, width: Int, trim: Boolean, policy: WidthPolicy): Vector[Line] = {
    val tokens = tokenise(line, width, policy)
    val ended  = endOfLine(tokens.foldLeft(emptyState)((state, token) => step(state, token, width, trim)), trim)
    val rows   = if (ended.rows.isEmpty) Vector(Vector.empty[Token]) else ended.rows
    rows.map(row => Line(mergeSpans(row), Style.empty, line.alignment))
  }

  private def step(state: MachineState, token: Token, width: Int, trim: Boolean): MachineState = {
    val committed = if (state.previousWasWord && token.isWhitespace) commit(state, trim) else state
    val buffered  =
      if (token.isWhitespace) {
        committed.copy(
          whitespace = committed.whitespace :+ token,
          whitespaceWidth = committed.whitespaceWidth + token.width,
          previousWasWord = false,
        )
      } else {
        committed.copy(word = committed.word :+ token, wordWidth = committed.wordWidth + token.width, previousWasWord = true)
      }
    resolve(buffered, width, trim)
  }

  /** Moves the pending whitespace (kept only on a non-empty row or when not trimming) and the pending word into the row. */
  private def commit(state: MachineState, trim: Boolean): MachineState = {
    val keepWhitespace = state.row.nonEmpty || !trim
    val row            = if (keepWhitespace) state.row ++ state.whitespace ++ state.word else state.row ++ state.word
    val rowWidth       = state.rowWidth + (if (keepWhitespace) state.whitespaceWidth else 0) + state.wordWidth
    state.copy(row = row, rowWidth = rowWidth, whitespace = Vector.empty, whitespaceWidth = 0, word = Vector.empty, wordWidth = 0)
  }

  /** Emits rows until the row plus the effective pending content fits the width. */
  @tailrec
  private def resolve(state: MachineState, width: Int, trim: Boolean): MachineState = {
    val effectiveWhitespace = if (state.row.nonEmpty || !trim) state.whitespaceWidth else 0
    if (state.rowWidth + effectiveWhitespace + state.wordWidth <= width) {
      state
    } else if (state.row.nonEmpty) {
      /* emit the row and drop the pending whitespace entirely (R2) */
      resolve(
        state.copy(rows = state.rows :+ state.row, row = Vector.empty, rowWidth = 0, whitespace = Vector.empty, whitespaceWidth = 0),
        width,
        trim,
      )
    } else {
      /* the row is empty: the pending unit itself exceeds the width, chunk it at cluster boundaries */
      val kept      = if (trim) Vector.empty[Token] else state.whitespace
      val unit      = kept ++ state.word
      val chunk     = takeWithin(unit, width, Vector.empty[Token], 0)
      val remainder = unit.drop(chunk.length)
      val leadingWs = remainder.takeWhile(_.isWhitespace)
      val rest      = remainder.drop(leadingWs.length)
      resolve(
        state.copy(
          rows = state.rows :+ chunk,
          row = Vector.empty,
          rowWidth = 0,
          whitespace = leadingWs,
          whitespaceWidth = leadingWs.foldLeft(0)((acc, token) => acc + token.width),
          word = rest,
          wordWidth = rest.foldLeft(0)((acc, token) => acc + token.width),
        ),
        width,
        trim,
      )
    }
  }

  private def endOfLine(state: MachineState, trim: Boolean): MachineState = {
    val committed = commit(state, trim)
    if (committed.row.nonEmpty) committed.copy(rows = committed.rows :+ committed.row, row = Vector.empty, rowWidth = 0)
    else committed
  }

  /** The clusters of every span in order, clusters wider than `width` skipped. */
  private def tokenise(line: Line, width: Int, policy: WidthPolicy): Vector[Token] =
    line
      .spans
      .flatMap { span =>
        val boundaries = Graphemes.boundaries(span.content)
        Vector.tabulate(math.max(0, boundaries.length - 1)) { i =>
          val symbol = span.content.substring(boundaries(i), boundaries(i + 1))
          Token(symbol, math.min(2, policy.clusterWidth(span.content, boundaries(i), boundaries(i + 1))), span.style, isWhitespace(symbol))
        }
      }
      .filter(_.width <= width)

  /* U+200B written as an escape on purpose: an invisible literal in source is unreadable (the M1b convention) */
  private val ZeroWidthSpace: String = "\u200b"

  private def isWhitespace(cluster: String): Boolean = cluster === ZeroWidthSpace || allWhitespace(cluster, 0)

  @tailrec
  private def allWhitespace(s: String, i: Int): Boolean =
    if (i >= s.length) {
      s.length !== 0
    } else {
      val cp = s.codePointAt(i)
      if (Character.isWhitespace(cp)) allWhitespace(s, i + Character.charCount(cp)) else false
    }

  /** Adjacent clusters with equal styles merge back into single spans, except where the merge would form one grapheme cluster (the span
    * join break): those clusters stay in separate spans, so every emitted span re-segments into exactly the tokens it was built from
    * and its width is their sum.
    */
  private def mergeSpans(tokens: Vector[Token]): Vector[Span] =
    tokens
      .foldLeft(Merge(Vector.empty[Span], "")) { (merge, token) =>
        /* `merge.previous` is the last span's last cluster: the break keeps every emitted span's clusters equal to its tokens, so by
         * induction the previous token's symbol is that span's final cluster, the single cluster `Graphemes.joins` expects as `last`. */
        val spans = merge.spans.lastOption match {
          case Some(last) if last.style === token.style && !Graphemes.joins(merge.previous, token.symbol) =>
            merge.spans.dropRight(1) :+ Span(last.content + token.symbol, last.style)
          case Some(_) | None => merge.spans :+ Span(token.symbol, token.style)
        }
        Merge(spans, token.symbol)
      }
      .spans

}
