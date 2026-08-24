package stui.testkit.gen

import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.Style
import stui.core.text.Line
import stui.testkit.gen.GeometryGens.nonNegOrZero

/** Buffers produced by random canvas operation sequences, so every generated buffer went through the one code path that maintains the
  * cell invariant.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object BufferGens {

  /** One canvas operation, replayable with [[run]]. */
  enum CanvasOp {
    case PutString(position: Position, text: String, style: Style)
    case PutStringMax(position: Position, text: String, style: Style, maxWidth: NonNegInt)
    case Fill(rect: Rect, symbol: String, style: Style)
    case Clear(rect: Rect)
    case PatchStyle(rect: Rect, style: Style)
    case PutLine(position: Position, line: Line, maxWidth: NonNegInt)
  }

  /** Replays one operation on the canvas. */
  def run(canvas: Canvas, op: CanvasOp): Unit = op match {
    case CanvasOp.PutString(position, text, style) => canvas.putString(position, text, style)
    case CanvasOp.PutStringMax(position, text, style, maxWidth) => canvas.putStringMax(position, text, style, maxWidth)
    case CanvasOp.Fill(rect, symbol, style) => canvas.fill(rect, symbol, style)
    case CanvasOp.Clear(rect) => canvas.clear(rect)
    case CanvasOp.PatchStyle(rect, style) => canvas.patchStyle(rect, style)
    case CanvasOp.PutLine(position, line, maxWidth) => canvas.putLine(position, line, maxWidth)
  }

  /** Replays the operations in order. */
  def runAll(canvas: Canvas, ops: List[CanvasOp]): Unit = ops.foreach(op => run(canvas, op))

  /** x and y in 0..5, width in 0..12, height in 0..6. */
  val area: Gen[Rect] =
    for {
      x      <- Gen.int(Range.linear(0, 5))
      y      <- Gen.int(Range.linear(0, 5))
      width  <- Gen.int(Range.linear(0, 12))
      height <- Gen.int(Range.linear(0, 6))
    } yield Rect(nonNegOrZero(x.toLong), nonNegOrZero(y.toLong), nonNegOrZero(width.toLong), nonNegOrZero(height.toLong))

  /** Mostly inside the area, sometimes just outside. */
  def positionNear(area: Rect): Gen[Position] =
    for {
      x <- Gen.long(Range.linear(area.x.value.toLong - 2L, area.x.value.toLong + area.width.value.toLong + 2L))
      y <- Gen.long(Range.linear(area.y.value.toLong - 2L, area.y.value.toLong + area.height.value.toLong + 2L))
    } yield Position(nonNegOrZero(x), nonNegOrZero(y))

  /** Rects mostly overlapping the area, sometimes hanging outside it. */
  def rectNear(area: Rect): Gen[Rect] =
    for {
      position <- positionNear(area)
      width    <- Gen.int(Range.linear(0, area.width.value + 2))
      height   <- Gen.int(Range.linear(0, area.height.value + 2))
    } yield Rect.at(position, Size(nonNegOrZero(width.toLong), nonNegOrZero(height.toLong)))

  /** Entirely outside the area (to the right of it or below it). */
  def rectOutside(area: Rect): Gen[Rect] =
    for {
      below  <- Gen.boolean
      x      <- Gen.int(Range.linear(0, 5))
      y      <- Gen.int(Range.linear(0, 5))
      width  <- Gen.int(Range.linear(0, 6))
      height <- Gen.int(Range.linear(0, 6))
    } yield
      if (below)
        Rect(
          nonNegOrZero(x.toLong),
          nonNegOrZero(area.y.value.toLong + area.height.value.toLong + y.toLong),
          nonNegOrZero(width.toLong),
          nonNegOrZero(height.toLong),
        )
      else
        Rect(
          nonNegOrZero(area.x.value.toLong + area.width.value.toLong + x.toLong),
          nonNegOrZero(y.toLong),
          nonNegOrZero(width.toLong),
          nonNegOrZero(height.toLong),
        )

  /** Mostly printable ASCII, sometimes nasty Unicode. */
  val text: Gen[String] = Gen.frequency1(
    3 -> Gens.asciiPrintable(Range.linear(0, 12)),
    2 -> NastyGens.text(Range.linear(0, 6)),
    1 -> NastyGens.nastyString(Range.linear(0, 8)),
  )

  /** Fill symbols: narrow ASCII, wide clusters, and arbitrary clusters. */
  val symbol: Gen[String] =
    Gen.frequency1(3 -> Gens.asciiPrintableChar.map(_.toString), 2 -> NastyGens.wideCluster, 1 -> NastyGens.cluster)

  /** Width limits for the bounded write operations. */
  val maxWidth: Gen[NonNegInt] = Gen.int(Range.linear(0, 14)).map(n => nonNegOrZero(n.toLong))

  /** One operation aimed mostly inside the area. */
  def canvasOp(area: Rect): Gen[CanvasOp] = Gen.frequency1(
    4 -> putStringOp(positionNear(area)),
    1 -> putStringMaxOp(positionNear(area)),
    2 -> fillOp(rectNear(area)),
    1 -> rectNear(area).map(CanvasOp.Clear(_)),
    2 -> patchStyleOp(rectNear(area)),
    1 -> putLineOp(positionNear(area)),
  )

  /** Operations whose positions and rects lie outside the area. */
  def outsideOp(area: Rect): Gen[CanvasOp] = Gen.frequency1(
    3 -> putStringOp(rectOutside(area).map(_.position)),
    1 -> putStringMaxOp(rectOutside(area).map(_.position)),
    2 -> fillOp(rectOutside(area)),
    1 -> rectOutside(area).map(CanvasOp.Clear(_)),
    2 -> patchStyleOp(rectOutside(area)),
    1 -> putLineOp(rectOutside(area).map(_.position)),
  )

  private def putStringOp(positions: Gen[Position]): Gen[CanvasOp] =
    for {
      position <- positions
      t        <- text
      style    <- StyleGens.style
    } yield CanvasOp.PutString(position, t, style)

  private def putStringMaxOp(positions: Gen[Position]): Gen[CanvasOp] =
    for {
      position <- positions
      t        <- text
      style    <- StyleGens.style
      max      <- maxWidth
    } yield CanvasOp.PutStringMax(position, t, style, max)

  private def fillOp(rects: Gen[Rect]): Gen[CanvasOp] =
    for {
      rect  <- rects
      s     <- symbol
      style <- StyleGens.style
    } yield CanvasOp.Fill(rect, s, style)

  private def patchStyleOp(rects: Gen[Rect]): Gen[CanvasOp] =
    for {
      rect  <- rects
      style <- StyleGens.style
    } yield CanvasOp.PatchStyle(rect, style)

  private def putLineOp(positions: Gen[Position]): Gen[CanvasOp] =
    for {
      position <- positions
      line     <- TextGens.line(Range.linear(0, 3), Range.linear(0, 6))
      max      <- maxWidth
    } yield CanvasOp.PutLine(position, line, max)

  /** Operation lists of a length in the range. */
  def ops(area: Rect, range: Range[Int]): Gen[List[CanvasOp]] = canvasOp(area).list(range)

  /** An empty buffer over the area with the operations replayed. */
  def bufferFrom(area: Rect, ops: List[CanvasOp]): Buffer = Buffer.empty(area).draw(canvas => runAll(canvas, ops))

  /** Buffers built from random operation sequences. */
  val buffer: Gen[Buffer] =
    for {
      a  <- area
      os <- ops(a, Range.linear(0, 8))
    } yield bufferFrom(a, os)

  /** A buffer and operations that all lie outside its area. */
  val bufferWithOutsideOps: Gen[(Buffer, List[CanvasOp])] =
    for {
      b  <- buffer
      os <- outsideOp(b.area).list(Range.linear(0, 5))
    } yield (b, os)

  /** Two buffers of the same area: independent, or `next` drawn on top of `prev`. */
  val bufferPair: Gen[(Buffer, Buffer)] =
    for {
      a           <- area
      os1         <- ops(a, Range.linear(0, 8))
      os2         <- ops(a, Range.linear(0, 8))
      independent <- Gen.boolean
    } yield {
      val prev = bufferFrom(a, os1)
      (prev, if (independent) bufferFrom(a, os2) else prev.draw(canvas => runAll(canvas, os2)))
    }

  /** Two buffers whose areas differ (the second is one column wider). */
  val mismatchedPair: Gen[(Buffer, Buffer)] =
    for {
      a   <- area
      os1 <- ops(a, Range.linear(0, 5))
      os2 <- ops(a, Range.linear(0, 5))
    } yield (bufferFrom(a, os1), bufferFrom(a.resize(Size(nonNegOrZero(a.width.value.toLong + 1L), a.height)), os2))

}
