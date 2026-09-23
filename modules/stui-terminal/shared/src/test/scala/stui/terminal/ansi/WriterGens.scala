package stui.terminal.ansi

import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas, Cell, CellUpdate}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.Style
import stui.core.text.{Line, Span}
import stui.testkit.TerminalModel.QuirkProfile
import stui.testkit.gen.{BufferGens, GeometryGens, NastyGens, StyleGens}

/** Generators for the writer laws: profiles, buffers at the origin, VS16-heavy buffers and pairs, and ill-formed update vectors.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object WriterGens {

  /** Any of the four quirk profiles. */
  val profile: Gen[QuirkProfile] = Gen.elementUnsafe(QuirkProfile.all)

  /** Areas at the origin (the writer addresses viewport coordinates), width 0 to 12, height 0 to 6. */
  val originArea: Gen[Rect] = BufferGens.area.map(area => Rect.sized(area.size))

  /** Two buffers of the same origin area: independent, or the second drawn on top of the first. */
  val bufferPair: Gen[(Buffer, Buffer)] =
    for {
      area        <- originArea
      first       <- BufferGens.ops(area, Range.linear(0, 8))
      second      <- BufferGens.ops(area, Range.linear(0, 8))
      independent <- Gen.boolean
    } yield {
      val prev = BufferGens.bufferFrom(area, first)
      (prev, if (independent) BufferGens.bufferFrom(area, second) else prev.draw(canvas => BufferGens.runAll(canvas, second)))
    }

  /** One-row buffers at the origin, 1 to 20 columns. */
  val rowBuffer: Gen[Buffer] =
    for {
      width <- Gen.int(Range.linear(1, 20))
      area = Rect.sized(stui.core.geometry.Size(GeometryGens.nonNegOrZero(width.toLong), GeometryGens.nonNegOrZero(1L)))
      ops <- BufferGens.ops(area, Range.linear(0, 6))
    } yield BufferGens.bufferFrom(area, ops)

  /** Updates whose positions lie outside the area. */
  def updatesOutside(area: Rect): Gen[Vector[CellUpdate]] =
    for {
      rects  <- BufferGens.rectOutside(area).list(Range.linear(1, 5))
      styles <- StyleGens.cellStyle.list(Range.linear(5, 5))
    } yield rects.zip(styles).map { case (rect, style) => CellUpdate(rect.position, Cell.blankWith(style)) }.toVector

  /** A non-empty area and continuation updates inside it with no owner in the vector. */
  val orphanUpdates: Gen[(Rect, Vector[CellUpdate])] =
    for {
      width  <- Gen.int(Range.linear(1, 12))
      height <- Gen.int(Range.linear(1, 6))
      area = Rect.sized(stui.core.geometry.Size(GeometryGens.nonNegOrZero(width.toLong), GeometryGens.nonNegOrZero(height.toLong)))
      positions <- positionWithin(area).list(Range.linear(1, 6))
      styles    <- StyleGens.cellStyle.list(Range.linear(6, 6))
    } yield (
      area,
      positions.zip(styles).map { case (position, style) => CellUpdate(position, Cell.continuation(style)) }.toVector.distinctBy(_.position),
    )

  /** A terminal size and a full-width viewport inside it (the inline shape): origin row 0 to height - 1, viewport height at least 1. */
  val terminalViewport: Gen[(Size, Rect)] =
    for {
      width  <- Gen.int(Range.linear(1, 10))
      termH  <- Gen.int(Range.linear(2, 8))
      origin <- Gen.int(Range.linear(0, termH - 1))
      height <- Gen.int(Range.linear(1, termH - origin))
    } yield (
      Size(GeometryGens.nonNegOrZero(width.toLong), GeometryGens.nonNegOrZero(termH.toLong)),
      Rect(
        NonNegInt(0),
        GeometryGens.nonNegOrZero(origin.toLong),
        GeometryGens.nonNegOrZero(width.toLong),
        GeometryGens.nonNegOrZero(height.toLong),
      ),
    )

  /** [[terminalViewport]] with at least two rows above the viewport (the scroll-region shape, plan refinement R3). */
  val terminalViewportWithRegion: Gen[(Size, Rect)] =
    for {
      width  <- Gen.int(Range.linear(1, 10))
      termH  <- Gen.int(Range.linear(3, 8))
      origin <- Gen.int(Range.linear(2, termH - 1))
      height <- Gen.int(Range.linear(1, termH - origin))
    } yield (
      Size(GeometryGens.nonNegOrZero(width.toLong), GeometryGens.nonNegOrZero(termH.toLong)),
      Rect(
        NonNegInt(0),
        GeometryGens.nonNegOrZero(origin.toLong),
        GeometryGens.nonNegOrZero(width.toLong),
        GeometryGens.nonNegOrZero(height.toLong),
      ),
    )

  /** Two buffers over the given area: independent, or the second drawn on top of the first. */
  def pairAt(area: Rect): Gen[(Buffer, Buffer)] =
    for {
      first       <- BufferGens.ops(area, Range.linear(0, 8))
      second      <- BufferGens.ops(area, Range.linear(0, 8))
      independent <- Gen.boolean
    } yield {
      val prev = BufferGens.bufferFrom(area, first)
      (prev, if (independent) BufferGens.bufferFrom(area, second) else prev.draw(canvas => BufferGens.runAll(canvas, second)))
    }

  /** A random buffer over the given area. */
  def bufferAt(area: Rect, ops: Range[Int]): Gen[Buffer] = BufferGens.ops(area, ops).map(BufferGens.bufferFrom(area, _))

  /** VS16 clusters terminals disagree on (rule R3a): the keyboard, the heart, the smiling face, the check mark, and the keycap one. */
  val vs16Cluster: Gen[String] = Gen.element1(
    NastyGens.render(List(0x2328, 0xfe0f)),
    NastyGens.render(List(0x2764, 0xfe0f)),
    NastyGens.render(List(0x263a, 0xfe0f)),
    NastyGens.render(List(0x2714, 0xfe0f)),
    NastyGens.render(List(0x31, 0xfe0f, 0x20e3)),
  )

  /** One segment of a VS16-heavy line: a VS16 cluster, an ASCII letter, space, or parenthesis, a Hangul syllable, or a flag. */
  val vs16Segment: Gen[String] = Gen.frequency1(
    3 -> vs16Cluster,
    3 -> Gen.element1("a", "b", " ", "("),
    1 -> Gen.element1("한", "글"),
    1 -> NastyGens.flagPair,
  )

  /** A line of 0 to 12 segments, each unstyled or randomly styled. */
  val vs16Line: Gen[Line] =
    (for {
      segment <- vs16Segment
      style   <- Gen.frequency1(2 -> Gen.constant(Style.empty), 1 -> StyleGens.style)
    } yield Span.styled(segment, style)).list(Range.linear(0, 12)).map(spans => Line.fromSpans(spans.toVector))

  /** The lines drawn from the area's left edge, line `y` on the area's row `y`, each cut at the area width. */
  private def drawLines(canvas: Canvas, area: Rect, lines: List[Line]): Unit =
    lines.zipWithIndex.foreach {
      case (line, y) =>
        canvas.putLine(Position(area.x, GeometryGens.nonNegOrZero(area.y.value.toLong + y.toLong)), line, area.width)
    }

  /** A VS16-heavy buffer over the area: one [[vs16Line]] per row. */
  def vs16BufferAt(area: Rect): Gen[Buffer] =
    vs16Line.list(Range.singleton(area.height.value)).map(lines => Buffer.empty(area).draw(canvas => drawLines(canvas, area, lines)))

  /** Two VS16-heavy buffers of one origin area (width 1 to 16, height 1 to 3): independent, or the second drawn on top of the first. */
  val vs16Pair: Gen[(Buffer, Buffer)] =
    for {
      width  <- Gen.int(Range.linear(1, 16))
      height <- Gen.int(Range.linear(1, 3))
      area = Rect.sized(Size(GeometryGens.nonNegOrZero(width.toLong), GeometryGens.nonNegOrZero(height.toLong)))
      prev        <- vs16BufferAt(area)
      lines       <- vs16Line.list(Range.singleton(height))
      independent <- Gen.boolean
    } yield (prev, (if (independent) Buffer.empty(area) else prev).draw(canvas => drawLines(canvas, area, lines)))

  /** Positions strictly inside a non-empty area. */
  def positionWithin(area: Rect): Gen[Position] =
    for {
      x <- Gen.int(Range.linear(0, area.width.value - 1))
      y <- Gen.int(Range.linear(0, area.height.value - 1))
    } yield Position(GeometryGens.nonNegOrZero(x.toLong), GeometryGens.nonNegOrZero(y.toLong))

}
