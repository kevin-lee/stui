package stui.terminal.ansi

import hedgehog.{Gen, Range}
import stui.core.buffer.{Buffer, Cell, CellUpdate}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect}
import stui.testkit.TerminalModel.QuirkProfile
import stui.testkit.gen.{BufferGens, CapabilityGens, GeometryGens, StyleGens}

/** Generators for the writer laws: profiles, capabilities that agree with a profile, buffers at the origin, and ill-formed update
  * vectors.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object WriterGens {

  /** Any of the four quirk profiles. */
  val profile: Gen[QuirkProfile] = Gen.elementUnsafe(QuirkProfile.all)

  /** Random capabilities whose VS16 width is the profile's. */
  def capabilitiesFor(profile: QuirkProfile): Gen[Capabilities] = CapabilityGens.capabilities.map(_.withVs16Width(profile.vs16Width))

  /** Lossless capabilities whose VS16 width is the profile's. */
  def losslessFor(profile: QuirkProfile): Capabilities = Capabilities.lossless.withVs16Width(profile.vs16Width)

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

  /** Positions strictly inside a non-empty area. */
  def positionWithin(area: Rect): Gen[Position] =
    for {
      x <- Gen.int(Range.linear(0, area.width.value - 1))
      y <- Gen.int(Range.linear(0, area.height.value - 1))
    } yield Position(GeometryGens.nonNegOrZero(x.toLong), GeometryGens.nonNegOrZero(y.toLong))

}
