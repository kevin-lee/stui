package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.core.buffer.Buffer
import stui.core.frame.{Frame, Region, RegionId}
import stui.core.geometry.Rect

/** Generators for frames, regions, and region ids.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object FrameGens {

  /** A handful of region ids, so generated frames repeat ids. */
  val regionId: Gen[RegionId] = BufferGens.regionId

  /** Regions mostly overlapping the area, sometimes hanging outside it. */
  def region(area: Rect): Gen[Region] =
    for {
      id   <- regionId
      rect <- BufferGens.rectNear(area)
    } yield Region(id, rect)

  /** Frames drawn from random canvas operations, cursor and region records included. */
  val frame: Gen[Frame] =
    for {
      area <- BufferGens.area
      ops  <- BufferGens.ops(area, Range.linear(0, 10))
    } yield Frame.draw(Buffer.empty(area))(canvas => BufferGens.runAll(canvas, ops))

}
