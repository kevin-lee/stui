package stui.core.frame

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.buffer.{Buffer, Canvas}
import stui.core.geometry.Position

/** The render surface of one frame as a value (design doc 6.4, decision D18): the buffer, the cursor to place after the present (`None`
  * keeps it hidden), and the hit regions for mouse routing. Every recorded value lies inside the buffer area by construction (the canvas
  * clips regions to its area and drops a cursor outside it). `Buffer.draw` discards the records, [[Frame.draw]] keeps them.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class Frame(buffer: Buffer, cursor: Option[Position], regions: Regions) derives Eq, Show, Hash

object Frame {

  /** Draws on a copy of `base` exactly as `Buffer.draw` does and keeps the cursor and the regions the render recorded. */
  def draw(base: Buffer)(render: Canvas => Unit): Frame =
    Buffer.drawRecording(base)(render) match {
      case (buffer, cursor, regions) => Frame(buffer, cursor, regions)
    }

}
