package stui.core.frame

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.geometry.{Position, Rect}
import stui.testkit.Assertions
import stui.testkit.gen.FrameGens
import stui.testkit.laws.FrameLaws

/** The frame laws on generated frames plus the clipping and precedence examples of `Canvas.cursor` and `Canvas.region`.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object FrameSpec extends Properties {

  private inline def rect(inline x: Int, inline y: Int, inline width: Int, inline height: Int): Rect =
    Rect(NonNegInt(x), NonNegInt(y), NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val a: RegionId = RegionId("a")

  private val b: RegionId = RegionId("b")

  private val base: Buffer = Buffer.empty(rect(0, 0, 3, 2))

  override def tests: List[Test] = FrameLaws.laws("frame", FrameGens.frame) ++ List(
    example("a cursor outside the area is dropped", Assertions.eqv(Frame.draw(base)(_.cursor(at(5, 5))).cursor, none[Position])),
    example("a cursor inside the area is kept", Assertions.eqv(Frame.draw(base)(_.cursor(at(1, 1))).cursor, at(1, 1).some)),
    example(
      "the last cursor call wins",
      Assertions.eqv(
        Frame
          .draw(base) { canvas =>
            canvas.cursor(at(0, 0))
            canvas.cursor(at(2, 1))
          }
          .cursor,
        at(2, 1).some,
      ),
    ),
    example(
      "a region hanging off the area is clipped",
      Assertions.eqv(Frame.draw(base)(_.region(a, rect(2, 0, 5, 5))).regions, Regions.of(Region(a, rect(2, 0, 1, 2)))),
    ),
    example("a region outside the area is dropped", Assertions.eqv(Frame.draw(base)(_.region(a, rect(3, 0, 2, 2))).regions, Regions.empty)),
    example(
      "overlapping regions resolve to the last drawn",
      Frame
        .draw(base) { canvas =>
          canvas.region(a, rect(0, 0, 3, 2))
          canvas.region(b, rect(1, 0, 1, 1))
        }
        .regions match {
        case regions =>
          Result.all(
            List(
              Assertions.eqv(regions.at(at(1, 0)), b.some),
              Assertions.eqv(regions.at(at(0, 0)), a.some),
              Assertions.eqv(regions.at(at(2, 1)), a.some),
              Assertions.eqv(regions.ids, Vector(a, b)),
              Assertions.eqv(regions.rectsOf(a), Vector(rect(0, 0, 3, 2))),
              Result.assert(!regions.isEmpty),
            )
          )
      },
    ),
    example(
      "Buffer.draw ignores the records and Frame.draw keeps the same cells",
      Assertions.eqv(
        base.draw { canvas =>
          canvas.cursor(at(1, 1))
          canvas.region(a, rect(0, 0, 1, 1))
          canvas.putString(at(0, 0), "x", stui.core.style.Style.empty)
        },
        Frame
          .draw(base) { canvas =>
            canvas.cursor(at(1, 1))
            canvas.region(a, rect(0, 0, 1, 1))
            canvas.putString(at(0, 0), "x", stui.core.style.Style.empty)
          }
          .buffer,
      ),
    ),
    example("region ids compare structurally", Assertions.eqv(RegionId("a"), a)),
  )

}
