package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Result}
import hedgehog.runner.*
import stui.core.frame.{Frame, RegionId, Regions}
import stui.core.geometry.Position
import stui.testkit.Assertions
import stui.testkit.gen.BufferGens

/** The frame laws (design doc 6.4 and 12): the cursor and every region lie inside the buffer area, and `Regions.at` is the last-drawn
  * lookup.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object FrameLaws {

  private def lastContaining(regions: Regions, position: Position): Option[RegionId] =
    regions.entries.foldLeft(none[RegionId])((acc, region) => if (region.rect.contains(position)) region.id.some else acc)

  /** The four laws on the generator, each test prefixed with `name`. */
  def laws(name: String, frames: Gen[Frame]): List[Test] = List(
    property(
      s"[$name] the cursor lies inside the buffer area",
      frames
        .forAll
        .map(frame =>
          Result
            .assert(frame.cursor.forall(frame.buffer.area.contains))
            .log(s"cursor ${frame.cursor.show} outside ${frame.buffer.area.show}")
        ),
    ),
    property(
      s"[$name] every region lies inside the buffer area",
      frames
        .forAll
        .map(frame =>
          Result.all(
            frame
              .regions
              .entries
              .toList
              .map(region =>
                Result
                  .assert(GeometryLaws.contained(region.rect, frame.buffer.area))
                  .log(s"region ${region.show} outside ${frame.buffer.area.show}")
              )
          )
        ),
    ),
    property(
      s"[$name] at answers with the last-drawn region containing the position",
      for {
        frame    <- frames.forAll
        position <- BufferGens.positionNear(frame.buffer.area).forAll
      } yield Assertions.eqv(frame.regions.at(position), lastContaining(frame.regions, position)),
    ),
    property(
      s"[$name] at is None outside the buffer area",
      for {
        frame   <- frames.forAll
        outside <- BufferGens.rectOutside(frame.buffer.area).forAll
      } yield Assertions.eqv(frame.regions.at(outside.position), none[RegionId]),
    ),
  )

}
