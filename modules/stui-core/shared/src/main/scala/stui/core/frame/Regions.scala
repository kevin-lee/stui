package stui.core.frame

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.geometry.{Position, Rect}

/** The hit-test map of a frame: every region in draw order (design doc 6.4). [[Regions.at]] is a pure lookup where the last-drawn
  * region containing the position wins, so a widget drawn over another takes the mouse.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class Regions(entries: Vector[Region]) derives Eq, Show, Hash

object Regions {

  /** No region. */
  val empty: Regions = Regions(Vector.empty[Region])

  /** The given regions in draw order. */
  def of(regions: Region*): Regions = Regions(regions.toVector)

  extension (regions: Regions) {

    /** The id of the last-drawn region containing the position, `None` when no region does. */
    def at(position: Position): Option[RegionId] = regions.entries.findLast(_.rect.contains(position)).map(_.id)

    /** The regions with one more drawn last. */
    def add(region: Region): Regions = Regions(regions.entries :+ region)

    /** Every id in draw order, duplicates kept. */
    def ids: Vector[RegionId] = regions.entries.map(_.id)

    /** Every rect tagged with the id, in draw order. */
    def rectsOf(id: RegionId): Vector[Rect] = regions.entries.withFilter(_.id === id).map(_.rect)

    /** True when no region was recorded. */
    def isEmpty: Boolean = regions.entries.isEmpty

  }

}
