package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Result}
import hedgehog.runner.*
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.geometry.Position
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

/** The diff laws: empty on equal buffers, round trip through `applyUpdates`, minimality, completeness, ordering, and full redraw on an
  * area mismatch.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object DiffLaws {

  private def indexOf(buffer: Buffer, position: Position): Int =
    (position.y.value - buffer.area.y.value) * buffer.area.width.value + (position.x.value - buffer.area.x.value)

  def laws(name: String, pairs: Gen[(Buffer, Buffer)], mismatched: Gen[(Buffer, Buffer)]): List[Test] = List(
    property(s"[$name] diff(b, b) is empty", pairs.forAll.map { case (b, _) => Result.assert(Buffer.diff(b, b).isEmpty) }),
    property(
      s"[$name] applyUpdates(prev, diff(prev, next)) == next",
      pairs.forAll.map { case (prev, next) => Assertions.eqv(Buffer.applyUpdates(prev, Buffer.diff(prev, next)), next) },
    ),
    property(
      s"[$name] every update differs from prev (minimality)",
      pairs.forAll.map {
        case (prev, next) =>
          Result.all(Buffer.diff(prev, next).toList.map { update =>
            Result.assert(!prev.cell(update.position).exists(_ === update.cell)).log(s"redundant update at ${update.position.show}")
          })
      },
    ),
    property(
      s"[$name] every differing position is an update (completeness) in increasing index order",
      pairs.forAll.map {
        case (prev, next) =>
          val updates  = Buffer.diff(prev, next)
          val indices  = updates.map(update => indexOf(next, update.position))
          val expected = (0 until next.cells.length).filter(i => prev.cells(i) =!= next.cells(i)).toVector
          Result.all(
            List(
              Result.assert(indices === expected).log(s"indices = ${indices.mkString(",")} expected = ${expected.mkString(",")}"),
              Result.assert(indices.zip(indices.drop(1)).forall { case (a, b) => a < b }).log("not increasing"),
            )
          )
      },
    ),
    property(
      s"[$name] different areas give a full redraw of next",
      mismatched.forAll.map {
        case (prev, next) =>
          val updates = Buffer.diff(prev, next)
          Result.all(
            List(
              Result.assert(updates.length === next.cells.length).log("update count"),
              Assertions.eqv(Buffer.applyUpdates(Buffer.empty(next.area), updates), next),
              Result.assert(updates.map(_.cell) === next.cells.toVector).log("cells in order"),
            )
          )
      },
    ),
  )

  /** Convenience for specs that want the update cells without positions. */
  def cellsOf(updates: Vector[CellUpdate]): Vector[String] = updates.map(_.cell.show)

}
