package stui.core.terminal

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Buffer
import stui.testkit.Assertions

/** The bounded transcript ring (design doc 7.2, decision D13).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object PrintRingSpec extends Properties {

  private def rows(lines: String*): Buffer = Buffer.fromLines(lines.toVector)

  private def rendered(ring: PrintRing): Vector[Vector[String]] = ring.entries.map(Buffer.renderRows)

  override def tests: List[Test] = List(
    example("an empty ring holds nothing", testEmpty),
    example("appends below the bound accumulate in order", testAppend),
    example("the oldest print is dropped when the total exceeds the bound", testEvict),
    example("a print taller than the bound keeps its last rows", testTall),
    example("a zero-height print changes nothing", testZero),
  )

  def testEmpty: Result = {
    val ring = PrintRing.empty(PosInt(3))
    Result.all(List(Result.assert(ring.isEmpty), Assertions.eqv(ring.totalRows, NonNegInt(0))))
  }

  def testAppend: Result = {
    val ring = PrintRing.empty(PosInt(3)).append(rows("a")).append(rows("b", "c"))
    Result.all(
      List(
        Assertions.eqv(rendered(ring), Vector(Vector("a"), Vector("b", "c"))),
        Assertions.eqv(ring.totalRows, NonNegInt(3)),
        Result.assert(!ring.isEmpty),
      )
    )
  }

  def testEvict: Result = {
    val ring = PrintRing.empty(PosInt(3)).append(rows("a", "b")).append(rows("c", "d"))
    Result.all(
      List(
        Assertions.eqv(rendered(ring), Vector(Vector("c", "d"))),
        Assertions.eqv(ring.totalRows, NonNegInt(2)),
      )
    )
  }

  def testTall: Result = {
    val ring = PrintRing.empty(PosInt(2)).append(rows("a", "b", "c", "d"))
    Result.all(
      List(
        Assertions.eqv(rendered(ring), Vector(Vector("c", "d"))),
        Assertions.eqv(ring.totalRows, NonNegInt(2)),
      )
    )
  }

  def testZero: Result = {
    val ring = PrintRing.empty(PosInt(2))
    Assertions.eqv(ring.append(Buffer.empty(stui.core.geometry.Rect.empty)), ring)
  }

}
