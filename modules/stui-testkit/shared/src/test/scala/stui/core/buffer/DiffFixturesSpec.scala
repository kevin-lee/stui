package stui.core.buffer

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Position, Rect}
import stui.core.style.{Color, Style}
import stui.testkit.Assertions
import stui.testkit.gen.{GeometryGens, NastyGens}

/** Ratatui's diff scenarios on Stui's structural diff, with the documented differences: the continuation of a restyled VS16 emoji is an
  * update too, default-styled shadows are overwritten explicitly, and a mismatched area is a full redraw instead of a panic.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object DiffFixturesSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private inline def rect(inline x: Int, inline y: Int, inline width: Int, inline height: Int): Rect =
    Rect(NonNegInt(x), NonNegInt(y), NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private def row(width: Int, text: String, style: Style): Buffer =
    Buffer
      .empty(Rect(NonNegInt(0), NonNegInt(0), GeometryGens.nonNegOrZero(width.toLong), NonNegInt(1)))
      .draw(_.putString(at(0, 0), text, style))

  private val blue: Style = Style.empty.withBg(Color.Blue)

  /** 你好，世界！ */
  private val niHaoShiJie: String = cps(0x4f60, 0x597d, 0xff0c, 0x4e16, 0x754c, 0xff01)

  /** 喵呜 */
  private val miaoWu: String = cps(0x55b5, 0x545c)

  private val keyboardVs16: String = cps(0x2328, 0xfe0f)

  private def columns(updates: Vector[CellUpdate]): Vector[Int] = updates.map(_.position.x.value)

  override def tests: List[Test] = List(
    example(
      "empty buffers give no updates",
      Result.assert(Buffer.diff(Buffer.empty(rect(0, 0, 5, 1)), Buffer.empty(rect(0, 0, 5, 1))).isEmpty),
    ),
    example(
      "identical buffers give no updates",
      Result.assert(Buffer.diff(Buffer.fromLines(Vector("hello")), Buffer.fromLines(Vector("hello"))).isEmpty),
    ),
    example("one changed cell", testSingleChange),
    example("all cells changed", Buffer.diff(Buffer.fromLines(Vector("aaa")), Buffer.fromLines(Vector("bbb"))).length ==== 3),
    example("restyled VS16 emoji: the glyph and its continuation", testVs16),
    example("wide text with a background replaced by narrow text: the shadows are updates", testShadowsCleared),
    example("wide text partially replaced: column 7 is an update", testPartialReplace),
    example("default-styled wide text replaced by narrow text: the shadows are updates too (divergence from Ratatui)", testDefaultShadows),
    example("a shrinking wide glyph with a background: both columns", testShrinking),
    example("a wide glyph moved by one column: four updates, column 2 becomes a continuation", testMovedWide),
    example("different areas give a full redraw of next", testMismatch),
  )

  def testSingleChange: Result = {
    val updates = Buffer.diff(Buffer.fromLines(Vector("hello")), Buffer.fromLines(Vector("hallo")))
    Result.all(
      List(
        Assertions.eqv(updates.map(_.position), Vector(at(1, 0))),
        Assertions.eqv(updates.map(_.cell.symbolOption.map(_.value)), Vector("a".some)),
      )
    )
  }

  def testVs16: Result = {
    val prev = Buffer.fromLines(Vector(keyboardVs16 + "ab"))
    val next = prev.draw(_.putString(at(0, 0), keyboardVs16, Style.empty.withFg(Color.Red)))
    Result.all(List(prev.area.width.value ==== 4, Assertions.eqv(Buffer.diff(prev, next).map(_.position), Vector(at(0, 0), at(1, 0)))))
  }

  def testShadowsCleared: Result = {
    val updates = Buffer.diff(row(12, niHaoShiJie, blue), row(12, "Hello", Style.empty))
    Result.all(List(updates.length ==== 12, Result.assert(List(5, 7, 9, 11).forall(columns(updates).contains))))
  }

  def testPartialReplace: Result = {
    val updates = Buffer.diff(row(12, niHaoShiJie, blue), row(12, miaoWu + "www", blue))
    Result.assert(columns(updates).contains(7))
  }

  def testDefaultShadows: Result = {
    val updates = Buffer.diff(row(12, niHaoShiJie, Style.empty), row(12, "Hello", Style.empty))
    Result.assert(List(5, 7, 9, 11).forall(columns(updates).contains))
  }

  def testShrinking: Result = {
    val updates = Buffer.diff(row(2, cps(0xff0b), Style.empty.withBg(Color.Red)), Buffer.empty(rect(0, 0, 2, 1)))
    Assertions.eqv(updates.map(_.position), Vector(at(0, 0), at(1, 0)))
  }

  def testMovedWide: Result = {
    val hao     = cps(0x597d)
    val prev    = row(4, cps(0x4f60) + hao, blue)
    val next    = Buffer.empty(rect(0, 0, 4, 1)).draw { canvas =>
      canvas.putString(at(0, 0), "a", blue)
      canvas.putString(at(1, 0), hao, blue)
    }
    val updates = Buffer.diff(prev, next)
    Result.all(
      List(
        updates.length ==== 4,
        Result
          .assert(updates.exists(update => update.position === at(2, 0) && update.cell.symbolOption.isEmpty))
          .log("column 2 is a continuation"),
        Result
          .assert(updates.exists(update => update.position === at(1, 0) && update.cell.symbolOption.map(_.value) === hao.some))
          .log("column 1 is the wide glyph"),
      )
    )
  }

  def testMismatch: Result = {
    val updates = Buffer.diff(Buffer.empty(rect(0, 0, 5, 1)), Buffer.fromLines(Vector("0123456789")))
    Result.all(List(updates.length ==== 10, Assertions.eqv(updates.map(_.position.x.value), Vector.tabulate(10)(identity))))
  }

}
