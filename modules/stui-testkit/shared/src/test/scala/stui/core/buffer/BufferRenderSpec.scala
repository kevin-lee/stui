package stui.core.buffer

import cats.syntax.all.*
import extras.render.syntax.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.gen.NastyGens

/** `Render[Buffer]` (the textual grid) and `Show[Buffer]` (logs).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object BufferRenderSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  override def tests: List[Test] = List(
    example("render joins the padded rows with LF", Buffer.fromLines(Vector("ab", "c")).render ==== "ab\nc "),
    example("a wide glyph is rendered once", Buffer.fromLines(Vector(cps(0x30b3) + "a")).render ==== cps(0x30b3) + "a"),
    example("show starts with the area and contains the rows", testShow),
  )

  def testShow: Result = {
    val shown = Buffer.fromLines(Vector("ab", "c")).show
    Result.all(
      List(Result.assert(shown.startsWith("Buffer(area = ")), Result.assert(shown.contains("ab")), Result.assert(shown.contains("c ")))
    )
  }

}
