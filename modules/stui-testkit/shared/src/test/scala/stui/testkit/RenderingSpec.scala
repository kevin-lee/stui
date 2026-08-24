package stui.testkit

import hedgehog.*
import hedgehog.core.{Error, ForAll, Info}
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Canvas
import stui.core.geometry.{Rect, Size}
import stui.core.style.Style
import stui.core.widget.{StatefulWidget, Widget}

/** The golden renderers and the grid assertion, including its failure format.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object RenderingSpec extends Properties {

  /** Fills its whole area with one symbol. */
  final private case class SymbolWidget(symbol: String) extends Widget {
    /* the method lives in the class body because it implements the Widget trait member */
    override def render(area: Rect, canvas: Canvas): Unit = canvas.fill(area, symbol, Style.empty)
  }

  /** Writes nothing and clamps its state at 3. */
  final private case class ClampAtThree() extends StatefulWidget[Int] {
    /* the method lives in the class body because it implements the StatefulWidget trait member */
    override def render(area: Rect, canvas: Canvas, state: Int): Int = math.min(state, 3)
  }

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  override def tests: List[Test] = List(
    example("widget renders into a fresh grid", Assertions.grid(Rendering.widget(SymbolWidget("x"), sized(3, 2)), Vector("xxx", "xxx"))),
    example(
      "widget renders the same grid twice",
      Assertions.eqv(Rendering.widget(SymbolWidget("x"), sized(3, 2)), Rendering.widget(SymbolWidget("x"), sized(3, 2))),
    ),
    example("stateful returns the buffer and the corrected state", testStateful),
    example("grid succeeds on equality", Assertions.grid(Rendering.widget(SymbolWidget("y"), sized(2, 1)), Vector("yy"))),
    example("grid fails with marked blocks on a mismatch", testGridFailure),
  )

  def testStateful: Result = {
    Rendering.stateful(ClampAtThree(), sized(2, 1), 10) match {
      case (buffer, corrected) =>
        Result.all(List(Assertions.grid(buffer, Vector("  ")), Assertions.eqv(corrected, 3)))
    }
  }

  def testGridFailure: Result = {
    val failed = Assertions.grid(Rendering.widget(SymbolWidget("x"), sized(2, 2)), Vector("xx", "x "))
    Result.all(
      List(
        Result.assert(!failed.success).log("expected a failure"),
        Result
          .assert(failed.logs.exists {
            case Info(value) => value.contains("*|x ") && value.contains("--- expected ---")
            case ForAll(_, _) => false
            case Error(_) => false
          })
          .log("expected the marked blocks in the failure log"),
      )
    )
  }

}
