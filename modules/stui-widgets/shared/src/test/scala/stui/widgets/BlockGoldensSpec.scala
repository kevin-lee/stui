package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Cell}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.{Color, Style}
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}

/** The Block golden grids: border sets, title placement and truncation, the borderless title row, degenerate sizes, canvas clipping,
  * and the style patch semantics.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object BlockGoldensSpec extends Properties {

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  override def tests: List[Test] = List(
    example(
      "a plain bordered box",
      Assertions.grid(Rendering.widget(Block.bordered, size(10, 3)), Vector("┌────────┐", "│        │", "└────────┘")),
    ),
    example(
      "a rounded bordered box",
      Assertions.grid(
        Rendering.widget(Block.bordered.withBorderSet(BorderSet.rounded), size(10, 3)),
        Vector("╭────────╮", "│        │", "╰────────╯"),
      ),
    ),
    example(
      "a double bordered box",
      Assertions.grid(
        Rendering.widget(Block.bordered.withBorderSet(BorderSet.double), size(10, 3)),
        Vector("╔════════╗", "║        ║", "╚════════╝"),
      ),
    ),
    example(
      "a thick bordered box",
      Assertions.grid(
        Rendering.widget(Block.bordered.withBorderSet(BorderSet.thick), size(10, 3)),
        Vector("┏━━━━━━━━┓", "┃        ┃", "┗━━━━━━━━┛"),
      ),
    ),
    example(
      "an ascii bordered box",
      Assertions.grid(
        Rendering.widget(Block.bordered.withBorderSet(BorderSet.ascii), size(10, 3)),
        Vector("+--------+", "|        |", "+--------+"),
      ),
    ),
    example(
      "a left title",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitle(Line.raw("Hi")), size(10, 3)),
        Vector("┌Hi──────┐", "│        │", "└────────┘"),
      ),
    ),
    example(
      "a right title",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitle(Line.raw("Hi").rightAligned), size(10, 3)),
        Vector("┌──────Hi┐", "│        │", "└────────┘"),
      ),
    ),
    example(
      "a centred title",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitle(Line.raw("Hi").centered), size(10, 3)),
        Vector("┌───Hi───┐", "│        │", "└────────┘"),
      ),
    ),
    example(
      "a bottom title",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitleBottom(Line.raw("B")), size(10, 3)),
        Vector("┌────────┐", "│        │", "└B───────┘"),
      ),
    ),
    example(
      "a left and a right title share the top row",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitle(Line.raw("A")).withTitle(Line.raw("B").rightAligned), size(10, 3)),
        Vector("┌A──────B┐", "│        │", "└────────┘"),
      ),
    ),
    example(
      "a long title truncates on the right",
      Assertions.grid(
        Rendering.widget(Block.bordered.withTitle(Line.raw("Hello")), size(6, 3)),
        Vector("┌Hell┐", "│    │", "└────┘"),
      ),
    ),
    example(
      "a borderless title still owns its row",
      Assertions.grid(Rendering.widget(Block.empty.withTitle(Line.raw("T")), size(5, 2)), Vector("T    ", "     ")),
    ),
    example("a 1x1 bordered box is the top-left corner", Assertions.grid(Rendering.widget(Block.bordered, size(1, 1)), Vector("┌"))),
    example("a 2x2 bordered box is all corners", Assertions.grid(Rendering.widget(Block.bordered, size(2, 2)), Vector("┌┐", "└┘"))),
    example("an area wider than the canvas clips at the canvas edge", testClipped),
    example("the block style fills the background and the border style patches over it", testStyles),
  )

  def testClipped: Result = {
    val buffer = Buffer
      .empty(Rect.sized(size(4, 3)))
      .draw(canvas => Block.bordered.render(Rect(NonNegInt(0), NonNegInt(0), NonNegInt(6), NonNegInt(3)), canvas))
    Assertions.grid(buffer, Vector("┌──┐", "│  │", "└──┘"))
  }

  def testStyles: Result = {
    val block  = Block.bordered.withStyle(Style.empty.withBg(Color.Blue)).withBorderStyle(Style.empty.withFg(Color.Red))
    val buffer = Rendering.widget(block, size(4, 3))
    Result.all(
      List(
        Assertions.eqv(buffer.cell(at(1, 1)).map(cell => cell.style.bg), Option[Color](Color.Blue)),
        Assertions.eqv(buffer.cell(at(0, 0)).map(cell => cell.style.fg), Option[Color](Color.Red)),
        Assertions.eqv(buffer.cell(at(0, 0)).map(cell => cell.style.bg), Option[Color](Color.Blue)),
        Assertions.eqv(buffer.cell(at(0, 0)).flatMap(_.symbolOption).map(_.value), Option[String]("┌")),
      )
    )
  }

}
