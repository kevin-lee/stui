package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Size
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}

/** The [[ListView]] golden grids: the symbol column under each spacing rule, a scrolled window, a multi-line item with a repeated
  * symbol, bottom-to-top stacking, a block, and truncation of long and wide items.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ListViewGoldensSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val three: ListView = ListView.raw("i1", "i2", "i3")

  private val symbol: Line = Line.raw("> ")

  private def grid(view: ListView, viewport: Size, state: Selection, expected: Vector[String]): Result =
    Rendering.stateful(view, viewport, state) match {
      case (buffer, _) => Assertions.grid(buffer, expected)
    }

  override def tests: List[Test] = List(
    example("no selection under WhenSelected", grid(three, size(5, 3), Selection.none, Vector("i1   ", "i2   ", "i3   "))),
    example(
      "a selected item carries the symbol",
      grid(three.withHighlightSymbol(symbol), size(5, 3), Selection.at(nn(1)), Vector("  i1 ", "> i2 ", "  i3 ")),
    ),
    example(
      "Always reserves the column without a selection",
      grid(
        three.withHighlightSymbol(symbol).withHighlightSpacing(HighlightSpacing.Always),
        size(5, 3),
        Selection.none,
        Vector("  i1 ", "  i2 ", "  i3 "),
      ),
    ),
    example(
      "Never draws no column",
      grid(
        three.withHighlightSymbol(symbol).withHighlightSpacing(HighlightSpacing.Never),
        size(5, 3),
        Selection.at(nn(1)),
        Vector("i1   ", "i2   ", "i3   "),
      ),
    ),
    example(
      "the window scrolls to the selection",
      grid(
        ListView.raw((1 to 8).map(n => s"i${n.toString}")*).withHighlightSymbol(symbol),
        size(5, 3),
        Selection.at(nn(5)),
        Vector("  i4 ", "  i5 ", "> i6 "),
      ),
    ),
    example(
      "a repeated symbol covers a multi-line item",
      grid(
        ListView.raw("a\nb", "c").withHighlightSymbol(symbol).withRepeatHighlightSymbol(true),
        size(5, 3),
        Selection.first,
        Vector("> a  ", "> b  ", "  c  "),
      ),
    ),
    example(
      "a symbol not repeated marks the first line only",
      grid(ListView.raw("a\nb", "c").withHighlightSymbol(symbol), size(5, 3), Selection.first, Vector("> a  ", "  b  ", "  c  ")),
    ),
    example(
      "bottom to top stacks upward",
      grid(three.withDirection(ListDirection.BottomToTop), size(5, 3), Selection.none, Vector("i3   ", "i2   ", "i1   ")),
    ),
    example(
      "a block around the items",
      grid(three.withBlock(Block.bordered), size(7, 5), Selection.none, Vector("┌─────┐", "│i1   │", "│i2   │", "│i3   │", "└─────┘")),
    ),
    example("a wide item truncates at a cluster boundary", grid(ListView.raw("한글abc"), size(5, 1), Selection.none, Vector("한글a"))),
    example("a long item truncates", grid(ListView.raw("abcdefgh"), size(5, 1), Selection.none, Vector("abcde"))),
    example(
      "a viewport shorter than the items shows the first ones",
      grid(three, size(5, 2), Selection.none, Vector("i1   ", "i2   ")),
    ),
  )

}
