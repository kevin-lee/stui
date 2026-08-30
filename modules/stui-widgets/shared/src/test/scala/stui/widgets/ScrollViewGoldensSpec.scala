package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Size
import stui.core.text.{Line, Text}
import stui.testkit.{Assertions, Rendering}

/** The [[ScrollView]] golden grids: windows at offsets, the clamp, a cut wide glyph, the ascii-bordered block, and a viewport larger
  * than the content.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ScrollViewGoldensSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  /** Eight rows, row `i` the letter `'a' + i - 1` repeated eight times (an 8x8 content, rows identifiable). */
  private val rows: Text = Text.fromLines(Vector.tabulate(8)(i => Line.raw(('a' + i).toChar.toString * 8)))

  /** One row of eight distinct letters (an 8x1 content, columns identifiable). */
  private val columns: Text = Text.of(Line.raw("abcdefgh"))

  private def grid(view: ScrollView, viewport: Size, state: Scroll, expected: Vector[String]): Result =
    Rendering.stateful(view, viewport, state) match {
      case (buffer, _) => Assertions.grid(buffer, expected)
    }

  override def tests: List[Test] = List(
    example(
      "the top-left window",
      grid(ScrollView.of(rows, size(8, 8)), size(5, 3), Scroll.none, Vector("aaaaa", "bbbbb", "ccccc")),
    ),
    example(
      "a row offset",
      grid(ScrollView.of(rows, size(8, 8)), size(5, 3), Scroll(nn(3), nn(0)), Vector("ddddd", "eeeee", "fffff")),
    ),
    example(
      "an oversized row offset clamps to the last window",
      grid(ScrollView.of(rows, size(8, 8)), size(5, 3), Scroll(nn(100), nn(0)), Vector("fffff", "ggggg", "hhhhh")),
    ),
    example(
      "a column offset",
      grid(ScrollView.of(columns, size(8, 1)), size(5, 1), Scroll(nn(0), nn(2)), Vector("cdefg")),
    ),
    example(
      "a wide glyph cut at the window's left edge becomes a blank",
      grid(ScrollView.of(Text.of(Line.raw("한글ab")), size(6, 1)), size(5, 1), Scroll(nn(0), nn(1)), Vector(" 글ab")),
    ),
    example(
      "an ascii-bordered block around the window",
      grid(
        ScrollView.of(rows, size(8, 8)).withBlock(Block.bordered.withBorderSet(BorderSet.ascii)),
        size(7, 5),
        Scroll.none,
        Vector("+-----+", "|aaaaa|", "|bbbbb|", "|ccccc|", "+-----+"),
      ),
    ),
    example(
      "a viewport larger than the content shows the base below",
      grid(ScrollView.of(columns, size(8, 1)), size(10, 3), Scroll.none, Vector("abcdefgh  ", "          ", "          ")),
    ),
  )

}
