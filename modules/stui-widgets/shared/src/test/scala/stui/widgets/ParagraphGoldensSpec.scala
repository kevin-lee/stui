package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.Size
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.NastyGens
import stui.unicode.WidthPolicy

/** The Paragraph golden grids: truncation, alignment, both wrap modes, the Ratatui `Wrap` scaladoc example, CJK and cluster corners,
  * scrolling, ZWSP and NBSP, blocks, and the measuring API.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object ParagraphGoldensSpec extends Properties {

  private def cps(codePoints: Int*): String = NastyGens.render(codePoints)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def scroll(inline rows: Int, inline columns: Int): Scroll = Scroll(NonNegInt(rows), NonNegInt(columns))

  private val a: String = cps(0x3042)

  override def tests: List[Test] = List(
    example("plain text fits", Assertions.grid(Rendering.widget(Paragraph.raw("hello world"), size(11, 1)), Vector("hello world"))),
    example("plain text truncates", Assertions.grid(Rendering.widget(Paragraph.raw("hello world"), size(8, 1)), Vector("hello wo"))),
    example("centred text", Assertions.grid(Rendering.widget(Paragraph.raw("hi").centered, size(6, 1)), Vector("  hi  "))),
    example("right-aligned text", Assertions.grid(Rendering.widget(Paragraph.raw("hi").rightAligned, size(6, 1)), Vector("    hi"))),
    example(
      "word wrap breaks at the space",
      Assertions.grid(Rendering.widget(Paragraph.raw("hello world").withWrap(Wrap.Word), size(5, 2)), Vector("hello", "world")),
    ),
    example(
      "trimming drops the leading whitespace",
      Assertions.grid(Rendering.widget(Paragraph.raw("  hi").withWrap(Wrap.WordTrimmed), size(5, 1)), Vector("hi   ")),
    ),
    example(
      "no trimming keeps the leading whitespace",
      Assertions.grid(Rendering.widget(Paragraph.raw("  hi").withWrap(Wrap.Word), size(5, 1)), Vector("  hi ")),
    ),
    example(
      "a wide cluster never splits",
      Assertions.grid(
        Rendering.widget(Paragraph.raw(cps(0x3042, 0x3044, 0x3046)).withWrap(Wrap.Word), size(5, 2)),
        Vector(cps(0x3042, 0x3044) + " ", cps(0x3046) + "   "),
      ),
    ),
    example("the Ratatui doc example, trimmed", testRatatuiTrimmed),
    example("the Ratatui doc example, untrimmed", testRatatuiUntrimmed),
    example(
      "a long word breaks at the width",
      Assertions.grid(Rendering.widget(Paragraph.raw("abcdef").withWrap(Wrap.Word), size(4, 2)), Vector("abcd", "ef  ")),
    ),
    example(
      "an empty line survives wrapping",
      Assertions.grid(Rendering.widget(Paragraph.raw("a\n\nb").withWrap(Wrap.Word), size(3, 3)), Vector("a  ", "   ", "b  ")),
    ),
    example(
      "vertical scroll skips wrapped rows",
      Assertions.grid(
        Rendering.widget(Paragraph.raw("one two three").withWrap(Wrap.Word).withScroll(scroll(1, 0)), size(5, 2)),
        Vector("two  ", "three"),
      ),
    ),
    example(
      "horizontal scroll drops leading columns in truncate mode",
      Assertions.grid(Rendering.widget(Paragraph.raw("abcdef").withScroll(scroll(0, 2)), size(3, 1)), Vector("cde")),
    ),
    example(
      "a centred line ignores the horizontal scroll",
      Assertions.grid(Rendering.widget(Paragraph.raw("ab").centered.withScroll(scroll(0, 2)), size(6, 1)), Vector("  ab  ")),
    ),
    example(
      "a wide glyph straddling the scroll boundary is dropped",
      Assertions.grid(Rendering.widget(Paragraph.raw(a + "bc").withScroll(scroll(0, 1)), size(3, 1)), Vector("bc ")),
    ),
    example(
      "a zero width space is a break opportunity",
      Assertions.grid(
        Rendering.widget(Paragraph.raw("ab" + cps(0x200b) + "cd").withWrap(Wrap.Word), size(2, 2)),
        Vector("ab", "cd"),
      ),
    ),
    example(
      "a no-break space is not a break opportunity",
      Assertions.grid(
        Rendering.widget(Paragraph.raw("a" + cps(0xa0) + "b").withWrap(Wrap.Word), size(2, 2)),
        Vector("a" + cps(0xa0), "b "),
      ),
    ),
    example(
      "a cluster wider than the width is dropped",
      Assertions.grid(Rendering.widget(Paragraph.raw(a).withWrap(Wrap.Word), size(1, 1)), Vector(" ")),
    ),
    example(
      "a paragraph in a block",
      Assertions.grid(
        Rendering.widget(Paragraph.raw("hi").withBlock(Block.bordered), size(6, 3)),
        Vector("┌────┐", "│hi  │", "└────┘"),
      ),
    ),
    example(
      "a zero-width text area renders the block only",
      Assertions.grid(Rendering.widget(Paragraph.raw("hi").withBlock(Block.bordered), size(2, 3)), Vector("┌┐", "││", "└┘")),
    ),
    example("lineCount counts wrapped rows", testLineCount),
  )

  private val ratatuiText: String =
    "Some indented points:\n    - First thing goes here and is long so that it wraps\n    - Here is another point that is long enough to wrap"

  private def pad(rows: Vector[String]): Vector[String] = rows.map(row => row.padTo(30, ' '))

  def testRatatuiTrimmed: Result =
    Assertions.grid(
      Rendering.widget(Paragraph.raw(ratatuiText).withWrap(Wrap.WordTrimmed), size(30, 5)),
      pad(
        Vector(
          "Some indented points:",
          "- First thing goes here and is",
          "long so that it wraps",
          "- Here is another point that",
          "is long enough to wrap",
        )
      ),
    )

  def testRatatuiUntrimmed: Result =
    Assertions.grid(
      Rendering.widget(Paragraph.raw(ratatuiText).withWrap(Wrap.Word), size(30, 5)),
      pad(
        Vector(
          "Some indented points:",
          "    - First thing goes here",
          "and is long so that it wraps",
          "    - Here is another point",
          "that is long enough to wrap",
        )
      ),
    )

  def testLineCount: Result = {
    val wrapped = Paragraph.raw("hello world").withWrap(Wrap.Word)
    Result.all(
      List(
        Assertions.eqv(wrapped.lineCount(NonNegInt(5), WidthPolicy.default), 2),
        Assertions.eqv(wrapped.noWrap.lineCount(NonNegInt(5), WidthPolicy.default), 1),
        Assertions.eqv(wrapped.lineWidth(WidthPolicy.default), 11),
      )
    )
  }

}
