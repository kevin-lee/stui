package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.{Capabilities, GlyphSet}
import stui.core.geometry.{Position, Size}
import stui.core.style.Modifier
import stui.core.text.Span
import stui.testkit.{Assertions, Rendering}
import stui.testkit.gen.GeometryGens

/** The [[Tabs]] golden grids: the default strip, the ASCII divider, the scrolled strip, custom paddings and dividers, and the
  * highlight landing on the title cells only.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object TabsGoldensSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val abc: Tabs = Tabs.raw("A", "B", "C")

  private val four: Tabs = Tabs.raw("aa", "bb", "cc", "dd")

  private val asciiCaps: Capabilities = Capabilities.conservative.copy(glyphs = GlyphSet.Ascii)

  private def grid(tabs: Tabs, viewport: Size, state: Selection, expected: Vector[String]): Result =
    Rendering.stateful(tabs, viewport, state) match {
      case (buffer, _) => Assertions.grid(buffer, expected)
    }

  override def tests: List[Test] = List(
    example("the default strip", grid(abc, size(11, 1), Selection.none, Vector(" A │ B │ C "))),
    example("the ASCII divider", grid(abc.withDividerFor(asciiCaps), size(11, 1), Selection.none, Vector(" A | B | C "))),
    example("the strip scrolled to the selected tab", grid(four, size(9, 1), Selection.at(nn(3)), Vector(" cc │ dd "))),
    example("the strip at the first tab", grid(four, size(9, 1), Selection.first, Vector(" aa │ bb "))),
    example(
      "custom paddings and divider",
      grid(abc.withPadding(Span.raw(""), Span.raw("")).withDivider(Span.raw("-")), size(5, 1), Selection.none, Vector("A-B-C")),
    ),
    example(
      "a strip wider than the tabs leaves the tail blank",
      grid(abc.withPadding(Span.raw(""), Span.raw("")).withDivider(Span.raw("-")), size(8, 1), Selection.none, Vector("A-B-C   ")),
    ),
    example("a strip narrower than a tab truncates it", grid(abc, size(2, 1), Selection.none, Vector(" A"))),
    example("only the first row is used", grid(abc, size(11, 2), Selection.none, Vector(" A │ B │ C ", "           "))),
    example("the highlight lands on the title cells only", testHighlight),
  )

  def testHighlight: Result = {
    val buffer                    = Rendering.stateful(abc, size(11, 1), Selection.first) match {
      case (rendered, _) => rendered
    }
    def reversed(x: Int): Boolean =
      buffer.cell(Position(GeometryGens.nonNegOrZero(x.toLong), nn(0))).exists(_.style.modifiers.contains(Modifier.Reversed))
    Result.all(
      List(
        Result.assert(reversed(1)).log("the selected title is not reversed"),
        Result.assert(!reversed(0)).log("the left padding is reversed"),
        Result.assert(!reversed(2)).log("the right padding is reversed"),
        Result.assert(!reversed(3)).log("the divider is reversed"),
        Result.assert(!reversed(5)).log("an unselected title is reversed"),
      )
    )
  }

}
