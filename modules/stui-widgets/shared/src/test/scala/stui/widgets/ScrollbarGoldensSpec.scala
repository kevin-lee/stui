package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.{Capabilities, GlyphSet}
import stui.core.geometry.Size
import stui.testkit.{Assertions, Rendering}

/** The [[Scrollbar]] golden grids: the four orientations, thumb positions, the ASCII set, no arrows, short lanes, and fitting
  * content.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ScrollbarGoldensSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def vertical(inline content: Int, inline viewport: Int, inline position: Int): Scrollbar =
    Scrollbar.vertical(NonNegInt(content), NonNegInt(viewport), NonNegInt(position))

  private inline def horizontal(inline content: Int, inline viewport: Int, inline position: Int): Scrollbar =
    Scrollbar.horizontal(NonNegInt(content), NonNegInt(viewport), NonNegInt(position))

  private val asciiCaps: Capabilities = Capabilities.conservative.copy(glyphs = GlyphSet.Ascii)

  override def tests: List[Test] = List(
    example(
      "a vertical bar at the top",
      Assertions.grid(Rendering.widget(vertical(12, 6, 0), size(3, 6)), Vector("  ▲", "  █", "  █", "  │", "  │", "  ▼")),
    ),
    example(
      "a vertical bar at the bottom",
      Assertions.grid(Rendering.widget(vertical(12, 6, 6), size(3, 6)), Vector("  ▲", "  │", "  │", "  █", "  █", "  ▼")),
    ),
    example(
      "a left bar halfway",
      Assertions.grid(
        Rendering.widget(vertical(12, 6, 3).withOrientation(ScrollbarOrientation.VerticalLeft), size(3, 6)),
        Vector("▲  ", "│  ", "█  ", "█  ", "│  ", "▼  "),
      ),
    ),
    example(
      "a horizontal bar at the bottom edge",
      Assertions.grid(Rendering.widget(horizontal(12, 6, 0), size(6, 2)), Vector("      ", "◄██──►")),
    ),
    example(
      "a horizontal bar at the top edge",
      Assertions.grid(
        Rendering.widget(horizontal(12, 6, 6).withOrientation(ScrollbarOrientation.HorizontalTop), size(6, 2)),
        Vector("◄──██►", "      "),
      ),
    ),
    example(
      "the ASCII vertical set",
      Assertions
        .grid(Rendering.widget(vertical(12, 6, 0).withSetFor(asciiCaps), size(3, 6)), Vector("  ^", "  #", "  #", "  |", "  |", "  v")),
    ),
    example(
      "the ASCII horizontal set",
      Assertions.grid(Rendering.widget(horizontal(12, 6, 0).withSetFor(asciiCaps), size(6, 1)), Vector("<##-->")),
    ),
    example(
      "without arrows the track takes the lane",
      Assertions
        .grid(Rendering.widget(vertical(8, 4, 4).withSet(ScrollbarSet.vertical.withoutArrows), size(1, 4)), Vector("│", "│", "█", "█")),
    ),
    example("a two-cell lane drops the arrows", Assertions.grid(Rendering.widget(vertical(12, 6, 0), size(1, 2)), Vector("█", "│"))),
    example(
      "content that fits fills the track",
      Assertions.grid(Rendering.widget(vertical(3, 6, 0), size(1, 4)), Vector("▲", "█", "█", "▼")),
    ),
    example(
      "the double sets",
      Assertions.grid(
        Rendering.widget(vertical(12, 6, 0).withSet(ScrollbarSet.doubleVertical), size(1, 6)),
        Vector("▲", "█", "█", "║", "║", "▼"),
      ),
    ),
  )

}
