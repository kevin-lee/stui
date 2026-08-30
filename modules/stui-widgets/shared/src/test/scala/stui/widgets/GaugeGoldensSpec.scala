package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.geometry.Size
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}

/** The [[Gauge]] golden grids: empty, half, full, thirds, eighths, the ASCII set, a custom label, a block, and a tall bar.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object GaugeGoldensSpec extends Properties {

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def fraction(inline done: Int, inline total: Int): Gauge = Gauge.fraction(NonNegInt(done), PosInt(total))

  override def tests: List[Test] = List(
    example("nothing done", Assertions.grid(Rendering.widget(fraction(0, 1), size(10, 1)), Vector("    0%    "))),
    example("half done", Assertions.grid(Rendering.widget(fraction(1, 2), size(10, 1)), Vector("███50%    "))),
    example("everything done", Assertions.grid(Rendering.widget(fraction(1, 1), size(10, 1)), Vector("███100%███"))),
    example("a third in twelve cells", Assertions.grid(Rendering.widget(fraction(1, 3), size(12, 1)), Vector("████33%     "))),
    example("five eighths draw a half block", Assertions.grid(Rendering.widget(fraction(5, 8), size(12, 1)), Vector("████63%▌    "))),
    example(
      "the ASCII set rounds to whole cells",
      Assertions.grid(Rendering.widget(fraction(1, 3).withSet(GaugeSet.ascii), size(10, 1)), Vector("###33%    ")),
    ),
    example(
      "the ASCII set at a half",
      Assertions.grid(Rendering.widget(fraction(1, 2).withSet(GaugeSet.ascii), size(10, 1)), Vector("###50%    ")),
    ),
    example(
      "a custom label",
      Assertions.grid(Rendering.widget(fraction(1, 2).withLabel(Line.raw("half")), size(10, 1)), Vector("███half   ")),
    ),
    example(
      "a gauge in a block",
      Assertions.grid(Rendering.widget(fraction(1, 2).withBlock(Block.bordered), size(8, 3)), Vector("┌──────┐", "│█50%  │", "└──────┘")),
    ),
    example(
      "a tall bar labels its middle row",
      Assertions.grid(Rendering.widget(fraction(1, 2), size(10, 3)), Vector("█████     ", "███50%    ", "█████     ")),
    ),
    example("a label wider than the bar truncates", Assertions.grid(Rendering.widget(fraction(1, 1), size(3, 1)), Vector("100"))),
  )

}
