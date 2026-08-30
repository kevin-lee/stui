package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, NonNegLong, PosInt}
import stui.core.geometry.Size
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}

/** The [[LogView]] golden grids: the followed tail, an anchored window, and truncation of long and wide lines.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object LogViewGoldensSpec extends Properties {

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private def ringOf(contents: String*): LogRing = LogRing.empty(PosInt(20)).appendAll(contents.toVector.map(Line.raw))

  private def grid(ring: LogRing, viewport: Size, state: LogViewState, expected: Vector[String]): Result =
    Rendering.stateful(LogView.of(ring), viewport, state) match {
      case (buffer, _) => Assertions.grid(buffer, expected)
    }

  override def tests: List[Test] = List(
    example(
      "following shows the tail",
      grid(ringOf("l1", "l2", "l3", "l4", "l5"), size(5, 3), LogViewState.following, Vector("l3   ", "l4   ", "l5   ")),
    ),
    example(
      "anchored at the top shows the head",
      grid(ringOf("l1", "l2", "l3", "l4", "l5"), size(5, 3), LogViewState.anchoredAt(NonNegLong(0L)), Vector("l1   ", "l2   ", "l3   ")),
    ),
    example(
      "a long line truncates at the width",
      grid(ringOf("0123456789"), size(4, 1), LogViewState.following, Vector("0123")),
    ),
    example(
      "a wide line truncates without splitting a glyph",
      grid(ringOf("한글a"), size(4, 1), LogViewState.following, Vector("한글")),
    ),
  )

}
