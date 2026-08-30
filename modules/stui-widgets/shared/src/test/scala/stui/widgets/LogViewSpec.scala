package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, NonNegLong, PosInt}
import stui.core.geometry.{Rect, Size}
import stui.core.text.Line
import stui.testkit.{Assertions, Rendering}
import stui.testkit.laws.WidgetLaws
import stui.widgets.gen.WidgetGens

/** The [[LogView]] contract laws, the fixed point, the follow transitions, and the eviction-stable anchor (design doc 6.6, decision
  * D21, M2a).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object LogViewSpec extends Properties {

  private val outer: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(24), NonNegInt(10))

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private inline def size(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private def ringOf(bound: PosInt, contents: String*): LogRing = LogRing.empty(bound).appendAll(contents.toVector.map(Line.raw))

  /** l1..l10 under a roomy bound. */
  private val ten: LogRing = ringOf(PosInt(20), (1 to 10).map(n => s"l${n.toString}")*)

  override def tests: List[Test] =
    WidgetLaws.laws("log view", WidgetGens.logViewWidget, outer) ++
      WidgetLaws.stateLaw("log view", WidgetGens.logViewInputs, outer) ++
      List(
        example(
          "following renders the tail and corrects the anchor",
          Rendering.stateful(LogView.of(ten), size(6, 3), LogViewState.following) match {
            case (buffer, corrected) =>
              Result.all(
                List(
                  Assertions.grid(buffer, Vector("l8    ", "l9    ", "l10   ")),
                  Assertions.eqv(corrected, LogViewState(NonNegLong(7L), true)),
                )
              )
          },
        ),
        example(
          "an anchored view renders its window",
          Rendering.stateful(LogView.of(ten), size(6, 2), LogViewState.anchoredAt(NonNegLong(3L))) match {
            case (buffer, corrected) =>
              Result.all(
                List(
                  Assertions.grid(buffer, Vector("l4    ", "l5    ")),
                  Assertions.eqv(corrected, LogViewState.anchoredAt(NonNegLong(3L))),
                )
              )
          },
        ),
        example(
          "scrolledUp releases follow",
          Assertions.eqv(
            LogView.scrolledUp(LogViewState.following, ten, nn(3), nn(2)),
            LogViewState.anchoredAt(NonNegLong(5L)),
          ),
        ),
        example(
          "scrolledDown regains follow at the bottom",
          Assertions.eqv(
            LogView.scrolledDown(LogViewState.anchoredAt(NonNegLong(6L)), ten, nn(3), nn(5)),
            LogViewState(NonNegLong(7L), true),
          ),
        ),
        example(
          "scrolledDown short of the bottom stays anchored",
          Assertions.eqv(
            LogView.scrolledDown(LogViewState.anchoredAt(NonNegLong(2L)), ten, nn(3), nn(1)),
            LogViewState.anchoredAt(NonNegLong(3L)),
          ),
        ),
        example(
          "an anchored reader is stable while the ring evicts", {
            val before  = ringOf(PosInt(6), "l1", "l2", "l3", "l4", "l5", "l6")
            val after   = before.appendAll(Vector(Line.raw("l7"), Line.raw("l8")))
            val state   = LogViewState.anchoredAt(NonNegLong(3L))
            val results =
              List(before, after).map(ring =>
                Rendering.stateful(LogView.of(ring), size(4, 2), state) match {
                  case (buffer, corrected) =>
                    Result.all(
                      List(
                        Assertions.grid(buffer, Vector("l4  ", "l5  ")),
                        Assertions.eqv(corrected, state),
                      )
                    )
                }
              )
            Result.all(results)
          },
        ),
        example(
          "an anchor evicted past pins to the oldest kept line", {
            val ring = ringOf(PosInt(3), "l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8")
            Rendering.stateful(LogView.of(ring), size(4, 2), LogViewState.anchoredAt(NonNegLong(1L))) match {
              case (buffer, corrected) =>
                Result.all(
                  List(
                    Assertions.grid(buffer, Vector("l6  ", "l7  ")),
                    Assertions.eqv(corrected, LogViewState.anchoredAt(NonNegLong(5L))),
                  )
                )
            }
          },
        ),
        example(
          "toTop anchors at the oldest kept line",
          Assertions.eqv(
            LogView.toTop(ringOf(PosInt(3), "a", "b", "c", "d", "e")),
            LogViewState.anchoredAt(NonNegLong(2L)),
          ),
        ),
        example("toBottom follows", Assertions.eqv(LogView.toBottom, LogViewState.following)),
        example(
          "an empty ring renders nothing and corrects the anchor to the ring start",
          Rendering.stateful(LogView.of(LogRing.empty(PosInt(5))), size(4, 2), LogViewState.anchoredAt(NonNegLong(7L))) match {
            case (buffer, corrected) =>
              Result.all(
                List(
                  Assertions.grid(buffer, Vector("    ", "    ")),
                  Assertions.eqv(corrected, LogViewState.anchoredAt(NonNegLong(0L))),
                )
              )
          },
        ),
      )

}
