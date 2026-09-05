package stui.app

import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind}
import stui.core.geometry.{Position, Size}
import stui.core.internal.NonNegInts
import stui.testkit.gen.Gens

/** Generators for the runtime laws over the counter application (design doc 12, M3c): its key vocabulary minus the deferred tasks,
  * the crash, and the exit (the laws append `q` themselves), and step traces of event batches and advances.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SimulatorGens {

  private def key(code: KeyCode): Event = Event.key(KeyEvent.press(code))

  private def char(c: Char): Event = key(KeyCode.Char(c))

  /** A square resize of side 1 to 30. */
  private val resize: Gen[Event] = Gen.int(Range.linear(1, 30)).map { n =>
    val side = NonNegInts.clamp(n.toLong)
    Event.resize(Size(side, side))
  }

  /** A left click at (1, 0), inside the counter's region on any viewport wider than one column. */
  private val click: Event =
    Event.mouse(MouseEvent(MouseEventKind.Down(MouseButton.Left), Position(NonNegInt(1), NonNegInt(0)), KeyModifiers.empty))

  /** The counter application's events: mostly increments, with prints, redraws, emits, a synchronous task, the tick toggle, focus
    * moves, resizes, a click, and a paste the application ignores.
    */
  val counterEvent: Gen[Event] = Gen.frequency1(
    6 -> Gen.constant(char('+')),
    1 -> Gen.constant(char('p')),
    1 -> Gen.constant(char('r')),
    2 -> Gen.constant(char('e')),
    1 -> Gen.constant(char('s')),
    1 -> Gen.constant(char('t')),
    2 -> Gen.constant(key(KeyCode.Tab)),
    2 -> Gen.constant(key(KeyCode.Down)),
    1 -> resize,
    1 -> Gen.constant(click),
    1 -> Gen.constant(Event.paste("x")),
  )

  /** A batch of one to four events, or up to 35 ms passing. */
  val step: Gen[Simulator.Step] = Gen.frequency1(
    6 -> counterEvent.list(Range.linear(1, 4)).map(events => Simulator.Step.Events(events.toVector)),
    2 -> Gens.millis(Range.linear(0, 35)).map(by => Simulator.Step.Advance(by)),
  )

  /** Up to ten steps. */
  val trace: Gen[Vector[Simulator.Step]] = step.list(Range.linear(0, 10)).map(_.toVector)

}
