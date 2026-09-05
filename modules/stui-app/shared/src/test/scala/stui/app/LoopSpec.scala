package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.app.internal.Loop
import stui.app.internal.Loop.Input
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind}
import stui.core.frame.{Region, Regions}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.spi.ScreenMode
import stui.core.terminal.RedrawReason
import stui.core.text.Line
import stui.testkit.Assertions

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The step laws (design doc 10 and 12, decision D20, M3b) over the counter application.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object LoopSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(20, 5))

  private def app: StuiApp[CounterModel, CounterMsg] =
    CounterApp.of(new AtomicReference(none[Either[Throwable, Int] => Unit]), none[Int])

  private def key(code: KeyCode): Input[CounterMsg] = Input.Received(Event.key(KeyEvent.press(code)))

  private def char(c: Char): Input[CounterMsg] = key(KeyCode.Char(c))

  private def resize(n: Int): Input[CounterMsg] = {
    val side = NonNegInts.clamp(n.toLong)
    Input.Received(Event.resize(Size(side, side)))
  }

  private def initial: CounterModel = Loop.start(app, env).model

  private val input: Gen[Input[CounterMsg]] = Gen.choice1(
    Gen.constant(char('+')),
    Gen.constant(char('p')),
    Gen.constant(char('r')),
    Gen.constant(char('e')),
    Gen.constant(key(KeyCode.Tab)),
    Gen.constant(key(KeyCode.Down)),
    Gen.int(Range.linear(1, 30)).map(resize),
  )

  private val regions: Regions = Regions.of(Region(CounterApp.region, Rect.sized(env.viewport)))

  private def click(x: Int, y: Int): Input[CounterMsg] =
    Input.Received(
      Event.mouse(
        MouseEvent(
          MouseEventKind.Down(MouseButton.Left),
          Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)),
          KeyModifiers.empty,
        )
      )
    )

  override def tests: List[Test] = List(
    property(
      "a batch's model equals the fold of single-input batches",
      input.list(Range.linear(0, 8)).map(_.toVector).forAll.map { inputs =>
        val whole  = Loop.step(app, Regions.empty, initial, inputs).model
        val folded = inputs.foldLeft(initial)((model, in) => Loop.step(app, Regions.empty, model, Vector(in)).model)
        Assertions.eqv(whole, folded)
      },
    ),
    example("a second key is routed against the model the first key produced", testRouting),
    example("Exit lets the batch complete and marks exit", testExit),
    example("prints keep command order", testPrints),
    example("Resize records the Resize reason and a later Redraw replaces it", testResizeThenRedraw),
    example("a Redraw then a Resize ends with Resize", testRedrawThenResize),
    example("a mouse input is routed through onMouse with the given regions", testMouse),
    example("start runs init's command", testStart),
    example("subscriptions come from the batch's final model", testSubscriptions),
  )

  def testRouting: Result =
    Assertions.eqv(Loop.step(app, Regions.empty, initial, Vector(key(KeyCode.Tab), key(KeyCode.Down))).model.moved, Vector(2.some))

  def testExit: Result = {
    val stepped = Loop.step(app, Regions.empty, initial, Vector(char('+'), char('q'), char('+')))
    Result.all(List(Assertions.eqv(stepped.model.count, 2), Assertions.eqv(stepped.exit, true), Assertions.eqv(stepped.model.exit, true)))
  }

  def testPrints: Result = {
    val stepped = Loop.step(app, Regions.empty, initial, Vector(char('p'), char('+'), char('p')))
    Result.all(
      List(Assertions.eqv(stepped.prints.length, 2), Assertions.eqv(stepped.model.prints, 2), Assertions.eqv(stepped.model.count, 1))
    )
  }

  def testResizeThenRedraw: Result =
    Assertions.eqv(Loop.step(app, Regions.empty, initial, Vector(resize(7), char('r'))).redraw, (RedrawReason.Requested: RedrawReason).some)

  def testRedrawThenResize: Result = {
    val stepped = Loop.step(app, Regions.empty, initial, Vector(char('r'), resize(7)))
    Result.all(
      List(
        Assertions.eqv(stepped.redraw, (RedrawReason.Resize: RedrawReason).some),
        Assertions.eqv(stepped.model.size, sized(7, 7)),
      )
    )
  }

  def testMouse: Result =
    Result.all(
      List(
        Assertions.eqv(Loop.step(app, regions, initial, Vector(click(1, 0))).model.over, CounterApp.region.some),
        Assertions.eqv(Loop.step(app, regions, initial, Vector(click(30, 30))).model.over, none[stui.core.frame.RegionId]),
        Assertions.eqv(Loop.step(app, Regions.empty, initial, Vector(click(1, 0))).model.over, none[stui.core.frame.RegionId]),
      )
    )

  def testStart: Result = {
    val emitting: StuiApp[Int, String] = new StuiApp[Int, String] {
      override def init(env: AppEnv): (Int, Cmd[String])               = (0, Cmd.emit("x"))
      override def onEvent(event: Event, model: Int): Option[String]   = none[String]
      override def update(model: Int, msg: String): (Int, Cmd[String]) = (model + 1, Cmd.none)
      override def view(model: Int): View[Int]                         = View.of(Line.raw(""))
    }
    Assertions.eqv(Loop.start(emitting, env).model, 1)
  }

  def testSubscriptions: Result = {
    val stepped = Loop.step(app, Regions.empty, initial, Vector(char('t')))
    Result.all(
      List(
        Assertions.eqv(stepped.subscriptions.map(leaf => Sub.key(leaf)), Vector[SubKey](SubKey.Every(10.millis))),
        Assertions.eqv(Loop.step(app, Regions.empty, initial, Vector(char('t'), char('t'))).subscriptions.length, 0),
      )
    )
  }

}
