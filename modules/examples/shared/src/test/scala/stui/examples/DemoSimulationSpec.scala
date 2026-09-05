package stui.examples

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.app.{AppEnv, Simulator}
import stui.core.buffer.Buffer
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier, KeyModifiers, MouseEvent, MouseEventKind}
import stui.core.frame.Frame
import stui.core.geometry.{Position, Rect, Size}
import stui.core.spi.ScreenMode
import stui.core.terminal.RedrawReason
import stui.examples.Demo.Pane
import stui.testkit.{Assertions, ManualScheduler, Rendering}

import scala.concurrent.duration.*

/** The demo tested through the public simulator alone, no terminal in this scope (design doc 10 and 12, M3c): the page goldens,
  * the focus ring, the wheel through the previous frame's regions, a tab click, the uptime tick, a paste, both exits, inline mode,
  * a resize, the requested redraw, and the returned-model fixed point as a property.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object DemoSimulationSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private inline def at(inline x: Int, inline y: Int): Position = Position(NonNegInt(x), NonNegInt(y))

  private val env: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(50, 20))

  /** Wide enough for the whole footer line, whose `last` span shows the event (over 170 columns with a paste). */
  private val wide: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.AlternateScreen, sized(200, 20))

  private val inlineEnv: AppEnv = AppEnv(Capabilities.conservative, ScreenMode.inlineOf(PosInt(12)), sized(50, 12))

  private def run(env: AppEnv, steps: Simulator.Step*): Vector[Simulator.Snapshot[Demo.State]] =
    Simulator.run(Demo.app, env, steps.toVector, ManualScheduler.of(0.millis))

  private def key(code: KeyCode): Event = Event.key(KeyEvent.press(code))

  private def char(c: Char): Event = key(KeyCode.Char(c))

  private def events(events: Event*): Simulator.Step = Simulator.Step.events(events*)

  private def rows(frame: Frame): Vector[String] = Buffer.renderRows(frame.buffer)

  private def lastOf(snapshots: Vector[Simulator.Snapshot[Demo.State]])(check: Simulator.Snapshot[Demo.State] => Result): Result =
    snapshots.lastOption.fold(Result.failure.log("no snapshot"))(check)

  /** The footer's text row on a 20-row alternate screen. */
  private val footerRow: Int = 18

  /** The keys the fixed-point property drives the demo with. */
  private val demoKey: Gen[Event] = Gen.element1(
    char('1'),
    char('2'),
    char('3'),
    char('4'),
    key(KeyCode.Tab),
    key(KeyCode.Up),
    key(KeyCode.Down),
    key(KeyCode.PageUp),
    key(KeyCode.PageDown),
    key(KeyCode.Home),
    key(KeyCode.End),
    char('+'),
    char('-'),
    key(KeyCode.Left),
    key(KeyCode.Right),
    char('r'),
  )

  /** The first frame at 50 x 20 under conservative capabilities (Unicode glyphs, 16 colours), captured from the first run and
    * reviewed row by row: the centred header title, the tab strip, the body block with its wrapped wide text beside the lane, the
    * six-row events block, and the footer cut at the inner width.
    */
  private val firstFrameGolden: Vector[String] = Vector(
    "┌──────────────── stui M2c demo ─────────────────┐",
    "│colours Ansi16  ssh no  mode alt  sync no  multi│",
    "└────────────────────────────────────────────────┘",
    " Scroll │ List │ Table │ Gauge                    ",
    "┌ 안녕하세요 · こんにちは · hello 👋 ───────────┐▲",
    "│한국어: 다람쥐 헌 쳇바퀴에 타고파. 넓은 글자는 │█",
    "│터미널에서 두 칸을 차지하고, 줄 바꿈은 글자    ││",
    "│경계에서만 일어납니다.                         ││",
    "│日本語: いろはにほへと ちりぬるを わかよたれそ ││",
    "│つねならむ うゐのおくやま けふこえて           ││",
    "└───────────────────────────────────────────────┘▼",
    "┌ events ────────────────────────────────────────┐",
    "│                                                │",
    "│                                                │",
    "│                                                │",
    "│                                                │",
    "└────────────────────────────────────────────────┘",
    "┌────────────────────────────────────────────────┐",
    "│pane body  page 0  scroll 0,0  list 0  row 0 col│",
    "└────────────────────────────────────────────────┘",
  )

  /** The list page at 50 x 20: the highlighted first item, the lane's thumb at the top, the `2` key logged, `page 1` in the footer. */
  private val listPageGolden: Vector[String] = Vector(
    "┌──────────────── stui M2c demo ─────────────────┐",
    "│colours Ansi16  ssh no  mode alt  sync no  multi│",
    "└────────────────────────────────────────────────┘",
    " Scroll │ List │ Table │ Gauge                    ",
    "┌ list ─────────────────────────────────────────┐▲",
    "│▶ item 01 - 한국어 항목                        │█",
    "│  item 02 - 日本語の項目                       ││",
    "│  item 03 - emoji 🎉 item                      ││",
    "│  item 04 - plain ascii item                   ││",
    "│  item 05 - 넓은 글자 wide item                ││",
    "└───────────────────────────────────────────────┘▼",
    "┌ events ────────────────────────────────────────┐",
    "│#0 Key(event = KeyEvent(code = Char(c = 2), modi│",
    "│                                                │",
    "│                                                │",
    "│                                                │",
    "└────────────────────────────────────────────────┘",
    "┌────────────────────────────────────────────────┐",
    "│pane body  page 1  scroll 0,0  list 0  row 0 col│",
    "└────────────────────────────────────────────────┘",
  )

  override def tests: List[Test] = List(
    example("the first frame shows the header, the tabs, the scroll page, the log, and the footer", testFirstFrame),
    example("2 shows the list page", testListPage),
    example("3 and 4 show the table and the gauge page", testTableAndGauge),
    example("Tab moves the focus to the log and wraps back", testFocus),
    example("Down moves the list selection", testListDown),
    example("the wheel scrolls the pane under the mouse through the previous frame's regions", testWheel),
    example("a click on a tab title selects the page", testTabClick),
    example("the one-second tick advances the uptime", testUptime),
    example("a paste is shown in the footer", testPaste),
    example("q exits and the rest of the trace is ignored", testQuit),
    example("Control-C exits with interrupted", testInterrupt),
    example("inline mode has no log pane and keeps the focus on the page", testInline),
    example("a resize re-lays out the frame", testResize),
    example("r records the requested redraw", testRedraw),
    property("rendering a snapshot's model again returns an equal model", testFixedPoint),
  )

  def testFirstFrame: Result = lastOf(run(env))(s => Assertions.grid(s.frame.buffer, firstFrameGolden))

  def testListPage: Result = lastOf(run(env, events(char('2'))))(s => Assertions.grid(s.frame.buffer, listPageGolden))

  def testTableAndGauge: Result =
    Result.all(
      List(
        lastOf(run(env, events(char('3'))))(s => Result.assert(rows(s.frame).lift(4).exists(_.contains(" table "))).log("table title")),
        lastOf(run(env, events(char('4'))))(s =>
          Result.assert(rows(s.frame).lift(4).exists(_.contains(" progress (+ / -) "))).log("gauge title")
        ),
      )
    )

  def testFocus: Result =
    Result.all(
      List(
        lastOf(run(env, events(key(KeyCode.Tab))))(s => Assertions.eqv(s.model.focus.current, Pane.Log.some)),
        lastOf(run(env, events(key(KeyCode.Tab)), events(key(KeyCode.Tab))))(s => Assertions.eqv(s.model.focus.current, Pane.Body.some)),
      )
    )

  def testListDown: Result =
    lastOf(run(env, events(char('2')), events(key(KeyCode.Down))))(s => Assertions.eqv(s.model.list.selected, NonNegInt(1).some))

  def testWheel: Result = {
    val wheel = Event.mouse(MouseEvent(MouseEventKind.ScrollDown, at(5, 6), KeyModifiers.empty))
    lastOf(run(env, events(wheel))) { s =>
      Result.all(
        List(Assertions.eqv(s.model.scroll.rows, NonNegInt(3)), Assertions.eqv(s.model.hovered, Demo.bodyRegion.some))
      )
    }
  }

  def testTabClick: Result =
    lastOf(run(env)) { first =>
      val column = rows(first.frame).lift(3).map(_.indexOf("Table")).getOrElse(-1)
      NonNegInt.from(column) match {
        case Right(x) =>
          val click =
            Event.mouse(MouseEvent(MouseEventKind.down(stui.core.event.MouseButton.Left), Position(x, NonNegInt(3)), KeyModifiers.empty))
          lastOf(run(env, events(click)))(s => Assertions.eqv(s.model.page.selected, NonNegInt(2).some))
        case Left(_) => Result.failure.log("the tab strip shows no Table title")
      }
    }

  def testUptime: Result =
    lastOf(run(wide, Simulator.Step.advance(1.second))) { s =>
      Result.all(
        List(
          Assertions.eqv(s.model.uptime, 1L),
          Result.assert(rows(s.frame).lift(footerRow).exists(_.contains("up 1s"))).log("footer uptime"),
        )
      )
    }

  def testPaste: Result =
    lastOf(run(wide, events(Event.paste("hello")))) { s =>
      Result.all(
        List(
          Assertions.eqv(s.model.paste, "hello".some),
          Result.assert(rows(s.frame).lift(footerRow).exists(_.contains("paste hello"))).log("footer paste"),
        )
      )
    }

  def testQuit: Result = {
    val snapshots = run(env, events(char('q')), events(char('+')))
    Result.all(
      List(
        Assertions.eqv(snapshots.length, 2),
        lastOf(snapshots)(s => Result.all(List(Result.assert(s.exit).log("exit"), Assertions.eqv(s.model.exit, "quit".some)))),
      )
    )
  }

  def testInterrupt: Result = {
    val control = Event.key(KeyEvent.pressWith(KeyCode.Char('c'), KeyModifiers.of(List(KeyModifier.Control))))
    lastOf(run(env, events(control)))(s => Assertions.eqv(s.model.exit, "interrupted".some))
  }

  def testInline: Result = {
    val snapshots = run(inlineEnv, events(key(KeyCode.Tab)), events(char('p')))
    Result.all(
      List(
        snapshots.headOption.fold(Result.failure.log("no first frame")) { first =>
          Result.all(
            List(
              Assertions.eqv(rows(first.frame).length, 12),
              Result.assert(!rows(first.frame).exists(_.contains(" events "))).log("no log pane"),
            )
          )
        },
        lastOf(snapshots) { s =>
          Result.all(
            List(
              Assertions.eqv(s.model.focus.current, Pane.Body.some),
              Assertions.eqv(s.prints.length, 1),
              Result
                .assert(
                  s.prints
                    .headOption
                    .flatMap(print => Buffer.renderRows(print).headOption)
                    .exists(_.startsWith("log 0: printed above the UI"))
                )
                .log("print row"),
            )
          )
        },
      )
    )
  }

  def testResize: Result =
    lastOf(run(env, events(Event.resize(sized(40, 14))))) { s =>
      Result.all(
        List(
          Assertions.eqv(s.frame.buffer.area, Rect.sized(sized(40, 14))),
          Assertions.eqv(s.model.viewport, sized(40, 14)),
          Assertions.eqv(s.redraw, RedrawReason.Resize.some),
        )
      )
    }

  def testRedraw: Result = lastOf(run(env, events(char('r'))))(s => Assertions.eqv(s.redraw, RedrawReason.Requested.some))

  def testFixedPoint: Property =
    demoKey.list(Range.linear(0, 12)).forAll.map { keys =>
      val snapshots = Simulator.runEvents(Demo.app, env, keys.toVector, ManualScheduler.of(0.millis))
      Result.all(
        snapshots.toList.map { s =>
          val (_, again) = Rendering.stateful(Demo.app.view(s.model).root, sized(50, 20), s.model)
          Assertions.eqv(again, s.model)
        }
      )
    }

}
