package stui.testkit

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.geometry.{Position, Size}
import stui.core.spi.{TerminalFeature, TerminalOptions}
import stui.testkit.TestBackend.*

/** The in-memory backend: the screen follows the draws, the call log keeps the order, and the harness resize is silent.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object TestBackendSpec extends Properties {

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  override def tests: List[Test] = List(
    example("the screen follows the draws and the log keeps the order", testDrawAndLog),
    example("cursor calls update the state", testCursor),
    example("clear blanks the screen", testClear),
    example("resize blanks and resizes without logging", testResize),
  )

  def testDrawAndLog: Result = {
    val backend = TestBackend.of(sized(5, 1))
    val hello   = Buffer.fromLines(Vector("hello"))
    val hallo   = Buffer.fromLines(Vector("hallo"))
    val first   = Buffer.diff(Buffer.empty(hello.area), hello)
    val second  = Buffer.diff(hello, hallo)
    val options = TerminalOptions.of(TerminalFeature.AlternateScreen)
    backend.enter(options)
    backend.draw(first)
    backend.draw(second)
    backend.flush()
    backend.exit()
    Result.all(
      List(
        Assertions.grid(backend.screen, Vector("hallo")),
        Assertions.eqv(
          backend.calls,
          Vector(
            BackendCall.Enter(options),
            BackendCall.Draw(first),
            BackendCall.Draw(second),
            BackendCall.Flush,
            BackendCall.Exit,
          ),
        ),
        Assertions.eqv(backend.entered, none[TerminalOptions]),
      )
    )
  }

  def testCursor: Result = {
    val backend  = TestBackend.of(sized(5, 2))
    val position = Position(NonNegInt(3), NonNegInt(1))
    backend.moveCursor(position)
    backend.showCursor()
    val shown    = backend.cursorVisible
    backend.hideCursor()
    Result.all(
      List(
        Assertions.eqv(backend.cursorPosition, position),
        Result.assert(shown).log("shown"),
        Result.assert(!backend.cursorVisible).log("hidden again"),
        Assertions.eqv(backend.calls, Vector(BackendCall.MoveCursor(position), BackendCall.ShowCursor, BackendCall.HideCursor)),
      )
    )
  }

  def testClear: Result = {
    val backend = TestBackend.of(sized(3, 1))
    backend.draw(Buffer.diff(Buffer.empty(Buffer.fromLines(Vector("abc")).area), Buffer.fromLines(Vector("abc"))))
    backend.clear()
    Result.all(
      List(
        Assertions.grid(backend.screen, Vector("   ")),
        Assertions.eqv(backend.calls.lastOption, Option(BackendCall.Clear)),
      )
    )
  }

  def testResize: Result = {
    val backend = TestBackend.of(sized(3, 1))
    backend.draw(Buffer.diff(Buffer.empty(Buffer.fromLines(Vector("abc")).area), Buffer.fromLines(Vector("abc"))))
    backend.resize(sized(4, 2))
    Result.all(
      List(
        Assertions.eqv(backend.size(), sized(4, 2)),
        Assertions.grid(backend.screen, Vector("    ", "    ")),
        Assertions.eqv(backend.calls.length, 1),
      )
    )
  }

}
