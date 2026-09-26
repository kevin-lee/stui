package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.geometry.Size
import stui.core.spi.{ScreenMode, TerminalFeature, TerminalOptions}
import stui.terminal.kitty.KittyFlags
import stui.testkit.{Assertions, TerminalModel}
import stui.testkit.TerminalModel.{QuirkProfile, Screen}

/** The entry and exit sequences interpreted by the oracle: a blank, hidden, alternate screen with exactly the wanted modes, never
  * 1003, and everything undone by the safe reset.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object SequencesSpec extends Properties {

  private val everything: TerminalOptions =
    TerminalOptions.of(
      ScreenMode.AlternateScreen,
      TerminalFeature.MouseCapture,
      TerminalFeature.BracketedPaste,
      TerminalFeature.FocusEvents,
      TerminalFeature.KeyReleaseEvents,
    )

  private val size: Size = Size(NonNegInt(3), NonNegInt(2))

  override def tests: List[Test] = List(
    example(
      "enter gives a blank hidden alternate screen with the wanted modes under a clearing terminal",
      testEnter(QuirkProfile(stui.core.buffer.GlyphWidth.Two, true)),
    ),
    example(
      "enter gives the same under a terminal that does not clear on entry",
      testEnter(QuirkProfile(stui.core.buffer.GlyphWidth.Two, false)),
    ),
    example("exit after enter undoes everything", testExit),
    example("the region bracket keeps the cursor while arming the region (R7)", testArmRegionCursor),
    example("the inline exit interpreted leaves no modes, a visible cursor, no region, and a parked cursor", testExitInlineInterpreted),
    example("the safe reset never mentions mode 2026", Result.assert(!Sequences.SafeReset.contains("2026"))),
    example(
      "the region is armed and reset inside the DECSC / DECRC bracket (R7)",
      Result.all(
        List(
          Assertions.eqv(Sequences.armRegion(1, 5), Sequences.SaveCursor + Sequences.Csi + "1;5r" + Sequences.RestoreCursor),
          Assertions.eqv(Sequences.resetRegion, Sequences.SaveCursor + Sequences.Csi + "r" + Sequences.RestoreCursor),
          Assertions.eqv(Sequences.SaveCursor, Sequences.Esc + "7"),
          Assertions.eqv(Sequences.RestoreCursor, Sequences.Esc + "8"),
        )
      ),
    ),
    example(
      "the 2026 bracket and the erase sequences are what the specifications say",
      Result.all(
        List(
          Assertions.eqv(Sequences.SyncBegin, Sequences.Csi + "?2026h"),
          Assertions.eqv(Sequences.SyncEnd, Sequences.Csi + "?2026l"),
          Assertions.eqv(Sequences.EraseBelow, Sequences.Csi + "J"),
          Assertions.eqv(Sequences.EraseToLineEnd, Sequences.Csi + "K"),
        )
      ),
    ),
    example("the kitty push and pop literals, and the conditional pop in the exit", testKittyLiterals),
    example("the kitty push follows the alternate-screen entry", testKittyEnterOrder),
    example("the push lands on the alternate stack and the exit pops it", testKittyAlternateStacks),
    example("the inline entry pushes and the inline exit pops the normal stack", testKittyInlineStacks),
    example(
      "no feature adds only the screen part",
      Assertions.eqv(
        Sequences.enter(TerminalOptions.alternateScreen, KittyFlags.none),
        Sequences.AlternateScreenEnter + Sequences.ClearScreen + Sequences.CursorHome + Sequences.CursorHide,
      ),
    ),
  )

  private val k1: KittyFlags = KittyFlags.Disambiguate

  def testKittyLiterals: Result =
    Result.all(
      List(
        Assertions.eqv(Sequences.kittyPush(k1), Sequences.Csi + ">1u"),
        Assertions.eqv(Sequences.kittyPush(KittyFlags.fromInt(7)), Sequences.Csi + ">7u"),
        Assertions.eqv(Sequences.kittyPop(k1), Sequences.Csi + "<u"),
        Assertions.eqv(Sequences.kittyPush(KittyFlags.none), ""),
        Assertions.eqv(Sequences.kittyPop(KittyFlags.none), ""),
        Assertions.eqv(AnsiWriter.exit(KittyFlags.none), Sequences.SafeReset),
        Result.assert(AnsiWriter.exit(k1).startsWith(Sequences.Csi + "<u")).log(AnsiWriter.exit(k1)),
      )
    )

  def testKittyEnterOrder: Result = {
    val entry = Sequences.enter(everything, k1)
    Result.all(
      List(
        Result.assert(entry.endsWith(Sequences.Csi + ">1u")).log(entry),
        Result.assert(entry.indexOf(Sequences.AlternateScreenEnter) < entry.indexOf(Sequences.Csi + ">1u")).log(entry),
      )
    )
  }

  def testKittyAlternateStacks: Result =
    TerminalModel.interpret(QuirkProfile.default, garbage(QuirkProfile.default), Sequences.enter(everything, k1)) match {
      case Right(entered) =>
        TerminalModel.interpret(QuirkProfile.default, entered, AnsiWriter.exit(k1)) match {
          case Right(exited) =>
            Result.all(
              List(
                Assertions.eqv(entered.keyboardAlternate, List(1)),
                Assertions.eqv(entered.keyboardMain, List.empty[Int]),
                Assertions.eqv(exited.keyboardAlternate, List.empty[Int]),
                Assertions.eqv(exited.keyboardMain, List.empty[Int]),
                Result.assert(!exited.alternate).log("normal screen"),
              )
            )
          case Left(error) => Result.failure.log(error.show)
        }
      case Left(error) => Result.failure.log(error.show)
    }

  def testKittyInlineStacks: Result = {
    val options  = TerminalOptions.of(ScreenMode.Inline(refined4s.types.numeric.PosInt(3)))
    val viewport = stui.core.geometry.Rect(NonNegInt(0), NonNegInt(3), NonNegInt(3), NonNegInt(3))
    val blank    = Screen.blank(QuirkProfile.default, Size(NonNegInt(3), NonNegInt(6)))
    TerminalModel.interpret(QuirkProfile.default, blank, AnsiWriter.enterInline(options, 0, None, k1)._2) match {
      case Right(entered) =>
        TerminalModel.interpret(
          QuirkProfile.default,
          entered.copy(cursor = stui.core.geometry.Position.origin),
          AnsiWriter.exitInline(viewport, k1),
        ) match {
          case Right(exited) =>
            Result.all(
              List(
                Assertions.eqv(entered.keyboardMain, List(1)),
                Assertions.eqv(exited.keyboardMain, List.empty[Int]),
              )
            )
          case Left(error) => Result.failure.log(error.show)
        }
      case Left(error) => Result.failure.log(error.show)
    }
  }

  private def garbage(profile: QuirkProfile): Screen =
    Screen
      .blank(profile, size)
      .copy(buffer = Buffer.fromLinesWith(TerminalModel.policyOf(profile), Vector("abc", "def")), cursorVisible = true)

  def testEnter(profile: QuirkProfile): Result =
    TerminalModel.interpret(profile, garbage(profile), Sequences.enter(everything, KittyFlags.none)) match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(Buffer.renderRows(screen.buffer), Vector("   ", "   ")),
            Result.assert(!screen.cursorVisible).log("hidden"),
            Result.assert(screen.alternate).log("alternate"),
            Assertions.eqv(screen.modes, Set(1000, 1002, 1006, 2004, 1004)),
            Assertions.eqv(screen.cursor, stui.core.geometry.Position.origin),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }

  def testArmRegionCursor: Result = {
    val parked = Screen
      .blank(QuirkProfile.default, Size(NonNegInt(3), NonNegInt(6)))
      .copy(cursor = stui.core.geometry.Position(NonNegInt(2), NonNegInt(4)))
    TerminalModel.interpret(QuirkProfile.default, parked, Sequences.armRegion(1, 4)) match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(screen.cursor, stui.core.geometry.Position(NonNegInt(2), NonNegInt(4))),
            Assertions.eqv(screen.region, TerminalModel.Region(NonNegInt(0), NonNegInt(3)).some),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }
  }

  def testExitInlineInterpreted: Result = {
    val viewport = stui.core.geometry.Rect(NonNegInt(0), NonNegInt(3), NonNegInt(3), NonNegInt(3))
    val entered  = Screen
      .blank(QuirkProfile.default, Size(NonNegInt(3), NonNegInt(6)))
      .copy(
        cursorVisible = false,
        modes = Set(1000, 1002, 1006, 2004, 1004),
        region = TerminalModel.Region(NonNegInt(0), NonNegInt(2)).some,
        style = stui.core.style.CellStyle.default.copy(fg = stui.core.style.Color.Red),
      )
    TerminalModel.interpret(QuirkProfile.default, entered, stui.terminal.ansi.AnsiWriter.exitInline(viewport, KittyFlags.none)) match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(screen.modes, Set.empty[Int]),
            Result.assert(screen.cursorVisible).log("visible"),
            Assertions.eqv(screen.region, none[TerminalModel.Region]),
            Assertions.eqv(screen.style, stui.core.style.CellStyle.default),
            Assertions.eqv(screen.cursor, stui.core.geometry.Position(NonNegInt(0), NonNegInt(5))),
            Assertions.eqv(screen.scrollback.length, 1),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }
  }

  def testExit: Result =
    TerminalModel.interpret(
      QuirkProfile.default,
      garbage(QuirkProfile.default),
      Sequences.enter(everything, KittyFlags.none) + Sequences.SafeReset,
    ) match {
      case Right(screen) =>
        Result.all(
          List(
            Assertions.eqv(screen.modes, Set.empty[Int]),
            Result.assert(!screen.alternate).log("normal screen"),
            Result.assert(screen.cursorVisible).log("visible"),
            Assertions.eqv(screen.style, stui.core.style.CellStyle.default),
          )
        )
      case Left(error) => Result.failure.log(error.show)
    }

}
