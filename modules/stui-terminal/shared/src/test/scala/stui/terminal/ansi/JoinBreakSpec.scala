package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, GlyphWidth}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.{CellStyle, Style}
import stui.testkit.{Assertions, TerminalModel}
import stui.testkit.TerminalModel.{QuirkProfile, Screen}
import stui.testkit.gen.{GeometryGens, NastyGens}
import stui.unicode.WidthPolicy
import stui.unicode.internal.CodePointProperties
import stui.unicode.internal.CodePointProperties.Gcb
import stui.unicode.internal.IntOps.*

/** Rule R2a (2026-08-31): two adjacent cells whose symbols would join into one grapheme cluster on the wire (lone regional
  * indicators, standalone spacing marks) stay two cells, because the writer places the cursor explicitly between them - CUP in a
  * present, CHA in a printed row. The two shapes hedgehog found (seeds 260613401291 and 276859882) are pinned as examples, and a
  * property places joinable symbols one per cell.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object JoinBreakSpec extends Properties {

  private val profile: QuirkProfile = QuirkProfile(GlyphWidth.Two, true)

  private val caps: Capabilities = WriterGens.losslessFor(profile)

  private def normalise(capabilities: Capabilities): CellStyle => CellStyle = style => Sgr.normalise(capabilities, style)

  /** A lone regional indicator (U+1F1E6). */
  private val ri: String = NastyGens.render(List(0x1f1e6))

  /** The standalone Tamil vowel sign I (U+0BBF, a spacing mark of width 1). */
  private val mark: String = NastyGens.render(List(0x0bbf))

  private val chaPattern = (Sequences.Esc + "\\[[0-9]*G").r

  private val cupPattern = (Sequences.Esc + "\\[[0-9;]*H").r

  private def nn(n: Int): NonNegInt = GeometryGens.nonNegOrZero(n.toLong)

  /** One row at the origin, each symbol written into its own cell. */
  private def cells(symbols: Vector[String]): Buffer = {
    val width = symbols.foldLeft(0)((acc, s) => acc + WidthPolicy.default.width(s))
    Buffer.empty(Rect.sized(Size(nn(width), nn(1)))).draw { canvas =>
      symbols.foldLeft(0) { (x, s) =>
        canvas.putString(Position(nn(x), nn(0)), s, Style.empty)
        x + WidthPolicy.default.width(s)
      }: Unit
    }
  }

  private def presentOutput(capabilities: Capabilities, buffer: Buffer): String =
    AnsiWriter.present(WriterState.initial, capabilities, buffer.area, Buffer.diff(Buffer.empty(buffer.area), buffer)) match {
      case (_, output) => output
    }

  private def printOutput(capabilities: Capabilities, buffer: Buffer): String =
    AnsiWriter.print(WriterState.initial, capabilities, buffer) match {
      case (_, output) => output
    }

  /** The present of `buffer` over a blank screen of its size shows `buffer`. */
  private def presentRoundTrip(profile: QuirkProfile, capabilities: Capabilities, buffer: Buffer): Result = {
    val size  = buffer.area.size
    val blank = Buffer.empty(buffer.area)
    TerminalModel.interpret(
      profile,
      Screen.ofTerminal(profile, normalise(capabilities), size, blank),
      presentOutput(capabilities, buffer),
    ) match {
      case Right(screen) => Assertions.eqv(screen.buffer, TerminalModel.placed(profile, normalise(capabilities), size, buffer))
      case Left(error) => Result.failure.log(s"${error.show} for ${presentOutput(capabilities, buffer).replace(Sequences.Esc, "ESC")}")
    }
  }

  /** The print of `buffer` on a blank two-row screen of its width writes `buffer` on the first row. */
  private def printRoundTrip(profile: QuirkProfile, capabilities: Capabilities, buffer: Buffer): Result = {
    val size  = Size(buffer.area.width, nn(2))
    val blank = Buffer.empty(Rect.sized(size))
    TerminalModel.interpret(
      profile,
      Screen.ofTerminal(profile, normalise(capabilities), size, blank),
      printOutput(capabilities, buffer),
    ) match {
      case Right(screen) => Assertions.eqv(screen.buffer, TerminalModel.placed(profile, normalise(capabilities), size, buffer))
      case Left(error) => Result.failure.log(s"${error.show} for ${printOutput(capabilities, buffer).replace(Sequences.Esc, "ESC")}")
    }
  }

  /** Width-1 spacing marks, the standalone-mark class; the narrow clusters when the tables offer none. */
  private val spacingMark: Gen[String] = {
    val pool = NastyGens.pool(Gcb.SpacingMark).filter(cp => CodePointProperties.width(cp) === 1).toList
    if (pool.isEmpty) NastyGens.narrowCluster else Gen.elementUnsafe(pool).map(cp => NastyGens.render(List(cp)))
  }

  private val joinable: Gen[String] = Gen.frequency1(1 -> NastyGens.regionalIndicators(Range.linear(1, 1)), 1 -> spacingMark)

  override def tests: List[Test] = List(
    example("two lone regional indicators stay two cells in a present", presentRoundTrip(profile, caps, cells(Vector(ri, ri)))),
    example("two adjacent spacing marks stay two cells in a present", presentRoundTrip(profile, caps, cells(Vector(mark, mark)))),
    example("two lone regional indicators stay two cells in a printed row", printRoundTrip(profile, caps, cells(Vector(ri, ri)))),
    example("two adjacent spacing marks stay two cells in a printed row", printRoundTrip(profile, caps, cells(Vector(mark, mark)))),
    example("the break sequence sits between the joining cells", testBreakSequence),
    example("an ASCII row emits no extra cursor placement", testAsciiRow),
    example("joins agrees with the segmenter", testJoins),
    property(
      "joinable symbols one per cell survive a present and a print",
      for {
        prof    <- WriterGens.profile.forAll
        cap     <- WriterGens.capabilitiesFor(prof).forAll
        symbols <- joinable.list(Range.linear(2, 6)).forAll
      } yield {
        val buffer = cells(symbols.toVector)
        Result.all(List(presentRoundTrip(prof, cap, buffer), printRoundTrip(prof, cap, buffer)))
      },
    ),
  )

  def testBreakSequence: Result = {
    val pair = cells(Vector(ri, ri))
    Result.all(
      List(
        Result.assert(presentOutput(caps, pair).contains(ri + Sequences.cup(1, 2) + ri)).log("no CUP between the regional indicators"),
        Result.assert(printOutput(caps, pair).contains(ri + Sequences.cha(2) + ri)).log("no CHA between the regional indicators"),
      )
    )
  }

  def testAsciiRow: Result = {
    val row = cells(Vector("a", "b", "c"))
    Result.all(
      List(
        Assertions.eqv(cupPattern.findAllIn(presentOutput(caps, row)).length, 1),
        Result.assert(chaPattern.findFirstIn(printOutput(caps, row)).isEmpty).log("a CHA in an ASCII printed row"),
      )
    )
  }

  def testJoins: Result =
    Result.all(
      List(
        Result.assert(AnsiWriter.joins(ri, ri)).log("two regional indicators do not join"),
        Result.assert(!AnsiWriter.joins("a", ri)).log("a letter joins a regional indicator"),
        Result.assert(AnsiWriter.joins("a", mark)).log("a spacing mark does not join a letter"),
        Result.assert(AnsiWriter.joins(" ", mark)).log("a spacing mark does not join a space"),
        Result.assert(!AnsiWriter.joins("", mark)).log("an empty last joins"),
        Result.assert(!AnsiWriter.joins("a", "b")).log("two letters join"),
      )
    )

}
