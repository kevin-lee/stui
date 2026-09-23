package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.Buffer
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.style.{CellStyle, Color, Style}
import stui.testkit.TerminalModel
import stui.testkit.TerminalModel.{QuirkProfile, Screen}
import stui.testkit.gen.{CapabilityGens, GeometryGens, NastyGens}

/** Rule R3a (issue 42): a two-column VS16 cluster is written as two blanking spaces in its style, a placement back on its first cell,
  * and the cluster, with the next write placed explicitly, so the screen matches the buffer whether the terminal advances one column or
  * two (iTerm2 3.7.3 advances one on the alternate screen and two on the normal screen by default). Every law here runs under all four
  * quirk profiles with capabilities that say nothing about the terminal's VS16 width.
  *
  * @author Kevin Lee
  * @since 2026-09-24
  */
object Vs16EmissionSpec extends Properties {

  /** `Assertions.eqv` on buffers logs only the grids, so a style-only mismatch is logged cell by cell here. */
  private def sameBuffer(actual: Buffer, expected: Buffer): Result =
    if (actual === expected) Result.success
    else Result.failure.log(s"--- actual cells ---\n${actual.cells.toVector.show}\n--- expected cells ---\n${expected.cells.toVector.show}")

  private def normaliseWith(caps: Capabilities): CellStyle => CellStyle = style => Sgr.normalise(caps, style)

  private def esc(output: String): String = output.replace(Sequences.Esc, "ESC")

  private def failed(profile: QuirkProfile, error: TerminalModel.ModelError, output: String): Result =
    Result.failure.log(s"${profile.show}: ${error.show} for ${esc(output)}")

  override def tests: List[Test] = List(
    property("a VS16-heavy present shows the next buffer under every quirk profile (R3a)", testRoundTrip),
    property("chained VS16-heavy presents from a blank screen show the next buffer under every quirk profile (R3a)", testChained),
    property("a VS16-heavy print shows its rows under every quirk profile (R3a)", testPrint),
    property("the byte budget holds for VS16-heavy presents and prints (R10, R3a)", testBudget),
    example("the demo's Emoji row and the scrolled row after it match the buffer under both VS16 widths (issue 42)", testDemoRow),
  )

  def testRoundTrip: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.vs16Pair.forAll
    } yield pair match {
      case (prev, next) =>
        val normalise = normaliseWith(caps)
        val output    = AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.diff(prev, next))._2
        Result.all(QuirkProfile.all.map { profile =>
          TerminalModel.interpret(profile, Screen.ofWith(profile, normalise, prev), output) match {
            case Right(screen) => sameBuffer(screen.buffer, TerminalModel.expectedWith(profile, normalise, next))
            case Left(error) => failed(profile, error, output)
          }
        })
    }

  def testChained: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.vs16Pair.forAll
    } yield pair match {
      case (prev, next) =>
        val normalise = normaliseWith(caps)
        AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.allUpdates(prev)) match {
          case (state1, out1) =>
            val out2 = AnsiWriter.present(state1, caps, prev.area, Buffer.diff(prev, next))._2
            Result.all(QuirkProfile.all.map { profile =>
              TerminalModel.interpret(profile, Screen.blank(profile, prev.area.size), out1 + out2) match {
                case Right(screen) => sameBuffer(screen.buffer, TerminalModel.expectedWith(profile, normalise, next))
                case Left(error) => failed(profile, error, out1 + out2)
              }
            })
        }
    }

  def testPrint: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.vs16Pair.forAll
    } yield {
      val buffer    = pair._1
      val normalise = normaliseWith(caps)
      val size      = Size(buffer.area.width, GeometryGens.nonNegOrZero(buffer.area.height.value.toLong + 1L))
      val blank     = Buffer.empty(Rect.sized(size))
      val output    = AnsiWriter.print(WriterState.initial, caps, buffer)._2
      Result.all(QuirkProfile.all.map { profile =>
        TerminalModel.interpret(profile, Screen.ofTerminal(profile, normalise, size, blank), output) match {
          case Right(screen) => sameBuffer(screen.buffer, TerminalModel.placed(profile, normalise, size, buffer))
          case Left(error) => failed(profile, error, output)
        }
      })
    }

  def testBudget: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.vs16Pair.forAll
    } yield pair match {
      case (prev, next) =>
        val updates      = Buffer.diff(prev, next)
        val presented    = AnsiWriter.utf8Length(AnsiWriter.present(WriterState.initial, caps, prev.area, updates)._2)
        val presentLimit = AnsiWriter.MaxBytesPerCell.toLong * updates.length.toLong + AnsiWriter.symbolBytes(updates) +
          AnsiWriter.MaxBytesPerPresent.toLong
        val rows         = next.area.height.value.toLong
        val cells        = next.area.width.value.toLong * rows
        val printed      = AnsiWriter.utf8Length(AnsiWriter.print(WriterState.initial, caps, next)._2)
        val printLimit   = AnsiWriter.MaxBytesPerCell.toLong * cells + AnsiWriter.bufferSymbolBytes(next) +
          AnsiWriter.MaxBytesPerPrintedRow.toLong * rows + AnsiWriter.MaxBytesPerPrint.toLong
        Result.all(
          List(
            Result.assert(presented <= presentLimit).log(s"present bytes ${presented.toString} > budget ${presentLimit.toString}"),
            Result.assert(printed <= printLimit).log(s"print bytes ${printed.toString} > budget ${printLimit.toString}"),
          )
        )
    }

  /** The demo's scroll page on iTerm2's alternate screen (issue 42): the Emoji row, then a marker row scrolled into its place, each
    * with the body's right border and the scrollbar lane beside it.
    */
  def testDemoRow: Result = {
    val area                        = Rect.sized(Size(NonNegInt(40), NonNegInt(1)))
    val keyboard                    = NastyGens.render(List(0x2328, 0xfe0f))
    val flags                       = NastyGens.render(List(0x1f1f0, 0x1f1f7)) + " " + NastyGens.render(List(0x1f1ef, 0x1f1f5))
    def edged(text: String): Buffer =
      Buffer.empty(area).draw { canvas =>
        canvas.putString(Position(NonNegInt(0), NonNegInt(0)), text, Style.empty)
        canvas.putString(Position(NonNegInt(38), NonNegInt(0)), "│", Style.empty.withFg(Color.Magenta))
        canvas.putString(Position(NonNegInt(39), NonNegInt(0)), "█", Style.empty)
      }
    val prev                        = edged("Emoji: 👋 🎉 " + flags + " " + keyboard + " (a VS16 two)")
    val next                        = edged("L0007 the quick brown fox jumps")
    AnsiWriter.present(WriterState.initial, Capabilities.lossless, area, Buffer.allUpdates(prev)) match {
      case (state1, out1) =>
        val out2 = AnsiWriter.present(state1, Capabilities.lossless, area, Buffer.diff(prev, next))._2
        Result.all(QuirkProfile.all.map { profile =>
          TerminalModel.interpret(profile, Screen.blank(profile, area.size), out1) match {
            case Right(first) =>
              TerminalModel.interpret(profile, first, out2) match {
                case Right(second) =>
                  Result.all(
                    List(
                      sameBuffer(first.buffer, TerminalModel.expected(profile, prev)),
                      sameBuffer(second.buffer, TerminalModel.expected(profile, next)),
                    )
                  )
                case Left(error) => failed(profile, error, out2)
              }
            case Left(error) => failed(profile, error, out1)
          }
        })
    }
  }

}
