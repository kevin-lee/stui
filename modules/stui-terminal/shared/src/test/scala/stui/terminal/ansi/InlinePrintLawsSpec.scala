package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Cell}
import stui.core.capability.Capabilities
import stui.core.geometry.{Rect, Size}
import stui.core.style.CellStyle
import stui.testkit.{Assertions, TerminalModel}
import stui.testkit.TerminalModel.{QuirkProfile, Screen}
import stui.testkit.gen.{CapabilityGens, GeometryGens}

/** The inline print strategies as laws over the oracle (design doc 7.2 and 12): the region print keeps the viewport and scrolls the
  * overflow into scrollback, the overlay print erases the viewport, stacks the rows above it, and scrolls the excess, a full present
  * after an overlay shows the frame at the moved viewport, the print byte budget, sanitisation, and determinism.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object InlinePrintLawsSpec extends Properties {

  final private case class PrintCase(
    profile: QuirkProfile,
    caps: Capabilities,
    terminal: Size,
    viewport: Rect,
    prev: Buffer,
    printed: Buffer,
    frame: Buffer,
  )

  private def printCaseOver(shape: Gen[(Size, Rect)], printedAt: Rect => Gen[Buffer]): Gen[PrintCase] =
    for {
      profile <- WriterGens.profile
      caps    <- CapabilityGens.capabilities
      tv      <- shape
      prev    <- WriterGens.bufferAt(Rect.sized(tv._1), Range.linear(0, 8))
      height  <- Gen.int(Range.linear(1, 4))
      printed <- printedAt(Rect.sized(Size(tv._1.width, GeometryGens.nonNegOrZero(height.toLong))))
      moved = movedViewport(tv._2, tv._1, height)
      frame <- WriterGens.bufferAt(moved, Range.linear(0, 6))
    } yield PrintCase(profile, caps, tv._1, tv._2, prev, printed, frame)

  private def movedViewport(viewport: Rect, terminal: Size, printedRows: Int): Rect = {
    val o2 = math.min(viewport.y.value + printedRows, terminal.height.value - viewport.height.value)
    Rect(NonNegInt(0), GeometryGens.nonNegOrZero(o2.toLong), terminal.width, viewport.height)
  }

  private val randomPrinted: Rect => Gen[Buffer] = area => WriterGens.bufferAt(area, Range.linear(0, 6))

  private val regionCase: Gen[PrintCase] = printCaseOver(WriterGens.terminalViewportWithRegion, randomPrinted)

  private val overlayCase: Gen[PrintCase] = printCaseOver(WriterGens.terminalViewport, randomPrinted)

  /* the same shapes with VS16-heavy printed rows (rule R3a, issue 42) */
  private val regionVs16Case: Gen[PrintCase] = printCaseOver(WriterGens.terminalViewportWithRegion, WriterGens.vs16BufferAt)

  private val overlayVs16Case: Gen[PrintCase] = printCaseOver(WriterGens.terminalViewport, WriterGens.vs16BufferAt)

  private def normaliseWith(caps: Capabilities): CellStyle => CellStyle = style => Sgr.normalise(caps, style)

  private def blankRow(width: Int): Vector[Cell] = Vector.fill(width)(Cell.blank)

  private def esc(output: String): String = output.replace(Sequences.Esc, "ESC")

  private def isSanitised(output: String): Boolean =
    output.forall { c =>
      val code    = c.toInt
      val control = code < 0x20 && code =!= 0x1b && code =!= 0x0d && code =!= 0x0a
      !control && code =!= 0x7f && !(code >= 0x80 && code <= 0x9f)
    }

  override def tests: List[Test] = List(
    property("the region print keeps the viewport and scrolls the overflow into scrollback", testRegion),
    property("the overlay print erases the viewport, stacks the rows above it, and scrolls the excess", testOverlay),
    property("a VS16-heavy region print keeps the viewport and scrolls the overflow into scrollback (R3a)", testRegionVs16),
    property(
      "a VS16-heavy overlay print erases the viewport, stacks the rows above it, and scrolls the excess (R3a)",
      testOverlayVs16,
    ),
    property("a full present after an overlay print shows the frame at the moved viewport", testOverlayThenPresent),
    property("the print byte budget holds (R10)", testBudget),
    property("the print outputs are sanitised and deterministic", testSanitised),
  )

  def testRegion: Property = regionLaw(regionCase)

  def testRegionVs16: Property = regionLaw(regionVs16Case)

  def testOverlay: Property = overlayLaw(overlayCase)

  def testOverlayVs16: Property = overlayLaw(overlayVs16Case)

  private def regionLaw(cases: Gen[PrintCase]): Property =
    cases.forAll.map { kase =>
      val o         = kase.viewport.y.value
      val region    = ScrollRegion(NonNegInt(0), NonNegInt.unsafeFrom(o - 1))
      val normalise = normaliseWith(kase.caps)
      val screen0   = Screen
        .ofTerminal(kase.profile, normalise, kase.terminal, kase.prev)
        .copy(region = TerminalModel.Region(NonNegInt(0), NonNegInt.unsafeFrom(o - 1)).some)
      AnsiWriter.printRegion(
        WriterState.initial.copy(region = region.some),
        kase.caps,
        region,
        kase.terminal.width.value,
        kase.printed,
      ) match {
        case (state, output) =>
          TerminalModel.interpret(kase.profile, screen0, output) match {
            case Right(screen1) =>
              val prevRows  = TerminalModel.placed(kase.profile, normalise, kase.terminal, kase.prev).rows
              val printRows = TerminalModel.expectedWith(kase.profile, normalise, kase.printed).rows
              val stacked   = prevRows.take(o) ++ printRows
              Result.all(
                List(
                  Assertions.eqv(screen1.buffer.rows.take(o), stacked.takeRight(o)),
                  Assertions.eqv(screen1.buffer.rows.drop(o), prevRows.drop(o)),
                  Assertions.eqv(screen1.scrollback, stacked.dropRight(o)),
                  Assertions.eqv(state.region, region.some),
                )
              )
            case Left(error) => Result.failure.log(s"${error.show} for ${esc(output)}")
          }
      }
    }

  private def overlayLaw(cases: Gen[PrintCase]): Property =
    cases.forAll.map { kase =>
      val o         = kase.viewport.y.value
      val h         = kase.viewport.height.value
      val termH     = kase.terminal.height.value
      val k         = kase.printed.area.height.value
      val normalise = normaliseWith(kase.caps)
      val screen0   = Screen.ofTerminal(kase.profile, normalise, kase.terminal, kase.prev)
      AnsiWriter.printOverlay(WriterState.initial, kase.caps, kase.viewport, kase.terminal, kase.printed) match {
        case (_, moved, output) =>
          TerminalModel.interpret(kase.profile, screen0, output) match {
            case Right(screen1) =>
              val o2        = moved.y.value
              val kk        = math.min(k, o2)
              val sTot      = o + k - o2
              val printRows = TerminalModel.expectedWith(kase.profile, normalise, kase.printed).rows
              val prevRows  = TerminalModel.placed(kase.profile, normalise, kase.terminal, kase.prev).rows
              val width     = kase.terminal.width.value
              Result.all(
                List(
                  Assertions.eqv(o2, math.min(o + k, termH - h)),
                  Assertions.eqv(screen1.buffer.rows.slice(o2, o2 + h), Vector.fill(h)(blankRow(width))),
                  Assertions.eqv(screen1.buffer.rows.slice(o2 + h, termH), Vector.fill(math.max(0, termH - o2 - h))(blankRow(width))),
                  Assertions.eqv(screen1.buffer.rows.slice(o2 - kk, o2), printRows.takeRight(kk)),
                  Assertions.eqv(screen1.buffer.rows.take(o2 - kk), prevRows.slice(sTot, sTot + math.max(0, o2 - kk))),
                  Assertions.eqv(screen1.scrollback.length, sTot),
                )
              )
            case Left(error) => Result.failure.log(s"${error.show} for ${esc(output)}")
          }
      }
    }

  def testOverlayThenPresent: Property =
    overlayCase.forAll.map { kase =>
      val normalise = normaliseWith(kase.caps)
      val screen0   = Screen.ofTerminal(kase.profile, normalise, kase.terminal, kase.prev)
      AnsiWriter.printOverlay(WriterState.initial, kase.caps, kase.viewport, kase.terminal, kase.printed) match {
        case (state1, moved, out1) =>
          AnsiWriter.present(state1, kase.caps, moved, Buffer.allUpdates(kase.frame)) match {
            case (_, out2) =>
              TerminalModel.interpret(kase.profile, screen0, out1 + out2) match {
                case Right(screen) =>
                  val h        = moved.height.value
                  val o2       = moved.y.value
                  val expected = TerminalModel.placed(kase.profile, normalise, kase.terminal, kase.frame)
                  Assertions.eqv(screen.buffer.rows.slice(o2, o2 + h), expected.rows.slice(o2, o2 + h))
                case Left(error) => Result.failure.log(s"${error.show} for ${esc(out1 + out2)}")
              }
          }
      }
    }

  def testBudget: Property =
    regionCase.forAll.map { kase =>
      val o      = kase.viewport.y.value
      val region = ScrollRegion(NonNegInt(0), NonNegInt.unsafeFrom(o - 1))
      val rows   = kase.printed.area.height.value
      val cells  = rows * kase.terminal.width.value
      AnsiWriter.printRegion(
        WriterState.initial.copy(region = region.some),
        kase.caps,
        region,
        kase.terminal.width.value,
        kase.printed,
      ) match {
        case (_, output) =>
          val bytes  = AnsiWriter.utf8Length(output)
          val budget = AnsiWriter.MaxBytesPerCell.toLong * cells.toLong + AnsiWriter.bufferSymbolBytes(kase.printed) +
            AnsiWriter.MaxBytesPerPrintedRow.toLong * rows.toLong + AnsiWriter.MaxBytesPerPrint.toLong
          Result.assert(bytes <= budget).log(s"bytes ${bytes.toString} > budget ${budget.toString}")
      }
    }

  def testSanitised: Property =
    overlayCase.forAll.map { kase =>
      val o          = math.max(2, kase.viewport.y.value)
      val region     = ScrollRegion(NonNegInt(0), NonNegInt.unsafeFrom(o - 1))
      val first      = AnsiWriter.printOverlay(WriterState.initial, kase.caps, kase.viewport, kase.terminal, kase.printed)
      val second     = AnsiWriter.printOverlay(WriterState.initial, kase.caps, kase.viewport, kase.terminal, kase.printed)
      val fromRegion =
        AnsiWriter.printRegion(WriterState.initial.copy(region = region.some), kase.caps, region, kase.terminal.width.value, kase.printed)
      Result.all(
        List(
          Result.assert(isSanitised(first._3)).log(esc(first._3)),
          Result.assert(isSanitised(fromRegion._2)).log(esc(fromRegion._2)),
          Assertions.eqv(first._3, second._3),
          Assertions.eqv(first._2, second._2),
        )
      )
    }

}
