package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.buffer.{Buffer, Cell, GlyphWidth}
import stui.core.capability.Capabilities
import stui.core.style.CellStyle
import stui.testkit.{Assertions, TerminalModel}
import stui.testkit.TerminalModel.{QuirkProfile, Screen}
import stui.testkit.gen.CapabilityGens
import stui.unicode.Graphemes
import stui.unicode.internal.IntOps.*

/** The writer's rules as laws over the oracle (design doc 7.1 and 12): the round trip under random capabilities (the expected screen
  * carries the writer's own style normalisation), the sanitisation law, R1 (or the unknown cursor after a closing VS16 cluster), one
  * move per row plus the R2a join breaks and the R3a placements, R8, R10, determinism, the style reset, and orphan continuations. The
  * laws run under every quirk profile with capabilities drawn independently of it (issue 42), so a writer that trusted a capability
  * about the terminal's VS16 width would fail them.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object AnsiWriterLawsSpec extends Properties {

  private val cupPattern = (Sequences.Esc + "\\[[0-9;]*H").r

  /** `Assertions.eqv` on buffers logs only the grids, so a style-only mismatch is logged cell by cell here. */
  private def sameBuffer(actual: Buffer, expected: Buffer): Result =
    if (actual === expected) Result.success
    else Result.failure.log(s"--- actual cells ---\n${actual.cells.toVector.show}\n--- expected cells ---\n${expected.cells.toVector.show}")

  private def isSanitised(output: String, allowCrLf: Boolean): Boolean =
    output.forall { c =>
      val code    = c.toInt
      val control = code < 0x20 && code =!= 0x1b && !(allowCrLf && (code === 0x0d || code === 0x0a))
      !control && code =!= 0x7f && !(code >= 0x80 && code <= 0x9f)
    }

  override def tests: List[Test] = List(
    property("a present from the previous screen shows the next buffer", testRoundTrip),
    property("two chained presents from a blank screen show the next buffer", testChained),
    property("the style is the default after a present, on both sides", testStyleReset),
    property("a full row leaves a pending wrap, or an unknown cursor after a closing VS16 cluster (R1, R3a)", testPendingWrap),
    property(
      "a full row from a blank screen moves the cursor once plus the join breaks and the VS16 placements (R2a, R3a)",
      testOneMove,
    ),
    property("updates outside the viewport emit nothing (R8)", testOutside),
    property("the byte budget holds (R10)", testBudget),
    property("present is deterministic", testDeterministic),
    property("the output is sanitised", testSanitised),
    property("print output is sanitised", testPrintSanitised),
    property("orphan continuations become default blanks (R2)", testOrphans),
    property("a present at a viewport shows the next buffer there and touches nothing else", testViewportRoundTrip),
  )

  def testViewportRoundTrip: Property =
    for {
      profile <- WriterGens.profile.forAll
      caps    <- CapabilityGens.capabilities.forAll
      tv      <- WriterGens.terminalViewport.forAll
      pair    <- WriterGens.pairAt(tv._2).forAll
    } yield tv match {
      case (terminal, viewport) =>
        pair match {
          case (prev, next) =>
            val normalise = (style: CellStyle) => Sgr.normalise(caps, style)
            AnsiWriter.present(WriterState.initial, caps, viewport, Buffer.diff(prev, next)) match {
              case (_, output) =>
                TerminalModel.interpret(profile, Screen.ofTerminal(profile, normalise, terminal, prev), output) match {
                  case Right(screen) => sameBuffer(screen.buffer, TerminalModel.placed(profile, normalise, terminal, next))
                  case Left(error) => Result.failure.log(s"${error.show} for ${output.replace(Sequences.Esc, "ESC")}")
                }
            }
        }
    }

  def testRoundTrip: Property =
    for {
      profile <- WriterGens.profile.forAll
      caps    <- CapabilityGens.capabilities.forAll
      pair    <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        val normalise = (style: CellStyle) => Sgr.normalise(caps, style)
        AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.diff(prev, next)) match {
          case (_, output) =>
            TerminalModel.interpret(profile, Screen.ofWith(profile, normalise, prev), output) match {
              case Right(screen) => sameBuffer(screen.buffer, TerminalModel.expectedWith(profile, normalise, next))
              case Left(error) => Result.failure.log(s"${error.show} for ${output.replace(Sequences.Esc, "ESC")}")
            }
        }
    }

  def testChained: Property =
    for {
      profile <- WriterGens.profile.forAll
      caps    <- CapabilityGens.capabilities.forAll
      pair    <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        val normalise = (style: CellStyle) => Sgr.normalise(caps, style)
        AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.allUpdates(prev)) match {
          case (state1, out1) =>
            AnsiWriter.present(state1, caps, prev.area, Buffer.diff(prev, next)) match {
              case (_, out2) =>
                TerminalModel.interpret(profile, Screen.blank(profile, prev.area.size), out1 + out2) match {
                  case Right(screen) => sameBuffer(screen.buffer, TerminalModel.expectedWith(profile, normalise, next))
                  case Left(error) => Result.failure.log(error.show)
                }
            }
        }
    }

  def testStyleReset: Property =
    for {
      profile <- WriterGens.profile.forAll
      caps    <- CapabilityGens.capabilities.forAll
      pair    <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.diff(prev, next)) match {
          case (state, output) =>
            TerminalModel.interpret(profile, Screen.ofWith(profile, style => Sgr.normalise(caps, style), prev), output) match {
              case Right(screen) =>
                Result.all(List(Assertions.eqv(state.style, CellStyle.default), Assertions.eqv(screen.style, CellStyle.default)))
              case Left(error) => Result.failure.log(error.show)
            }
        }
    }

  /* a full row ends in a pending wrap (R1), unless its last two cells are a VS16 cluster, which R3a leaves with an unknown cursor */
  def testPendingWrap: Property =
    WriterGens.rowBuffer.forAll.map { buffer =>
      val closingVs16 = buffer.rows.headOption.flatMap(_.lift(buffer.area.width.value - 2)).exists {
        case Cell.Glyph(symbol, GlyphWidth.Two, _) => symbol.isVs16Sequence
        case Cell.Glyph(_, _, _) | Cell.Continuation(_) => false
      }
      val expected    = if (closingVs16) CursorState.Unknown else CursorState.PendingWrap(buffer.area.y)
      AnsiWriter.present(WriterState.initial, Capabilities.lossless, buffer.area, Buffer.allUpdates(buffer)) match {
        case (state, _) => Assertions.eqv(state.cursor, expected)
      }
    }

  /* the CUPs of a full row from a blank screen: the first cell is placed (the cursor starts unknown), rule R2a places a glyph that
   * would join the previously emitted cluster (a lone regional indicator after another, a standalone mark after any glyph), and rule
   * R3a gives a VS16 cluster a CUP back after its blanking spaces (whose first space is the join candidate) and leaves the cursor
   * unknown, so the next cell is placed too */
  private def expectedMoves(buffer: Buffer): Int =
    buffer
      .rows
      .headOption
      .fold(0) { row =>
        row
          .foldLeft((0, none[String], true)) {
            case ((moves, last, placed), Cell.Glyph(symbol, GlyphWidth.Two, _)) if symbol.isVs16Sequence =>
              val start = if (placed || last.exists(previous => Graphemes.joins(previous, " "))) 1 else 0
              (moves + start + 1, none[String], true)
            case ((moves, last, placed), Cell.Glyph(symbol, _, _)) =>
              val start = if (placed || last.exists(previous => Graphemes.joins(previous, symbol.value))) 1 else 0
              (moves + start, symbol.value.some, false)
            case ((moves, last, placed), Cell.Continuation(_)) => (moves, last, placed)
          }
          ._1
      }

  def testOneMove: Property =
    WriterGens.rowBuffer.forAll.map { buffer =>
      val expected = expectedMoves(buffer)
      AnsiWriter.present(WriterState.initial, Capabilities.lossless, buffer.area, Buffer.allUpdates(buffer)) match {
        case (_, output) =>
          Result.assert(cupPattern.findAllIn(output).length === expected).log(output.replace(Sequences.Esc, "ESC"))
      }
    }

  def testOutside: Property =
    for {
      area    <- WriterGens.originArea.forAll
      updates <- WriterGens.updatesOutside(area).forAll
    } yield AnsiWriter.present(WriterState.initial, Capabilities.lossless, area, updates) match {
      case (state, output) => Result.all(List(Assertions.eqv(output, ""), Assertions.eqv(state, WriterState.initial)))
    }

  def testBudget: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        val updates = Buffer.diff(prev, next)
        AnsiWriter.present(WriterState.initial, caps, prev.area, updates) match {
          case (_, output) =>
            val bytes  = AnsiWriter.utf8Length(output)
            val budget = AnsiWriter.MaxBytesPerCell.toLong * updates.length.toLong + AnsiWriter
              .symbolBytes(updates) + AnsiWriter.MaxBytesPerPresent.toLong
            Result.assert(bytes <= budget).log(s"bytes $bytes > budget $budget")
        }
    }

  def testDeterministic: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        val updates = Buffer.diff(prev, next)
        Assertions.eqv(
          AnsiWriter.present(WriterState.initial, caps, prev.area, updates),
          AnsiWriter.present(WriterState.initial, caps, prev.area, updates),
        )
    }

  def testSanitised: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, next) =>
        AnsiWriter.present(WriterState.initial, caps, prev.area, Buffer.diff(prev, next)) match {
          case (_, output) => Result.assert(isSanitised(output, false)).log(output.map(c => c.toInt.toHexString).mkString(" "))
        }
    }

  def testPrintSanitised: Property =
    for {
      caps <- CapabilityGens.capabilities.forAll
      pair <- WriterGens.bufferPair.forAll
    } yield pair match {
      case (prev, _) =>
        AnsiWriter.print(WriterState.initial, caps, prev) match {
          case (state, output) =>
            Result.all(List(Result.assert(isSanitised(output, true)), Assertions.eqv(state.cursor, CursorState.Unknown)))
        }
    }

  def testOrphans: Property =
    WriterGens.orphanUpdates.forAll.map {
      case (area, updates) =>
        val profile = QuirkProfile.default
        AnsiWriter.present(WriterState.initial, Capabilities.lossless, area, updates) match {
          case (_, output) =>
            TerminalModel.interpret(profile, Screen.blank(profile, area.size), output) match {
              case Right(screen) =>
                Result.all(updates.toList.map(update => Assertions.eqv(screen.buffer.cell(update.position), Cell.blank.some)))
              case Left(error) => Result.failure.log(error.show)
            }
        }
    }

}
