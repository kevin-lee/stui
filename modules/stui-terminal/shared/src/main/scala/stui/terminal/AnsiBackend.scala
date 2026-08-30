package stui.terminal

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, CellUpdate}
import stui.core.capability.Capabilities
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.spi.{PrintEffect, ScreenMode, TerminalBackend, TerminalError, TerminalOptions}
import stui.terminal.ansi.{AnsiWriter, InlineStrategy, ScrollRegion, Sequences, WriterState}
import stui.unicode.WidthPolicy

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

/** The shared ANSI backend (design doc 7.1 and 7.2): a [[stui.core.spi.TerminalBackend]] over a platform [[Tty]] that queues the
  * writer's output until `flush`. On the alternate screen the viewport is the whole terminal. In inline mode the viewport is computed
  * at entry from the cursor row the probe measured (design doc 7.2: the effective height is clamped to the terminal, the entry
  * scrolls only as far as needed, and without a cursor row the entry scrolls a full screen and anchors at the bottom, plan
  * refinement R7 of M1f), reconciled on every terminal size change (the origin re-clamped and the region re-armed through the
  * DECSC / DECRC bracket), and printed rows go through the scroll-region strategy when it is armed and the overlay otherwise.
  * `flush` wraps the pending output in the DEC 2026 bracket when the capabilities say so (writer rule R6 at flush granularity, plan
  * refinement R9). `enter` writes the entry sequence immediately after raw mode succeeds, `exit` discards anything not yet flushed,
  * writes the mode's fixed reset, and restores the terminal mode, once. The size is the device's last non-zero report, 80 x 24
  * before any report.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class AnsiBackend private (
  private val tty: Tty,
  val capabilities: Capabilities,
  val policy: WidthPolicy,
  private val entryRow: Option[NonNegInt],
  private val ref: AtomicReference[AnsiBackend.State],
) extends TerminalBackend {

  import AnsiBackend.InlineGeometry

  /* the methods live in the class body because they implement the TerminalBackend trait members */

  /** The device's size, the last non-zero report when the query fails. */
  override def size(): Size = tty.size() match {
    case Some(size) => ref.updateAndGet(_.copy(lastSize = size)).lastSize
    case None => ref.get().lastSize
  }

  /** The whole terminal at the origin on the alternate screen; in inline mode the viewport, reconciled first when the terminal size
    * changed since it was computed (the effective height re-clamped, the origin re-clamped so the viewport fits, and the region
    * re-armed or reset through the DECSC / DECRC bracket).
    */
  override def viewport(): Rect = {
    val current = size()
    ref
      .updateAndGet { state =>
        state.inline match {
          case Some(geometry) if geometry.terminal =!= current => reconcile(state, geometry, current)
          case Some(_) | None => state
        }
      }
      .inline
      .fold(Rect.sized(current))(_.viewport)
  }

  private def reconcile(state: AnsiBackend.State, geometry: InlineGeometry, current: Size): AnsiBackend.State = {
    val requested = state.mode match {
      case Some(ScreenMode.Inline(height)) => height.value
      case Some(ScreenMode.AlternateScreen) | None => geometry.viewport.height.value
    }
    val termH     = current.height.value
    val h         = math.max(1, math.min(requested, termH))
    val o         = math.max(0, math.min(geometry.viewport.y.value, termH - h))
    val viewport  = Rect(NonNegInt(0), NonNegInts.clamp(o.toLong), current.width, NonNegInts.clamp(h.toLong))
    val armed     =
      if (InlineStrategy.of(capabilities) === InlineStrategy.ScrollRegion && o >= 2) {
        ScrollRegion(NonNegInt(0), NonNegInts.clamp(o.toLong - 1L)).some
      } else {
        none[ScrollRegion]
      }
    val output    = armed match {
      case Some(region) => Sequences.armRegion(region.top.value + 1, region.bottom.value + 1)
      case None => if (state.writer.region.isDefined) Sequences.resetRegion else ""
    }
    state.copy(
      writer = state.writer.copy(region = armed),
      pending = if (output.isEmpty) state.pending else state.pending :+ output,
      inline = InlineGeometry(viewport, current).some,
    )
  }

  /** Queues the writer's output for the updates over the current viewport. */
  override def draw(updates: Vector[CellUpdate]): Unit = {
    val target = viewport()
    ref.updateAndGet { state =>
      AnsiWriter.present(state.writer, capabilities, target, updates) match {
        case (writer, output) => state.copy(writer = writer, pending = state.pending :+ output)
      }
    }: Unit
  }

  /** Writes the queued output as UTF-8, inside the DEC 2026 bracket when the capabilities support it (plan refinement R9), and
    * returns the byte count, the bracket included.
    */
  override def flush(): NonNegInt = {
    val state  = ref.getAndUpdate(_.copy(pending = Vector.empty[String]))
    val output = state.pending.mkString
    if (output.isEmpty) {
      NonNegInt(0)
    } else {
      val text  = if (capabilities.syncOutput) Sequences.SyncBegin + output + Sequences.SyncEnd else output
      val bytes = text.getBytes(StandardCharsets.UTF_8)
      tty.write(bytes)
      NonNegInts.clamp(bytes.length.toLong)
    }
  }

  /** Queues a cursor move clamped into the viewport (nothing when the tracked cursor is already there). */
  override def moveCursor(position: Position): Unit = {
    val target = viewport()
    queue(state => AnsiWriter.moveCursor(state, target, position))
  }

  /** Queues DECTCEM set. */
  override def showCursor(): Unit = queue(AnsiWriter.showCursor)

  /** Queues DECTCEM reset. */
  override def hideCursor(): Unit = queue(AnsiWriter.hideCursor)

  /** Queues Erase in Display and home on the alternate screen, a viewport erase in inline mode (never a full-screen clear there). */
  override def clear(): Unit = ref.get().mode match {
    case Some(ScreenMode.Inline(_)) =>
      val target = viewport()
      queue(state => AnsiWriter.clearViewport(state, target))
    case Some(ScreenMode.AlternateScreen) | None => queue(AnsiWriter.clearAll)
  }

  /** Inline mode: the scroll-region print when a region is armed (the viewport kept), the overlay print otherwise (the viewport
    * moved and lost). Any other mode: the rows as styled lines at the cursor (the transcript flush), the viewport kept.
    */
  override def print(rows: Buffer): PrintEffect = {
    val state = ref.get()
    state.mode match {
      case Some(ScreenMode.Inline(_)) if state.entered => inlinePrint(rows)
      case Some(_) | None =>
        queue(writer => AnsiWriter.print(writer, capabilities, rows))
        PrintEffect.ViewportKept
    }
  }

  private def inlinePrint(rows: Buffer): PrintEffect = {
    val target = viewport()
    val width  = ref.get().lastSize.width.value
    ref.get().writer.region match {
      case Some(region) =>
        queue(writer => AnsiWriter.printRegion(writer, capabilities, region, width, rows))
        PrintEffect.ViewportKept
      case None =>
        ref.updateAndGet { state =>
          val geometry = state.inline.getOrElse(InlineGeometry(target, state.lastSize))
          AnsiWriter.printOverlay(state.writer, capabilities, geometry.viewport, geometry.terminal, rows) match {
            case (writer, moved, output) =>
              state.copy(
                writer = writer,
                pending = if (output.isEmpty) state.pending else state.pending :+ output,
                inline = geometry.copy(viewport = moved).some,
              )
          }
        }: Unit
        PrintEffect.ViewportLost
    }
  }

  /** `Left` for a non-terminal or a raw-mode failure, otherwise raw mode (idempotent) and the mode's entry sequence written
    * immediately: the alternate screen with an explicit clear, or the inline entry computed from the probed cursor row (design doc
    * 7.2).
    */
  override def enter(options: TerminalOptions): Either[TerminalError, Unit] =
    if (!tty.isTerminal) {
      (TerminalError.NotATerminal: TerminalError).asLeft[Unit]
    } else {
      options.screenMode match {
        case ScreenMode.AlternateScreen =>
          tty.enterRawMode().map { _ =>
            AnsiWriter.enter(options) match {
              case (writer, output) =>
                tty.write(output.getBytes(StandardCharsets.UTF_8))
                ref.updateAndGet(
                  _.copy(
                    writer = writer,
                    pending = Vector.empty[String],
                    entered = true,
                    mode = (ScreenMode.AlternateScreen: ScreenMode).some,
                    inline = none[InlineGeometry],
                  )
                ): Unit
            }
          }
        case mode @ ScreenMode.Inline(height) =>
          tty.enterRawMode().map { _ =>
            val term     = size()
            val termH    = term.height.value
            val h        = math.max(1, math.min(height.value, termH))
            val (pad, o) = entryRow match {
              case Some(row) =>
                val r = math.min(row.value, math.max(0, termH - 1))
                (if (r + h > termH) h - 1 else 0, math.min(r, termH - h))
              case None => (math.max(0, termH - 1), termH - h)
            }
            val region   =
              if (InlineStrategy.of(capabilities) === InlineStrategy.ScrollRegion && o >= 2) {
                ScrollRegion(NonNegInt(0), NonNegInts.clamp(o.toLong - 1L)).some
              } else {
                none[ScrollRegion]
              }
            AnsiWriter.enterInline(options, pad, region) match {
              case (writer, output) =>
                tty.write(output.getBytes(StandardCharsets.UTF_8))
                ref.updateAndGet(
                  _.copy(
                    writer = writer,
                    pending = Vector.empty[String],
                    entered = true,
                    mode = (mode: ScreenMode).some,
                    inline = InlineGeometry(
                      Rect(NonNegInt(0), NonNegInts.clamp(o.toLong), term.width, NonNegInts.clamp(h.toLong)),
                      term,
                    ).some,
                  )
                ): Unit
            }
          }
      }
    }

  /** Discards queued output, writes the mode's fixed reset (the safe reset on the alternate screen, the inline exit with the region
    * reset and the parked cursor in inline mode), and restores the terminal mode, only when entered.
    */
  override def exit(): Unit = {
    val state = ref.getAndUpdate(
      _.copy(
        writer = WriterState.initial,
        pending = Vector.empty[String],
        entered = false,
        mode = none[ScreenMode],
        inline = none[InlineGeometry],
      )
    )
    if (state.entered) {
      val reset = state.mode match {
        case Some(ScreenMode.Inline(_)) => AnsiWriter.exitInline(state.inline.fold(Rect.sized(state.lastSize))(_.viewport))
        case Some(ScreenMode.AlternateScreen) | None => AnsiWriter.exit
      }
      tty.write(reset.getBytes(StandardCharsets.UTF_8))
      tty.restoreMode()
    } else {
      ()
    }
  }

  private def queue(f: WriterState => (WriterState, String)): Unit =
    ref.updateAndGet { state =>
      f(state.writer) match {
        case (writer, output) => state.copy(writer = writer, pending = state.pending :+ output)
      }
    }: Unit

}

object AnsiBackend {

  /** The inline viewport in screen coordinates and the terminal size it was computed for (design doc 7.2). */
  final case class InlineGeometry(viewport: Rect, terminal: Size)

  /** The backend's state: the writer state, the output queued since the last flush, the last non-zero size, whether `enter`
    * succeeded, the entered screen mode, and the inline geometry.
    */
  final case class State(
    writer: WriterState,
    pending: Vector[String],
    lastSize: Size,
    entered: Boolean,
    mode: Option[ScreenMode],
    inline: Option[InlineGeometry],
  )

  /** The size assumed before the device reports one. */
  val fallbackSize: Size = Size(NonNegInt(80), NonNegInt(24))

  /** A backend over the device with [[WidthPolicy.default]] and no probed entry row. */
  def apply(tty: Tty, capabilities: Capabilities): AnsiBackend =
    withPolicyAndEntryRow(WidthPolicy.default, tty, capabilities, none[NonNegInt])

  /** A backend over the device with the given policy and no probed entry row. */
  def withPolicy(policy: WidthPolicy, tty: Tty, capabilities: Capabilities): AnsiBackend =
    withPolicyAndEntryRow(policy, tty, capabilities, none[NonNegInt])

  /** A backend over the device with [[WidthPolicy.default]] and the cursor row the probe measured (the inline entry anchor). */
  def withEntryRow(tty: Tty, capabilities: Capabilities, entryRow: Option[NonNegInt]): AnsiBackend =
    withPolicyAndEntryRow(WidthPolicy.default, tty, capabilities, entryRow)

  /** A backend over the device with the given policy and the cursor row the probe measured. */
  def withPolicyAndEntryRow(policy: WidthPolicy, tty: Tty, capabilities: Capabilities, entryRow: Option[NonNegInt]): AnsiBackend =
    new AnsiBackend(
      tty,
      capabilities,
      policy,
      entryRow,
      new AtomicReference(
        State(WriterState.initial, Vector.empty[String], fallbackSize, false, none[ScreenMode], none[InlineGeometry])
      ),
    )

  extension (backend: AnsiBackend) {

    /** The output queued since the last flush, for tests. */
    def pendingOutput: String = backend.ref.get().pending.mkString

    /** The writer state, for tests. */
    def writerState: WriterState = backend.ref.get().writer

    /** The inline geometry, for tests, `None` on the alternate screen. */
    def inlineGeometry: Option[InlineGeometry] = backend.ref.get().inline

  }

}
