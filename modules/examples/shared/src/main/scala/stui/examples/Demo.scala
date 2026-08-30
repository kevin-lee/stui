package stui.examples

import cats.syntax.all.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Canvas
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier, MouseEvent, MouseEventKind}
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.layout.{Constraint, Layout}
import stui.core.spi.ScreenMode
import stui.core.style.{Color, Style, UnderlineStyle}
import stui.core.terminal.{CompletedFrame, RedrawReason, RenderStats}
import stui.core.text.{Line, Span, Text}
import stui.terminal.TerminalSession
import stui.terminal.TerminalSession.*
import stui.widgets.{Block, BorderSet, LogRing, LogView, LogViewState, Paragraph, Scroll, ScrollView, Scrolling, Wrap}
import stui.widgets.LogRing.*
import stui.widgets.LogView.*
import stui.widgets.ScrollView.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The stui demo (M2a): a titled header, a scrollable body of long wrapped Korean, Japanese, and emoji text (a [[ScrollView]] whose
  * content size is computed per frame, the immediate-mode sizing pattern), an event log pane (a [[LogView]] following its tail,
  * alternate screen only), and a status footer. Borders come from the capabilities ([[BorderSet.forCapabilities]]). The wheel
  * scrolls the pane under the mouse (hit-region routing), Tab moves the key focus, Up / Down / PageUp / PageDown / Home / End
  * scroll the focused pane, `q` or Control-C quits, `r` forces a redraw, `p` and `P` print above the UI, and a paste is shown.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Demo {

  /** Which pane the scroll keys drive. */
  enum Pane {
    case Body
    case Log
  }

  /** What the loop remembers between frames. */
  final case class State(
    lastEvent: Option[Event],
    hovered: Option[RegionId],
    stats: Option[RenderStats],
    frames: Int,
    paste: Option[String],
    focused: Boolean,
    printed: Int,
    scroll: Scroll,
    log: LogRing,
    logState: LogViewState,
    pane: Pane,
  )

  object State {

    /** Nothing seen yet: no offset, an empty 200-line event log following its tail, the body focused. */
    val initial: State =
      State(
        none[Event],
        none[RegionId],
        none[RenderStats],
        0,
        none[String],
        true,
        0,
        Scroll.none,
        LogRing.empty(PosInt(200)),
        LogViewState.following,
        Pane.Body,
      )

  }

  /** The body pane's hit region. */
  val bodyRegion: RegionId = RegionId("body")

  /** The log pane's hit region. */
  val logRegion: RegionId = RegionId("log")

  /** The status pane's hit region. */
  val statusRegion: RegionId = RegionId("status")

  /** Rows the wheel scrolls per event (the app's documented choice, design doc 6.6). */
  private val WheelStep: Int = 3

  private def cps(codePoints: Int*): String =
    codePoints.foldLeft(new java.lang.StringBuilder)((builder, cp) => builder.appendCodePoint(cp)).toString

  private val keyboardVs16: String = cps(0x2328, 0xfe0f)

  private def gradient(text: String): Vector[Span] =
    text.toVector.zipWithIndex.map {
      case (c, i) =>
        val t = if (text.length <= 1) 0 else (255 * i) / (text.length - 1)
        Span.styled(c.toString, Style.empty.withFg(Color.rgbFrom(255 - t, 80 + t / 2, t).getOrElse(Color.Reset)))
    }

  private val bodyText: Text = Text.of(
    Line.raw("한국어: 다람쥐 헌 쳇바퀴에 타고파. 넓은 글자는 터미널에서 두 칸을 차지하고, 줄 바꿈은 글자 경계에서만 일어납니다."),
    Line.raw("日本語: いろはにほへと ちりぬるを わかよたれそ つねならむ うゐのおくやま けふこえて あさきゆめみし ゑひもせす。"),
    Line.of(
      Span.raw("Emoji: 👋 🎉 🇰🇷 🇯🇵 "),
      Span.raw(keyboardVs16),
      Span.raw(" (a VS16 sequence, drawn two columns wide by the tables) "),
      Span.styled("bold", Style.empty.bold),
      Span.raw(" "),
      Span.styled("italic", Style.empty.italic),
      Span.raw(" "),
      Span.styled("reversed", Style.empty.reversed),
      Span.raw(" "),
      Span.styled("crossed", Style.empty.crossedOut),
    ),
    Line.fromSpans(Span.raw("Truecolour: ") +: gradient("a smooth gradient degrades to 256 or 16 colours where needed")),
    Line.of(
      Span.raw("Underlines: "),
      Span.styled("single", Style.empty.withUnderline(UnderlineStyle.Single)),
      Span.raw(" "),
      Span.styled("double", Style.empty.withUnderline(UnderlineStyle.Double)),
      Span.raw(" "),
      Span.styled("curly", Style.empty.withUnderline(UnderlineStyle.Curly).withUnderlineColor(Color.rgb(255, 80, 80))),
      Span.raw(" "),
      Span.styled("dotted", Style.empty.withUnderline(UnderlineStyle.Dotted)),
      Span.raw(" "),
      Span.styled("dashed", Style.empty.withUnderline(UnderlineStyle.Dashed)),
      Span.raw("  256-colour: "),
      Span.styled("indexed 208", Style.empty.withFg(Color.indexed(208))),
    ),
    Line.raw(""),
    Line.raw(
      "Keys: q quits, r redraws, Tab switches the pane, Up/Down/PageUp/PageDown/Home/End scroll it, the wheel scrolls the hovered pane, p and P print above the UI, paste something."
    ),
  )

  /** The body plus forty marker rows, so scrolling is visible (and assertable under a pseudo-terminal driver). */
  private val longBodyText: Text =
    Text.fromLines(
      bodyText.lines ++ Vector.tabulate(40)(i => Line.raw(f"L${i + 1}%04d the quick brown fox jumps over the lazy dog 한글 넓은 글자"))
    )

  /** The pane rects of one frame. The layout laws guarantee one rect per constraint, so the fallback rows are unreachable. */
  final private case class PaneAreas(header: Rect, body: Rect, log: Option[Rect], footer: Rect)

  private def paneAreas(area: Rect, inline: Boolean): PaneAreas =
    if (inline) {
      Layout.vertical(Constraint.length(3), Constraint.fill(1), Constraint.length(3)).split(area) match {
        case Vector(header, body, footer) => PaneAreas(header, body, none[Rect], footer)
        case _ => PaneAreas(area, area, none[Rect], area)
      }
    } else {
      Layout.vertical(Constraint.length(3), Constraint.fill(1), Constraint.length(8), Constraint.length(3)).split(area) match {
        case Vector(header, body, log, footer) => PaneAreas(header, body, log.some, footer)
        case _ => PaneAreas(area, area, none[Rect], area)
      }
    }

  private def isInline(session: TerminalSession): Boolean = session.terminal.options.screenMode match {
    case ScreenMode.AlternateScreen => false
    case ScreenMode.Inline(_) => true
  }

  private def borders(session: TerminalSession): BorderSet = BorderSet.forCapabilities(session.capabilities)

  private def bodyBlock(borders: BorderSet): Block =
    Block
      .bordered
      .withBorderSet(borders)
      .withTitle(Line.raw(" 안녕하세요 · こんにちは · hello 👋 "))
      .withBorderStyle(Style.empty.withFg(Color.Magenta))

  private def logBlock(borders: BorderSet): Block =
    Block.bordered.withBorderSet(borders).withTitle(Line.raw(" events ")).withBorderStyle(Style.empty.withFg(Color.Yellow))

  /** The body pane's inner rows at the current viewport (what PageUp / PageDown step by). */
  private def bodyRows(session: TerminalSession): NonNegInt =
    bodyBlock(borders(session)).inner(paneAreas(session.terminal.viewport, isInline(session)).body).height

  /** The log pane's inner rows at the current viewport. */
  private def logRows(session: TerminalSession): NonNegInt =
    paneAreas(session.terminal.viewport, isInline(session))
      .log
      .fold(NonNegInt(0))(area => logBlock(borders(session)).inner(area).height)

  /** Renders one frame; the corrected scroll states land in `corrections` (the returned-state pattern captured out of the render
    * closure, the `Rendering.statefulWith` shape).
    */
  def view(state: State, session: TerminalSession, corrections: AtomicReference[(Scroll, LogViewState)])(canvas: Canvas): Unit = {
    val set   = borders(session)
    val panes = paneAreas(canvas.area, isInline(session))
    renderHeader(session, set, panes.header, canvas)
    renderBody(state, set, panes.body, canvas, corrections)
    panes.log.foreach(area => renderLog(state, set, area, canvas, corrections))
    renderFooter(state, session, set, panes.footer, canvas)
  }

  private def renderHeader(session: TerminalSession, set: BorderSet, area: Rect, canvas: Canvas): Unit = {
    val block =
      Block.bordered.withBorderSet(set).withTitle(Line.raw(" stui M2a demo ").centered).withBorderStyle(Style.empty.withFg(Color.Cyan))
    block.render(area, canvas)
    val caps  = session.capabilities
    val line  = Line.of(
      Span.styled("colours ", Style.empty.dim),
      Span.raw(caps.colors.show),
      Span.styled("  ssh ", Style.empty.dim),
      Span.raw(yesNo(caps.ssh)),
      Span.styled("  mode ", Style.empty.dim),
      Span.raw(modeName(session)),
      Span.styled("  sync ", Style.empty.dim),
      Span.raw(yesNo(caps.syncOutput)),
      Span.styled("  multiplexer ", Style.empty.dim),
      Span.raw(caps.multiplexer.show),
      Span.styled("  glyphs ", Style.empty.dim),
      Span.raw(caps.effectiveGlyphs.show),
      Span.styled("  VS16 ", Style.empty.dim),
      Span.raw(caps.vs16Width.show),
    )
    line.render(block.inner(area), canvas)
  }

  private def renderBody(
    state: State,
    set: BorderSet,
    area: Rect,
    canvas: Canvas,
    corrections: AtomicReference[(Scroll, LogViewState)],
  ): Unit = {
    val block       = bodyBlock(set)
    val inner       = block.inner(area)
    val paragraph   = Paragraph.of(longBodyText).withWrap(Wrap.Word)
    val contentSize = Size(inner.width, NonNegInts.clamp(paragraph.lineCount(inner.width, canvas.policy).toLong))
    val corrected   = ScrollView
      .of(paragraph, contentSize)
      .withBlock(block)
      .withRegion(bodyRegion)
      .render(area, canvas, state.scroll)
    corrections.updateAndGet { case (_, logState) => (corrected, logState) }: Unit
  }

  private def renderLog(
    state: State,
    set: BorderSet,
    area: Rect,
    canvas: Canvas,
    corrections: AtomicReference[(Scroll, LogViewState)],
  ): Unit = {
    val corrected = LogView
      .of(state.log)
      .withBlock(logBlock(set))
      .withRegion(logRegion)
      .render(area, canvas, state.logState)
    corrections.updateAndGet { case (scroll, _) => (scroll, corrected) }: Unit
  }

  private def renderFooter(state: State, session: TerminalSession, set: BorderSet, area: Rect, canvas: Canvas): Unit = {
    val block = Block.bordered.withBorderSet(set).withBorderStyle(Style.empty.withFg(Color.Green))
    block.render(area, canvas)
    val inner = block.inner(area)
    val size  = session.terminal.viewport.size
    val stats =
      state.stats.fold("-")(s => s"${s.bytes.value.toString} B, ${s.cells.value.toString} cells, ${s.duration.toMicros.toString} us")
    val log   = s"${state.logState.anchor.value.toString}${if (state.logState.following) " f" else ""}"
    val line  = Line.of(
      Span.styled("pane ", Style.empty.dim),
      Span.raw(paneName(state.pane)),
      Span.styled("  scroll ", Style.empty.dim),
      Span.raw(s"${state.scroll.rows.value.toString},${state.scroll.columns.value.toString}"),
      Span.styled("  log ", Style.empty.dim),
      Span.raw(log),
      Span.styled("  size ", Style.empty.dim),
      Span.raw(s"${size.width.value.toString}x${size.height.value.toString}"),
      Span.styled("  frame ", Style.empty.dim),
      Span.raw(state.frames.toString),
      Span.styled("  last ", Style.empty.dim),
      Span.raw(state.lastEvent.fold("-")(_.show)),
      Span.styled("  over ", Style.empty.dim),
      Span.raw(state.hovered.fold("-")(_.value)),
      Span.styled("  stats ", Style.empty.dim),
      Span.raw(stats),
      Span.styled("  focus ", Style.empty.dim),
      Span.raw(yesNo(state.focused)),
      Span.styled("  printed ", Style.empty.dim),
      Span.raw(state.printed.toString),
      Span.styled("  paste ", Style.empty.dim),
      Span.raw(state.paste.fold("-")(_.take(20))),
    )
    line.render(inner, canvas)
    canvas.region(statusRegion, area)
    if (inner.isEmpty) () else canvas.cursor(Position(inner.x, inner.y))
  }

  private def yesNo(flag: Boolean): String = if (flag) "yes" else "no"

  private def paneName(pane: Pane): String = pane match {
    case Pane.Body => "body"
    case Pane.Log => "log"
  }

  private def modeName(session: TerminalSession): String = session.terminal.options.screenMode match {
    case ScreenMode.AlternateScreen => "alt"
    case ScreenMode.Inline(height) => s"inline(${height.value.toString})"
  }

  private def eventLine(frame: Int, event: Event): Line =
    Line.of(Span.styled(s"#${frame.toString} ", Style.empty.dim), Span.raw(event.show))

  private def togglePane(pane: Pane): Pane = pane match {
    case Pane.Body => Pane.Log
    case Pane.Log => Pane.Body
  }

  /** The focused pane scrolled by `delta` rows (negative is up); the render clamps. */
  private def scrollFocused(session: TerminalSession, state: State, delta: Int): State = state.pane match {
    case Pane.Body => state.copy(scroll = Scrolling.scrolledBy(state.scroll, delta, 0))
    case Pane.Log =>
      val rows = NonNegInts.clamp(math.abs(delta).toLong)
      val next =
        if (delta < 0) LogView.scrolledUp(state.logState, state.log, logRows(session), rows)
        else LogView.scrolledDown(state.logState, state.log, logRows(session), rows)
      state.copy(logState = next)
  }

  private def pageStep(session: TerminalSession, state: State): Int = {
    val rows = state.pane match {
      case Pane.Body => bodyRows(session)
      case Pane.Log => logRows(session)
    }
    math.max(1, rows.value)
  }

  private def toTopFocused(state: State): State = state.pane match {
    case Pane.Body => state.copy(scroll = Scroll.none)
    case Pane.Log => state.copy(logState = LogView.toTop(state.log))
  }

  private def toBottomFocused(state: State): State = state.pane match {
    case Pane.Body => state.copy(scroll = Scrolling.scrolledBy(state.scroll, 1000000, 0))
    case Pane.Log => state.copy(logState = LogView.toBottom)
  }

  /** The wheel scrolls the pane under the mouse, resolved through the frame's hit regions (design doc 6.6). */
  private def wheel(session: TerminalSession, state: State, over: Option[RegionId], delta: Int): State =
    if (over.exists(_ === bodyRegion)) {
      state.copy(scroll = Scrolling.scrolledBy(state.scroll, delta, 0))
    } else if (over.exists(_ === logRegion)) {
      val rows = NonNegInts.clamp(math.abs(delta).toLong)
      val next =
        if (delta < 0) LogView.scrolledUp(state.logState, state.log, logRows(session), rows)
        else LogView.scrolledDown(state.logState, state.log, logRows(session), rows)
      state.copy(logState = next)
    } else {
      state
    }

  private def mouseStep(session: TerminalSession, state: State, mouse: MouseEvent, completed: CompletedFrame): State = {
    val over = completed.frame.regions.at(mouse.position)
    val next = mouse.kind match {
      case MouseEventKind.ScrollUp => wheel(session, state, over, -WheelStep)
      case MouseEventKind.ScrollDown => wheel(session, state, over, WheelStep)
      case _ => state
    }
    next.copy(hovered = over)
  }

  /** One event folded into the state (the event was already logged and remembered by the loop). */
  private def step(session: TerminalSession, state: State, event: Event, completed: CompletedFrame): State = event match {
    case Event.Key(KeyEvent(KeyCode.Char('r'), _, _)) =>
      session.terminal.redraw(RedrawReason.Requested)
      state
    case Event.Key(KeyEvent(KeyCode.Char('p'), _, _)) =>
      session
        .terminal
        .print(
          Line.of(
            Span.raw(s"log ${state.printed.toString}: "),
            Span.styled("printed above the UI", Style.empty.withFg(Color.Green)),
            Span.raw(" 안녕하세요 · こんにちは"),
          )
        )
      state.copy(printed = state.printed + 1)
    case Event.Key(KeyEvent(KeyCode.Char('P'), _, _)) =>
      session
        .terminal
        .print(
          Text.of(
            Line.of(Span.styled(s"=== block ${state.printed.toString} ===", Style.empty.bold)),
            Line.raw("a three-row paragraph printed through the cell pipeline"),
            Line.raw("emoji: 🎉 wide: 한글"),
          )
        )
      state.copy(printed = state.printed + 1)
    case Event.Key(KeyEvent(KeyCode.Tab, _, _)) =>
      if (isInline(session)) state else state.copy(pane = togglePane(state.pane))
    case Event.Key(KeyEvent(KeyCode.Up, _, _)) => scrollFocused(session, state, -1)
    case Event.Key(KeyEvent(KeyCode.Down, _, _)) => scrollFocused(session, state, 1)
    case Event.Key(KeyEvent(KeyCode.PageUp, _, _)) => scrollFocused(session, state, -pageStep(session, state))
    case Event.Key(KeyEvent(KeyCode.PageDown, _, _)) => scrollFocused(session, state, pageStep(session, state))
    case Event.Key(KeyEvent(KeyCode.Home, _, _)) => toTopFocused(state)
    case Event.Key(KeyEvent(KeyCode.End, _, _)) => toBottomFocused(state)
    case Event.Resize(_) =>
      session.terminal.redraw(RedrawReason.Resize)
      state
    case Event.Mouse(mouse) => mouseStep(session, state, mouse, completed)
    case Event.Paste(text) => state.copy(paste = text.some)
    case Event.FocusGained => state.copy(focused = true)
    case Event.FocusLost => state.copy(focused = false)
    case Event.Key(_) => state
  }

  /** Draws, waits for an event, and loops until `q`, Control-C, or a termination signal. Returns why it stopped. */
  @tailrec
  def loop(session: TerminalSession, state: State): String = {
    /* the corrected scroll states are captured out of the render closure through a reference (the Rendering.statefulWith shape) */
    val corrections        = new AtomicReference((state.scroll, state.logState))
    val completed          = session.terminal.draw(view(state, session, corrections))
    val (scroll, logState) = corrections.get()
    val next               =
      state.copy(stats = completed.stats.some, frames = state.frames + 1, scroll = scroll, logState = logState)
    session.events.poll(100.millis) match {
      case Some(Event.Key(KeyEvent(KeyCode.Char('q'), _, _))) => "quit"
      case Some(Event.Key(KeyEvent(KeyCode.Char('c'), modifiers, _))) if modifiers.contains(KeyModifier.Control) => "interrupted"
      case Some(event) =>
        val logged = next.copy(log = next.log.append(eventLine(next.frames, event)), lastEvent = event.some)
        loop(session, step(session, logged, event, completed))
      case None => if (session.terminationRequested) "signal" else loop(session, next)
    }
  }

}
