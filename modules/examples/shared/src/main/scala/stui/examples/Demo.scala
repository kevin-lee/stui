package stui.examples

import cats.syntax.all.*
import stui.core.buffer.Canvas
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier}
import stui.core.frame.RegionId
import stui.core.geometry.{Position, Rect}
import stui.core.layout.{Constraint, Layout}
import stui.core.style.{Color, Style, UnderlineStyle}
import stui.core.terminal.{RedrawReason, RenderStats}
import stui.core.text.{Line, Span, Text}
import stui.terminal.TerminalSession
import stui.terminal.TerminalSession.*
import stui.widgets.{Block, Paragraph, Wrap}

import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The M1e demo: the alternate screen with a titled header, a wrapped body of Korean, Japanese, and emoji text in rich colour, and a
  * status line that shows the last event, the hit region under the mouse, the size, the render statistics, and a recorded cursor.
  * `q` or Control-C quits, `r` forces a redraw, a resize redraws, and a paste is shown.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Demo {

  /** What the loop remembers between frames. */
  final case class State(
    lastEvent: Option[Event],
    hovered: Option[RegionId],
    stats: Option[RenderStats],
    frames: Int,
    paste: Option[String],
    focused: Boolean,
  )

  object State {

    /** Nothing seen yet. */
    val initial: State = State(none[Event], none[RegionId], none[RenderStats], 0, none[String], true)

  }

  /** The body pane's hit region. */
  val bodyRegion: RegionId = RegionId("body")

  /** The status pane's hit region. */
  val statusRegion: RegionId = RegionId("status")

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
    Line.raw("Keys: q quits, Control-C quits, r redraws, resize the window, move the mouse over the panes, paste something."),
  )

  /** Renders one frame of the demo. */
  def view(state: State, session: TerminalSession)(canvas: Canvas): Unit =
    Layout.vertical(Constraint.length(3), Constraint.fill(1), Constraint.length(3)).split(canvas.area) match {
      case Vector(header, body, footer) =>
        renderHeader(session, header, canvas)
        renderBody(body, canvas)
        renderFooter(state, session, footer, canvas)
      case _ => ()
    }

  private def renderHeader(session: TerminalSession, area: Rect, canvas: Canvas): Unit = {
    val block = Block.bordered.withTitle(Line.raw(" stui M1e demo ").centered).withBorderStyle(Style.empty.withFg(Color.Cyan))
    block.render(area, canvas)
    val caps  = session.capabilities
    val line  = Line.of(
      Span.styled("colours ", Style.empty.dim),
      Span.raw(caps.colors.show),
      Span.styled("  ssh ", Style.empty.dim),
      Span.raw(yesNo(caps.ssh)),
      Span.styled("  multiplexer ", Style.empty.dim),
      Span.raw(caps.multiplexer.show),
      Span.styled("  extended underline ", Style.empty.dim),
      Span.raw(yesNo(caps.extendedUnderline)),
      Span.styled("  VS16 ", Style.empty.dim),
      Span.raw(caps.vs16Width.show),
    )
    line.render(block.inner(area), canvas)
  }

  private def renderBody(area: Rect, canvas: Canvas): Unit = {
    val block = Block.bordered.withTitle(Line.raw(" 안녕하세요 · こんにちは · hello 👋 ")).withBorderStyle(Style.empty.withFg(Color.Magenta))
    Paragraph.of(bodyText).withWrap(Wrap.Word).withBlock(block).render(area, canvas)
    canvas.region(bodyRegion, area)
  }

  private def renderFooter(state: State, session: TerminalSession, area: Rect, canvas: Canvas): Unit = {
    val block = Block.bordered.withBorderStyle(Style.empty.withFg(Color.Green))
    block.render(area, canvas)
    val inner = block.inner(area)
    val size  = session.terminal.viewport.size
    val stats =
      state.stats.fold("-")(s => s"${s.bytes.value.toString} B, ${s.cells.value.toString} cells, ${s.duration.toMicros.toString} us")
    val line  = Line.of(
      Span.styled("last ", Style.empty.dim),
      Span.raw(state.lastEvent.fold("-")(_.show)),
      Span.styled("  over ", Style.empty.dim),
      Span.raw(state.hovered.fold("-")(_.value)),
      Span.styled("  size ", Style.empty.dim),
      Span.raw(s"${size.width.value.toString}x${size.height.value.toString}"),
      Span.styled("  frame ", Style.empty.dim),
      Span.raw(state.frames.toString),
      Span.styled("  stats ", Style.empty.dim),
      Span.raw(stats),
      Span.styled("  focus ", Style.empty.dim),
      Span.raw(yesNo(state.focused)),
      Span.styled("  paste ", Style.empty.dim),
      Span.raw(state.paste.fold("-")(_.take(20))),
    )
    line.render(inner, canvas)
    canvas.region(statusRegion, area)
    if (inner.isEmpty) () else canvas.cursor(Position(inner.x, inner.y))
  }

  private def yesNo(flag: Boolean): String = if (flag) "yes" else "no"

  /** Draws, waits for an event, and loops until `q`, Control-C, or a termination signal. Returns why it stopped. */
  @tailrec
  def loop(session: TerminalSession, state: State): String = {
    val completed = session.terminal.draw(view(state, session))
    val next      = state.copy(stats = completed.stats.some, frames = state.frames + 1)
    session.events.poll(100.millis) match {
      case Some(Event.Key(KeyEvent(KeyCode.Char('q'), _, _))) => "quit"
      case Some(Event.Key(KeyEvent(KeyCode.Char('c'), modifiers, _))) if modifiers.contains(KeyModifier.Control) => "interrupted"
      case Some(event @ Event.Key(KeyEvent(KeyCode.Char('r'), _, _))) =>
        session.terminal.redraw(RedrawReason.Requested)
        loop(session, next.copy(lastEvent = event.some))
      case Some(event @ Event.Resize(_)) =>
        session.terminal.redraw(RedrawReason.Resize)
        loop(session, next.copy(lastEvent = event.some))
      case Some(event @ Event.Mouse(mouse)) =>
        loop(session, next.copy(lastEvent = event.some, hovered = completed.frame.regions.at(mouse.position)))
      case Some(event @ Event.Paste(text)) => loop(session, next.copy(lastEvent = event.some, paste = text.some))
      case Some(Event.FocusGained) => loop(session, next.copy(lastEvent = Event.FocusGained.some, focused = true))
      case Some(Event.FocusLost) => loop(session, next.copy(lastEvent = Event.FocusLost.some, focused = false))
      case Some(event @ Event.Key(_)) => loop(session, next.copy(lastEvent = event.some))
      case None => if (session.terminationRequested) "signal" else loop(session, next)
    }
  }

}
