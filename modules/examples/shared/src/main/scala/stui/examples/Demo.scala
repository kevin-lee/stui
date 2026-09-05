package stui.examples

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.app.{AppEnv, Cmd, StuiApp, Sub, View}
import stui.app.AppEnv.*
import stui.core.buffer.Canvas
import stui.core.capability.{Capabilities, GlyphSet}
import stui.core.event.{Event, KeyCode, KeyEvent, KeyModifier, MouseButton, MouseEvent, MouseEventKind}
import stui.core.focus.FocusRing
import stui.core.focus.FocusRing.*
import stui.core.frame.{RegionId, Regions}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.layout.{Axis, Constraint, Percent}
import stui.core.spi.ScreenMode
import stui.core.style.{Color, Style, UnderlineStyle}
import stui.core.text.{Line, Span, Text}
import stui.widgets.{
  Block,
  BorderSet,
  Gauge,
  ItemRegions,
  ListView,
  LogRing,
  LogView,
  LogViewState,
  Paragraph,
  Row,
  Scroll,
  ScrollView,
  Scrollbar,
  Scrolling,
  Selection,
  Table,
  TableState,
  Tabs,
  Wrap,
}
import stui.widgets.Gauge.*
import stui.widgets.ListView.*
import stui.widgets.LogRing.*
import stui.widgets.LogView.*
import stui.widgets.Row.*
import stui.widgets.Scrollbar.*
import stui.widgets.ScrollView.*
import stui.widgets.Table.*
import stui.widgets.Tabs.*

import scala.concurrent.duration.*

/** The stui demo (M3b): a [[StuiApp]] over [[State]] and [[Msg]] that names no terminal type - a titled header, a [[Tabs]] strip
  * selecting one of four pages - the scrollable body of long wrapped Korean, Japanese, and emoji text with a [[Scrollbar]] beside it
  * (M2a), a [[ListView]] with a scrollbar, a [[Table]] of Unicode samples, and three [[Gauge]]s - an event log pane (a [[LogView]]
  * following its tail, alternate screen only), and a status footer whose `up` counter is a one-second [[Sub]] tick. Every glyph
  * comes from the capabilities ([[BorderSet.forCapabilities]], [[Tabs.dividerFor]], the scrollbar and gauge sets, the highlight
  * symbol). Digits `1` to `4` and a click on a title select the page, `Left` / `Right` move the table's column (the page elsewhere),
  * Tab moves the key focus between the page and the log (a [[FocusRing]] over the panes, decision D25), Up / Down / PageUp /
  * PageDown / Home / End drive the focused pane (the scroll offset, the list or row selection, or the gauge), `+` / `-` adjust the
  * gauge, a click selects a list item or a table row, the wheel scrolls the pane under the mouse (the runtime hands the last frame's
  * hit map to `onMouse`, the application decodes it through [[ItemRegions]]), `q` or Control-C quits, `r` forces a redraw, `p` and
  * `P` print above the UI, `!` deliberately crashes (the exception restore path under a pseudo-terminal driver), and a paste is
  * shown.
  *
  * The rendering is the returned-state pattern lifted to the application (design doc 10, decision D8 refined in M3b): the root's
  * render hands back the corrected widget states inside the model, and the runtime starts the next batch from it.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Demo {

  /** Which pane the key focus can be on: the page or the log. */
  enum Pane derives Eq {
    case Body
    case Log
  }

  /** The four pages of the tab strip. */
  enum Page {
    case ScrollPage
    case ListPage
    case TablePage
    case GaugePage
  }

  object Page {

    /** The page the tab selection names (the first when nothing or something beyond the strip is selected). */
    def of(selection: Selection): Page =
      Vector(ScrollPage, ListPage, TablePage, GaugePage).lift(selection.selected.fold(0)(_.value)).getOrElse(ScrollPage)

  }

  /** The model: the session's environment, the viewport size, what the loop remembers between frames, the widget states, and the
    * exit reason once one was asked for.
    */
  final case class State(
    env: AppEnv,
    viewport: Size,
    lastEvent: Option[Event],
    hovered: Option[RegionId],
    events: Int,
    uptime: Long,
    paste: Option[String],
    focused: Boolean,
    printed: Int,
    scroll: Scroll,
    log: LogRing,
    logState: LogViewState,
    focus: FocusRing[Pane],
    page: Selection,
    list: Selection,
    table: TableState,
    gauge: NonNegInt,
    exit: Option[String],
  )

  object State {

    /** Nothing seen yet: no offset, an empty 200-line event log following its tail, the page focused, the first page, the first
      * list item and table row selected, the gauge at half.
      */
    def initial(env: AppEnv): State =
      State(
        env,
        env.viewport,
        none[Event],
        none[RegionId],
        0,
        0L,
        none[String],
        true,
        0,
        Scroll.none,
        LogRing.empty(PosInt(200)),
        LogViewState.following,
        FocusRing.of(Pane.Body, Pane.Log),
        Selection.first,
        Selection.first,
        TableState.of(Selection.first),
        NonNegInt(10),
        none[String],
      )

  }

  /** The messages: an exit request with its reason, a terminal event, a mouse event with the region under it, and the uptime tick. */
  enum Msg derives Eq, Show, Hash {
    case Quit(reason: String)
    case Input(event: Event)
    case Mouse(mouse: MouseEvent, over: Option[RegionId])
    case Tick(now: FiniteDuration)
  }

  /** The tab strip's hit region. */
  val tabsRegion: RegionId = RegionId("tabs")

  /** The scroll page's hit region. */
  val bodyRegion: RegionId = RegionId("body")

  /** The list page's hit region. */
  val listRegion: RegionId = RegionId("list")

  /** The table page's hit region. */
  val tableRegion: RegionId = RegionId("table")

  /** The log pane's hit region. */
  val logRegion: RegionId = RegionId("log")

  /** The status pane's hit region. */
  val statusRegion: RegionId = RegionId("status")

  /** Rows or items the wheel scrolls per event (the app's documented choice, design doc 6.6). */
  private val WheelStep: Int = 3

  /** The gauge counts twentieths. */
  private val GaugeTotal: PosInt = PosInt(20)

  private val PageTitles: Vector[String] = Vector("Scroll", "List", "Table", "Gauge")

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
      "Keys: q quits, r redraws, 1-4 pick a page, Tab switches the pane, Up/Down/PageUp/PageDown/Home/End drive it, the wheel scrolls the hovered pane, a click selects, p and P print above the UI, paste something."
    ),
  )

  /** The body plus forty marker rows, so scrolling is visible (and assertable under a pseudo-terminal driver). */
  private val longBodyText: Text =
    Text.fromLines(
      bodyText.lines ++ Vector.tabulate(40)(i => Line.raw(f"L${i + 1}%04d the quick brown fox jumps over the lazy dog 한글 넓은 글자"))
    )

  private val listSamples: Vector[String] =
    Vector("한국어 항목", "日本語の項目", "emoji 🎉 item", "plain ascii item", "넓은 글자 wide item")

  /** Thirty numbered items cycling through the samples. */
  private val listItems: Vector[String] =
    Vector.tabulate(30)(i => f"item ${i + 1}%02d - ${listSamples.lift(i % listSamples.length).getOrElse("")}")

  /** The number of list items. */
  private val ListCount: NonNegInt = NonNegInt(30)

  private val tableRows: Vector[Row] = Vector(
    Row.raw("한", "U+D55C", "2", "Hangul syllable"),
    Row.raw("글", "U+AE00", "2", "Hangul syllable"),
    Row.raw("あ", "U+3042", "2", "Hiragana"),
    Row.raw("👋", "U+1F44B", "2", "emoji"),
    Row.raw("🇰🇷", "U+1F1F0 U+1F1F7", "2", "flag, two regional indicators"),
    Row.raw(keyboardVs16, "U+2328 U+FE0F", "2", "VS16 sequence"),
    Row.raw("a", "U+0061", "1", "ASCII"),
    Row.raw("é", "U+00E9", "1", "Latin-1"),
    Row.raw("ﾃ", "U+FF83", "1", "halfwidth katakana"),
    Row.raw("ㅏ", "U+314F", "2", "Hangul compatibility jamo"),
    Row.raw("☆", "U+2606", "1", "East Asian Ambiguous"),
    Row.raw("─", "U+2500", "1", "box drawing, Ambiguous"),
  )

  /** The pane rects of one frame. Built by the fixed-arity splits (decision D26), so no fallback exists. */
  final private case class PaneAreas(header: Rect, tabs: Rect, page: Rect, log: Option[Rect], footer: Rect)

  private def paneAreas(area: Rect, inline: Boolean): PaneAreas =
    if (inline) {
      val (header, tabs, page, footer) =
        Axis.vertical.split4(area, Constraint.length(3), Constraint.length(1), Constraint.fill(1), Constraint.length(3))
      PaneAreas(header, tabs, page, none[Rect], footer)
    } else {
      val (header, tabs, page, log, footer) =
        Axis
          .vertical
          .split5(area, Constraint.length(3), Constraint.length(1), Constraint.fill(1), Constraint.length(6), Constraint.length(3))
      PaneAreas(header, tabs, page, log.some, footer)
    }

  /** A page area split into the widget's area and the one-column scrollbar lane beside it. */
  private def withLane(area: Rect): (Rect, Rect) = Axis.horizontal.split2(area, Constraint.fill(1), Constraint.length(1))

  private def borders(env: AppEnv): BorderSet = BorderSet.forCapabilities(env.capabilities)

  /** The highlight symbol: the right-pointing triangle (U+25B6, East Asian Ambiguous) under Unicode glyphs, `>` otherwise. */
  private def symbol(capabilities: Capabilities): Line = capabilities.effectiveGlyphs match {
    case GlyphSet.Unicode => Line.raw("▶ ")
    case GlyphSet.Ascii => Line.raw("> ")
  }

  private def titled(set: BorderSet, title: String, color: Color): Block =
    Block.bordered.withBorderSet(set).withTitle(Line.raw(title)).withBorderStyle(Style.empty.withFg(color))

  private def bodyBlock(set: BorderSet): Block = titled(set, " 안녕하세요 · こんにちは · hello 👋 ", Color.Magenta)

  private def listBlock(set: BorderSet): Block = titled(set, " list ", Color.Blue)

  private def tableBlock(set: BorderSet): Block = titled(set, " table ", Color.Cyan)

  private def logBlock(set: BorderSet): Block = titled(set, " events ", Color.Yellow)

  /** The focused page's inner rows at the current viewport (what PageUp / PageDown step by), at least 1. */
  private def pageRows(state: State): Int =
    math.max(1, paneAreas(Rect.sized(state.viewport), state.env.isInline).page.height.value - 2)

  /** The log pane's inner rows at the current viewport. */
  private def logRows(state: State): NonNegInt =
    paneAreas(Rect.sized(state.viewport), state.env.isInline)
      .log
      .fold(NonNegInt(0))(area => logBlock(borders(state.env)).inner(area).height)

  /** Renders one frame and returns the state carrying the corrected widget states (the returned-state pattern, design doc 6.4 and
    * 10): the focus ring retargeted to the panes actually drawn (inline mode has no log, so a one-item ring stays put under Tab), the
    * tab selection, the page's scroll offset or selection, and the log anchor.
    */
  private def render(area: Rect, canvas: Canvas, state: State): State = {
    val set      = borders(state.env)
    val caps     = state.env.capabilities
    val panes    = paneAreas(area, state.env.isInline)
    val present  = panes.log.fold(Vector(Pane.Body))(_ => Vector(Pane.Body, Pane.Log))
    val focus    = state.focus.withItems(present)
    renderHeader(state, set, panes.header, canvas)
    val page     = renderTabs(state, caps, panes.tabs, canvas)
    val paged    = Page.of(state.page) match {
      case Page.ScrollPage => state.copy(scroll = renderScrollPage(state, set, caps, panes.page, canvas))
      case Page.ListPage => state.copy(list = renderListPage(state, set, caps, panes.page, canvas))
      case Page.TablePage => state.copy(table = renderTablePage(state, set, caps, panes.page, canvas))
      case Page.GaugePage =>
        renderGaugePage(state, set, caps, panes.page, canvas)
        state
    }
    val logState = panes.log.fold(state.logState)(logArea => renderLog(state, set, logArea, canvas))
    renderFooter(state, set, panes.footer, canvas)
    paged.copy(focus = focus, page = page, logState = logState)
  }

  private def renderHeader(state: State, set: BorderSet, area: Rect, canvas: Canvas): Unit = {
    val block =
      Block.bordered.withBorderSet(set).withTitle(Line.raw(" stui M2c demo ").centered).withBorderStyle(Style.empty.withFg(Color.Cyan))
    block.render(area, canvas)
    val caps  = state.env.capabilities
    val line  = Line.of(
      Span.styled("colours ", Style.empty.dim),
      Span.raw(caps.colors.show),
      Span.styled("  ssh ", Style.empty.dim),
      Span.raw(yesNo(caps.ssh)),
      Span.styled("  mode ", Style.empty.dim),
      Span.raw(modeName(state.env)),
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

  private def renderTabs(state: State, caps: Capabilities, area: Rect, canvas: Canvas): Selection =
    Tabs
      .fromLines(PageTitles.map(Line.raw))
      .withDividerFor(caps)
      .withHighlightStyle(Style.empty.reversed.bold)
      .withRegion(tabsRegion)
      .render(area, canvas, state.page)

  private def renderScrollPage(state: State, set: BorderSet, caps: Capabilities, area: Rect, canvas: Canvas): Scroll = {
    val (body, lane) = withLane(area)
    val block        = bodyBlock(set)
    val inner        = block.inner(body)
    val paragraph    = Paragraph.of(longBodyText).withWrap(Wrap.Word)
    val contentSize  = Size(inner.width, NonNegInts.clamp(paragraph.lineCount(inner.width, canvas.policy).toLong))
    val corrected    = ScrollView
      .of(paragraph, contentSize)
      .withBlock(block)
      .withRegion(bodyRegion)
      .render(body, canvas, state.scroll)
    Scrollbar.ofScroll(corrected, contentSize, inner.size).withSetFor(caps).render(lane, canvas)
    corrected
  }

  private def renderListPage(state: State, set: BorderSet, caps: Capabilities, area: Rect, canvas: Canvas): Selection = {
    val (body, lane) = withLane(area)
    val block        = listBlock(set)
    val corrected    = ListView
      .raw(listItems*)
      .withBlock(block)
      .withHighlightSymbol(symbol(caps))
      .withHighlightStyle(Style.empty.reversed)
      .withScrollPadding(NonNegInt(1))
      .withRegion(listRegion)
      .render(body, canvas, state.list)
    Scrollbar.ofSelection(corrected, ListCount, block.inner(body).height).withSetFor(caps).render(lane, canvas)
    corrected
  }

  private def renderTablePage(state: State, set: BorderSet, caps: Capabilities, area: Rect, canvas: Canvas): TableState =
    Table
      .of(tableRows)
      .withWidths(Vector(Constraint.length(6), Constraint.length(16), Constraint.length(5), Constraint.fill(1)))
      .withHeader(Row.raw("glyph", "code points", "width", "note").withStyle(Style.empty.bold))
      .withBlock(tableBlock(set))
      .withHighlightSymbol(symbol(caps))
      .withRowHighlightStyle(Style.empty.reversed)
      .withColumnHighlightStyle(Style.empty.bold.underlined)
      .withCellHighlightStyle(Style.empty.withFg(Color.Yellow))
      .withScrollPadding(NonNegInt(1))
      .withRegion(tableRegion)
      .render(area, canvas, state.table)

  private def renderGaugePage(state: State, set: BorderSet, caps: Capabilities, area: Rect, canvas: Canvas): Unit = {
    val (first, second, third, _) =
      Axis.vertical.split4(area, Constraint.length(3), Constraint.length(3), Constraint.length(3), Constraint.fill(1))
    val listIndex                 = NonNegInts.clamp(math.min(state.list.selected.fold(0L)(_.value.toLong), ListCount.value.toLong - 1L))
    Gauge
      .fraction(state.gauge, GaugeTotal)
      .withBlock(titled(set, " progress (+ / -) ", Color.Green))
      .withGaugeStyle(Style.empty.withFg(Color.Green))
      .withSetFor(caps)
      .render(first, canvas)
    Gauge
      .fraction(listIndex, PosInt(29))
      .withBlock(titled(set, " list position ", Color.Cyan))
      .withGaugeStyle(Style.empty.withFg(Color.Cyan))
      .withSetFor(caps)
      .render(second, canvas)
    Gauge
      .percent(Percent(100))
      .withLabel(Line.raw("done"))
      .withBlock(titled(set, " done ", Color.Magenta))
      .withGaugeStyle(Style.empty.withFg(Color.Magenta))
      .withSetFor(caps)
      .render(third, canvas)
  }

  private def renderLog(state: State, set: BorderSet, area: Rect, canvas: Canvas): LogViewState =
    LogView
      .of(state.log)
      .withBlock(logBlock(set))
      .withRegion(logRegion)
      .render(area, canvas, state.logState)

  private def renderFooter(state: State, set: BorderSet, area: Rect, canvas: Canvas): Unit = {
    val block = Block.bordered.withBorderSet(set).withBorderStyle(Style.empty.withFg(Color.Green))
    block.render(area, canvas)
    val inner = block.inner(area)
    val size  = canvas.area.size
    val log   = s"${state.logState.anchor.value.toString}${if (state.logState.following) " f" else ""}"
    val row   = s"${state.table.rows.selected.fold("-")(_.value.toString)} col ${state.table.column.fold("-")(_.value.toString)}"
    val line  = Line.of(
      Span.styled("pane ", Style.empty.dim),
      Span.raw(paneName(focusedPane(state))),
      Span.styled("  page ", Style.empty.dim),
      Span.raw(state.page.selected.fold("-")(_.value.toString)),
      Span.styled("  scroll ", Style.empty.dim),
      Span.raw(s"${state.scroll.rows.value.toString},${state.scroll.columns.value.toString}"),
      Span.styled("  list ", Style.empty.dim),
      Span.raw(state.list.selected.fold("-")(_.value.toString)),
      Span.styled("  row ", Style.empty.dim),
      Span.raw(row),
      Span.styled("  gauge ", Style.empty.dim),
      Span.raw(s"${Gauge.percentOf(Gauge.fraction(state.gauge, GaugeTotal)).toString}%"),
      Span.styled("  log ", Style.empty.dim),
      Span.raw(log),
      Span.styled("  size ", Style.empty.dim),
      Span.raw(s"${size.width.value.toString}x${size.height.value.toString}"),
      Span.styled("  up ", Style.empty.dim),
      Span.raw(s"${state.uptime.toString}s"),
      Span.styled("  events ", Style.empty.dim),
      Span.raw(state.events.toString),
      Span.styled("  last ", Style.empty.dim),
      Span.raw(state.lastEvent.fold("-")(_.show)),
      Span.styled("  over ", Style.empty.dim),
      Span.raw(state.hovered.fold("-")(_.value)),
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

  private def modeName(env: AppEnv): String = env.screenMode match {
    case ScreenMode.AlternateScreen => "alt"
    case ScreenMode.Inline(height) => s"inline(${height.value.toString})"
  }

  private def eventLine(index: Int, event: Event): Line =
    Line.of(Span.styled(s"#${index.toString} ", Style.empty.dim), Span.raw(event.show))

  private def isBody(pane: Pane): Boolean = pane match {
    case Pane.Body => true
    case Pane.Log => false
  }

  /** The pane the key focus is on: the current target of the ring, the page when nothing is focused. */
  private def focusedPane(state: State): Pane = state.focus.current.getOrElse(Pane.Body)

  /** The gauge moved by `delta` twentieths, clamped into 0..20. */
  private def gaugeBy(state: State, delta: Int): State =
    state.copy(gauge = NonNegInts.clamp(math.min(GaugeTotal.value.toLong, state.gauge.value.toLong + delta.toLong)))

  /** The log scrolled by `delta` rows (negative is up) through the pure helpers. */
  private def logBy(state: State, delta: Int): State = {
    val rows = NonNegInts.clamp(math.abs(delta).toLong)
    val next =
      if (delta < 0) LogView.scrolledUp(state.logState, state.log, logRows(state), rows)
      else LogView.scrolledDown(state.logState, state.log, logRows(state), rows)
    state.copy(logState = next)
  }

  /** The focused pane moved by `delta` (negative is up): the log, the scroll offset, the list or row selection, or the gauge; the
    * render clamps.
    */
  private def moveFocused(state: State, delta: Int): State = focusedPane(state) match {
    case Pane.Log => logBy(state, delta)
    case Pane.Body =>
      Page.of(state.page) match {
        case Page.ScrollPage => state.copy(scroll = Scrolling.scrolledBy(state.scroll, delta, 0))
        case Page.ListPage => state.copy(list = state.list.movedBy(delta))
        case Page.TablePage => state.copy(table = state.table.rowsMovedBy(delta))
        case Page.GaugePage => gaugeBy(state, delta)
      }
  }

  private def pageStep(state: State): Int = focusedPane(state) match {
    case Pane.Body => pageRows(state)
    case Pane.Log => math.max(1, logRows(state).value)
  }

  private def toTopFocused(state: State): State = focusedPane(state) match {
    case Pane.Log => state.copy(logState = LogView.toTop(state.log))
    case Pane.Body =>
      Page.of(state.page) match {
        case Page.ScrollPage => state.copy(scroll = Scroll.none)
        case Page.ListPage => state.copy(list = state.list.selectFirst)
        case Page.TablePage => state.copy(table = state.table.selectFirstRow)
        case Page.GaugePage => state.copy(gauge = NonNegInt(0))
      }
  }

  private def toBottomFocused(state: State): State = focusedPane(state) match {
    case Pane.Log => state.copy(logState = LogView.toBottom)
    case Pane.Body =>
      Page.of(state.page) match {
        case Page.ScrollPage => state.copy(scroll = Scrolling.scrolledBy(state.scroll, 1000000, 0))
        case Page.ListPage => state.copy(list = state.list.selectLast)
        case Page.TablePage => state.copy(table = state.table.selectLastRow)
        case Page.GaugePage => state.copy(gauge = NonNegInts.clamp(GaugeTotal.value.toLong))
      }
  }

  /** Left and Right move the table's column on the table page and the page selection elsewhere. */
  private def sideways(state: State, delta: Int): State =
    Page.of(state.page) match {
      case Page.TablePage if isBody(focusedPane(state)) =>
        state.copy(table = if (delta < 0) state.table.selectPreviousColumn else state.table.selectNextColumn)
      case Page.ScrollPage | Page.ListPage | Page.TablePage | Page.GaugePage =>
        state.copy(page = if (delta < 0) state.page.selectPrevious else state.page.selectNext)
    }

  /** A click selects the tab, the list item, or the table row under the mouse (design doc 6.6, [[ItemRegions]]). */
  private def click(state: State, over: Option[RegionId]): State =
    over.fold(state) { id =>
      ItemRegions
        .indexOf(tabsRegion, id)
        .map(i => state.copy(page = state.page.select(i.some)))
        .orElse(ItemRegions.indexOf(listRegion, id).map(i => state.copy(list = state.list.select(i.some))))
        .orElse(ItemRegions.indexOf(tableRegion, id).map(i => state.copy(table = state.table.selectRow(i.some))))
        .getOrElse(state)
    }

  /** The wheel scrolls the pane under the mouse, resolved through the last frame's hit regions (design doc 6.6). */
  private def wheel(state: State, over: Option[RegionId], delta: Int): State =
    over.fold(state) { id =>
      if (id === bodyRegion) state.copy(scroll = Scrolling.scrolledBy(state.scroll, delta, 0))
      else if (ItemRegions.owns(listRegion, id)) state.copy(list = state.list.movedBy(delta))
      else if (ItemRegions.owns(tableRegion, id)) state.copy(table = state.table.rowsMovedBy(delta))
      else if (id === logRegion) logBy(state, delta)
      else state
    }

  private def mouseStep(state: State, mouse: MouseEvent, over: Option[RegionId]): State = {
    val next = mouse.kind match {
      case MouseEventKind.Down(MouseButton.Left) => click(state, over)
      case MouseEventKind.ScrollUp => wheel(state, over, -WheelStep)
      case MouseEventKind.ScrollDown => wheel(state, over, WheelStep)
      case _ => state
    }
    next.copy(hovered = over)
  }

  private def isDigitPage(c: Char): Boolean = c >= '1' && c <= '4'

  /* the deliberate crash for verifying the exception restore path under a pseudo-terminal driver (issue 23) */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def crash(): (State, Cmd[Msg]) = throw new RuntimeException("stui demo crash test") // scalafix:ok DisableSyntax.throw

  /** The event logged, remembered, and counted. */
  private def logged(state: State, event: Event): State =
    state.copy(log = state.log.append(eventLine(state.events, event)), lastEvent = event.some, events = state.events + 1)

  /** One terminal event folded into the state with the command it asks for (a mouse event arrives as [[Msg.Mouse]] instead). */
  private def stepKey(state: State, event: Event): (State, Cmd[Msg]) = event match {
    case Event.Key(KeyEvent(KeyCode.Char('r'), _, _)) => (state, Cmd.redraw)
    case Event.Key(KeyEvent(KeyCode.Char('p'), _, _)) =>
      (
        state.copy(printed = state.printed + 1),
        Cmd.print(
          Line.of(
            Span.raw(s"log ${state.printed.toString}: "),
            Span.styled("printed above the UI", Style.empty.withFg(Color.Green)),
            Span.raw(" 안녕하세요 · こんにちは"),
          )
        ),
      )
    case Event.Key(KeyEvent(KeyCode.Char('P'), _, _)) =>
      (
        state.copy(printed = state.printed + 1),
        Cmd.print(
          Text.of(
            Line.of(Span.styled(s"=== block ${state.printed.toString} ===", Style.empty.bold)),
            Line.raw("a three-row paragraph printed through the cell pipeline"),
            Line.raw("emoji: 🎉 wide: 한글"),
          )
        ),
      )
    case Event.Key(KeyEvent(KeyCode.Char('+'), _, _)) => (gaugeBy(state, 1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Char('-'), _, _)) => (gaugeBy(state, -1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Char('!'), _, _)) => crash()
    case Event.Key(KeyEvent(KeyCode.Char(c), _, _)) if isDigitPage(c) =>
      (state.copy(page = state.page.select(NonNegInts.clamp((c - '1').toLong).some)), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Tab, _, _)) => (state.copy(focus = state.focus.next), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Left, _, _)) => (sideways(state, -1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Right, _, _)) => (sideways(state, 1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Up, _, _)) => (moveFocused(state, -1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Down, _, _)) => (moveFocused(state, 1), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.PageUp, _, _)) => (moveFocused(state, -pageStep(state)), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.PageDown, _, _)) => (moveFocused(state, pageStep(state)), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.Home, _, _)) => (toTopFocused(state), Cmd.none)
    case Event.Key(KeyEvent(KeyCode.End, _, _)) => (toBottomFocused(state), Cmd.none)
    /* the runtime records the Resize redraw reason itself */
    case Event.Resize(size) => (state.copy(viewport = size), Cmd.none)
    /* mouse events reach update as Msg.Mouse through onMouse, never through this path */
    case Event.Mouse(_) => (state, Cmd.none)
    case Event.Paste(text) => (state.copy(paste = text.some), Cmd.none)
    case Event.FocusGained => (state.copy(focused = true), Cmd.none)
    case Event.FocusLost => (state.copy(focused = false), Cmd.none)
    case Event.Key(_) => (state, Cmd.none)
  }

  /** The demo as an application: `q` and Control-C ask to exit, every other event is logged and stepped, a mouse event carries the
    * region under it, and a one-second tick drives the footer's uptime.
    */
  val app: StuiApp[State, Msg] = new StuiApp[State, Msg] {

    override def init(env: AppEnv): (State, Cmd[Msg]) = (State.initial(env), Cmd.none)

    override def onEvent(event: Event, state: State): Option[Msg] = event match {
      case Event.Key(KeyEvent(KeyCode.Char('q'), _, _)) => Msg.Quit("quit").some
      case Event.Key(KeyEvent(KeyCode.Char('c'), modifiers, _)) if modifiers.contains(KeyModifier.Control) =>
        Msg.Quit("interrupted").some
      case Event.Key(_) | Event.Mouse(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost =>
        Msg.Input(event).some
    }

    override def onMouse(mouse: MouseEvent, regions: Regions, state: State): Option[Msg] =
      Msg.Mouse(mouse, regions.at(mouse.position)).some

    override def update(state: State, msg: Msg): (State, Cmd[Msg]) = msg match {
      case Msg.Quit(reason) => (state.copy(exit = reason.some), Cmd.exit)
      case Msg.Tick(_) => (state.copy(uptime = state.uptime + 1L), Cmd.none)
      case Msg.Input(event) => stepKey(logged(state, event), event)
      case Msg.Mouse(mouse, over) => (mouseStep(logged(state, Event.mouse(mouse)), mouse, over), Cmd.none)
    }

    override def view(state: State): View[State] = View.stateful((area, canvas, current) => render(area, canvas, current))

    override def subscriptions(state: State): Sub[Msg] = Sub.every(1.second)(now => Msg.Tick(now))

  }

}
