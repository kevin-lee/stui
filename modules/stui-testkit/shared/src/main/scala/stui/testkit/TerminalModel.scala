package stui.testkit

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.buffer.{Buffer, Canvas, Cell, CellUpdate, GlyphWidth}
import stui.core.geometry.{Position, Rect, Size}
import stui.core.internal.NonNegInts
import stui.core.style.{CellStyle, Color, Modifier, UnderlineStyle}
import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** The terminal-model oracle (design doc 9.3 and 12, decision D16): a pure interpreter of exactly the sequences the writer emits over
  * a screen, parameterised by a quirk profile, so `interpret(profile, Screen.of(profile, prev), present(diff(prev, next)))` must
  * equal `expected(profile, next)`, and any sequence outside the vocabulary is an error (the sanitisation law of principle 9). Vocabulary:
  * printable text (segmented into clusters and written with the profile's widths under DECAWM pending wrap), CR, LF (at the scroll
  * region's bottom margin the region scrolls, its top row reaching the modelled scrollback only when the region starts at row one
  * and the screen is normal, the verified xterm and kitty rule; at the screen bottom the normal screen scrolls into the scrollback
  * while the alternate screen refuses), Cursor Position, Cursor Horizontal Absolute (the writer's join break, 2026-08-31, and its R3a placements, 2026-09-24), Erase in Display 0 and 2, Erase in Line 0, DECSC and DECRC (`ESC 7`,
  * `ESC 8`, restoring the cursor, the wrap flag, and the style, or homing with defaults when nothing was saved), DECSTBM with and
  * without margins (both home the cursor, invalid margins refused), SGR (attributes, `4:n`, the named, indexed, and RGB colours, the
  * underline colour), the private modes 25, 1049, 1000, 1002, 1003, 1005, 1006, 1015, 1016, 2004, 1004, and 2026, and the kitty keyboard
  * protocol's push `CSI > flags u` and pop `CSI < n u` over separate stacks for the normal and alternate screens (M3d, the
  * specification's rule: a pop that empties a stack leaves no flags).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object TerminalModel {

  /** How a terminal deviates: the width it gives a VS16 cluster (fact 6 of the comparison report) - iTerm2 3.7.3 draws text-default
    * VS16 sequences one column wide on the alternate screen and two on the normal screen by default (issue 42) - and whether mode 1049
    * clears the alternate screen on entry (fact 13). Ambiguous width waits for the East Asian Ambiguous property in the tables (M4).
    */
  final case class QuirkProfile(vs16Width: GlyphWidth, clearsOnAlternateEntry: Boolean) derives Eq, Show, Hash

  object QuirkProfile {

    /** VS16 two columns wide, clears on entry: the terminal the bundled tables assume. */
    val default: QuirkProfile = QuirkProfile(GlyphWidth.Two, true)

    /** The four combinations. */
    val all: List[QuirkProfile] =
      for {
        width  <- List(GlyphWidth.Two, GlyphWidth.One)
        clears <- List(true, false)
      } yield QuirkProfile(width, clears)

  }

  /** The bundled tables with the profile's VS16 width. */
  final class ProfileWidthPolicy(vs16Width: GlyphWidth) extends WidthPolicy {

    /* the method lives in the class body because it implements the WidthPolicy trait member */
    /** The profile's VS16 width when the code point after the base is U+FE0F, the table width otherwise. */
    override def clusterWidth(s: String, start: Int, end: Int): Int =
      if (start < 0 || end > s.length || start >= end) {
        0
      } else {
        val base = s.codePointAt(start)
        val next = start + Character.charCount(base)
        if (next < end && s.codePointAt(next) === 0xfe0f) vs16Width.columns else WidthPolicy.default.clusterWidth(s, start, end)
      }

  }

  /** The policy of the profile. */
  def policyOf(profile: QuirkProfile): WidthPolicy = new ProfileWidthPolicy(profile.vs16Width)

  /** The active scroll region as 0-based inclusive screen rows (the oracle's own value, top at or below bottom). */
  final case class Region(top: NonNegInt, bottom: NonNegInt) derives Eq, Show, Hash

  /** What DECSC saved: the cursor, the wrap flag, and the style. */
  final case class SavedCursor(cursor: Position, pendingWrap: Boolean, style: CellStyle) derives Eq, Show, Hash

  /** The modelled terminal: the screen cells (over the profile's policy), the cursor with the DECAWM pending-wrap flag, the cursor
    * visibility, the current SGR state, whether the alternate screen is active, the private modes currently set, the scroll region,
    * the DECSC save, the scrollback rows that scrolled off the top of the normal screen (oldest first), and the kitty keyboard stacks
    * of the normal and the alternate screen (the head is the current flags, an empty stack means none).
    */
  final case class Screen(
    buffer: Buffer,
    cursor: Position,
    pendingWrap: Boolean,
    cursorVisible: Boolean,
    style: CellStyle,
    alternate: Boolean,
    modes: Set[Int],
    region: Option[Region],
    saved: Option[SavedCursor],
    scrollback: Vector[Vector[Cell]],
    keyboardMain: List[Int],
    keyboardAlternate: List[Int],
  ) derives Eq,
        Show

  object Screen {

    /** A blank screen of the size: cursor at the origin and visible, default style, the normal screen, no mode, no region, no save,
      * an empty scrollback.
      */
    def blank(profile: QuirkProfile, size: Size): Screen =
      Screen(
        Buffer.emptyWith(policyOf(profile), Rect.sized(size)),
        Position.origin,
        false,
        true,
        CellStyle.default,
        false,
        Set.empty[Int],
        none[Region],
        none[SavedCursor],
        Vector.empty[Vector[Cell]],
        List.empty[Int],
        List.empty[Int],
      )

    /** The screen showing [[expected]] of the buffer, cursor at the origin and hidden, default style. */
    def of(profile: QuirkProfile, buffer: Buffer): Screen = ofWith(profile, identity, buffer)

    /** [[of]] with every style passed through `normalise` (the writer's capability normalisation). */
    def ofWith(profile: QuirkProfile, normalise: CellStyle => CellStyle, buffer: Buffer): Screen =
      Screen(
        expectedWith(profile, normalise, buffer),
        Position.origin,
        false,
        false,
        CellStyle.default,
        false,
        Set.empty[Int],
        none[Region],
        none[SavedCursor],
        Vector.empty[Vector[Cell]],
        List.empty[Int],
        List.empty[Int],
      )

    /** The screen showing `part` placed at its own area on a blank terminal of the given size, cursor at the origin and hidden (the
      * viewport laws of the inline mode).
      */
    def ofTerminal(profile: QuirkProfile, normalise: CellStyle => CellStyle, terminal: Size, part: Buffer): Screen =
      Screen(
        placed(profile, normalise, terminal, part),
        Position.origin,
        false,
        false,
        CellStyle.default,
        false,
        Set.empty[Int],
        none[Region],
        none[SavedCursor],
        Vector.empty[Vector[Cell]],
        List.empty[Int],
        List.empty[Int],
      )

  }

  /** Why the model refused the output. */
  enum ModelError derives Eq, Show, Hash {
    case Unknown(sequence: String)
    case Scrolled
    case OutsideViewport(position: Position)
  }

  private val Named: IArray[Color] = IArray(
    Color.Black,
    Color.Red,
    Color.Green,
    Color.Yellow,
    Color.Blue,
    Color.Magenta,
    Color.Cyan,
    Color.Gray,
    Color.DarkGray,
    Color.LightRed,
    Color.LightGreen,
    Color.LightYellow,
    Color.LightBlue,
    Color.LightMagenta,
    Color.LightCyan,
    Color.White,
  )

  private val KnownModes: Set[Int] = Set(25, 1049, 1000, 1002, 1003, 1005, 1006, 1015, 1016, 2004, 1004, 2026)

  private val Esc: Char = '\u001b'

  private val Del: Char = '\u007f'

  private val C1First: Char = '\u0080'

  private val C1Last: Char = '\u009f'

  /** What the terminal should show for the buffer under the profile: every cell as it is, except a VS16 glyph of width two on a
    * profile that draws VS16 one column wide, which becomes the glyph in one column and a blank in its style in the shadow column.
    */
  def expected(profile: QuirkProfile, buffer: Buffer): Buffer = expectedWith(profile, identity, buffer)

  /** `part`'s glyphs written at their absolute area positions on a blank terminal-sized buffer, every style through `normalise` (the
    * projection the inline viewport laws compare against).
    */
  def placed(profile: QuirkProfile, normalise: CellStyle => CellStyle, terminal: Size, part: Buffer): Buffer =
    Buffer
      .emptyWith(policyOf(profile), Rect.sized(terminal))
      .draw(canvas => expectedLoop(profile, normalise, part.cells, 0, part.area.width.value, part.area, canvas))

  /** [[expected]] with every style passed through `normalise`: what the terminal shows once the writer has degraded the styles to the
    * capabilities (the same function the writer uses, so the round-trip law holds for any capabilities).
    */
  def expectedWith(profile: QuirkProfile, normalise: CellStyle => CellStyle, buffer: Buffer): Buffer = {
    val width = buffer.area.width.value
    Buffer
      .emptyWith(policyOf(profile), buffer.area)
      .draw(canvas => expectedLoop(profile, normalise, buffer.cells, 0, width, buffer.area, canvas))
  }

  @tailrec
  private def expectedLoop(
    profile: QuirkProfile,
    normalise: CellStyle => CellStyle,
    cells: IArray[Cell],
    i: Int,
    width: Int,
    area: Rect,
    canvas: Canvas,
  ): Unit =
    if (i >= cells.length) {
      ()
    } else {
      val x = if (width === 0) 0 else i % width
      val y = if (width === 0) 0 else i / width
      cells(i) match {
        case Cell.Glyph(symbol, glyphWidth, rawStyle) =>
          val style    = normalise(rawStyle)
          val position = Position(NonNegInts.clamp(area.x.value.toLong + x.toLong), NonNegInts.clamp(area.y.value.toLong + y.toLong))
          canvas.putString(position, symbol.value, style.toStyle)
          if (glyphWidth === GlyphWidth.Two && profile.vs16Width === GlyphWidth.One && symbol.isVs16Sequence) {
            canvas.fill(Rect(NonNegInts.clamp(position.x.value.toLong + 1L), position.y, NonNegInt(1), NonNegInt(1)), " ", style.toStyle)
          } else {
            ()
          }
        case Cell.Continuation(_) => ()
      }
      expectedLoop(profile, normalise, cells, i + 1, width, area, canvas)
    }

  /** Interprets the writer's output over the screen, `Left` on the first sequence outside the vocabulary, a scroll, or a cursor move
    * outside the viewport.
    */
  def interpret(profile: QuirkProfile, screen: Screen, output: String): Either[ModelError, Screen] = loop(profile, screen, output, 0)

  @tailrec
  private def loop(profile: QuirkProfile, screen: Screen, out: String, i: Int): Either[ModelError, Screen] =
    if (i >= out.length) {
      screen.asRight[ModelError]
    } else {
      val c = out.charAt(i)
      if (c === Esc) {
        if (i + 1 < out.length && out.charAt(i + 1) === '[') {
          csi(profile, screen, out, i + 2) match {
            case Left(error) => error.asLeft[Screen]
            case Right((next, updated)) => loop(profile, updated, out, next)
          }
        } else if (i + 1 < out.length && out.charAt(i + 1) === '7') {
          loop(profile, screen.copy(saved = SavedCursor(screen.cursor, screen.pendingWrap, screen.style).some), out, i + 2)
        } else if (i + 1 < out.length && out.charAt(i + 1) === '8') {
          val restored = screen.saved match {
            case Some(saved) => screen.copy(cursor = saved.cursor, pendingWrap = saved.pendingWrap, style = saved.style)
            case None => screen.copy(cursor = Position.origin, pendingWrap = false, style = CellStyle.default)
          }
          loop(profile, restored, out, i + 2)
        } else {
          ModelError.Unknown(out.substring(i, math.min(out.length, i + 2))).asLeft[Screen]
        }
      } else if (c === '\r') {
        loop(profile, screen.copy(cursor = Position(NonNegInt(0), screen.cursor.y), pendingWrap = false), out, i + 1)
      } else if (c === '\n') {
        lineFeed(screen) match {
          case Left(error) => error.asLeft[Screen]
          case Right(updated) => loop(profile, updated, out, i + 1)
        }
      } else if (isControl(c)) {
        ModelError.Unknown(c.toInt.toHexString).asLeft[Screen]
      } else {
        val end = runEnd(out, i)
        writeText(screen, out.substring(i, end)) match {
          case Left(error) => error.asLeft[Screen]
          case Right(updated) => loop(profile, updated, out, end)
        }
      }
    }

  private def isControl(c: Char): Boolean = c < ' ' || c === Del || (c >= C1First && c <= C1Last)

  @tailrec
  private def runEnd(out: String, i: Int): Int = if (i >= out.length || isControl(out.charAt(i))) i else runEnd(out, i + 1)

  private def lineFeed(screen: Screen): Either[ModelError, Screen] = {
    val height = screen.buffer.area.height.value
    val y      = screen.cursor.y.value
    screen.region match {
      case Some(region) if y === region.bottom.value => scrollRegionUp(screen, region).asRight[ModelError]
      case Some(_) | None =>
        if (y + 1 >= height) {
          if (screen.alternate || screen.region.isDefined) {
            (ModelError.Scrolled: ModelError).asLeft[Screen]
          } else {
            scrollRegionUp(screen, Region(NonNegInt(0), NonNegInts.clamp(height.toLong - 1L))).asRight[ModelError]
          }
        } else {
          screen.copy(cursor = Position(screen.cursor.x, NonNegInts.clamp(y.toLong + 1L)), pendingWrap = false).asRight[ModelError]
        }
    }
  }

  /** The region's rows shifted up by one with a blank bottom row, the region's top row appended to the scrollback only when the
    * region starts at row 0 on the normal screen (the verified xterm and kitty rule), the cursor kept, the wrap flag cleared.
    */
  private def scrollRegionUp(screen: Screen, region: Region): Screen = {
    val rows     = screen.buffer.rows
    val top      = region.top.value
    val bottom   = region.bottom.value
    val width    = screen.buffer.area.width.value
    val blankRow = Vector.fill(width)(Cell.blank)
    val shifted  = Vector.tabulate(rows.length) { i =>
      if (i >= top && i < bottom) rows.lift(i + 1).getOrElse(blankRow)
      else if (i === bottom) blankRow
      else rows.lift(i).getOrElse(blankRow)
    }
    val kept     =
      if (top === 0 && !screen.alternate) rows.lift(top).fold(screen.scrollback)(screen.scrollback :+ _) else screen.scrollback
    screen.copy(buffer = rebuild(screen.buffer, shifted), scrollback = kept, pendingWrap = false)
  }

  private def rebuild(buffer: Buffer, rows: Vector[Vector[Cell]]): Buffer = {
    val updates = rows.zipWithIndex.flatMap {
      case (row, y) =>
        row.zipWithIndex.map {
          case (cell, x) => CellUpdate(Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)), cell)
        }
    }
    Buffer.applyUpdates(Buffer.emptyWith(buffer.policy, buffer.area), updates)
  }

  /** Cells from the cursor (inclusive) to the end of the screen blanked (ED 0). */
  private def eraseBelow(screen: Screen): Screen = {
    val width = screen.buffer.area.width.value
    if (width === 0) {
      screen
    } else {
      val start   = screen.cursor.y.value * width + screen.cursor.x.value
      val updates = Vector.range(start, screen.buffer.cells.length).map { j =>
        CellUpdate(Position(NonNegInts.clamp((j % width).toLong), NonNegInts.clamp((j / width).toLong)), Cell.blank)
      }
      screen.copy(buffer = Buffer.applyUpdates(screen.buffer, updates))
    }
  }

  /** Cells from the cursor (inclusive) to the end of its row blanked (EL 0). */
  private def eraseLineEnd(screen: Screen): Screen = {
    val width = screen.buffer.area.width.value
    if (width === 0) {
      screen
    } else {
      val y       = screen.cursor.y.value
      val updates = Vector.range(screen.cursor.x.value, width).map { x =>
        CellUpdate(Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)), Cell.blank)
      }
      screen.copy(buffer = Buffer.applyUpdates(screen.buffer, updates))
    }
  }

  final private case class Placement(cluster: String, position: Position)

  private def writeText(screen: Screen, text: String): Either[ModelError, Screen] = {
    val boundaries = Graphemes.boundaries(text)
    val width      = screen.buffer.area.width.value
    val height     = screen.buffer.area.height.value
    val policy     = screen.buffer.policy
    place(
      text,
      boundaries,
      0,
      policy,
      width,
      height,
      screen.cursor.x.value,
      screen.cursor.y.value,
      screen.pendingWrap,
      Vector.empty[Placement],
    )
      .map {
        case (placements, x, y, pending) =>
          val style = screen.style.toStyle
          val drawn = screen.buffer.draw(canvas => placements.foreach(p => canvas.putString(p.position, p.cluster, style)))
          screen.copy(buffer = drawn, cursor = Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong)), pendingWrap = pending)
      }
  }

  @tailrec
  private def place(
    text: String,
    boundaries: Array[Int],
    i: Int,
    policy: WidthPolicy,
    width: Int,
    height: Int,
    x: Int,
    y: Int,
    pending: Boolean,
    acc: Vector[Placement],
  ): Either[ModelError, (Vector[Placement], Int, Int, Boolean)] =
    if (i + 1 >= boundaries.length) {
      (acc, x, y, pending).asRight[ModelError]
    } else {
      val start = boundaries(i)
      val end   = boundaries(i + 1)
      val w     = math.min(2, policy.clusterWidth(text, start, end))
      if (w === 0) {
        place(text, boundaries, i + 1, policy, width, height, x, y, pending, acc)
      } else {
        val wraps = pending || (w === 2 && x + 1 >= width)
        if (width === 0 || (wraps && y + 1 >= height)) {
          (ModelError.Scrolled: ModelError).asLeft[(Vector[Placement], Int, Int, Boolean)]
        } else {
          val px      = if (wraps) 0 else x
          val py      = if (wraps) y + 1 else y
          val nx      = px + w
          val next    = acc :+ Placement(text.substring(start, end), Position(NonNegInts.clamp(px.toLong), NonNegInts.clamp(py.toLong)))
          val wrapped = nx >= width
          place(text, boundaries, i + 1, policy, width, height, if (wrapped) width - 1 else nx, py, wrapped, next)
        }
      }
    }

  /** Parses the CSI sequence starting after `ESC [` and applies it, returning the index after the final byte. */
  private def csi(profile: QuirkProfile, screen: Screen, out: String, start: Int): Either[ModelError, (Int, Screen)] = {
    val end = finalByte(out, start)
    if (end >= out.length) {
      ModelError.Unknown(out.substring(start - 2)).asLeft[(Int, Screen)]
    } else {
      val body = out.substring(start, end)
      val fin  = out.charAt(end)
      dispatch(profile, screen, body, fin).map(updated => (end + 1, updated))
    }
  }

  @tailrec
  private def finalByte(out: String, i: Int): Int =
    if (i >= out.length) {
      i
    } else {
      val c = out.charAt(i)
      if (c >= '@' && c <= '~') i else if (c >= ' ' && c <= '?') finalByte(out, i + 1) else out.length
    }

  private def dispatch(profile: QuirkProfile, screen: Screen, body: String, fin: Char): Either[ModelError, Screen] = fin match {
    case 'H' => cup(screen, body)
    case 'G' => cha(screen, body)
    case 'J' =>
      if (body === "2") screen.copy(buffer = Buffer.emptyWith(screen.buffer.policy, screen.buffer.area)).asRight[ModelError]
      else if (body.isEmpty || body === "0") eraseBelow(screen).asRight[ModelError]
      else unknown(body, fin)
    case 'K' =>
      if (body.isEmpty || body === "0") eraseLineEnd(screen).asRight[ModelError] else unknown(body, fin)
    case 'r' =>
      if (body.isEmpty) {
        screen.copy(region = none[Region], cursor = Position.origin, pendingWrap = false).asRight[ModelError]
      } else {
        val parts = body.split(";", -1).toVector
        (parts.headOption.flatMap(parseNumber), parts.lift(1).flatMap(parseNumber)) match {
          case (Some(top), Some(bottom)) if top >= 1 && top < bottom && bottom <= screen.buffer.area.height.value =>
            screen
              .copy(
                region = Region(NonNegInts.clamp(top.toLong - 1L), NonNegInts.clamp(bottom.toLong - 1L)).some,
                cursor = Position.origin,
                pendingWrap = false,
              )
              .asRight[ModelError]
          case (Some(_), Some(_)) | (Some(_), None) | (None, Some(_)) | (None, None) => unknown(body, fin)
        }
      }
    case 'm' => sgr(splitParams(body), screen.style).map(style => screen.copy(style = style))
    case 'u' =>
      if (body.startsWith(">")) keyboard(screen, parseNumber(body.substring(1)).getOrElse(0) :: currentKeyboard(screen)).asRight[ModelError]
      else if (body.startsWith("<"))
        keyboard(screen, currentKeyboard(screen).drop(parseNumber(body.substring(1)).getOrElse(1))).asRight[ModelError]
      else unknown(body, fin)
    case 'h' | 'l' =>
      if (body.startsWith("?")) {
        parseNumber(body.substring(1)) match {
          case Some(mode) if KnownModes.contains(mode) => privateMode(profile, screen, mode, fin === 'h').asRight[ModelError]
          case Some(_) | None => unknown(body, fin)
        }
      } else {
        unknown(body, fin)
      }
    case _ => unknown(body, fin)
  }

  private def unknown(body: String, fin: Char): Either[ModelError, Screen] = ModelError.Unknown("CSI " + body + fin.toString).asLeft[Screen]

  private def currentKeyboard(screen: Screen): List[Int] = if (screen.alternate) screen.keyboardAlternate else screen.keyboardMain

  private def keyboard(screen: Screen, stack: List[Int]): Screen =
    if (screen.alternate) screen.copy(keyboardAlternate = stack) else screen.copy(keyboardMain = stack)

  /** The kitty keyboard flags of the active screen: its stack's head, 0 when the stack is empty. */
  def keyboardFlags(screen: Screen): Int = currentKeyboard(screen).headOption.getOrElse(0)

  /** Cursor Horizontal Absolute (the writer's join break and R3a placements in a printed row): the column within the current row, an error beyond the
    * width, the pending wrap cleared.
    */
  private def cha(screen: Screen, body: String): Either[ModelError, Screen] = {
    val column = if (body.isEmpty) 1 else parseNumber(body).getOrElse(1)
    val x      = math.max(0, column - 1)
    val target = Position(NonNegInts.clamp(x.toLong), screen.cursor.y)
    if (x >= screen.buffer.area.width.value) ModelError.OutsideViewport(target).asLeft[Screen]
    else screen.copy(cursor = target, pendingWrap = false).asRight[ModelError]
  }

  private def cup(screen: Screen, body: String): Either[ModelError, Screen] = {
    val parts  = if (body.isEmpty) Vector.empty[String] else body.split(";", -1).toVector
    val row    = parts.headOption.flatMap(parseNumber).getOrElse(1)
    val column = parts.lift(1).flatMap(parseNumber).getOrElse(1)
    val y      = math.max(0, row - 1)
    val x      = math.max(0, column - 1)
    val target = Position(NonNegInts.clamp(x.toLong), NonNegInts.clamp(y.toLong))
    if (y >= screen.buffer.area.height.value || x >= screen.buffer.area.width.value) ModelError.OutsideViewport(target).asLeft[Screen]
    else screen.copy(cursor = target, pendingWrap = false).asRight[ModelError]
  }

  private def privateMode(profile: QuirkProfile, screen: Screen, mode: Int, set: Boolean): Screen = mode match {
    case 25 => screen.copy(cursorVisible = set)
    case 1049 =>
      if (set) {
        val buffer = if (profile.clearsOnAlternateEntry) Buffer.emptyWith(screen.buffer.policy, screen.buffer.area) else screen.buffer
        screen.copy(alternate = true, buffer = buffer)
      } else {
        screen.copy(alternate = false)
      }
    case _ => screen.copy(modes = if (set) screen.modes + mode else screen.modes - mode)
  }

  private def splitParams(body: String): List[String] = if (body.isEmpty) List("0") else body.split(";", -1).toList

  @tailrec
  private def sgr(params: List[String], style: CellStyle): Either[ModelError, CellStyle] = params match {
    case Nil => style.asRight[ModelError]
    case "38" :: rest =>
      extendedColor(rest, style, (s, c) => s.copy(fg = c)) match {
        case Left(error) => error.asLeft[CellStyle]
        case Right((remaining, updated)) => sgr(remaining, updated)
      }
    case "48" :: rest =>
      extendedColor(rest, style, (s, c) => s.copy(bg = c)) match {
        case Left(error) => error.asLeft[CellStyle]
        case Right((remaining, updated)) => sgr(remaining, updated)
      }
    case "58" :: rest =>
      extendedColor(rest, style, (s, c) => s.copy(underlineColor = c)) match {
        case Left(error) => error.asLeft[CellStyle]
        case Right((remaining, updated)) => sgr(remaining, updated)
      }
    case param :: rest =>
      simpleSgr(param, style) match {
        case Some(updated) => sgr(rest, updated)
        case None => ModelError.Unknown("SGR " + param).asLeft[CellStyle]
      }
  }

  private def simpleSgr(param: String, style: CellStyle): Option[CellStyle] = param match {
    case "" | "0" => CellStyle.default.some
    case "1" => add(style, Modifier.Bold)
    case "2" => add(style, Modifier.Dim)
    case "3" => add(style, Modifier.Italic)
    case "4" | "4:1" => style.copy(underline = UnderlineStyle.Single).some
    case "4:0" | "24" => style.copy(underline = UnderlineStyle.None).some
    case "4:2" => style.copy(underline = UnderlineStyle.Double).some
    case "4:3" => style.copy(underline = UnderlineStyle.Curly).some
    case "4:4" => style.copy(underline = UnderlineStyle.Dotted).some
    case "4:5" => style.copy(underline = UnderlineStyle.Dashed).some
    case "5" => add(style, Modifier.SlowBlink)
    case "6" => add(style, Modifier.RapidBlink)
    case "7" => add(style, Modifier.Reversed)
    case "8" => add(style, Modifier.Hidden)
    case "9" => add(style, Modifier.CrossedOut)
    case "22" => style.copy(modifiers = style.modifiers.remove(Modifier.Bold).remove(Modifier.Dim)).some
    case "23" => remove(style, Modifier.Italic)
    case "25" => style.copy(modifiers = style.modifiers.remove(Modifier.SlowBlink).remove(Modifier.RapidBlink)).some
    case "27" => remove(style, Modifier.Reversed)
    case "28" => remove(style, Modifier.Hidden)
    case "29" => remove(style, Modifier.CrossedOut)
    case "39" => style.copy(fg = Color.Reset).some
    case "49" => style.copy(bg = Color.Reset).some
    case "59" => style.copy(underlineColor = Color.Reset).some
    case other =>
      parseNumber(other).flatMap { n =>
        if (n >= 30 && n <= 37) style.copy(fg = Named(n - 30)).some
        else if (n >= 40 && n <= 47) style.copy(bg = Named(n - 40)).some
        else if (n >= 90 && n <= 97) style.copy(fg = Named(n - 90 + 8)).some
        else if (n >= 100 && n <= 107) style.copy(bg = Named(n - 100 + 8)).some
        else none[CellStyle]
      }
  }

  private def add(style: CellStyle, modifier: Modifier): Option[CellStyle] = style.copy(modifiers = style.modifiers.add(modifier)).some

  private def remove(style: CellStyle, modifier: Modifier): Option[CellStyle] =
    style.copy(modifiers = style.modifiers.remove(modifier)).some

  /** `5;n` or `2;r;g;b` after `38`, `48`, or `58`. */
  private def extendedColor(
    params: List[String],
    style: CellStyle,
    set: (CellStyle, Color) => CellStyle,
  ): Either[ModelError, (List[String], CellStyle)] = params match {
    case "5" :: n :: rest =>
      parseNumber(n).flatMap(i => Color.indexedFrom(i).toOption) match {
        case Some(color) => (rest, set(style, color)).asRight[ModelError]
        case None => ModelError.Unknown("SGR 5;" + n).asLeft[(List[String], CellStyle)]
      }
    case "2" :: r :: g :: b :: rest =>
      (parseNumber(r), parseNumber(g), parseNumber(b)) match {
        case (Some(red), Some(green), Some(blue)) =>
          Color.rgbFrom(red, green, blue) match {
            case Right(color) => (rest, set(style, color)).asRight[ModelError]
            case Left(_) => ModelError.Unknown(s"SGR 2;$r;$g;$b").asLeft[(List[String], CellStyle)]
          }
        case (_, _, _) => ModelError.Unknown(s"SGR 2;$r;$g;$b").asLeft[(List[String], CellStyle)]
      }
    case other => ModelError.Unknown("SGR " + other.mkString(";")).asLeft[(List[String], CellStyle)]
  }

  private def parseNumber(s: String): Option[Int] =
    if (s.nonEmpty && s.length <= 7 && s.forall(c => c >= '0' && c <= '9')) s.toInt.some else none[Int]

}
