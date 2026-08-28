package stui.terminal.ansi

import cats.syntax.all.*
import stui.core.capability.{Capabilities, ColorProfile}
import stui.core.style.{CellStyle, Color, Modifier, UnderlineStyle}

import scala.annotation.tailrec

/** Select Graphic Rendition (SGR) as the writer emits it (design doc 7.1, rule R5): a style is first normalised to what the
  * capabilities can show, then only the parameters that differ from the tracked style are emitted in one `CSI ... m`.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object Sgr {

  /** The SGR index (0 to 15) of a named colour, `None` for `Reset`, `Rgb`, and `Indexed`. */
  def colorIndex(color: Color): Option[Int] = colorIndexLoop(color, 0)

  @tailrec
  private def colorIndexLoop(color: Color, i: Int): Option[Int] =
    if (i >= ColorDegradation.palette.length) none[Int]
    else if (ColorDegradation.palette(i).color === color) i.some
    else colorIndexLoop(color, i + 1)

  /** The style as the terminal will show it: colours degraded to the profile, the underline colour dropped unless extended underlines
    * and at least 256 colours are supported, every underline style other than `None` folded to `Single` without extended underlines.
    */
  def normalise(capabilities: Capabilities, style: CellStyle): CellStyle = {
    val profile                 = capabilities.colors
    val underlineColorSupported =
      capabilities.extendedUnderline && (profile === ColorProfile.Truecolor || profile === ColorProfile.Ansi256)
    val underline               =
      if (capabilities.extendedUnderline) {
        style.underline
      } else {
        style.underline match {
          case UnderlineStyle.None => UnderlineStyle.None
          case UnderlineStyle.Single | UnderlineStyle.Double | UnderlineStyle.Curly | UnderlineStyle.Dotted | UnderlineStyle.Dashed =>
            UnderlineStyle.Single
        }
      }
    CellStyle(
      ColorDegradation.degrade(profile, style.fg),
      ColorDegradation.degrade(profile, style.bg),
      if (underlineColorSupported) underlineColorForm(ColorDegradation.degrade(profile, style.underlineColor)) else Color.Reset,
      underline,
      style.modifiers,
    )
  }

  /** A named underline colour as `Indexed` of its palette index, because SGR 58 has no named form and the terminal cannot tell the
    * two apart (so the tracked style matches what the terminal knows).
    */
  private def underlineColorForm(color: Color): Color = color match {
    case Color.Reset | Color.Rgb(_, _, _) | Color.Indexed(_) => color
    case Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan | Color.Gray | Color.DarkGray |
        Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta | Color.LightCyan | Color.White =>
      colorIndex(color).flatMap(i => Color.indexedFrom(i).toOption).getOrElse(Color.Reset)
  }

  /** The parameters that turn `from` into `to`, both already normalised, as one `CSI params m`, or the empty string when equal: the
    * attribute-off codes for removed modifiers (`22` for bold and dim, `23`, `25` for both blinks, `27`, `28`, `29`), the re-adds a
    * shared off code forces (dim kept when bold goes, and the three mirror cases), the attribute-on codes for added modifiers, the
    * underline (`24`, `4:n` with extended underlines, else `4`), the foreground (`39`, `30`-`37`, `90`-`97`, `38;5;n`, `38;2;r;g;b`), the
    * background (`49`, `40`-`47`, `100`-`107`, `48;5;n`, `48;2;r;g;b`), and the underline colour (`59`, `58;5;n`, `58;2;r;g;b`).
    */
  def delta(from: CellStyle, to: CellStyle, capabilities: Capabilities): String =
    if (from === to) {
      ""
    } else {
      val removed   = from.modifiers.diff(to.modifiers)
      val added     = to.modifiers.diff(from.modifiers)
      val offCodes  = Modifier.all.withFilter(removed.contains).map(offCode).distinct
      val readds    = List(
        Option.when(removed.contains(Modifier.Bold) && kept(from, to, Modifier.Dim))("2"),
        Option.when(removed.contains(Modifier.Dim) && kept(from, to, Modifier.Bold))("1"),
        Option.when(removed.contains(Modifier.SlowBlink) && kept(from, to, Modifier.RapidBlink))("6"),
        Option.when(removed.contains(Modifier.RapidBlink) && kept(from, to, Modifier.SlowBlink))("5"),
      ).flatten
      val onCodes   = Modifier.all.withFilter(added.contains).map(onCode)
      val underline = if (to.underline =!= from.underline) List(underlineParam(to.underline, capabilities.extendedUnderline)) else Nil
      val fg        = if (to.fg =!= from.fg) List(colorParams(to.fg, 30, 90, 38, 39)) else Nil
      val bg        = if (to.bg =!= from.bg) List(colorParams(to.bg, 40, 100, 48, 49)) else Nil
      val ul        = if (to.underlineColor =!= from.underlineColor) List(underlineColorParams(to.underlineColor)) else Nil
      val params    = offCodes ++ readds ++ onCodes ++ underline ++ fg ++ bg ++ ul
      if (params.isEmpty) "" else Sequences.Csi + params.mkString(";") + "m"
    }

  private def kept(from: CellStyle, to: CellStyle, modifier: Modifier): Boolean =
    from.modifiers.contains(modifier) && to.modifiers.contains(modifier)

  private def offCode(modifier: Modifier): String = modifier match {
    case Modifier.Bold | Modifier.Dim => "22"
    case Modifier.Italic => "23"
    case Modifier.SlowBlink | Modifier.RapidBlink => "25"
    case Modifier.Reversed => "27"
    case Modifier.Hidden => "28"
    case Modifier.CrossedOut => "29"
  }

  private def onCode(modifier: Modifier): String = modifier match {
    case Modifier.Bold => "1"
    case Modifier.Dim => "2"
    case Modifier.Italic => "3"
    case Modifier.SlowBlink => "5"
    case Modifier.RapidBlink => "6"
    case Modifier.Reversed => "7"
    case Modifier.Hidden => "8"
    case Modifier.CrossedOut => "9"
  }

  private def underlineParam(underline: UnderlineStyle, extended: Boolean): String = underline match {
    case UnderlineStyle.None => "24"
    case UnderlineStyle.Single => if (extended) "4:1" else "4"
    case UnderlineStyle.Double => if (extended) "4:2" else "4"
    case UnderlineStyle.Curly => if (extended) "4:3" else "4"
    case UnderlineStyle.Dotted => if (extended) "4:4" else "4"
    case UnderlineStyle.Dashed => if (extended) "4:5" else "4"
  }

  private def colorParams(color: Color, base: Int, brightBase: Int, extended: Int, reset: Int): String = color match {
    case Color.Reset => reset.toString
    case Color.Rgb(red, green, blue) => s"${extended.toString};2;${red.value.toString};${green.value.toString};${blue.value.toString}"
    case Color.Indexed(index) => s"${extended.toString};5;${index.value.toString}"
    case Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan | Color.Gray | Color.DarkGray |
        Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta | Color.LightCyan | Color.White =>
      val index = colorIndex(color).getOrElse(0)
      if (index < 8) (base + index).toString else (brightBase + index - 8).toString
  }

  private def underlineColorParams(color: Color): String = color match {
    case Color.Reset => "59"
    case Color.Rgb(red, green, blue) => s"58;2;${red.value.toString};${green.value.toString};${blue.value.toString}"
    case Color.Indexed(index) => s"58;5;${index.value.toString}"
    case Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan | Color.Gray | Color.DarkGray |
        Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta | Color.LightCyan | Color.White =>
      s"58;5;${colorIndex(color).getOrElse(0).toString}"
  }

}
