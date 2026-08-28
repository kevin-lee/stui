package stui.terminal.ansi

import stui.core.capability.ColorProfile
import stui.core.style.Color

import scala.annotation.tailrec

/** Colour degradation at emission time (design doc 3.6 and 7.3): widgets author in rich colour, the writer maps each colour to what the
  * terminal's profile can show. `Rgb` goes to the nearest entry of the xterm 256-colour cube or gray ramp, and `Rgb` or `Indexed` to the
  * nearest of the sixteen named colours by squared Euclidean distance on the xterm default palette (a data table, a perceptual distance
  * is a later refinement). Named colours and `Reset` are never changed except under `Mono`, which drops every colour.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object ColorDegradation {

  /** One of the sixteen named colours with its xterm default red, green, and blue values. */
  final case class PaletteEntry(color: Color, red: Int, green: Int, blue: Int)

  /** Index 0 to 15 in SGR order: the xterm default palette. */
  val palette: IArray[PaletteEntry] = IArray(
    PaletteEntry(Color.Black, 0, 0, 0),
    PaletteEntry(Color.Red, 205, 0, 0),
    PaletteEntry(Color.Green, 0, 205, 0),
    PaletteEntry(Color.Yellow, 205, 205, 0),
    PaletteEntry(Color.Blue, 0, 0, 238),
    PaletteEntry(Color.Magenta, 205, 0, 205),
    PaletteEntry(Color.Cyan, 0, 205, 205),
    PaletteEntry(Color.Gray, 229, 229, 229),
    PaletteEntry(Color.DarkGray, 127, 127, 127),
    PaletteEntry(Color.LightRed, 255, 0, 0),
    PaletteEntry(Color.LightGreen, 0, 255, 0),
    PaletteEntry(Color.LightYellow, 255, 255, 0),
    PaletteEntry(Color.LightBlue, 92, 92, 255),
    PaletteEntry(Color.LightMagenta, 255, 0, 255),
    PaletteEntry(Color.LightCyan, 0, 255, 255),
    PaletteEntry(Color.White, 255, 255, 255),
  )

  private val CubeLevels: IArray[Int] = IArray(0, 95, 135, 175, 215, 255)

  /** The colour the profile can show for the given one. */
  def degrade(profile: ColorProfile, color: Color): Color = profile match {
    case ColorProfile.Truecolor => color
    case ColorProfile.Mono => Color.Reset
    case ColorProfile.Ansi256 =>
      color match {
        case Color.Rgb(red, green, blue) => indexed(nearest256(red.value, green.value, blue.value))
        case Color.Indexed(_) | Color.Reset | Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta |
            Color.Cyan | Color.Gray | Color.DarkGray | Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue |
            Color.LightMagenta | Color.LightCyan | Color.White =>
          color
      }
    case ColorProfile.Ansi16 =>
      color match {
        case Color.Rgb(red, green, blue) => nearest16(red.value, green.value, blue.value)
        case Color.Indexed(index) =>
          if (index.value < palette.length) {
            palette(index.value).color
          } else {
            rgbOf(index.value) match {
              case (red, green, blue) => nearest16(red, green, blue)
            }
          }
        case Color.Reset | Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan | Color.Gray |
            Color.DarkGray | Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta |
            Color.LightCyan | Color.White =>
          color
      }
  }

  /** The 256-colour table entry nearest to the components: the cube entry (levels 0, 95, 135, 175, 215, 255) or the gray ramp entry
    * (8 to 238 in steps of 10), whichever is closer, the cube on ties.
    */
  def nearest256(red: Int, green: Int, blue: Int): Int = {
    val qr        = cubeLevel(red)
    val qg        = cubeLevel(green)
    val qb        = cubeLevel(blue)
    val cubeIndex = 16 + 36 * qr + 6 * qg + qb
    val cubeDist  = distance(red, green, blue, CubeLevels(qr), CubeLevels(qg), CubeLevels(qb))
    val average   = (red + green + blue) / 3
    val gi        = math.max(0, math.min(23, (average - 8) / 10))
    val gray      = 8 + 10 * gi
    val grayDist  = distance(red, green, blue, gray, gray, gray)
    if (grayDist < cubeDist) 232 + gi else cubeIndex
  }

  /** The named colour nearest to the components on the xterm default palette, the first on ties. */
  def nearest16(red: Int, green: Int, blue: Int): Color = nearestLoop(red, green, blue, 1, 0, distance(red, green, blue, 0, 0, 0))

  @tailrec
  private def nearestLoop(red: Int, green: Int, blue: Int, i: Int, best: Int, bestDist: Long): Color =
    if (i >= palette.length) {
      palette(best).color
    } else {
      val entry = palette(i)
      val d     = distance(red, green, blue, entry.red, entry.green, entry.blue)
      if (d < bestDist) nearestLoop(red, green, blue, i + 1, i, d) else nearestLoop(red, green, blue, i + 1, best, bestDist)
    }

  /** The red, green, and blue values of a 256-colour table index (0 to 255): the palette, the cube, or the gray ramp. */
  def rgbOf(index: Int): (Int, Int, Int) =
    if (index < palette.length) {
      val entry = palette(index)
      (entry.red, entry.green, entry.blue)
    } else if (index < 232) {
      val i = index - 16
      (CubeLevels(i / 36), CubeLevels((i / 6) % 6), CubeLevels(i % 6))
    } else {
      val gray = 8 + 10 * (index - 232)
      (gray, gray, gray)
    }

  private def cubeLevel(component: Int): Int = if (component < 48) 0 else if (component < 115) 1 else (component - 35) / 40

  private def distance(r1: Int, g1: Int, b1: Int, r2: Int, g2: Int, b2: Int): Long = {
    val dr = (r1 - r2).toLong
    val dg = (g1 - g2).toLong
    val db = (b1 - b2).toLong
    dr * dr + dg * dg + db * db
  }

  /** `Indexed` for a table index within 0 to 255 (the fold is unreachable, the callers stay in range). */
  private def indexed(index: Int): Color = Color.indexedFrom(index).fold(_ => Color.Reset, identity)

}
