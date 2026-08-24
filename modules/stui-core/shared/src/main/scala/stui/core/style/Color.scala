package stui.core.style

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.modules.cats.derivation.{CatsHash, CatsShow}
import refined4s.types.numeric.InlinedNumericMinMax

/** A colour as authored by a widget. The 16 named colours are the terminal's own palette (theme-defined), `Rgb` is truecolor, `Indexed`
  * is the 256-colour table, and `Reset` is the terminal default. Widgets always author in rich colour, degrading `Rgb` or `Indexed` for a
  * terminal that cannot show them is the writer's job (design doc 3.6).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
enum Color derives Eq, Show, Hash {
  case Reset
  case Black
  case Red
  case Green
  case Yellow
  case Blue
  case Magenta
  case Cyan
  case Gray
  case DarkGray
  case LightRed
  case LightGreen
  case LightYellow
  case LightBlue
  case LightMagenta
  case LightCyan
  case White
  case Rgb(red: Color.Channel, green: Color.Channel, blue: Color.Channel)
  case Indexed(index: Color.Index)
}

object Color {

  /** One truecolor component, 0 to 255. */
  type Channel = Channel.Type
  object Channel extends InlinedNumericMinMax[Int], CatsHash[Int], CatsShow[Int] {

    /** The darkest component value. */
    override inline def minValue: Int = 0

    /** The brightest component value. */
    override inline def maxValue: Int = 255
  }

  /** A 256-colour table index, 0 to 255. */
  type Index = Index.Type
  object Index extends InlinedNumericMinMax[Int], CatsHash[Int], CatsShow[Int] {

    /** The first table entry. */
    override inline def minValue: Int = 0

    /** The last table entry. */
    override inline def maxValue: Int = 255
  }

  /** Truecolor from literals, validated at compile time: `Color.rgb(300, 0, 0)` does not compile. Use [[rgbFrom]] for runtime values. */
  inline def rgb(inline red: Int, inline green: Int, inline blue: Int): Color = Rgb(Channel(red), Channel(green), Channel(blue))

  /** Truecolor from runtime values, `Left` with refined4s's message when a component is outside 0..255. */
  def rgbFrom(red: Int, green: Int, blue: Int): Either[String, Color] =
    for {
      r <- Channel.from(red)
      g <- Channel.from(green)
      b <- Channel.from(blue)
    } yield Rgb(r, g, b)

  /** Truecolor from already-refined channels. */
  def rgbOf(red: Channel, green: Channel, blue: Channel): Color = Rgb(red, green, blue)

  /** A 256-colour table entry from a literal, validated at compile time. Use [[indexedFrom]] for runtime values. */
  inline def indexed(inline index: Int): Color = Indexed(Index(index))

  /** A 256-colour table entry from a runtime value, `Left` with refined4s's message when outside 0..255. */
  def indexedFrom(index: Int): Either[String, Color] = Index.from(index).map(Indexed(_))

  /** A 256-colour table entry from an already-refined index. */
  def indexedOf(index: Index): Color = Indexed(index)

}
