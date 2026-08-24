package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.core.style.{CellStyle, Color, Modifier, Modifiers, Style}

/** Generators for colours, modifiers, and styles.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object StyleGens {

  /** Any truecolor component. */
  val channel: Gen[Color.Channel] =
    Gen.int(Range.linear(0, 255)).map(n => Color.Channel.from(n).fold(_ => Color.Channel.MinValue, identity))

  /** Any 256-colour table index. */
  val index: Gen[Color.Index] =
    Gen.int(Range.linear(0, 255)).map(n => Color.Index.from(n).fold(_ => Color.Index.MinValue, identity))

  private val namedColor: Gen[Color] = Gen.element1(
    Color.Reset,
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

  private val rgb: Gen[Color] =
    for {
      r <- channel
      g <- channel
      b <- channel
    } yield Color.rgbOf(r, g, b)

  /** Mostly named colours, sometimes `Rgb` or `Indexed`. */
  val color: Gen[Color] = Gen.frequency1(8 -> namedColor, 1 -> rgb, 1 -> index.map(Color.indexedOf))

  /** Any single modifier. */
  val modifier: Gen[Modifier] = Gen.elementUnsafe(Modifier.all)

  /** A random subset. */
  val modifiers: Gen[Modifiers] = modifier.list(Range.linear(0, 9)).map(Modifiers.of)

  /** Independent add and sub sets, so a modifier can be in both. */
  val style: Gen[Style] =
    for {
      fg  <- color.option
      bg  <- color.option
      ul  <- color.option
      add <- modifiers
      sub <- modifiers
    } yield Style(fg, bg, ul, add, sub)

  /** Any resolved cell style. */
  val cellStyle: Gen[CellStyle] =
    for {
      fg <- color
      bg <- color
      ul <- color
      ms <- modifiers
    } yield CellStyle(fg, bg, ul, ms)

}
