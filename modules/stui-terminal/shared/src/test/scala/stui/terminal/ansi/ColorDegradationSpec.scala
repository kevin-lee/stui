package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.capability.ColorProfile
import stui.core.style.Color
import stui.testkit.Assertions
import stui.testkit.gen.{CapabilityGens, StyleGens}

/** Colour degradation is idempotent, respects the profile, and keeps the named colours.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object ColorDegradationSpec extends Properties {

  private def isRgb(color: Color): Boolean = color match {
    case Color.Rgb(_, _, _) => true
    case Color.Indexed(_) | Color.Reset | Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan |
        Color.Gray | Color.DarkGray | Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta |
        Color.LightCyan | Color.White =>
      false
  }

  private def isIndexed(color: Color): Boolean = color match {
    case Color.Indexed(_) => true
    case Color.Rgb(_, _, _) | Color.Reset | Color.Black | Color.Red | Color.Green | Color.Yellow | Color.Blue | Color.Magenta | Color.Cyan |
        Color.Gray | Color.DarkGray | Color.LightRed | Color.LightGreen | Color.LightYellow | Color.LightBlue | Color.LightMagenta |
        Color.LightCyan | Color.White =>
      false
  }

  private val namedOrReset: Gen[Color] = Gen.element1(Color.Reset, ColorDegradation.palette.map(_.color).toList*)

  override def tests: List[Test] = List(
    property("degrade is idempotent", testIdempotent),
    property(
      "Ansi256 never gives Rgb",
      StyleGens.color.forAll.map(c => Result.assert(!isRgb(ColorDegradation.degrade(ColorProfile.Ansi256, c)))),
    ),
    property(
      "Ansi16 gives a named colour or Reset",
      StyleGens.color.forAll.map { c =>
        val d = ColorDegradation.degrade(ColorProfile.Ansi16, c)
        Result.assert(!isRgb(d) && !isIndexed(d)).log(d.show)
      },
    ),
    property(
      "Mono gives Reset",
      StyleGens.color.forAll.map(c => Assertions.eqv(ColorDegradation.degrade(ColorProfile.Mono, c), Color.Reset)),
    ),
    property(
      "Truecolor is the identity",
      StyleGens.color.forAll.map(c => Assertions.eqv(ColorDegradation.degrade(ColorProfile.Truecolor, c), c)),
    ),
    property(
      "named colours and Reset are fixed points outside Mono",
      for {
        c <- namedOrReset.forAll
        p <- CapabilityGens.colorProfile.forAll
      } yield if (p === ColorProfile.Mono) Result.success else Assertions.eqv(ColorDegradation.degrade(p, c), c),
    ),
    example("every palette entry maps to itself under Ansi16", testPalette),
    example("black and white", testBlackWhite),
    example(
      "rgb(95, 135, 175) is index 67 under Ansi256",
      Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi256, Color.rgb(95, 135, 175)), Color.indexed(67)),
    ),
    example(
      "rgb(128, 128, 128) is the gray ramp index 244 under Ansi256",
      Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi256, Color.rgb(128, 128, 128)), Color.indexed(244)),
    ),
    example(
      "Indexed(9) is LightRed under Ansi16",
      Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi16, Color.indexed(9)), Color.LightRed),
    ),
    example(
      "Indexed(196) is LightRed under Ansi16",
      Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi16, Color.indexed(196)), Color.LightRed),
    ),
    example(
      "rgbOf inverts the cube and the ramp",
      Result.all(
        List(
          ColorDegradation.rgbOf(67) ==== (95, 135, 175),
          ColorDegradation.rgbOf(244) ==== (128, 128, 128),
          ColorDegradation.rgbOf(1) ==== (205, 0, 0),
        )
      ),
    ),
  )

  def testIdempotent: Property =
    for {
      c <- StyleGens.color.forAll
      p <- CapabilityGens.colorProfile.forAll
    } yield Assertions.eqv(ColorDegradation.degrade(p, ColorDegradation.degrade(p, c)), ColorDegradation.degrade(p, c))

  def testPalette: Result =
    Result.all(
      ColorDegradation
        .palette
        .toList
        .map(entry => Assertions.eqv(ColorDegradation.nearest16(entry.red, entry.green, entry.blue), entry.color))
    )

  def testBlackWhite: Result =
    Result.all(
      List(
        Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi16, Color.rgb(0, 0, 0)), Color.Black),
        Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi16, Color.rgb(255, 255, 255)), Color.White),
        Assertions.eqv(ColorDegradation.degrade(ColorProfile.Ansi256, Color.rgb(0, 0, 0)), Color.indexed(16)),
      )
    )

}
