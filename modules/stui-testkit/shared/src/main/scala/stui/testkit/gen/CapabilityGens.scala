package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.core.buffer.GlyphWidth
import stui.core.capability.{Capabilities, ColorProfile, GlyphSet, Multiplexer}

/** Generators for the capabilities value and for environments `Capabilities.fromEnv` reads.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object CapabilityGens {

  /** Any colour profile. */
  val colorProfile: Gen[ColorProfile] = Gen.elementUnsafe(ColorProfile.all)

  /** Any multiplexer, `Multiplexer.None` included. */
  val multiplexer: Gen[Multiplexer] =
    Gen.element1(Multiplexer.None, Multiplexer.Tmux, Multiplexer.Screen, Multiplexer.Zellij, Multiplexer.Other)

  /** Any glyph set. */
  val glyphSet: Gen[GlyphSet] = Gen.element1(GlyphSet.Unicode, GlyphSet.Ascii)

  /** One or two columns. */
  val glyphWidth: Gen[GlyphWidth] = Gen.element1(GlyphWidth.One, GlyphWidth.Two)

  /** Every field random. */
  val capabilities: Gen[Capabilities] =
    for {
      colors            <- colorProfile
      syncOutput        <- Gen.boolean
      kittyKeyboard     <- Gen.boolean
      sgrMouse          <- Gen.boolean
      focusEvents       <- Gen.boolean
      scrollRegionsSafe <- Gen.boolean
      extendedUnderline <- Gen.boolean
      mux               <- multiplexer
      ssh               <- Gen.boolean
      glyphs            <- glyphSet
      ambiguousWide     <- Gen.boolean
      vs16Width         <- glyphWidth
    } yield Capabilities(
      colors,
      syncOutput,
      kittyKeyboard,
      sgrMouse,
      focusEvents,
      scrollRegionsSafe,
      extendedUnderline,
      mux,
      ssh,
      glyphs,
      ambiguousWide,
      vs16Width,
    )

  private val keys: List[String] = List(
    "TERM",
    "COLORTERM",
    "NO_COLOR",
    "TERM_PROGRAM",
    "TMUX",
    "STY",
    "ZELLIJ",
    "SSH_TTY",
    "SSH_CONNECTION",
    "SSH_CLIENT",
    "LANG",
    "LC_ALL",
    "LC_CTYPE",
  )

  private val value: Gen[String] = Gen.frequency1(
    3 -> Gen.element1(
      "xterm-256color",
      "xterm-kitty",
      "xterm-direct",
      "dumb",
      "truecolor",
      "24bit",
      "1",
      "",
      "tmux-256color",
      "screen",
      "iTerm.app",
      "WezTerm",
      "Apple_Terminal",
      "C",
      "POSIX",
      "ko_KR.UTF-8",
      "en_US.utf8",
      "/dev/pts/0",
    ),
    1 -> Gens.asciiPrintable(Range.linear(0, 12)),
  )

  /** A random subset of the variables `fromEnv` reads, with realistic and random values. */
  val env: Gen[Map[String, String]] =
    keys.foldLeft(Gen.constant(Map.empty[String, String])) { (acc, key) =>
      for {
        map     <- acc
        include <- Gen.boolean
        v       <- value
      } yield if (include) map + (key -> v) else map
    }

}
