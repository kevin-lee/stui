package stui.core.capability

import hedgehog.*
import hedgehog.runner.*
import stui.core.buffer.GlyphWidth
import stui.testkit.Assertions
import stui.testkit.gen.CapabilityGens

/** `Capabilities.fromEnv` on a fixture table of environments plus its totality and its M1e constants.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object CapabilitiesSpec extends Properties {

  private def env(pairs: (String, String)*): Map[String, String] = pairs.toMap

  private def from(pairs: (String, String)*): Capabilities = Capabilities.fromEnv(env(pairs*))

  private val conservative: Capabilities = Capabilities.conservative

  override def tests: List[Test] = List(
    example("an empty environment is conservative", Assertions.eqv(from(), conservative)),
    example("TERM=xterm-256color gives Ansi256", Assertions.eqv(from("TERM" -> "xterm-256color").colors, ColorProfile.Ansi256)),
    example("COLORTERM=truecolor gives Truecolor", Assertions.eqv(from("COLORTERM" -> "truecolor").colors, ColorProfile.Truecolor)),
    example("COLORTERM=24bit gives Truecolor", Assertions.eqv(from("COLORTERM" -> "24bit").colors, ColorProfile.Truecolor)),
    example("NO_COLOR wins over COLORTERM", Assertions.eqv(from("NO_COLOR" -> "1", "COLORTERM" -> "truecolor").colors, ColorProfile.Mono)),
    example(
      "an empty NO_COLOR is not set",
      Assertions.eqv(from("NO_COLOR" -> "", "COLORTERM" -> "truecolor").colors, ColorProfile.Truecolor),
    ),
    example("TERM=dumb is mono, ASCII, no mouse, no focus, no scroll regions", testDumb),
    example("TERM=xterm-kitty gives truecolour and extended underlines", testKitty),
    example("TERM_PROGRAM=iTerm.app gives truecolour and extended underlines", testITerm),
    example(
      "Apple_Terminal with xterm-256color gives Ansi256 and no extended underline",
      from("TERM_PROGRAM" -> "Apple_Terminal", "TERM" -> "xterm-256color") match {
        case c => Result.all(List(Assertions.eqv(c.colors, ColorProfile.Ansi256), Result.assert(!c.extendedUnderline)))
      },
    ),
    example("tmux is detected and disables scroll regions and extended underlines", testTmux),
    example("screen is detected and disables SGR mouse", testScreen),
    example("ZELLIJ gives Zellij", Assertions.eqv(from("ZELLIJ" -> "0").multiplexer, Multiplexer.Zellij)),
    example("TERM=screen-256color alone gives Other and Ansi256", testScreenTerm),
    example("SSH_TTY gives ssh", Result.assert(from("SSH_TTY" -> "/dev/pts/0").ssh)),
    example("SSH_CONNECTION gives ssh", Result.assert(from("SSH_CONNECTION" -> "10.0.0.2 51234 10.0.0.1 22").ssh)),
    example("LANG=C gives ASCII glyphs", Assertions.eqv(from("LANG" -> "C").glyphs, GlyphSet.Ascii)),
    example("LANG=ko_KR.UTF-8 gives Unicode glyphs", Assertions.eqv(from("LANG" -> "ko_KR.UTF-8").glyphs, GlyphSet.Unicode)),
    example("LC_ALL wins over LANG", Assertions.eqv(from("LC_ALL" -> "C", "LANG" -> "en_US.UTF-8").glyphs, GlyphSet.Ascii)),
    example("LC_CTYPE=ja_JP.utf8 gives Unicode glyphs", Assertions.eqv(from("LC_CTYPE" -> "ja_JP.utf8").glyphs, GlyphSet.Unicode)),
    example("TERM=xterm-direct gives Truecolor", Assertions.eqv(from("TERM" -> "xterm-direct").colors, ColorProfile.Truecolor)),
    property(
      "fromEnv is deterministic and total",
      CapabilityGens.env.forAll.map(e => Assertions.eqv(Capabilities.fromEnv(e), Capabilities.fromEnv(e))),
    ),
    property(
      "the probed fields keep their M1e constants",
      CapabilityGens.env.forAll.map { e =>
        val c = Capabilities.fromEnv(e)
        Result.all(
          List(
            Result.assert(!c.syncOutput).log("syncOutput"),
            Result.assert(!c.kittyKeyboard).log("kittyKeyboard"),
            Result.assert(!c.ambiguousWide).log("ambiguousWide"),
            Assertions.eqv(c.vs16Width, GlyphWidth.Two),
          )
        )
      },
    ),
    property("generated capabilities are equal to themselves", CapabilityGens.capabilities.forAll.map(c => Assertions.eqv(c, c))),
    example(
      "lossless keeps everything else conservative",
      Assertions.eqv(Capabilities.lossless.copy(colors = ColorProfile.Ansi16, extendedUnderline = false), conservative),
    ),
    example(
      "an ambiguous-wide terminal selects ASCII glyphs",
      Assertions.eqv(conservative.copy(ambiguousWide = true).effectiveGlyphs, GlyphSet.Ascii),
    ),
    example(
      "a narrow-ambiguous terminal keeps its detected glyphs",
      Assertions.eqv(conservative.effectiveGlyphs, GlyphSet.Unicode),
    ),
    example(
      "ASCII glyphs stay ASCII either way",
      Assertions.eqv(conservative.copy(glyphs = GlyphSet.Ascii, ambiguousWide = true).effectiveGlyphs, GlyphSet.Ascii),
    ),
    property(
      "effectiveGlyphs is ASCII exactly when ambiguous-wide or detected ASCII",
      CapabilityGens.capabilities.forAll.map { c =>
        Assertions.eqv(c.effectiveGlyphs, if (c.ambiguousWide) GlyphSet.Ascii else c.glyphs)
      },
    ),
  )

  def testDumb: Result = {
    val c = from("TERM" -> "dumb", "COLORTERM" -> "truecolor", "LANG" -> "en_US.UTF-8")
    Result.all(
      List(
        Assertions.eqv(c.colors, ColorProfile.Mono),
        Assertions.eqv(c.glyphs, GlyphSet.Ascii),
        Result.assert(!c.sgrMouse).log("sgrMouse"),
        Result.assert(!c.focusEvents).log("focusEvents"),
        Result.assert(!c.scrollRegionsSafe).log("scrollRegionsSafe"),
        Result.assert(!c.extendedUnderline).log("extendedUnderline"),
      )
    )
  }

  def testKitty: Result = {
    val c = from("TERM" -> "xterm-kitty")
    Result.all(List(Assertions.eqv(c.colors, ColorProfile.Truecolor), Result.assert(c.extendedUnderline)))
  }

  def testITerm: Result = {
    val c = from("TERM_PROGRAM" -> "iTerm.app", "TERM" -> "xterm-256color")
    Result.all(List(Assertions.eqv(c.colors, ColorProfile.Truecolor), Result.assert(c.extendedUnderline)))
  }

  def testTmux: Result = {
    val c = from("TMUX" -> "/tmp/tmux-501/default,1,0", "TERM" -> "tmux-256color", "TERM_PROGRAM" -> "WezTerm")
    Result.all(
      List(
        Assertions.eqv(c.multiplexer, Multiplexer.Tmux),
        Assertions.eqv(c.colors, ColorProfile.Truecolor),
        Result.assert(!c.scrollRegionsSafe).log("scrollRegionsSafe"),
        Result.assert(!c.extendedUnderline).log("extendedUnderline"),
        Result.assert(c.sgrMouse).log("sgrMouse"),
        Result.assert(c.focusEvents).log("focusEvents"),
      )
    )
  }

  def testScreen: Result = {
    val c = from("STY" -> "1234.pts-0", "TERM" -> "screen")
    Result.all(List(Assertions.eqv(c.multiplexer, Multiplexer.Screen), Result.assert(!c.sgrMouse), Result.assert(c.focusEvents)))
  }

  def testScreenTerm: Result = {
    val c = from("TERM" -> "screen-256color")
    Result.all(List(Assertions.eqv(c.multiplexer, Multiplexer.Other), Assertions.eqv(c.colors, ColorProfile.Ansi256)))
  }

}
