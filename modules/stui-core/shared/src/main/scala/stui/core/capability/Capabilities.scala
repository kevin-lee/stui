package stui.core.capability

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.buffer.GlyphWidth

/** What the terminal can do, as a value (design doc 7.3, decision D15). Three pure layers build it: [[Capabilities.fromEnv]] reads the
  * environment into a conservative value (M1e), probe results and application overrides are patches merged on top with the law that no
  * field upgrades except through a probe or an override (M1f). The writer degrades colours and underlines from it, the decoder scales
  * its ESC timeout by `ssh`, and the widgets pick their glyph set from it (M2).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final case class Capabilities(
  colors: ColorProfile,
  syncOutput: Boolean,
  kittyKeyboard: Boolean,
  sgrMouse: Boolean,
  focusEvents: Boolean,
  scrollRegionsSafe: Boolean,
  extendedUnderline: Boolean,
  multiplexer: Multiplexer,
  ssh: Boolean,
  glyphs: GlyphSet,
  ambiguousWide: Boolean,
  vs16Width: GlyphWidth,
) derives Eq,
      Show,
      Hash

object Capabilities {

  /** What every xterm-compatible terminal can do: 16 colours, SGR mouse, focus events, scroll regions, no synchronised output, no kitty
    * keyboard, no extended underline, no multiplexer, not ssh, Unicode glyphs, narrow ambiguous characters, VS16 two columns wide.
    */
  val conservative: Capabilities =
    Capabilities(
      ColorProfile.Ansi16,
      false,
      false,
      true,
      true,
      true,
      false,
      Multiplexer.None,
      false,
      GlyphSet.Unicode,
      false,
      GlyphWidth.Two,
    )

  /** [[conservative]] with truecolour and extended underlines: nothing is degraded, for tests and terminals known to be complete. */
  val lossless: Capabilities = conservative.copy(colors = ColorProfile.Truecolor, extendedUnderline = true)

  private val TruecolorTerms: Set[String] =
    Set("xterm-kitty", "alacritty", "foot", "foot-extra", "wezterm", "xterm-ghostty", "ghostty", "contour", "rio")

  private val TruecolorPrograms: Set[String] = Set("iterm.app", "wezterm", "vscode", "ghostty", "contour", "rio", "hyper")

  private val ExtendedUnderlineTerms: Set[String] =
    Set("xterm-kitty", "foot", "foot-extra", "wezterm", "xterm-ghostty", "ghostty", "contour")

  private val ExtendedUnderlinePrograms: Set[String] = Set("wezterm", "iterm.app", "ghostty", "contour")

  /** ASCII lower-casing (`java.util.Locale` does not exist on the Scala.js and Scala Native javalibs, and environment values are ASCII). */
  private def asciiLower(s: String): String = s.map(c => if (c >= 'A' && c <= 'Z') (c + 32).toChar else c)

  /** The environment layer, conservative by design (design doc 7.3). Values are trimmed and compared in ASCII lower case, "set" means
    * present and non-empty:
    *
    *   - `multiplexer`: `TMUX` set is `Tmux`, else `STY` set is `Screen`, else `ZELLIJ` set is `Zellij`, else a `TERM` starting with
    *     `screen` or `tmux` is `Other`, else `Multiplexer.None`.
    *   - `ssh`: `SSH_TTY`, `SSH_CONNECTION`, or `SSH_CLIENT` set (sshd_config(5)).
    *   - `colors`: `NO_COLOR` set is `Mono` (no-color.org), else `TERM=dumb` is `Mono`, else `COLORTERM` `truecolor` or `24bit` is
    *     `Truecolor`, else a known truecolour terminal (`TERM` in the kitty, Alacritty, foot, WezTerm, ghostty, contour, and rio names
    *     or ending in `-direct`, `TERM_PROGRAM` in the iTerm2, WezTerm, vscode, ghostty, contour, rio, and Hyper names) is `Truecolor`,
    *     else a `TERM` containing `256color` is `Ansi256`, else `Ansi16` (an unset `TERM` included).
    *   - `extendedUnderline`: a known terminal (kitty, foot, WezTerm, ghostty, contour, iTerm2) outside any multiplexer and not dumb.
    *   - `sgrMouse`: not dumb and not under screen. `focusEvents`: not dumb. `scrollRegionsSafe`: not dumb and no multiplexer.
    *   - `syncOutput` and `kittyKeyboard`: false, they need a probe (M1f, M3).
    *   - `glyphs`: `Ascii` when dumb or when the effective locale (`LC_ALL`, else `LC_CTYPE`, else `LANG`, when set) names no UTF-8
    *     charset, `Unicode` otherwise (an unset locale included).
    *   - `ambiguousWide` false and `vs16Width` two columns, the M4 width probe fills them.
    */
  def fromEnv(env: Map[String, String]): Capabilities = {
    def value(key: String): Option[String] = env.get(key).map(_.trim).filter(_.nonEmpty)
    def lower(key: String): Option[String] = value(key).map(asciiLower)
    val term                               = lower("TERM")
    val program                            = lower("TERM_PROGRAM")
    val dumb                               = term.exists(_ === "dumb")
    val multiplexer                        =
      if (value("TMUX").isDefined) Multiplexer.Tmux
      else if (value("STY").isDefined) Multiplexer.Screen
      else if (value("ZELLIJ").isDefined) Multiplexer.Zellij
      else if (term.exists(t => t.startsWith("screen") || t.startsWith("tmux"))) Multiplexer.Other
      else Multiplexer.None
    val noMultiplexer                      = multiplexer === Multiplexer.None
    val ssh                    = value("SSH_TTY").isDefined || value("SSH_CONNECTION").isDefined || value("SSH_CLIENT").isDefined
    val knownTruecolor         =
      term.exists(t => TruecolorTerms.contains(t) || t.endsWith("-direct")) || program.exists(TruecolorPrograms.contains)
    val knownExtendedUnderline = term.exists(ExtendedUnderlineTerms.contains) || program.exists(ExtendedUnderlinePrograms.contains)
    val colors                 =
      if (value("NO_COLOR").isDefined || dumb) ColorProfile.Mono
      else if (lower("COLORTERM").exists(c => c === "truecolor" || c === "24bit")) ColorProfile.Truecolor
      else if (knownTruecolor) ColorProfile.Truecolor
      else if (term.exists(_.contains("256color"))) ColorProfile.Ansi256
      else ColorProfile.Ansi16
    val locale                 = value("LC_ALL").orElse(value("LC_CTYPE")).orElse(value("LANG")).map(asciiLower)
    val glyphs                 =
      if (dumb || locale.exists(l => !(l.contains("utf-8") || l.contains("utf8")))) GlyphSet.Ascii else GlyphSet.Unicode
    Capabilities(
      colors,
      false,
      false,
      !dumb && multiplexer =!= Multiplexer.Screen,
      !dumb,
      !dumb && noMultiplexer,
      !dumb && knownExtendedUnderline && noMultiplexer,
      multiplexer,
      ssh,
      glyphs,
      false,
      GlyphWidth.Two,
    )
  }

  extension (capabilities: Capabilities) {

    /** The capabilities with the colour profile replaced (an application override, design doc 7.3). */
    def withColors(profile: ColorProfile): Capabilities = capabilities.copy(colors = profile)

    /** The capabilities with the VS16 width replaced. */
    def withVs16Width(width: GlyphWidth): Capabilities = capabilities.copy(vs16Width = width)

  }

}
