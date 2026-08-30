package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** Which string sequence the decoder is consuming (the DEC ANSI parser reference): only DCS bodies are buffered, because the probe
  * replies (XTGETTCAP, XTVERSION) arrive as DCS strings, while OSC, APC, PM, and SOS are counted and dropped.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum StringKind derives Eq, Show, Hash {
  case Dcs
  case Osc
  case Apc
  case Pm
  case Sos
}
