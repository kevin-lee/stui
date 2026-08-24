package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** How a [[Paragraph]] wraps its lines at the text area width: `Word` keeps the whitespace layout on the first row of a logical line,
  * `WordTrimmed` drops the leading whitespace of every produced row. No wrap at all is `Paragraph`'s `wrap = None` (truncation).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Wrap derives Eq, Show, Hash {
  case Word
  case WordTrimmed
}
