package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** When a [[ListView]] or [[Table]] reserves the column its highlight symbol is drawn in (Ratatui's documented three modes):
  * `Always` keeps the content in place whether or not something is selected, `WhenSelected` (the default) reserves it only while an
  * item is selected (the content shifts when the selection appears), `Never` draws no symbol column at all.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
enum HighlightSpacing derives Eq, Show, Hash {
  case Always
  case WhenSelected
  case Never
}
