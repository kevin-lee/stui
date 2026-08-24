package stui.core.text

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.style.Style
import stui.unicode.WidthPolicy

/** A run of text with one style patch.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Span(content: String, style: Style) derives Eq, Show, Hash

object Span {

  /** The content with the empty style patch. */
  def raw(content: String): Span = Span(content, Style.empty)

  /** The content with the given style patch. */
  def styled(content: String, style: Style): Span = Span(content, style)

  extension (span: Span) {

    /** The display width of the content under the policy. */
    def width(policy: WidthPolicy): Int = policy.width(span.content)

    /** The span with its style patched by `style`. */
    def patchStyle(style: Style): Span = span.copy(style = span.style.patch(style))

    /** The span with the empty style patch. */
    def resetStyle: Span = span.copy(style = Style.empty)

  }

}
