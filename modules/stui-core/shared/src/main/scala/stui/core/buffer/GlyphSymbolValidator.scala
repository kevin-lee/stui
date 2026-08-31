package stui.core.buffer

import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.quoted.*

object GlyphSymbolValidator {

  object Macros {
    inline def isValidGlyphSymbol(inline value: String): Boolean = ${ isValidGlyphSymbolImpl('value) }

    def isValidGlyphSymbolImpl(valueExpr: Expr[String])(using Quotes): Expr[Boolean] = {

      val value = stui.core.internal.macros.Macros.exprToString(valueExpr)

      Expr(GlyphSymbolValidator.isValidGlyphSymbol(value))

    }
  }

  @scala.annotation.tailrec
  private[core] def hasUnpairedSurrogate(s: String, i: Int): Boolean =
    if (i >= s.length) {
      false
    } else {
      val cp = s.codePointAt(i)
      if (cp >= 0xd800 && cp <= 0xdfff) true else hasUnpairedSurrogate(s, i + Character.charCount(cp))
    }

  def isValidGlyphSymbol(value: String): Boolean =
    Graphemes.count(value) === 1 && WidthPolicy.default.clusterWidth(value) > 0 && !hasUnpairedSurrogate(value, 0)

}
