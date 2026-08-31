package stui.core.buffer

import stui.unicode.{Graphemes, WidthPolicy}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec
import scala.quoted.*

/** The [[GlyphSymbol]] predicate in a form both the runtime `predicate` and the compile-time `inlinedPredicate` can call. The macro
  * lives in a nested object because a macro implementation cannot be called from the object it is defined in at the same compilation
  * stage.
  */
object GlyphSymbolValidator {

  object Macros {

    /** The predicate folded at compile time, so an invalid [[GlyphSymbol]] literal is a compile error rather than a runtime one. */
    inline def isValidGlyphSymbol(inline value: String): Boolean = ${ isValidGlyphSymbolImpl('value) }

    def isValidGlyphSymbolImpl(valueExpr: Expr[String])(using Quotes): Expr[Boolean] = {
      val value = stui.core.internal.macros.Macros.exprToString(valueExpr)
      Expr(GlyphSymbolValidator.isValidGlyphSymbol(value))
    }
  }

  @tailrec
  private[core] def hasUnpairedSurrogate(s: String, i: Int): Boolean =
    if (i >= s.length) {
      false
    } else {
      val cp = s.codePointAt(i)
      if (cp >= 0xd800 && cp <= 0xdfff) true else hasUnpairedSurrogate(s, i + Character.charCount(cp))
    }

  /** One cluster, valid UTF-16, non-zero width under the bundled tables. */
  def isValidGlyphSymbol(value: String): Boolean =
    Graphemes.count(value) === 1 && WidthPolicy.default.clusterWidth(value) > 0 && !hasUnpairedSurrogate(value, 0)

}
