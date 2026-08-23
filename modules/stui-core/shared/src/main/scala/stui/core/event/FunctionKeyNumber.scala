package stui.core.event

import refined4s.modules.cats.derivation.{CatsHash, CatsShow}
import refined4s.types.numeric.InlinedNumericMinMax

/** The number of a function key, 1 to 35 (the kitty keyboard protocol range). */
type FunctionKeyNumber = FunctionKeyNumber.Type

/** @author Kevin Lee
  * @since 2026-08-23
  */
object FunctionKeyNumber extends InlinedNumericMinMax[Int], CatsHash[Int], CatsShow[Int] {
  override inline def minValue: Int = 1
  override inline def maxValue: Int = 35
}
