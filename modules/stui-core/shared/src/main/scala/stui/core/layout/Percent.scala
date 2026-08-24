package stui.core.layout

import refined4s.modules.cats.derivation.{CatsHash, CatsShow}
import refined4s.types.numeric.InlinedNumericMinMax

/** A whole-number percentage of an available length, 0 to 100. `Percent(150)` does not compile (design principle 3), runtime values go
  * through `Percent.from`.
  */
type Percent = Percent.Type

/** @author Kevin Lee
  * @since 2026-08-24
  */
object Percent extends InlinedNumericMinMax[Int], CatsHash[Int], CatsShow[Int] {

  /** Nothing of the available length. */
  override inline def minValue: Int = 0

  /** The whole available length. */
  override inline def maxValue: Int = 100
}
