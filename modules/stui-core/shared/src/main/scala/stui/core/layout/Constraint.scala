package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.{NonNegInt, PosInt}

/** What one segment of a [[Layout.split]] asks for. The exact resolution order is specified on [[Layout.split]]: `Min` floors first,
  * then `Length`, then the proportional demands (`Percentage`, `Ratio`), then `Max` preferences, and finally `Fill` and `Min` grow over
  * whatever remains.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
enum Constraint derives Eq, Show, Hash {

  /** Exactly `n` cells, cut only when the space runs out. */
  case Length(n: NonNegInt)

  /** The percentage of the available length (the space left after spacing), rounded half up. */
  case Percentage(percent: Percent)

  /** `numerator / denominator` of the available length, rounded half up. A ratio above one asks for the whole available length. */
  case Ratio(numerator: NonNegInt, denominator: PosInt)

  /** At least `n` cells, and the segment grows over the remaining space like `Fill(1)`. */
  case Min(n: NonNegInt)

  /** At most `n` cells, preferred at exactly `n`. The preference ranks below every other demand and the segment never grows. */
  case Max(n: NonNegInt)

  /** No demand: the segment takes a share of the remaining space proportional to `weight` (with the `Min` segments at weight 1). */
  case Fill(weight: PosInt)
}

object Constraint {

  /** A [[Length]] from a literal, validated at compile time. */
  inline def length(inline n: Int): Constraint = Length(NonNegInt(n))

  /** A [[Length]] from an already-refined count. */
  def lengthOf(n: NonNegInt): Constraint = Length(n)

  /** A [[Percentage]] from a literal, validated at compile time: `Constraint.percentage(150)` does not compile. */
  inline def percentage(inline p: Int): Constraint = Percentage(Percent(p))

  /** A [[Percentage]] from an already-refined percent. */
  def percentageOf(percent: Percent): Constraint = Percentage(percent)

  /** A [[Percentage]] from a runtime value, `Left` with refined4s's message outside 0..100. */
  def percentageFrom(p: Int): Either[String, Constraint] = Percent.from(p).map(Percentage(_))

  /** A [[Ratio]] from literals, validated at compile time (the denominator must be positive). */
  inline def ratio(inline numerator: Int, inline denominator: Int): Constraint = Ratio(NonNegInt(numerator), PosInt(denominator))

  /** A [[Ratio]] from already-refined values. */
  def ratioOf(numerator: NonNegInt, denominator: PosInt): Constraint = Ratio(numerator, denominator)

  /** A [[Ratio]] from runtime values, `Left` with refined4s's message when the numerator is negative or the denominator is not
    * positive.
    */
  def ratioFrom(numerator: Int, denominator: Int): Either[String, Constraint] =
    for {
      n <- NonNegInt.from(numerator)
      d <- PosInt.from(denominator)
    } yield Ratio(n, d)

  /** A [[Min]] from a literal, validated at compile time. */
  inline def min(inline n: Int): Constraint = Min(NonNegInt(n))

  /** A [[Min]] from an already-refined count. */
  def minOf(n: NonNegInt): Constraint = Min(n)

  /** A [[Max]] from a literal, validated at compile time. */
  inline def max(inline n: Int): Constraint = Max(NonNegInt(n))

  /** A [[Max]] from an already-refined count. */
  def maxOf(n: NonNegInt): Constraint = Max(n)

  /** A [[Fill]] from a literal, validated at compile time (the weight must be positive). */
  inline def fill(inline weight: Int): Constraint = Fill(PosInt(weight))

  /** A [[Fill]] from an already-refined weight. */
  def fillOf(weight: PosInt): Constraint = Fill(weight)

}
