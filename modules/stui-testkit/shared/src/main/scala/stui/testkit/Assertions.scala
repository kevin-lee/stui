package stui.testkit

import cats.{Eq, Show}
import cats.syntax.all.*
import hedgehog.Result
import stui.core.buffer.Buffer

/** hedgehog results that compare with cats `Eq` and log with `Show` (logging is the one use of `Show` in stui).
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object Assertions {

  /** Success when `actual === expected`, otherwise a failure logging both sides rendered with `Show`. */
  def eqv[A: Eq: Show](actual: A, expected: A): Result =
    if (actual === expected) Result.success
    else Result.failure.log(s"--- actual ---\n${actual.show}\n--- expected ---\n${expected.show}")

  /** Success when the buffer's rendered rows equal `expected`, otherwise a failure logging both grids between `|` markers with a `*`
    * prefix on every differing row. Only the symbols are compared (a continuation renders as part of its glyph): assert styles
    * separately through `Buffer.cell` or an expected buffer built with `Buffer.fromLines` and `patchStyle`.
    */
  def grid(actual: Buffer, expected: Vector[String]): Result = {
    val actualRows = Buffer.renderRows(actual)
    if (actualRows === expected) {
      Result.success
    } else {
      val height                                      = math.max(actualRows.length, expected.length)
      val differs                                     = IArray.tabulate(height)(i => actualRows.lift(i) =!= expected.lift(i))
      def block(rows: Vector[String]): Vector[String] =
        Vector.tabulate(height)(i => s"${if (differs(i)) "*" else " "}|${rows.lift(i).getOrElse("<missing row>")}|")
      Result
        .failure
        .log(
          (Vector("--- actual (copy this block to update the golden) ---") ++ block(actualRows) ++
            Vector("--- expected ---") ++ block(expected)).mkString("\n")
        )
    }
  }

}
