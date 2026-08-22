package stui.unicode.corpus

import scala.util.Try

/** One GraphemeBreakTest line: the expected clusters as code points. `lineNumber` is the 1-based index in the corpus vector.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
final case class GraphemeBreakCase(lineNumber: Int, clusters: Vector[Vector[Int]])

object GraphemeBreakCase {

  /** Parses the data part of a test line, `÷ 0020 × 0308 ÷ 0020 ÷`: `÷` opens a cluster, `×` continues it, hex tokens are code points. */
  def parse(lineNumber: Int, line: String): Either[String, GraphemeBreakCase] = {
    val tokens = line.trim.split("\\s+").toList
    val parsed =
      tokens.foldLeft(Right((Vector.empty[Vector[Int]], Vector.empty[Int])): Either[String, (Vector[Vector[Int]], Vector[Int])]) {
        (acc, token) =>
          acc.flatMap {
            case (completed, current) =>
              token match {
                case "÷" => Right((if (current.isEmpty) completed else completed :+ current, Vector.empty[Int]))
                case "×" => Right((completed, current))
                case hex =>
                  Try(Integer.parseInt(hex, 16))
                    .toOption
                    .map(cp => (completed, current :+ cp))
                    .toRight(s"line ${lineNumber.toString}: not a code point token '$hex'")
              }
          }
      }
    parsed.map { case (completed, current) => GraphemeBreakCase(lineNumber, if (current.isEmpty) completed else completed :+ current) }
  }

  /** The string made of the given code points (surrogate halves are appended as they are). */
  def render(codePoints: Iterable[Int]): String =
    codePoints.foldLeft(new java.lang.StringBuilder)((builder, cp) => builder.appendCodePoint(cp)).toString

  extension (testCase: GraphemeBreakCase) {

    def codePoints: Vector[Int] = testCase.clusters.flatten

    def input: String = render(testCase.codePoints)

    def expected: Vector[String] = testCase.clusters.map(render)

  }

}
