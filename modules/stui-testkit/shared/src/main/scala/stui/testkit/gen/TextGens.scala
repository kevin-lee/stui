package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.core.text.{Alignment, Line, Span, Text}

/** Generators for the text model.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object TextGens {

  val alignment: Gen[Alignment] = Gen.element1(Alignment.Left, Alignment.Center, Alignment.Right)

  /** Mostly printable ASCII, sometimes nasty Unicode. */
  def content(range: Range[Int]): Gen[String] = Gen.frequency1(3 -> Gens.asciiPrintable(range), 1 -> NastyGens.nastyString(range))

  def span(range: Range[Int]): Gen[Span] =
    for {
      c <- content(range)
      s <- StyleGens.style
    } yield Span(c, s)

  def line(spans: Range[Int], range: Range[Int]): Gen[Line] =
    for {
      ss <- span(range).list(spans)
      st <- StyleGens.style
      al <- alignment.option
    } yield Line(ss.toVector, st, al)

  def text(lines: Range[Int], spans: Range[Int], range: Range[Int]): Gen[Text] =
    for {
      ls <- line(spans, range).list(lines)
      st <- StyleGens.style
      al <- alignment.option
    } yield Text(ls.toVector, st, al)

  /** Printable ASCII lines (no LF, no CR). */
  def plainLines(lines: Range[Int], range: Range[Int]): Gen[Vector[String]] = Gens.asciiPrintable(range).list(lines).map(_.toVector)

}
