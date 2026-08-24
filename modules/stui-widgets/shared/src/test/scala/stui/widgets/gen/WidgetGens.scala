package stui.widgets.gen

import cats.syntax.all.*
import hedgehog.{Gen, Range}
import refined4s.types.numeric.NonNegInt
import stui.core.widget.Widget
import stui.testkit.gen.{GeometryGens, StyleGens, TextGens}
import stui.widgets.*

/** Generators for the M1d widgets.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object WidgetGens {

  /** Any side. */
  val side: Gen[Side] = Gen.elementUnsafe(Side.all)

  /** Any subset of sides. */
  val borders: Gen[Borders] = side.list(Range.linear(0, 4)).map(sides => Borders(sides.toSet))

  /** One of the five bundled sets. */
  val borderSet: Gen[BorderSet] =
    Gen.element1(BorderSet.plain, BorderSet.rounded, BorderSet.double, BorderSet.thick, BorderSet.empty)

  /** Small paddings (components 0 to 2). */
  val padding: Gen[Padding] =
    for {
      left   <- small(2)
      top    <- small(2)
      right  <- small(2)
      bottom <- small(2)
    } yield Padding(left, top, right, bottom)

  /** A short title with an optional explicit position. */
  val title: Gen[Title] =
    for {
      line     <- TextGens.line(Range.linear(0, 2), Range.linear(0, 6))
      position <- Gen.element1(Option.empty[TitlePosition], TitlePosition.Top.some, TitlePosition.Bottom.some)
    } yield Title(line, position)

  /** Blocks over every field. */
  val block: Gen[Block] =
    for {
      titles          <- title.list(Range.linear(0, 3)).map(_.toVector)
      titlesStyle     <- StyleGens.style
      titlesAlignment <- TextGens.alignment
      titlesPosition  <- Gen.element1(TitlePosition.Top, TitlePosition.Bottom)
      bs              <- borders
      set             <- borderSet
      borderStyle     <- StyleGens.style
      style           <- StyleGens.style
      pad             <- padding
    } yield Block(titles, titlesStyle, titlesAlignment, titlesPosition, bs, set, borderStyle, style, pad)

  /** Any wrap mode. */
  val wrap: Gen[Wrap] = Gen.element1(Wrap.Word, Wrap.WordTrimmed)

  /** Small scrolls (components 0 to 4). */
  val scroll: Gen[Scroll] =
    for {
      rows    <- small(4)
      columns <- small(4)
    } yield Scroll(rows, columns)

  /** Paragraphs over small texts with optional blocks and wraps. */
  val paragraph: Gen[Paragraph] =
    for {
      text  <- TextGens.text(Range.linear(0, 3), Range.linear(0, 2), Range.linear(0, 8))
      b     <- block.option
      style <- StyleGens.style
      w     <- wrap.option
      s     <- scroll
    } yield Paragraph(text, b, style, w, s)

  /** [[block]] widened for the widget laws. */
  val blockWidget: Gen[Widget] = block.map(b => b: Widget)

  /** [[paragraph]] widened for the widget laws. */
  val paragraphWidget: Gen[Widget] = paragraph.map(p => p: Widget)

  private def small(max: Int): Gen[NonNegInt] = Gen.int(Range.linear(0, max)).map(n => GeometryGens.nonNegOrZero(n.toLong))

}
