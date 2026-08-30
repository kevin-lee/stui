package stui.widgets.gen

import cats.syntax.all.*
import hedgehog.{Gen, Range}
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Canvas
import stui.core.frame.RegionId
import stui.core.geometry.{Rect, Size}
import stui.core.widget.{StatefulWidget, Widget}
import stui.testkit.gen.{GeometryGens, StyleGens, TextGens}
import stui.widgets.*

/** Generators for the M1d and M2a widgets.
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

  /** Content sizes for scroll views (width 0..14, height 0..8). */
  val scrollContentSize: Gen[Size] =
    for {
      width  <- small(14)
      height <- small(8)
    } yield Size(width, height)

  /** A small pool of region ids, so generated frames repeat them. */
  val scrollRegionId: Gen[RegionId] = Gen.element1("body", "log").map(RegionId(_))

  /** Scroll views over paragraph or block content, with optional blocks, styles, and regions. */
  val scrollView: Gen[ScrollView] =
    for {
      content <- Gen.frequency1(3 -> paragraphWidget, 1 -> blockWidget)
      size    <- scrollContentSize
      b       <- block.option
      style   <- StyleGens.style
      region  <- scrollRegionId.option
    } yield ScrollView(content, size, b, style, region)

  /** [[scrollView]] paired with an offset for the fixed-point law. */
  val scrollViewInputs: Gen[(StatefulWidget[Scroll], Scroll)] =
    for {
      view  <- scrollView
      state <- scroll
    } yield (view: StatefulWidget[Scroll], state)

  /** [[scrollView]] rendered at a generated offset, widened for the stateless widget laws. */
  val scrollViewWidget: Gen[Widget] =
    for {
      view  <- scrollView
      state <- scroll
    } yield new Widget {
      /* the method lives in the class body because it implements the Widget trait member */
      override def render(area: Rect, canvas: Canvas): Unit = view.render(area, canvas, state): Unit
    }

  /** Rings built by appending 0..9 lines over bounds 1..5, so eviction and a moved `firstIndex` occur naturally. */
  val logRing: Gen[LogRing] =
    for {
      /* the bound is 1..5, so the PosInt fallback is unreachable */
      bound <- Gen.int(Range.linear(1, 5)).map(n => PosInt.from(n).getOrElse(PosInt(1)))
      lines <- TextGens.line(Range.linear(0, 2), Range.linear(0, 6)).list(Range.linear(0, 9))
    } yield LogRing.empty(bound).appendAll(lines.toVector)

  /** Anchors near the generated rings' index ranges, following or not. */
  val logViewState: Gen[LogViewState] =
    for {
      anchor    <- Gen.long(Range.linear(0L, 12L)).map(LogRing.nonNegLong)
      following <- Gen.boolean
    } yield LogViewState(anchor, following)

  /** Log views over generated rings, with optional blocks, styles, and regions. */
  val logView: Gen[LogView] =
    for {
      ring   <- logRing
      b      <- block.option
      style  <- StyleGens.style
      region <- scrollRegionId.option
    } yield LogView(ring, b, style, region)

  /** [[logView]] paired with a state for the fixed-point law. */
  val logViewInputs: Gen[(StatefulWidget[LogViewState], LogViewState)] =
    for {
      view  <- logView
      state <- logViewState
    } yield (view: StatefulWidget[LogViewState], state)

  /** [[logView]] rendered at a generated state, widened for the stateless widget laws. */
  val logViewWidget: Gen[Widget] =
    for {
      view  <- logView
      state <- logViewState
    } yield new Widget {
      /* the method lives in the class body because it implements the Widget trait member */
      override def render(area: Rect, canvas: Canvas): Unit = view.render(area, canvas, state): Unit
    }

  private def small(max: Int): Gen[NonNegInt] = Gen.int(Range.linear(0, max)).map(n => GeometryGens.nonNegOrZero(n.toLong))

}
