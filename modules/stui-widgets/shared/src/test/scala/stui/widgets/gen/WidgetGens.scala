package stui.widgets.gen

import cats.syntax.all.*
import hedgehog.{Gen, Range}
import hedgehog.extra.refined4s.gens.NumGens
import refined4s.types.numeric.{NonNegInt, PosInt}
import stui.core.buffer.Canvas
import stui.core.frame.RegionId
import stui.core.geometry.{Rect, Size}
import stui.core.widget.{Measurable, StatefulWidget, Widget}
import stui.testkit.gen.{GeometryGens, LayoutGens, StyleGens, TextGens}
import stui.unicode.WidthPolicy
import stui.widgets.*

/** Generators for the M1d, M2a, and M2b widgets.
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
  val scrollRegionId: Gen[RegionId] = Gen.element1("body", "log", "list", "tabs", "table").map(RegionId(_))

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

  /** Offsets 0..12 with an optional selection 0..12, sometimes the provisional last (`NonNegInt.MaxValue`). */
  val selection: Gen[Selection] =
    for {
      offset   <- small(12)
      selected <- Gen.frequency1(6 -> small(12), 1 -> Gen.constant(NonNegInt.MaxValue)).option
    } yield Selection(offset, selected)

  /** Items of 0..2 lines with a style. */
  val listItem: Gen[ListItem] =
    for {
      text  <- TextGens.text(Range.linear(0, 2), Range.linear(0, 2), Range.linear(0, 6))
      style <- StyleGens.style
    } yield ListItem(text, style)

  /** Any highlight-spacing rule. */
  val highlightSpacing: Gen[HighlightSpacing] =
    Gen.element1(HighlightSpacing.Always, HighlightSpacing.WhenSelected, HighlightSpacing.Never)

  /** Any list direction. */
  val listDirection: Gen[ListDirection] = Gen.element1(ListDirection.TopToBottom, ListDirection.BottomToTop)

  /** List views over 0..8 items with optional blocks, symbols, and regions, every rule and style random. */
  val listView: Gen[ListView] =
    for {
      items     <- listItem.list(Range.linear(0, 8))
      b         <- block.option
      style     <- StyleGens.style
      highlight <- StyleGens.style
      symbol    <- TextGens.line(Range.linear(0, 1), Range.linear(0, 2)).option
      repeat    <- Gen.boolean
      spacing   <- highlightSpacing
      padding   <- small(2)
      direction <- listDirection
      region    <- scrollRegionId.option
    } yield ListView(items.toVector, b, style, highlight, symbol, repeat, spacing, padding, direction, region)

  /** [[listView]] paired with a selection for the fixed-point law. */
  val listViewInputs: Gen[(StatefulWidget[Selection], Selection)] =
    for {
      view  <- listView
      state <- selection
    } yield (view: StatefulWidget[Selection], state)

  /** [[listView]] rendered at a generated selection, widened for the stateless widget laws. */
  val listViewWidget: Gen[Widget] =
    for {
      view  <- listView
      state <- selection
    } yield new Widget {
      /* the method lives in the class body because it implements the Widget trait member */
      override def render(area: Rect, canvas: Canvas): Unit = view.render(area, canvas, state): Unit
    }

  /** Tab strips over 0..5 titles with random dividers, paddings, styles, and regions. */
  val tabs: Gen[Tabs] =
    for {
      titles    <- TextGens.line(Range.linear(0, 2), Range.linear(0, 5)).list(Range.linear(0, 5))
      divider   <- TextGens.span(Range.linear(0, 2))
      left      <- TextGens.span(Range.linear(0, 2))
      right     <- TextGens.span(Range.linear(0, 2))
      style     <- StyleGens.style
      highlight <- StyleGens.style
      region    <- scrollRegionId.option
    } yield Tabs(titles.toVector, divider, left, right, style, highlight, region)

  /** [[tabs]] paired with a selection for the fixed-point law. */
  val tabsInputs: Gen[(StatefulWidget[Selection], Selection)] =
    for {
      strip <- tabs
      state <- selection
    } yield (strip: StatefulWidget[Selection], state)

  /** [[tabs]] rendered at a generated selection and measuring as the strip, for the Measurable laws. */
  val tabsMeasurable: Gen[Widget & Measurable] =
    for {
      strip <- tabs
      state <- selection
    } yield new Widget with Measurable {
      /* the methods live in the class body because they implement the Widget and Measurable trait members */
      override def render(area: Rect, canvas: Canvas): Unit = strip.render(area, canvas, state): Unit

      override def measure(constraints: Size, policy: WidthPolicy): Size = strip.measure(constraints, policy)
    }

  /** [[tabs]] rendered at a generated selection, widened for the stateless widget laws. */
  val tabsWidget: Gen[Widget] = tabsMeasurable.map(strip => strip: Widget)

  /** Rows of 0..4 cells with a style, a height of 1..3, and margins of 0..1. */
  val row: Gen[Row] =
    for {
      cells  <- TextGens.text(Range.linear(0, 2), Range.linear(0, 2), Range.linear(0, 5)).list(Range.linear(0, 4))
      style  <- StyleGens.style
      height <- NumGens.genPosIntMaxTo(PosInt(3))
      top    <- small(1)
      bottom <- small(1)
    } yield Row(cells.toVector, style, height, top, bottom)

  /** Table states over [[selection]] and a small optional column. */
  val tableState: Gen[TableState] =
    for {
      rows   <- selection
      column <- small(4).option
    } yield TableState(rows, column)

  /** Tables over 0..6 rows with random widths, spacing, flex, header, footer, block, styles, symbol, and region. */
  val table: Gen[Table] =
    for {
      rows      <- row.list(Range.linear(0, 6))
      widths    <- LayoutGens.constraint(NonNegInt(8)).list(Range.linear(0, 4))
      spacing   <- small(2)
      flex      <- LayoutGens.flex
      header    <- row.option
      footer    <- row.option
      b         <- block.option
      style     <- StyleGens.style
      rowHl     <- StyleGens.style
      columnHl  <- StyleGens.style
      cellHl    <- StyleGens.style
      symbol    <- TextGens.line(Range.linear(0, 1), Range.linear(0, 2)).option
      hlSpacing <- highlightSpacing
      padding   <- small(2)
      region    <- scrollRegionId.option
    } yield Table(
      rows.toVector,
      widths.toVector,
      spacing,
      flex,
      header,
      footer,
      b,
      style,
      rowHl,
      columnHl,
      cellHl,
      symbol,
      hlSpacing,
      padding,
      region,
    )

  /** [[table]] paired with a state for the fixed-point law. */
  val tableInputs: Gen[(StatefulWidget[TableState], TableState)] =
    for {
      t     <- table
      state <- tableState
    } yield (t: StatefulWidget[TableState], state)

  /** [[table]] rendered at a generated state, widened for the stateless widget laws. */
  val tableWidget: Gen[Widget] =
    for {
      t     <- table
      state <- tableState
    } yield new Widget {
      /* the method lives in the class body because it implements the Widget trait member */
      override def render(area: Rect, canvas: Canvas): Unit = t.render(area, canvas, state): Unit
    }

  /** Either gauge set. */
  val gaugeSet: Gen[GaugeSet] = Gen.element1(GaugeSet.unicode, GaugeSet.ascii)

  /** Gauges over small fractions with optional labels and blocks, random styles, either set. */
  val gauge: Gen[Gauge] =
    for {
      done       <- small(12)
      total      <- NumGens.genPosIntMaxTo(PosInt(10))
      label      <- TextGens.line(Range.linear(0, 1), Range.linear(0, 4)).option
      b          <- block.option
      style      <- StyleGens.style
      gaugeStyle <- StyleGens.style
      set        <- gaugeSet
    } yield Gauge(done, total, label, b, style, gaugeStyle, set)

  /** [[gauge]] widened for the widget laws. */
  val gaugeWidget: Gen[Widget] = gauge.map(g => g: Widget)

  /** Any orientation. */
  val scrollbarOrientation: Gen[ScrollbarOrientation] = Gen.element1(
    ScrollbarOrientation.VerticalRight,
    ScrollbarOrientation.VerticalLeft,
    ScrollbarOrientation.HorizontalBottom,
    ScrollbarOrientation.HorizontalTop,
  )

  /** One of the six bundled sets, sometimes without arrows. */
  val scrollbarSet: Gen[ScrollbarSet] =
    for {
      set    <- Gen.element1(
                  ScrollbarSet.vertical,
                  ScrollbarSet.doubleVertical,
                  ScrollbarSet.horizontal,
                  ScrollbarSet.doubleHorizontal,
                  ScrollbarSet.asciiVertical,
                  ScrollbarSet.asciiHorizontal,
                )
      arrows <- Gen.boolean
    } yield if (arrows) set else set.withoutArrows

  /** Scrollbars over small contents, viewports, and positions (sometimes beyond the content), any set and styles. */
  val scrollbar: Gen[Scrollbar] =
    for {
      orientation <- scrollbarOrientation
      content     <- small(30)
      viewport    <- small(12)
      position    <- small(40)
      set         <- scrollbarSet
      style       <- StyleGens.style
      thumbStyle  <- StyleGens.style
    } yield Scrollbar(orientation, content, viewport, position, set, style, thumbStyle)

  /** [[scrollbar]] widened for the widget laws. */
  val scrollbarWidget: Gen[Widget] = scrollbar.map(bar => bar: Widget)

  /** [[scrollbar]] widened for the Measurable laws. */
  val scrollbarMeasurable: Gen[Widget & Measurable] = scrollbar.map(bar => bar: Widget & Measurable)

  private def small(max: Int): Gen[NonNegInt] = Gen.int(Range.linear(0, max)).map(n => GeometryGens.nonNegOrZero(n.toLong))

}
