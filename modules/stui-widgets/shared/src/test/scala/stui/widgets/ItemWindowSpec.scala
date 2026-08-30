package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.GeometryGens
import stui.widgets.gen.WidgetGens
import stui.widgets.internal.ItemWindow

/** The item-offset correction rule (design doc 6.6, M2b): fixture rows over uniform and variable extents, and the fixed-point,
  * bounds, and visibility laws.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ItemWindowSpec extends Properties {

  private def sel(offset: Int, selected: Option[Int]): Selection =
    Selection(GeometryGens.nonNegOrZero(offset.toLong), selected.map(i => GeometryGens.nonNegOrZero(i.toLong)))

  private val ten: Vector[Int] = Vector.fill(10)(1)

  private val extents: Gen[Vector[Int]] = Gen.int(Range.linear(1, 3)).list(Range.linear(0, 12)).map(_.toVector)

  override def tests: List[Test] = List(
    example("the selection scrolls the window down", Assertions.eqv(ItemWindow.correct(sel(0, Some(5)), ten, 3, 0), sel(3, Some(5)))),
    example(
      "the offset clamps, then the selection pulls it back",
      Assertions.eqv(ItemWindow.correct(sel(9, Some(5)), ten, 3, 0), sel(5, Some(5))),
    ),
    example(
      "without a selection the offset clamps to the largest useful one",
      Assertions.eqv(ItemWindow.correct(sel(9, None), ten, 3, 0), sel(7, None)),
    ),
    example("padding keeps context below the selection", Assertions.eqv(ItemWindow.correct(sel(0, Some(6)), ten, 5, 1), sel(3, Some(6)))),
    example("trailingFit counts whole trailing items", Assertions.eqv(ItemWindow.trailingFit(Vector(1, 3, 1, 1), 3), 2)),
    example(
      "variable extents clamp the offset by the trailing fit",
      Assertions.eqv(ItemWindow.correct(sel(9, None), Vector(1, 3, 1, 1), 3, 0), sel(2, None)),
    ),
    example("a single oversized item still counts as fitting", Assertions.eqv(ItemWindow.trailingFit(Vector(5), 3), 1)),
    example("an oversized item keeps offset zero", Assertions.eqv(ItemWindow.correct(sel(0, None), Vector(5), 3, 0), sel(0, None))),
    example("no items correct to none", Assertions.eqv(ItemWindow.correct(sel(3, Some(2)), Vector.empty[Int], 3, 0), Selection.none)),
    example("no items have a trailing fit of zero", Assertions.eqv(ItemWindow.trailingFit(Vector.empty[Int], 3), 0)),
    example("a selection beyond the items clamps", Assertions.eqv(ItemWindow.correct(sel(0, Some(40)), ten, 3, 0), sel(7, Some(9)))),
    property(
      "correct is a fixed point",
      for {
        ext       <- extents.forAll
        viewport  <- Gen.int(Range.linear(0, 6)).forAll
        padding   <- Gen.int(Range.linear(0, 2)).forAll
        selection <- WidgetGens.selection.forAll
      } yield {
        val once = ItemWindow.correct(selection, ext, viewport, padding)
        Assertions.eqv(ItemWindow.correct(once, ext, viewport, padding), once)
      },
    ),
    property(
      "the corrected offset and selection lie within the items",
      for {
        ext       <- extents.forAll
        viewport  <- Gen.int(Range.linear(0, 6)).forAll
        padding   <- Gen.int(Range.linear(0, 2)).forAll
        selection <- WidgetGens.selection.forAll
      } yield {
        val corrected = ItemWindow.correct(selection, ext, viewport, padding)
        Result.all(
          List(
            Result.assert(corrected.offset.value <= math.max(0, ext.length - 1)).log("offset beyond the items"),
            Result.assert(corrected.selected.forall(_.value < ext.length)).log("selection beyond the items"),
          )
        )
      },
    ),
    property(
      "the selected item is visible from the offset",
      for {
        ext      <- extents.forAll
        viewport <- Gen.int(Range.linear(1, 6)).forAll
        padding  <- Gen.int(Range.linear(0, 2)).forAll
        offset   <- Gen.int(Range.linear(0, 12)).forAll
        index    <- Gen.int(Range.linear(0, 12)).forAll
      } yield {
        val corrected = ItemWindow.correct(sel(offset, Some(index)), ext, viewport, padding)
        corrected.selected match {
          case None => Result.assert(ext.isEmpty).log("the selection vanished")
          case Some(s) =>
            val o    = corrected.offset.value
            val used = ext.slice(o, s.value + 1).foldLeft(0L)((acc, e) => acc + e.toLong)
            Result.all(
              List(
                Result.assert(s.value >= o).log("the selection is above the offset"),
                Result.assert(used <= viewport.toLong || o >= s.value).log("the selection is below the window"),
              )
            )
        }
      },
    ),
  )

}
