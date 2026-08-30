package stui.widgets

import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.testkit.Assertions
import stui.widgets.gen.WidgetGens

/** The [[Selection]] transitions (design doc 6.6, M2b): provisional indexes for "the last item", saturating moves, and the offset kept
  * by `deselect`.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object SelectionSpec extends Properties {

  private inline def nn(inline n: Int): NonNegInt = NonNegInt(n)

  private val last: Option[NonNegInt] = Some(NonNegInt.MaxValue)

  override def tests: List[Test] = List(
    example("selectNext from nothing selects the first", Assertions.eqv(Selection.none.selectNext, Selection.first)),
    example("selectNext advances", Assertions.eqv(Selection.at(nn(2)).selectNext, Selection.at(nn(3)))),
    example("selectPrevious floors at the first", Assertions.eqv(Selection.first.selectPrevious, Selection.first)),
    example(
      "selectPrevious from nothing selects the provisional last",
      Assertions.eqv(Selection.none.selectPrevious, Selection(nn(0), last)),
    ),
    example("selectFirst selects the first", Assertions.eqv(Selection.at(nn(5)).selectFirst, Selection.first)),
    example("selectLast is provisional", Assertions.eqv(Selection.none.selectLast, Selection(nn(0), last))),
    example("deselect keeps the offset", Assertions.eqv(Selection(nn(4), Some(nn(6))).deselect, Selection(nn(4), Option.empty[NonNegInt]))),
    example("movedBy floors at zero", Assertions.eqv(Selection.at(nn(2)).movedBy(-5), Selection.first)),
    example("movedBy from nothing starts at zero", Assertions.eqv(Selection.none.movedBy(3), Selection.at(nn(3)))),
    example("scrolledBy floors at zero", Assertions.eqv(Selection.none.scrolledBy(-1), Selection.none)),
    example("scrolledBy moves the offset", Assertions.eqv(Selection.none.scrolledBy(4), Selection(nn(4), Option.empty[NonNegInt]))),
    property(
      "selectNext never decreases the index",
      WidgetGens.selection.forAll.map { selection =>
        val before = selection.selected.fold(-1L)(_.value.toLong)
        Result.assert(selection.selectNext.selected.exists(_.value.toLong >= before)).log("selectNext went backwards")
      },
    ),
    property(
      "selectPrevious never increases a defined index",
      WidgetGens.selection.forAll.map { selection =>
        val next = selection.selectPrevious.selected
        Result.assert(selection.selected.forall(i => next.exists(_.value <= i.value))).log("selectPrevious went forwards")
      },
    ),
    property(
      "select and deselect keep the offset",
      for {
        selection <- WidgetGens.selection.forAll
        index     <- WidgetGens.selection.map(_.selected).forAll
      } yield Result.all(
        List(
          Assertions.eqv(selection.select(index).offset, selection.offset),
          Assertions.eqv(selection.deselect.offset, selection.offset),
        )
      ),
    ),
  )

}
