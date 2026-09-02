package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.geometry.Rect

/** A [[Layout]] without its constraints (design doc 9.2, decision D26): the direction, spacing, and flex of a split, with the
  * constraints supplied where the arity is known, so each fixed-arity split returns a tuple of exactly that many rects and no call
  * site needs an unreachable fallback. The semantics are [[Layout.split]]'s exactly, each form is computed by it. `Layout.split` stays
  * for variable arity (a table's columns), larger layouts nest.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
final case class Axis(direction: Direction, spacing: Spacing, flex: Flex) derives Eq, Show, Hash

object Axis {

  /** Segments side by side with no spacing and [[Flex.Start]]. */
  val horizontal: Axis = Axis(Direction.Horizontal, Spacing.none, Flex.Start)

  /** Segments stacked with no spacing and [[Flex.Start]]. */
  val vertical: Axis = Axis(Direction.Vertical, Spacing.none, Flex.Start)

  /** The given direction with no spacing and [[Flex.Start]]. */
  def of(direction: Direction): Axis = Axis(direction, Spacing.none, Flex.Start)

  extension (axis: Axis) {

    /** The axis with the spacing replaced. */
    def withSpacing(spacing: Spacing): Axis = axis.copy(spacing = spacing)

    /** The axis with the flex mode replaced. */
    def withFlex(flex: Flex): Axis = axis.copy(flex = flex)

    /** The [[Layout]] over these constraints, the bridge to the variable-arity form. */
    def layout(constraints: Vector[Constraint]): Layout = Layout(axis.direction, constraints, axis.spacing, axis.flex)

    /* The index reads below are total because `Layout.split` returns one rect per constraint (the "one rect per constraint" law in
     * `LayoutLaws`), so the single partial point of a fixed-arity split lives here under that law instead of in every application. */

    /** The area split into two segments under these constraints, in order, total on every input (see [[Layout.split]] for the rule). */
    def split2(area: Rect, first: Constraint, second: Constraint): (Rect, Rect) = {
      val rects = IArray.from(axis.layout(Vector(first, second)).split(area))
      (rects(0), rects(1))
    }

    /** The area split into three segments under these constraints, in order, total on every input (see [[Layout.split]] for the
      * rule).
      */
    def split3(area: Rect, first: Constraint, second: Constraint, third: Constraint): (Rect, Rect, Rect) = {
      val rects = IArray.from(axis.layout(Vector(first, second, third)).split(area))
      (rects(0), rects(1), rects(2))
    }

    /** The area split into four segments under these constraints, in order, total on every input (see [[Layout.split]] for the rule). */
    def split4(area: Rect, first: Constraint, second: Constraint, third: Constraint, fourth: Constraint): (Rect, Rect, Rect, Rect) = {
      val rects = IArray.from(axis.layout(Vector(first, second, third, fourth)).split(area))
      (rects(0), rects(1), rects(2), rects(3))
    }

    /** The area split into five segments under these constraints, in order, total on every input (see [[Layout.split]] for the rule). */
    def split5(
      area: Rect,
      first: Constraint,
      second: Constraint,
      third: Constraint,
      fourth: Constraint,
      fifth: Constraint,
    ): (Rect, Rect, Rect, Rect, Rect) = {
      val rects = IArray.from(axis.layout(Vector(first, second, third, fourth, fifth)).split(area))
      (rects(0), rects(1), rects(2), rects(3), rects(4))
    }

    /** The area split into six segments under these constraints, in order, total on every input (see [[Layout.split]] for the rule). */
    def split6(
      area: Rect,
      first: Constraint,
      second: Constraint,
      third: Constraint,
      fourth: Constraint,
      fifth: Constraint,
      sixth: Constraint,
    ): (Rect, Rect, Rect, Rect, Rect, Rect) = {
      val rects = IArray.from(axis.layout(Vector(first, second, third, fourth, fifth, sixth)).split(area))
      (rects(0), rects(1), rects(2), rects(3), rects(4), rects(5))
    }

  }

}
