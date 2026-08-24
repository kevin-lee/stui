package stui.core.geometry

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import refined4s.types.numeric.NonNegInt
import stui.core.internal.NonNegInts
import stui.unicode.internal.IntOps.*

/** An axis-aligned rectangle of cells: `x` / `y` is the top-left corner, `width` / `height` its size. Every operation is total: sums
  * saturate at `Int.MaxValue`, differences floor at 0, and the exact rule is documented on each operation.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
final case class Rect(x: NonNegInt, y: NonNegInt, width: NonNegInt, height: NonNegInt) derives Eq, Show, Hash

object Rect {

  /** The zero-sized rect at the origin. */
  val empty: Rect = Rect(NonNegInt(0), NonNegInt(0), NonNegInt(0), NonNegInt(0))

  /** `Left` with refined4s's message when a component is negative. */
  def from(x: Int, y: Int, width: Int, height: Int): Either[String, Rect] =
    for {
      px <- NonNegInt.from(x)
      py <- NonNegInt.from(y)
      w  <- NonNegInt.from(width)
      h  <- NonNegInt.from(height)
    } yield Rect(px, py, w, h)

  /** The rect of that size at that position. */
  def at(position: Position, size: Size): Rect = Rect(position.x, position.y, size.width, size.height)

  /** The rect of that size at the origin. */
  def sized(size: Size): Rect = at(Position.origin, size)

  private def clampLong(n: Long, low: Long, high: Long): Long = math.max(low, math.min(n, high))

  extension (rect: Rect) {

    /** The top-left corner. */
    def position: Position = Position(rect.x, rect.y)

    /** The width and height. */
    def size: Size = Size(rect.width, rect.height)

    /** The first column, same as `x`. */
    def left: NonNegInt = rect.x

    /** The first row, same as `y`. */
    def top: NonNegInt = rect.y

    /** `x + width`, saturated at `Int.MaxValue`. */
    def right: NonNegInt = NonNegInts.plus(rect.x, rect.width)

    /** `y + height`, saturated at `Int.MaxValue`. */
    def bottom: NonNegInt = NonNegInts.plus(rect.y, rect.height)

    /** `width * height`, exact. */
    def area: Long = NonNegInts.times(rect.width, rect.height)

    /** True when the width or the height is 0. */
    def isEmpty: Boolean = rect.width.value === 0 || rect.height.value === 0

    /** Evaluated in `Long`, so a saturated `right` or `bottom` cannot produce a false positive. */
    def contains(position: Position): Boolean = {
      val px = position.x.value.toLong
      val py = position.y.value.toLong
      px >= rect.x.value.toLong && px < rightLong(rect) && py >= rect.y.value.toLong && py < bottomLong(rect)
    }

    /** The rect shrunk by the margin on every side: the origin moves in (saturated), the size loses twice the margin (floored at 0), so an
      * over-large margin gives an empty rect at the moved origin.
      */
    def inner(margin: Margin): Rect = {
      val h = margin.horizontal.value.toLong
      val v = margin.vertical.value.toLong
      Rect(
        NonNegInts.clamp(rect.x.value.toLong + h),
        NonNegInts.clamp(rect.y.value.toLong + v),
        NonNegInts.clamp(rect.width.value.toLong - 2L * h),
        NonNegInts.clamp(rect.height.value.toLong - 2L * v),
      )
    }

    /** The rect grown by the margin on every side: the origin moves out (floored at 0), the size gains twice the margin (saturated). */
    def outer(margin: Margin): Rect = {
      val h = margin.horizontal.value.toLong
      val v = margin.vertical.value.toLong
      Rect(
        NonNegInts.clamp(rect.x.value.toLong - h),
        NonNegInts.clamp(rect.y.value.toLong - v),
        NonNegInts.clamp(rect.width.value.toLong + 2L * h),
        NonNegInts.clamp(rect.height.value.toLong + 2L * v),
      )
    }

    /** The origin moved by the offset (floored at 0, saturated), the size unchanged. */
    def offset(delta: Offset): Rect =
      Rect(NonNegInts.offset(rect.x, delta.dx), NonNegInts.offset(rect.y, delta.dy), rect.width, rect.height)

    /** Same origin, new size. */
    def resize(size: Size): Rect = Rect(rect.x, rect.y, size.width, size.height)

    /** The smallest rect containing both. An empty rect still contributes its origin (Ratatui semantics). */
    def union(other: Rect): Rect = {
      val x1 = math.min(rect.x.value.toLong, other.x.value.toLong)
      val y1 = math.min(rect.y.value.toLong, other.y.value.toLong)
      val x2 = math.max(rightLong(rect), rightLong(other))
      val y2 = math.max(bottomLong(rect), bottomLong(other))
      Rect(NonNegInts.clamp(x1), NonNegInts.clamp(y1), NonNegInts.clamp(x2 - x1), NonNegInts.clamp(y2 - y1))
    }

    /** The overlap of both, an empty rect at `(max of the lefts, max of the tops)` when they do not overlap. */
    def intersection(other: Rect): Rect = {
      val x1 = math.max(rect.x.value.toLong, other.x.value.toLong)
      val y1 = math.max(rect.y.value.toLong, other.y.value.toLong)
      val x2 = math.min(rightLong(rect), rightLong(other))
      val y2 = math.min(bottomLong(rect), bottomLong(other))
      Rect(NonNegInts.clamp(x1), NonNegInts.clamp(y1), NonNegInts.clamp(x2 - x1), NonNegInts.clamp(y2 - y1))
    }

    /** True when the intersection is non-empty, so an empty rect intersects nothing (Ratatui's edge semantics differ). */
    def intersects(other: Rect): Boolean = !rect.intersection(other).isEmpty

    /** The rect moved, and shrunk if larger, so that it lies inside `other`: the size becomes the minimum of both sizes, then the origin is
      * clamped into the range that keeps the rect inside `other`.
      */
    def clamp(other: Rect): Rect = {
      val w  = NonNegInts.min(rect.width, other.width)
      val h  = NonNegInts.min(rect.height, other.height)
      val px = clampLong(rect.x.value.toLong, other.x.value.toLong, rightLong(other) - w.value.toLong)
      val py = clampLong(rect.y.value.toLong, other.y.value.toLong, bottomLong(other) - h.value.toLong)
      Rect(NonNegInts.clamp(px), NonNegInts.clamp(py), w, h)
    }

    /** `height` rects of height 1 with this rect's x and width, top to bottom. */
    def rows: Vector[Rect] =
      Vector.tabulate(rect.height.value)(i => Rect(rect.x, NonNegInts.offset(rect.y, i), rect.width, NonNegInt(1)))

    /** `width` rects of width 1 with this rect's y and height, left to right. */
    def columns: Vector[Rect] =
      Vector.tabulate(rect.width.value)(i => Rect(NonNegInts.offset(rect.x, i), rect.y, NonNegInt(1), rect.height))

  }

  private def rightLong(rect: Rect): Long = rect.x.value.toLong + rect.width.value.toLong

  private def bottomLong(rect: Rect): Long = rect.y.value.toLong + rect.height.value.toLong

}
