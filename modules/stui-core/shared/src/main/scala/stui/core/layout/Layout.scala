package stui.core.layout

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.geometry.Rect
import stui.core.internal.NonNegInts
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** A deterministic split of a [[Rect]] into one segment per [[Constraint]] (design doc 9.2, decision D4): pure integer arithmetic, no
  * solver, no cache. Build one with [[Layout.horizontal]] or [[Layout.vertical]], refine it with the `with` builders, and run it with
  * [[Layout.split]].
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Layout(direction: Direction, constraints: Vector[Constraint], spacing: Spacing, flex: Flex) derives Eq, Show, Hash

object Layout {

  /** A horizontal split (segments side by side) with no spacing and [[Flex.Start]]. */
  def horizontal(constraints: Constraint*): Layout = Layout(Direction.Horizontal, constraints.toVector, Spacing.none, Flex.Start)

  /** A vertical split (segments stacked) with no spacing and [[Flex.Start]]. */
  def vertical(constraints: Constraint*): Layout = Layout(Direction.Vertical, constraints.toVector, Spacing.none, Flex.Start)

  /** A split along the given direction with no spacing and [[Flex.Start]]. */
  def of(direction: Direction, constraints: Vector[Constraint]): Layout = Layout(direction, constraints, Spacing.none, Flex.Start)

  extension (layout: Layout) {

    /** The layout with the direction replaced. */
    def withDirection(direction: Direction): Layout = layout.copy(direction = direction)

    /** The layout with the constraints replaced. */
    def withConstraints(constraints: Vector[Constraint]): Layout = layout.copy(constraints = constraints)

    /** The layout with the spacing replaced. */
    def withSpacing(spacing: Spacing): Layout = layout.copy(spacing = spacing)

    /** The layout with the flex mode replaced. */
    def withFlex(flex: Flex): Layout = layout.copy(flex = flex)

    /** Splits `area` into exactly one rect per constraint, in order, total on every input.
      *
      * The length along the direction minus the spacing between neighbours (`spacing * (n - 1)`, an overlap adds space back) is the
      * available length, capped at `Int.MaxValue`. Sizes are decided on it in two phases and the excess is placed by the flex mode:
      *
      *   1. Allocation, class by class in priority order, left to right within a class, each grant capped by what remains:
      *      [[Constraint.Min]] floors first, then [[Constraint.Length]] demands, then the proportional demands
      *      ([[Constraint.Percentage]] as `available * p / 100` and [[Constraint.Ratio]] as `available * a / b`, each rounded half up),
      *      then [[Constraint.Max]] preferences. When the space runs out, the lower priority and then the rightmost demand gives way.
      *   1. Growth: the remaining space and the granted `Min` floors form a pool shared by the [[Constraint.Fill]] segments (their
      *      weights) and the `Min` segments (weight 1) in proportion, floors respected (a `Min` whose share falls below its granted
      *      floor keeps the floor and leaves the pool), the final shares rounded by largest remainder (ties to the leftmost). `Max`
      *      never grows. Without growers the leftover is the flex excess.
      *   1. Placement: [[Flex.Start]] leads with 0, [[Flex.End]] with the excess, [[Flex.Center]] with half of it (rounded down).
      *      [[Flex.SpaceBetween]] spreads the excess over the inner gaps, [[Flex.SpaceEvenly]] over all `n + 1` gaps equally, and
      *      [[Flex.SpaceAround]] with the edge gaps at half weight, each by largest remainder with the leftover to the leftmost gaps,
      *      on top of the spacing (the minimum gap). An [[Spacing.Overlap]] makes the inner gap negative, and a segment never starts
      *      before its predecessor (`start = max(previous start, previous end - overlap)`). Every rect is clipped to the area, and the
      *      cross axis always spans the area.
      *
      * Documented divergences from Ratatui 0.30: the proportional base is the post-spacing available length, over-constrained demands
      * are cut rightmost first instead of solver compromises, `SpaceBetween` with one segment behaves like `Start` (CSS), there is no
      * `Legacy` mode (a trailing `Fill` expresses it), and exact parity in conflicting-constraint corners is a non-goal. As in Ratatui
      * 0.30, `Min` grows like `Fill(1)` and `Max` stays at its preference.
      *
      * Saturation: an area whose cells would extend beyond `Int.MaxValue` saturates like the rest of the geometry, so segments past the
      * last representable column collapse onto it.
      */
    def split(area: Rect): Vector[Rect] = {
      val constraints = IArray.from(layout.constraints)
      val n           = constraints.length
      if (n === 0) {
        Vector.empty[Rect]
      } else {
        val total     = mainLength(layout.direction, area)
        val start0    = mainStart(layout.direction, area)
        val areaEnd   = start0 + total
        val gap       = signedGap(layout.spacing)
        val available = math.min(math.max(0L, total - gap * (n - 1).toLong), Int.MaxValue.toLong)

        /* local arrays that never escape this call, the blessed pattern for index loops (design principle 1) */
        val sizes = new Array[Long](n)

        val afterMins    = grant(constraints, sizes, minWant, 0, available)
        val afterLengths = grant(constraints, sizes, lengthWant, 0, afterMins)
        val afterShares  = grant(constraints, sizes, proportionalWant(available), 0, afterLengths)
        val afterMaxes   = grant(constraints, sizes, maxWant, 0, afterShares)

        val growers = collectGrowers(constraints, sizes, 0, Vector.empty[Grower])
        val excess  =
          if (growers.isEmpty) {
            afterMaxes
          } else {
            val floors = growers.foldLeft(0L)((acc, grower) => acc + grower.floor)
            waterFill(growers, afterMaxes + floors, sizes)
          }

        val placement = gapPlacement(layout.flex, excess, gap, n)
        val positions = new Array[Long](n)
        placeLoop(sizes, placement, 0, start0 + placement.leading, positions)
        Vector.tabulate(n)(i => segmentRect(layout.direction, area, areaEnd, positions(i), sizes(i)))
      }
    }

  }

  private def mainLength(direction: Direction, area: Rect): Long = direction match {
    case Direction.Horizontal => area.width.value.toLong
    case Direction.Vertical => area.height.value.toLong
  }

  private def mainStart(direction: Direction, area: Rect): Long = direction match {
    case Direction.Horizontal => area.x.value.toLong
    case Direction.Vertical => area.y.value.toLong
  }

  private def signedGap(spacing: Spacing): Long = spacing match {
    case Spacing.Space(n) => n.value.toLong
    case Spacing.Overlap(n) => -n.value.toLong
  }

  /** Adds `min(want, remaining)` to each segment the class covers, left to right, and returns what remains. */
  @tailrec
  private def grant(constraints: IArray[Constraint], sizes: Array[Long], want: Constraint => Option[Long], i: Int, remaining: Long): Long =
    if (i >= constraints.length) {
      remaining
    } else {
      want(constraints(i)) match {
        case Some(demand) =>
          val granted = math.min(math.max(0L, demand), remaining)
          sizes(i) = sizes(i) + granted
          grant(constraints, sizes, want, i + 1, remaining - granted)
        case None => grant(constraints, sizes, want, i + 1, remaining)
      }
    }

  private def minWant(constraint: Constraint): Option[Long] = constraint match {
    case Constraint.Min(m) => m.value.toLong.some
    case Constraint.Length(_) | Constraint.Percentage(_) | Constraint.Ratio(_, _) | Constraint.Max(_) | Constraint.Fill(_) =>
      none[Long]
  }

  private def lengthWant(constraint: Constraint): Option[Long] = constraint match {
    case Constraint.Length(m) => m.value.toLong.some
    case Constraint.Percentage(_) | Constraint.Ratio(_, _) | Constraint.Min(_) | Constraint.Max(_) | Constraint.Fill(_) =>
      none[Long]
  }

  /** The proportional share of the available length, rounded half up: `(2 * available * num + den) / (2 * den)`. */
  private def proportionalWant(available: Long)(constraint: Constraint): Option[Long] = constraint match {
    case Constraint.Percentage(p) => ((2L * available * p.value.toLong + 100L) / 200L).some
    case Constraint.Ratio(a, b) => ((2L * available * a.value.toLong + b.value.toLong) / (2L * b.value.toLong)).some
    case Constraint.Length(_) | Constraint.Min(_) | Constraint.Max(_) | Constraint.Fill(_) => none[Long]
  }

  private def maxWant(constraint: Constraint): Option[Long] = constraint match {
    case Constraint.Max(m) => m.value.toLong.some
    case Constraint.Length(_) | Constraint.Percentage(_) | Constraint.Ratio(_, _) | Constraint.Min(_) | Constraint.Fill(_) =>
      none[Long]
  }

  /** A growth participant: a `Fill` with its weight, or a `Min` with weight 1 and its granted floor. */
  final private case class Grower(index: Int, weight: Long, floor: Long)

  @tailrec
  private def collectGrowers(constraints: IArray[Constraint], sizes: Array[Long], i: Int, acc: Vector[Grower]): Vector[Grower] =
    if (i >= constraints.length) {
      acc
    } else {
      constraints(i) match {
        case Constraint.Min(_) => collectGrowers(constraints, sizes, i + 1, acc :+ Grower(i, 1L, sizes(i)))
        case Constraint.Fill(weight) => collectGrowers(constraints, sizes, i + 1, acc :+ Grower(i, weight.value.toLong, 0L))
        case Constraint.Length(_) | Constraint.Percentage(_) | Constraint.Ratio(_, _) | Constraint.Max(_) =>
          collectGrowers(constraints, sizes, i + 1, acc)
      }
    }

  /** Distributes the pool over the growers proportionally with the floors respected (pinning), and returns the leftover (0 unless
    * every grower is a pinned `Min`).
    */
  @tailrec
  private def waterFill(active: Vector[Grower], pool: Long, sizes: Array[Long]): Long =
    if (active.isEmpty) {
      pool
    } else {
      val weightSum = active.foldLeft(0L)((acc, grower) => acc + grower.weight)
      val pinned    = active.filter(grower => pool * grower.weight / weightSum < grower.floor)
      if (pinned.isEmpty) {
        distribute(active, pool, weightSum, sizes)
        0L
      } else {
        pinned.foreach(grower => sizes(grower.index) = grower.floor)
        val pinnedIndices = pinned.map(_.index).toSet
        val floorsTaken   = pinned.foldLeft(0L)((acc, grower) => acc + grower.floor)
        waterFill(active.filterNot(grower => pinnedIndices.contains(grower.index)), pool - floorsTaken, sizes)
      }
    }

  /** Largest-remainder distribution of the pool over the active growers, ties to the lowest index, the sum exactly the pool. */
  private def distribute(active: Vector[Grower], pool: Long, weightSum: Long, sizes: Array[Long]): Unit = {
    val based    = active.map(grower => (grower, pool * grower.weight / weightSum, pool * grower.weight % weightSum))
    val baseSum  = based.foldLeft(0L) { case (acc, (_, base, _)) => acc + base }
    val leftover = (pool - baseSum).toInt
    val ordered  = based.sortBy { case (grower, _, remainder) => (-remainder, grower.index) }
    ordered.zipWithIndex.foreach {
      case ((grower, base, _), k) =>
        sizes(grower.index) = base + (if (k < leftover) 1L else 0L)
    }
  }

  /** The leading offset and the extra (on top of the spacing) for each of the `n - 1` inner gaps. */
  final private case class GapPlacement(leading: Long, gap: Long, extras: Array[Long])

  private def gapPlacement(flex: Flex, excess: Long, gap: Long, n: Int): GapPlacement = flex match {
    case Flex.Start => GapPlacement(0L, gap, new Array[Long](math.max(0, n - 1)))
    case Flex.End => GapPlacement(excess, gap, new Array[Long](math.max(0, n - 1)))
    case Flex.Center => GapPlacement(excess / 2L, gap, new Array[Long](math.max(0, n - 1)))
    case Flex.SpaceBetween =>
      if (n <= 1) GapPlacement(0L, gap, new Array[Long](math.max(0, n - 1)))
      else GapPlacement(0L, gap, largestRemainder(excess, Array.fill(n - 1)(1L)))
    case Flex.SpaceEvenly =>
      val shares = largestRemainder(excess, Array.fill(n + 1)(1L))
      GapPlacement(shares(0), gap, Array.tabulate(math.max(0, n - 1))(i => shares(i + 1)))
    case Flex.SpaceAround =>
      val weights = Array.tabulate(n + 1)(j => if (j === 0 || j === n) 1L else 2L)
      val shares  = largestRemainder(excess, weights)
      GapPlacement(shares(0), gap, Array.tabulate(math.max(0, n - 1))(i => shares(i + 1)))
  }

  /** Splits `total` units over the weights by largest remainder (ties to the lowest index), the sum exactly `total`. */
  private def largestRemainder(total: Long, weights: Array[Long]): Array[Long] = {
    val count     = weights.length
    /* local arrays that never escape this method */
    val shares    = new Array[Long](count)
    val weightSum = weights.foldLeft(0L)((acc, weight) => acc + weight)
    if (weightSum <= 0L || total <= 0L) {
      shares
    } else {
      val remainders = new Array[Long](count)

      @tailrec
      def bases(i: Int, allocated: Long): Long =
        if (i >= count) {
          allocated
        } else {
          shares(i) = total * weights(i) / weightSum
          remainders(i) = total * weights(i) % weightSum
          bases(i + 1, allocated + shares(i))
        }

      val leftover = (total - bases(0, 0L)).toInt
      val ordered  = (0 until count).sortBy(i => (-remainders(i), i))
      ordered.take(leftover).foreach(i => shares(i) = shares(i) + 1L)
      shares
    }
  }

  /** Fills `positions`: each segment starts at the running position and never before its predecessor (the overlap clamp). */
  @tailrec
  private def placeLoop(sizes: Array[Long], placement: GapPlacement, i: Int, position: Long, positions: Array[Long]): Unit =
    if (i >= sizes.length) {
      ()
    } else {
      positions(i) = position
      val innerGap = if (i < sizes.length - 1) placement.gap + placement.extras(i) else 0L
      placeLoop(sizes, placement, i + 1, math.max(position, position + sizes(i) + innerGap), positions)
    }

  /** One clipped segment rect: the main axis from the position with the size (both clipped into the area), the cross axis spanning the
    * area.
    */
  private def segmentRect(direction: Direction, area: Rect, areaEnd: Long, position: Long, size: Long): Rect = {
    val origin   = math.min(position, areaEnd)
    val mainSize = math.max(0L, math.min(size, areaEnd - origin))
    direction match {
      case Direction.Horizontal => Rect(NonNegInts.clamp(origin), area.y, NonNegInts.clamp(mainSize), area.height)
      case Direction.Vertical => Rect(area.x, NonNegInts.clamp(origin), area.width, NonNegInts.clamp(mainSize))
    }
  }

}
