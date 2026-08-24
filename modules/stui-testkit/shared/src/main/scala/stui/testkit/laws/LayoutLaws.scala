package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Range, Result}
import hedgehog.runner.*
import stui.core.geometry.{Rect, Size}
import stui.core.layout.{Constraint, Direction, Layout, Spacing}
import stui.testkit.Assertions
import stui.testkit.gen.{GeometryGens, LayoutGens}

/** The layout laws of design doc 9.2: count, containment, cross axis, order, bounded overlap, Fill sums and balance, equal-weight
  * monotonicity, single-percentage exactness, and duality.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
object LayoutLaws {

  private def mainStart(direction: Direction, rect: Rect): Long = direction match {
    case Direction.Horizontal => rect.x.value.toLong
    case Direction.Vertical => rect.y.value.toLong
  }

  private def mainSize(direction: Direction, rect: Rect): Long = direction match {
    case Direction.Horizontal => rect.width.value.toLong
    case Direction.Vertical => rect.height.value.toLong
  }

  private def crossMatchesArea(direction: Direction, area: Rect, rect: Rect): Boolean = direction match {
    case Direction.Horizontal => rect.y === area.y && rect.height === area.height
    case Direction.Vertical => rect.x === area.x && rect.width === area.width
  }

  private def allowedOverlap(spacing: Spacing): Long = spacing match {
    case Spacing.Space(_) => 0L
    case Spacing.Overlap(k) => k.value.toLong
  }

  private def transpose(rect: Rect): Rect = Rect(rect.y, rect.x, rect.height, rect.width)

  private def check(condition: Boolean, message: => String): Result = Result.assert(condition).log(message)

  /** Laws that hold for any area and layout, extreme inputs included. */
  def laws(name: String, inputs: Gen[(Rect, Layout)]): List[Test] = List(
    property(
      s"[$name] one rect per constraint",
      inputs.forAll.map { case (area, layout) => Assertions.eqv(layout.split(area).length, layout.constraints.length) },
    ),
    property(
      s"[$name] every rect is contained in the area",
      inputs.forAll.map {
        case (area, layout) =>
          Result.all(layout.split(area).toList.map(rect => check(GeometryLaws.contained(rect, area), s"rect = ${rect.show}")))
      },
    ),
    property(
      s"[$name] every rect spans the cross axis",
      inputs.forAll.map {
        case (area, layout) =>
          Result.all(
            layout.split(area).toList.map(rect => check(crossMatchesArea(layout.direction, area, rect), s"rect = ${rect.show}"))
          )
      },
    ),
    property(
      s"[$name] main-axis origins are non-decreasing",
      inputs.forAll.map {
        case (area, layout) =>
          val rects = layout.split(area)
          Result.all(
            rects.zip(rects.drop(1)).toList.map {
              case (a, b) =>
                check(mainStart(layout.direction, a) <= mainStart(layout.direction, b), s"${a.show} then ${b.show}")
            }
          )
      },
    ),
    property(
      /* a rect saturated at the last representable column cannot stay disjoint from its clamped successor (the M1c saturation
       * precedent: laws are guarded where Int.MaxValue saturation loses information) */
      s"[$name] neighbours overlap by at most the declared overlap unless the coordinates saturate",
      inputs.forAll.map {
        case (area, layout) =>
          val rects   = layout.split(area)
          val allowed = allowedOverlap(layout.spacing)
          Result.all(
            rects.zip(rects.drop(1)).toList.map {
              case (a, b) =>
                val endA = mainStart(layout.direction, a) + mainSize(layout.direction, a)
                if (endA > Int.MaxValue.toLong) {
                  Result.success
                } else {
                  val overlap = endA - mainStart(layout.direction, b)
                  check(overlap <= allowed, s"overlap $overlap > allowed $allowed for ${a.show} then ${b.show}")
                }
            }
          )
      },
    ),
    property(
      s"[$name] a horizontal split is the transposed vertical split of the transposed input",
      inputs.forAll.map {
        case (area, layout) =>
          Assertions.eqv(
            layout.withDirection(Direction.Horizontal).split(area).map(transpose),
            layout.withDirection(Direction.Vertical).split(transpose(area)),
          )
      },
    ),
  )

  /** The Fill-specific laws on small areas: exact sums, equal-weight balance, and equal-weight monotonicity under area growth. */
  def fillLaws(
    name: String,
    areas: Gen[Rect],
    fills: Gen[Vector[Constraint]],
    equalFills: Gen[Vector[Constraint]],
  ): List[Test] = List(
    property(
      /* the rect widths are the observable: under an Overlap a clamped start clips widths, so the sum law holds for Space spacing */
      s"[$name] Fill-only rect sizes sum to the available length under Space spacing",
      for {
        area    <- areas.forAll
        cs      <- fills.forAll
        gapSize <- Gen.int(Range.linear(0, 5)).forAll
        flex    <- LayoutGens.flex.forAll
      } yield {
        val spacing   = Spacing.spaceOf(GeometryGens.nonNegOrZero(gapSize.toLong))
        val layout    = Layout(Direction.Horizontal, cs, spacing, flex)
        val n         = cs.length
        val available =
          math.min(math.max(0L, area.width.value.toLong - gapSize.toLong * (n - 1).toLong), Int.MaxValue.toLong)
        val sum       = layout.split(area).foldLeft(0L)((acc, rect) => acc + rect.width.value.toLong)
        if (n === 0) Result.success
        else Result.assert(sum === available).log(s"sum $sum, available $available")
      },
    ),
    property(
      s"[$name] equal-weight sizes differ by at most one",
      for {
        area <- areas.forAll
        cs   <- equalFills.forAll
      } yield {
        val sizes = Layout.horizontal(cs*).split(area).map(_.width.value)
        sizes
          .maxOption
          .flatMap(max => sizes.minOption.map(min => Result.assert(max - min <= 1).log(s"sizes = ${sizes.toString}")))
          .getOrElse(Result.success)
      },
    ),
    property(
      s"[$name] equal-weight splits are monotone under area growth",
      for {
        area   <- areas.forAll
        cs     <- equalFills.forAll
        growth <- Gen.int(Range.linear(0, 5)).forAll
      } yield {
        val layout = Layout.horizontal(cs*)
        val grown  = area.resize(Size(GeometryGens.nonNegOrZero(area.width.value.toLong + growth.toLong), area.height))
        val before = layout.split(area).map(_.width.value)
        val after  = layout.split(grown).map(_.width.value)
        Result.all(
          before.zip(after).toList.map { case (b, a) => Result.assert(b <= a).log(s"before ${b.toString}, after ${a.toString}") }
        )
      },
    ),
    property(
      s"[$name] a single percentage is its share rounded half up",
      for {
        area    <- areas.forAll
        percent <- LayoutGens.percent.forAll
      } yield {
        val available = area.width.value.toLong
        val expected  = (2L * available * percent.value.toLong + 100L) / 200L
        val sizes     = Layout.horizontal(Constraint.percentageOf(percent)).split(area).map(_.width.value.toLong)
        Assertions.eqv(sizes, Vector(expected))
      },
    ),
  )

}
