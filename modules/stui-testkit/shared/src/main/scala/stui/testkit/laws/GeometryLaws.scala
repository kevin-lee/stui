package stui.testkit.laws

import cats.syntax.all.*
import hedgehog.{Gen, Result}
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.geometry.{Margin, Offset, Position, Rect}
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

/** The geometry algebra laws: containment of `inner` / `outer` / `intersection` / `union` / `clamp`, commutativity, idempotence, and
  * the row / column partition.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object GeometryLaws {

  private def rightLong(rect: Rect): Long = rect.x.value.toLong + rect.width.value.toLong

  private def bottomLong(rect: Rect): Long = rect.y.value.toLong + rect.height.value.toLong

  /** `a` is contained in `b`: empty rects are contained in everything, otherwise every edge of `a` is within `b` (computed in `Long`). */
  def contained(a: Rect, b: Rect): Boolean =
    a.isEmpty || (
      b.x.value.toLong <= a.x.value.toLong && rightLong(a) <= rightLong(b) &&
        b.y.value.toLong <= a.y.value.toLong && bottomLong(a) <= bottomLong(b)
    )

  private def check(condition: Boolean, message: => String): Result = Result.assert(condition).log(message)

  private val MaxValue: Long = Int.MaxValue.toLong

  /** True when the width or height of the exact (`Long`) result would exceed `Int.MaxValue`, so the operation saturated (decision 4C) and
    * containment cannot hold.
    */
  private def outerSaturates(r: Rect, m: Margin): Boolean =
    r.width.value.toLong + 2L * m.horizontal.value.toLong > MaxValue || r.height.value.toLong + 2L * m.vertical.value.toLong > MaxValue

  private def unionSaturates(a: Rect, b: Rect): Boolean = {
    val x1 = math.min(a.x.value.toLong, b.x.value.toLong)
    val y1 = math.min(a.y.value.toLong, b.y.value.toLong)
    math.max(rightLong(a), rightLong(b)) - x1 > MaxValue || math.max(bottomLong(a), bottomLong(b)) - y1 > MaxValue
  }

  /** Laws that hold on any rects, margins, and offsets, extreme values included. */
  def algebraLaws(name: String, rects: Gen[Rect], margins: Gen[Margin], offsets: Gen[Offset]): List[Test] = List(
    property(
      s"[$name] inner(m) is contained in the rect",
      for {
        r <- rects.forAll
        m <- margins.forAll
      } yield check(contained(r.inner(m), r), s"inner = ${r.inner(m).show}"),
    ),
    property(
      s"[$name] the rect is contained in outer(m) unless the size saturates",
      for {
        r <- rects.forAll
        m <- margins.forAll
      } yield if (outerSaturates(r, m)) Result.success else check(contained(r, r.outer(m)), s"outer = ${r.outer(m).show}"),
    ),
    property(
      s"[$name] intersection is contained in both and commutative",
      for {
        a <- rects.forAll
        b <- rects.forAll
      } yield Result.all(
        List(
          check(contained(a.intersection(b), a), s"intersection = ${a.intersection(b).show}"),
          check(contained(a.intersection(b), b), s"intersection = ${a.intersection(b).show}"),
          Assertions.eqv(a.intersection(b), b.intersection(a)),
        )
      ),
    ),
    property(s"[$name] intersection(a, a) == a", rects.forAll.map(a => Assertions.eqv(a.intersection(a), a))),
    property(
      s"[$name] both operands are contained in the union unless the size saturates, and the union is commutative",
      for {
        a <- rects.forAll
        b <- rects.forAll
      } yield Result.all(
        List(
          if (unionSaturates(a, b)) Result.success else check(contained(a, a.union(b)), s"union = ${a.union(b).show}"),
          if (unionSaturates(a, b)) Result.success else check(contained(b, a.union(b)), s"union = ${a.union(b).show}"),
          Assertions.eqv(a.union(b), b.union(a)),
        )
      ),
    ),
    property(s"[$name] union(a, a) == a", rects.forAll.map(a => Assertions.eqv(a.union(a), a))),
    property(
      s"[$name] intersects iff the intersection is non-empty",
      for {
        a <- rects.forAll
        b <- rects.forAll
      } yield Result.assert(a.intersects(b) === !a.intersection(b).isEmpty),
    ),
    property(
      s"[$name] clamp(r, o) is contained in o and keeps the size when r fits",
      for {
        r <- rects.forAll
        o <- rects.forAll
      } yield {
        val clamped = r.clamp(o)
        val fits    = r.width.value <= o.width.value && r.height.value <= o.height.value
        Result.all(
          List(
            check(contained(clamped, o), s"clamped = ${clamped.show}"),
            if (fits) Assertions.eqv(clamped.size, r.size) else Result.success,
          )
        )
      },
    ),
    property(s"[$name] offset(zero) == r", rects.forAll.map(r => Assertions.eqv(r.offset(Offset.zero), r))),
    property(
      s"[$name] area == width * height",
      rects.forAll.map(r => Result.assert(r.area === r.width.value.toLong * r.height.value.toLong)),
    ),
    property(
      s"[$name] every operation returns on any input",
      for {
        r <- rects.forAll
        o <- rects.forAll
        m <- margins.forAll
        d <- offsets.forAll
      } yield {
        val results =
          List(r.inner(m), r.outer(m), r.offset(d), r.offset(d.negate), r.union(o), r.intersection(o), r.clamp(o), r.resize(o.size))
        Result.assert(results.length === 8)
      },
    ),
  )

  /** The [[stui.core.geometry.Rect.inset]] laws: containment, the `inner` equivalence, and the size arithmetic in `Long`. */
  def insetLaws(name: String, rects: Gen[Rect], insets: Gen[NonNegInt]): List[Test] = List(
    property(
      s"[$name] inset is contained in the rect",
      for {
        r <- rects.forAll
        l <- insets.forAll
        t <- insets.forAll
        g <- insets.forAll
        b <- insets.forAll
      } yield check(contained(r.inset(l, t, g, b), r), s"inset = ${r.inset(l, t, g, b).show}"),
    ),
    property(
      s"[$name] inner(m) equals inset(h, v, h, v)",
      for {
        r <- rects.forAll
        h <- insets.forAll
        v <- insets.forAll
      } yield Assertions.eqv(r.inner(Margin(h, v)), r.inset(h, v, h, v)),
    ),
    property(
      s"[$name] the inset arithmetic holds in Long",
      for {
        r <- rects.forAll
        l <- insets.forAll
        t <- insets.forAll
        g <- insets.forAll
        b <- insets.forAll
      } yield {
        val result = r.inset(l, t, g, b)
        Result.all(
          List(
            Result
              .assert(result.x.value.toLong === math.min(r.x.value.toLong + l.value.toLong, MaxValue))
              .log("x"),
            Result
              .assert(result.y.value.toLong === math.min(r.y.value.toLong + t.value.toLong, MaxValue))
              .log("y"),
            Result
              .assert(result.width.value.toLong === math.max(0L, r.width.value.toLong - l.value.toLong - g.value.toLong))
              .log("width"),
            Result
              .assert(result.height.value.toLong === math.max(0L, r.height.value.toLong - t.value.toLong - b.value.toLong))
              .log("height"),
          )
        )
      },
    ),
  )

  /** Laws on small rects only (`rows` and `columns` allocate one rect per row or column). */
  def partitionLaws(name: String, rects: Gen[Rect], positions: Gen[Position]): List[Test] = List(
    property(
      s"[$name] rows partition the rect",
      rects.forAll.map { r =>
        val rows = r.rows
        Result.all(
          List(
            Result.assert(rows.length === r.height.value).log("row count"),
            Result.assert(rows.forall(row => row.height.value === 1 && row.x === r.x && row.width === r.width)).log("row shape"),
            Result.assert(rows.zip(rows.drop(1)).forall { case (a, b) => !a.intersects(b) }).log("rows overlap"),
            if (r.isEmpty) Result.success else Assertions.eqv(rows.reduceOption(_ union _).getOrElse(Rect.empty), r),
          )
        )
      },
    ),
    property(
      s"[$name] columns partition the rect",
      rects.forAll.map { r =>
        val columns = r.columns
        Result.all(
          List(
            Result.assert(columns.length === r.width.value).log("column count"),
            Result
              .assert(columns.forall(column => column.width.value === 1 && column.y === r.y && column.height === r.height))
              .log("column shape"),
            Result.assert(columns.zip(columns.drop(1)).forall { case (a, b) => !a.intersects(b) }).log("columns overlap"),
            if (r.isEmpty) Result.success else Assertions.eqv(columns.reduceOption(_ union _).getOrElse(Rect.empty), r),
          )
        )
      },
    ),
    property(
      s"[$name] contains agrees with the row and column partition",
      for {
        r <- rects.forAll
        p <- positions.forAll
      } yield Result.assert(r.contains(p) === (r.rows.exists(row => row.y === p.y) && r.columns.exists(column => column.x === p.x))),
    ),
  )

}
