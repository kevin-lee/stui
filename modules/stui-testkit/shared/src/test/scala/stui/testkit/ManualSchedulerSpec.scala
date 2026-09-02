package stui.testkit

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.spi.Subscription
import stui.testkit.ManualScheduler.*
import stui.unicode.internal.IntOps.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** The deterministic scheduler laws (design doc 6.3 and 12, M3a) over generated traces of schedule, advance, and cancel operations,
  * plus the in-action cases by example.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
object ManualSchedulerSpec extends Properties {

  private enum Op {
    case Schedule(delay: FiniteDuration)
    case Advance(by: FiniteDuration)
    case Cancel(pick: Int)
  }

  /** The outcome of a trace: the fired ticks as (index, fired-at nanos) in order, each scheduled tick's due, each tick's horizon (the
    * clock after the last advance that followed its scheduling, none when no advance did: time moves only under advance, so a tick
    * fires exactly when its horizon reaches its due), the indexes whose cancel landed before they fired, and the final clock.
    */
  final private case class Run(log: Vector[(Int, Long)], dues: Vector[Long], horizons: Vector[Option[Long]], cancelled: Set[Int], now: Long)

  final private case class Acc(handles: Vector[Subscription], dues: Vector[Long], horizons: Vector[Option[Long]], cancelled: Set[Int])

  private val millis: Gen[FiniteDuration] = Gen.int(Range.linear(0, 100)).map(_.millis)

  private val op: Gen[Op] = Gen.choice1(
    millis.map(delay => Op.Schedule(delay)),
    millis.map(by => Op.Advance(by)),
    Gen.int(Range.linear(0, 11)).map(pick => Op.Cancel(pick)),
  )

  private val trace: Gen[List[Op]] = op.list(Range.linear(0, 12))

  private def run(ops: List[Op]): Run = {
    val scheduler = ManualScheduler.of(0.millis)
    val log       = new AtomicReference(Vector.empty[(Int, Long)])
    val acc = ops.foldLeft(Acc(Vector.empty[Subscription], Vector.empty[Long], Vector.empty[Option[Long]], Set.empty[Int])) { (acc, op) =>
      op match {
        case Op.Schedule(delay) =>
          val index  = acc.dues.length
          val due    = scheduler.now.toNanos + delay.toNanos
          val handle = scheduler.schedule(delay, () => log.updateAndGet(_ :+ (index -> scheduler.monotonicNanos())): Unit)
          acc.copy(handles = acc.handles :+ handle, dues = acc.dues :+ due, horizons = acc.horizons :+ none[Long])
        case Op.Advance(by) =>
          scheduler.advance(by)
          acc.copy(horizons = acc.horizons.map(_ => scheduler.now.toNanos.some))
        case Op.Cancel(pick) =>
          if (acc.handles.isEmpty) {
            acc
          } else {
            val index = pick % acc.handles.length
            val fired = log.get().exists { case (i, _) => i === index }
            acc.handles.lift(index).foreach(_.cancel())
            if (fired) acc else acc.copy(cancelled = acc.cancelled + index)
          }
      }
    }
    Run(log.get(), acc.dues, acc.horizons, acc.cancelled, scheduler.now.toNanos)
  }

  private def duplicateCancels(ops: List[Op]): List[Op] = ops.flatMap {
    case cancel @ Op.Cancel(_) => List(cancel, cancel)
    case other => List(other)
  }

  private def firedIndexes(run: Run): Set[Int] = run.log.map { case (i, _) => i }.toSet

  override def tests: List[Test] = List(
    property(
      "ticks fire in due order, ties in schedule order",
      trace.forAll.map { ops =>
        val outcome = run(ops)
        val keys    = outcome.log.map { case (i, _) => (outcome.dues.lift(i).getOrElse(-1L), i) }
        Result
          .assert(keys.zip(keys.drop(1)).forall { case (a, b) => Ordering[(Long, Int)].lteq(a, b) })
          .log(s"log = ${outcome.log.toString}")
      },
    ),
    property(
      "a tick fires with the clock at its due time",
      trace.forAll.map { ops =>
        val outcome = run(ops)
        Result.assert(outcome.log.forall { case (i, at) => outcome.dues.lift(i).contains(at) }).log(s"log = ${outcome.log.toString}")
      },
    ),
    property(
      "after the trace, every tick not effectively cancelled whose horizon reached its due has fired, and no other tick has",
      trace.forAll.map { ops =>
        val outcome = run(ops)
        val fired   = firedIndexes(outcome)
        Result.all(
          outcome.dues.zip(outcome.horizons).zipWithIndex.toList.map {
            case ((due, horizon), i) =>
              val reached    = horizon.exists(_ >= due)
              val shouldFire = !outcome.cancelled.contains(i) && reached
              val mustNot    = !reached
              Result
                .assert((!shouldFire || fired.contains(i)) && (!mustNot || !fired.contains(i)))
                .log(
                  s"tick ${i.toString} due ${due.toString} horizon ${horizon.fold("none")(_.toString)} fired ${fired.contains(i).toString}"
                )
          }
        )
      },
    ),
    property(
      "the same trace fires the same sequence",
      trace.forAll.map { ops =>
        val first  = run(ops)
        val second = run(ops)
        Result.all(List(Assertions.eqv(first.log, second.log), first.now ==== second.now))
      },
    ),
    property(
      "cancel is idempotent and an effectively cancelled tick never fires",
      trace.forAll.map { ops =>
        val once  = run(ops)
        val twice = run(duplicateCancels(ops))
        Result.all(
          List(
            Assertions.eqv(twice.log, once.log),
            Result.assert(once.cancelled.forall(i => !firedIndexes(once).contains(i))).log(s"log = ${once.log.toString}"),
          )
        )
      },
    ),
    example("nothing fires before advance, and advance by zero fires what is due now", testAdvanceZero),
    example("an action scheduling a zero-delay tick fires it in the same advance, after itself", testScheduleInside),
    example("cancel from inside the action is a no-op", testCancelInside),
    example("pending lists due times in firing order and drops cancelled ticks", testPending),
    example("a scheduler is a clock: monotonicNanos equals now", testClock),
  )

  private def recorder(): (AtomicReference[Vector[String]], String => () => Unit) = {
    val log = new AtomicReference(Vector.empty[String])
    (log, name => () => log.updateAndGet(_ :+ name): Unit)
  }

  def testAdvanceZero: Result = {
    val scheduler   = ManualScheduler.of(0.millis)
    val (log, tick) = recorder()
    scheduler.schedule(10.millis, tick("a")): Unit
    scheduler.schedule(0.millis, tick("b")): Unit
    val before      = log.get()
    scheduler.advance(0.millis)
    val atZero      = log.get()
    scheduler.advance(10.millis)
    Result.all(
      List(
        Assertions.eqv(before, Vector.empty[String]),
        Assertions.eqv(atZero, Vector("b")),
        Assertions.eqv(log.get(), Vector("b", "a")),
      )
    )
  }

  def testScheduleInside: Result = {
    val scheduler   = ManualScheduler.of(0.millis)
    val (log, tick) = recorder()
    scheduler.schedule(
      5.millis,
      () => {
        tick("a")()
        scheduler.schedule(0.millis, tick("b")): Unit
      },
    ): Unit
    scheduler.advance(5.millis)
    Result.all(List(Assertions.eqv(log.get(), Vector("a", "b")), Assertions.eqv(scheduler.now, 5.millis)))
  }

  def testCancelInside: Result = {
    val scheduler   = ManualScheduler.of(0.millis)
    val (log, tick) = recorder()
    val self        = new AtomicReference(none[Subscription])
    val handle      = scheduler.schedule(
      1.millis,
      () => {
        tick("a")()
        self.get().foreach(_.cancel())
      },
    )
    self.set(handle.some)
    scheduler.schedule(2.millis, tick("b")): Unit
    scheduler.advance(2.millis)
    Assertions.eqv(log.get(), Vector("a", "b"))
  }

  def testPending: Result = {
    val scheduler = ManualScheduler.of(0.millis)
    scheduler.schedule(30.millis, () => ()): Unit
    val second    = scheduler.schedule(10.millis, () => ())
    scheduler.schedule(10.millis, () => ()): Unit
    val all       = scheduler.pending
    second.cancel()
    second.cancel()
    val afterOne  = scheduler.pending
    scheduler.advance(10.millis)
    Result.all(
      List(
        Assertions.eqv(all, Vector(10.millis, 10.millis, 30.millis)),
        Assertions.eqv(afterOne, Vector(10.millis, 30.millis)),
        Assertions.eqv(scheduler.pending, Vector(30.millis)),
      )
    )
  }

  def testClock: Result = {
    val scheduler = ManualScheduler.of(3.millis)
    scheduler.advance(4.millis)
    Result.all(List(scheduler.monotonicNanos() ==== scheduler.now.toNanos, Assertions.eqv(scheduler.now, 7.millis)))
  }

}
