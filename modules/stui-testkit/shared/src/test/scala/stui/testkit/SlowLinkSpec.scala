package stui.testkit

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.ManualClock.*
import stui.testkit.ManualScheduler.*
import stui.testkit.SlowLink.{Arrival, Timeline}
import stui.testkit.SlowLink.Timeline.*
import stui.testkit.gen.SlowLinkGens
import stui.unicode.internal.IntOps.*

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The slow-link timing model (design doc 7.5 and 12, M3c): the timeline's order, the pull player's clock movement, and the push
  * player's schedule.
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SlowLinkSpec extends Properties {

  private def chunk(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes(StandardCharsets.UTF_8))

  private def observed(timeline: Timeline): Vector[(FiniteDuration, Vector[Byte])] =
    timeline.arrivals.map(arrival => (arrival.at, arrival.chunk.toVector))

  private val bytes: Gen[IArray[Byte]] = Gen.int(Range.linear(0, 255)).list(Range.linear(0, 20)).map(bs => IArray.from(bs.map(_.toByte)))

  private val timeline: Gen[Timeline] = bytes.flatMap(bs => SlowLinkGens.timeline(bs, 30.millis))

  override def tests: List[Test] = List(
    property("of orders arrivals by time and keeps the given order among equal times", testOrder),
    example("spaced places chunk i at i times the gap", testSpaced),
    property("the timeline's bytes are the concatenation of its chunks", testBytes),
    property("the pull player delivers each chunk at its time and moves the clock only by the wait", testPull),
    example("the pull player returns None and advances by the timeout when nothing is due within it", testPullNone),
    property("the push player over ManualScheduler delivers every chunk in order by the end time", testPush),
  )

  def testOrder: Property =
    SlowLinkGens.arrival.list(Range.linear(0, 8)).forAll.map { arrivals =>
      val sorted = Timeline.of(arrivals*)
      val times  = sorted.arrivals.map(_.at)
      Result.all(
        List(
          Result.assert(times.zip(times.drop(1)).forall { case (before, after) => before <= after }).log("non-decreasing"),
          Assertions.eqv(sorted.arrivals.length, arrivals.length),
          Result
            .assert(times.distinct.forall { t =>
              sorted
                .arrivals
                .withFilter(_.at === t)
                .map(_.chunk.toVector) === arrivals.withFilter(_.at === t).map(_.chunk.toVector).toVector
            })
            .log("stable"),
        )
      )
    }

  def testSpaced: Result = {
    val spaced = Timeline.spaced(10.millis, chunk("a"), chunk("b"), chunk("c"))
    Result.all(
      List(
        Assertions.eqv(spaced.arrivals.map(_.at), Vector(0.millis, 10.millis, 20.millis)),
        Assertions.eqv(spaced.end, 20.millis),
        Assertions.eqv(Timeline.empty.end, Duration.Zero: FiniteDuration),
      )
    )
  }

  def testBytes: Property =
    for {
      bs <- bytes.forAll
      tl <- SlowLinkGens.timeline(bs, 5.millis).forAll
    } yield Assertions.eqv(tl.bytes.toVector, bs.toVector)

  def testPull: Property =
    timeline.forAll.map { tl =>
      val clock  = ManualClock.of(0.millis)
      val player = SlowLink.pull(tl, clock)

      @tailrec
      def drain(acc: Vector[(FiniteDuration, Vector[Byte])]): Vector[(FiniteDuration, Vector[Byte])] =
        if (player.remaining === 0) {
          acc
        } else {
          player.poll(10.millis) match {
            case Some(delivered) => drain(acc :+ (clock.now, delivered.toVector))
            case None => drain(acc)
          }
        }

      val delivered = drain(Vector.empty[(FiniteDuration, Vector[Byte])])
      val before    = clock.now
      val after     = player.poll(10.millis)
      Result.all(
        List(
          Assertions.eqv(delivered, observed(tl)),
          Assertions.eqv(after.map(_.toVector), none[Vector[Byte]]),
          Assertions.eqv(clock.now, before + 10.millis),
        )
      )
    }

  def testPullNone: Result = {
    val clock  = ManualClock.of(0.millis)
    val player = SlowLink.pull(Timeline.of(Arrival(50.millis, chunk("x"))), clock)
    val first  = player.poll(7.millis)
    val at7    = clock.now
    val second = SlowLink.pull(Timeline.empty, clock).poll(3.millis)
    Result.all(
      List(
        Assertions.eqv(first.map(_.toVector), none[Vector[Byte]]),
        Assertions.eqv(at7, 7.millis),
        Assertions.eqv(second.map(_.toVector), none[Vector[Byte]]),
        Assertions.eqv(clock.now, 10.millis),
        Assertions.eqv(player.remaining, 1),
      )
    )
  }

  def testPush: Property =
    timeline.forAll.map { tl =>
      val scheduler = ManualScheduler.of(0.millis)
      val recorded  = new AtomicReference(Vector.empty[(FiniteDuration, Vector[Byte])])
      val handles   = SlowLink.push(tl, scheduler, delivered => recorded.updateAndGet(_ :+ (scheduler.now, delivered.toVector)): Unit)
      scheduler.advance(tl.end)
      Result.all(
        List(
          Assertions.eqv(recorded.get(), observed(tl)),
          Assertions.eqv(handles.length, tl.arrivals.length),
          Assertions.eqv(scheduler.pending, Vector.empty[FiniteDuration]),
        )
      )
    }

}
