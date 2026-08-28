package stui.testkit

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.ManualClock.*

import scala.concurrent.duration.*

/** The deterministic clock.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object ManualClockSpec extends Properties {

  override def tests: List[Test] = List(
    example("reads return the start until advanced", testOf),
    example("ticking advances by the step after every read", testTicking),
  )

  def testOf: Result = {
    val clock = ManualClock.of(5.millis)
    val first = clock.monotonicNanos()
    clock.advance(2.millis)
    Result.all(
      List(
        first ==== 5.millis.toNanos,
        clock.monotonicNanos() ==== 7.millis.toNanos,
        Assertions.eqv(clock.now, 7.millis),
      )
    )
  }

  def testTicking: Result = {
    val clock  = ManualClock.ticking(0.nanos, 1.milli)
    val first  = clock.monotonicNanos()
    val second = clock.monotonicNanos()
    Result.all(List(first ==== 0L, second ==== 1.milli.toNanos, Assertions.eqv(clock.now, 2.millis)))
  }

}
