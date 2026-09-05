package stui.app

import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.Ticks
import stui.testkit.Assertions

import scala.concurrent.duration.*

/** The subscription value (design doc 10, M3b): flattening, keys, and the interval floor.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object SubSpec extends Properties {

  override def tests: List[Test] = List(
    example("leaves flatten depth-first in order", testLeaves),
    example("keys are the intervals", testKeys),
    example("every floors a non-positive interval to one millisecond", testFloor),
    example("Ticks.period floors too", testPeriod),
  )

  def testLeaves: Result = {
    val a                = Sub.every[String](1.millis)(_ => "a")
    val b                = Sub.every[String](2.millis)(_ => "b")
    val c                = Sub.every[String](3.millis)(_ => "c")
    val sub: Sub[String] = Sub.batch(a, Sub.batch(Sub.none, b), Sub.none, c)
    Assertions.eqv(sub.leaves.map(leaf => leaf.tag(0.millis)), Vector("a", "b", "c"))
  }

  def testKeys: Result =
    Assertions.eqv(
      Sub.batch(Sub.every[Int](5.millis)(_ => 1), Sub.every[Int](5.millis)(_ => 2), Sub.every[Int](7.millis)(_ => 3)).keys,
      Set[SubKey](SubKey.Every(5.millis), SubKey.Every(7.millis)),
    )

  def testFloor: Result = Result.all(
    List(
      Assertions.eqv(Sub.every[Int](0.millis)(_ => 1).keys, Set[SubKey](SubKey.Every(1.millis))),
      Assertions.eqv(Sub.every[Int](-3.millis)(_ => 1).keys, Set[SubKey](SubKey.Every(1.millis))),
      Assertions.eqv(Sub.every[Int](4.millis)(_ => 1).keys, Set[SubKey](SubKey.Every(4.millis))),
    )
  )

  def testPeriod: Result = Result.all(
    List(
      Assertions.eqv(Ticks.period(SubKey.Every(0.millis)), 1.millis),
      Assertions.eqv(Ticks.period(SubKey.Every(9.millis)), 9.millis),
    )
  )

}
