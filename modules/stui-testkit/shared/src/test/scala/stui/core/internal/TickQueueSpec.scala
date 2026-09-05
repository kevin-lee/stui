package stui.core.internal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.internal.TickQueue.*
import stui.testkit.Assertions

import scala.annotation.tailrec

/** The shared due-time queue (design doc 6.3, M3b): ids, removal, the head, the pop order by example, and the pop order as a property.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object TickQueueSpec extends Properties {

  private val noop: () => Unit = () => ()

  private def ids(queue: TickQueue): Vector[Long] = queue.entries.map(_.id)

  override def tests: List[Test] = List(
    example("add assigns increasing ids and remove drops exactly the id", testAddRemove),
    example("nextDue is the smallest due and None on empty", testNextDue),
    example("popDue takes the earliest due then the earliest id and leaves the rest", testPopOrder),
    example("popDue gives None when nothing is due by the limit", testPopNone),
    property("popping everything yields the entries sorted by due then id, which is dues", testPopAll),
  )

  def testAddRemove: Result = {
    val queue = TickQueue.empty.add(5L, noop).add(3L, noop).add(5L, noop)
    Result.all(
      List(
        Assertions.eqv(ids(queue), Vector(0L, 1L, 2L)),
        Assertions.eqv(queue.nextId, 3L),
        Assertions.eqv(ids(queue.remove(1L)), Vector(0L, 2L)),
        Assertions.eqv(ids(queue.remove(7L)), ids(queue)),
        Assertions.eqv(queue.remove(1L).nextId, 3L),
      )
    )
  }

  def testNextDue: Result = {
    val queue = TickQueue.empty.add(5L, noop).add(3L, noop)
    Result.all(
      List(
        Assertions.eqv(queue.nextDue, 3L.some),
        Assertions.eqv(TickQueue.empty.nextDue, none[Long]),
        Assertions.eqv(TickQueue.empty.isEmpty, true),
        Assertions.eqv(queue.isEmpty, false),
      )
    )
  }

  def testPopOrder: Result = {
    val queue = TickQueue.empty.add(5L, noop).add(3L, noop).add(3L, noop)
    queue.popDue(10L) match {
      case Some((entry, rest)) =>
        Result.all(
          List(
            Assertions.eqv(entry.id, 1L),
            Assertions.eqv(entry.due, 3L),
            Assertions.eqv(ids(rest), Vector(0L, 2L)),
            Assertions.eqv(rest.nextId, 3L),
          )
        )
      case None => Result.failure.log("nothing popped")
    }
  }

  def testPopNone: Result = {
    val queue = TickQueue.empty.add(5L, noop).add(3L, noop)
    Assertions.eqv(queue.popDue(2L).map { case (entry, _) => entry.id }, none[Long])
  }

  def testPopAll: Property =
    Gen.long(Range.linear(0L, 100L)).list(Range.linear(0, 12)).forAll.map { dues =>
      val queue  = dues.foldLeft(TickQueue.empty)((q, due) => q.add(due, noop))
      val popped = popAll(queue, Vector.empty[(Long, Long)])
      val sorted = queue.entries.sortBy(entry => (entry.due, entry.id)).map(entry => (entry.due, entry.id))
      Result.all(List(Assertions.eqv(popped, sorted), Assertions.eqv(queue.dues, sorted.map { case (due, _) => due })))
    }

  @tailrec
  private def popAll(queue: TickQueue, acc: Vector[(Long, Long)]): Vector[(Long, Long)] =
    queue.popDue(Long.MaxValue) match {
      case Some((entry, rest)) => popAll(rest, acc :+ (entry.due -> entry.id))
      case None => acc
    }

}
