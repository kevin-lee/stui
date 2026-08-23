package stui.core.spi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.testkit.Assertions

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*

/** A queue-backed `BlockingEventSource`, which exists on the JVM and Native only: this spec proves the `jvm-native` source directory is
  * compiled and linked on both platforms.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object BlockingEventSourceSpec extends Properties {

  /** `poll` returns the head immediately or `None`, the timeout is not waited for. */
  final class QueueEventSource extends BlockingEventSource {
    private val queue: ConcurrentLinkedQueue[Event]               = new ConcurrentLinkedQueue[Event]()
    def push(event: Event): Unit                                  = queue.add(event): Unit
    override def poll(timeout: FiniteDuration): Option[Event]     = Option(queue.poll())
    override def subscribe(listener: Event => Unit): Subscription = () => ()
  }

  override def tests: List[Test] = List(
    example("pushed events come back in order", testOrder),
    example("an empty queue gives None", testEmpty),
  )

  def testOrder: Result = {
    val source = new QueueEventSource
    val first  = Event.key(KeyEvent.press(KeyCode.Enter))
    val second = Event.FocusGained
    source.push(first)
    source.push(second)
    Assertions.eqv(List(source.poll(1.millisecond), source.poll(1.millisecond)), List(first.some, second.some))
  }

  def testEmpty: Result = Assertions.eqv(new QueueEventSource().poll(1.millisecond), none[Event])

}
