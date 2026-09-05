package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.{Loop, Tasks}
import stui.app.internal.Tasks.*
import stui.testkit.Assertions

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/** The task starter (design doc 8.3 and 10, decision D27, M3b): synchronous and deferred completions, cancel, and a late result.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object TasksSpec extends Properties {

  final private case class Setup(
    tasks: Tasks[String],
    log: AtomicReference[Vector[String]],
    captured: AtomicReference[Option[String => Unit]],
    cancelled: AtomicBoolean,
  )

  private def setup(): Setup = {
    val log = new AtomicReference(Vector.empty[String])
    Setup(
      Tasks.of[String](msg => log.updateAndGet(_ :+ msg): Unit),
      log,
      new AtomicReference(none[String => Unit]),
      new AtomicBoolean(false),
    )
  }

  private val sync: Loop.Launch[String] = post => {
    post("done")
    () => ()
  }

  private def deferred(s: Setup): Loop.Launch[String] = post => {
    s.captured.set(post.some)
    () => s.cancelled.set(true)
  }

  override def tests: List[Test] = List(
    example("a synchronous completion posts and leaves nothing running", testSync),
    example("a deferred completion posts when its callback fires and then leaves nothing running", testDeferred),
    example("cancelAll cancels a running task", testCancelAll),
    example("a result after cancelAll is still posted", testLate),
  )

  def testSync: Result = {
    val s = setup()
    s.tasks.start(sync)
    Result.all(List(Assertions.eqv(s.log.get(), Vector("done")), Assertions.eqv(s.tasks.runningCount, 0)))
  }

  def testDeferred: Result = {
    val s       = setup()
    s.tasks.start(deferred(s))
    val running = s.tasks.runningCount
    s.captured.get().foreach(post => post("later"))
    Result.all(
      List(
        Assertions.eqv(running, 1),
        Assertions.eqv(s.log.get(), Vector("later")),
        Assertions.eqv(s.tasks.runningCount, 0),
      )
    )
  }

  def testCancelAll: Result = {
    val s = setup()
    s.tasks.start(deferred(s))
    s.tasks.cancelAll()
    Result.all(List(Assertions.eqv(s.cancelled.get(), true), Assertions.eqv(s.tasks.runningCount, 0)))
  }

  def testLate: Result = {
    val s = setup()
    s.tasks.start(deferred(s))
    s.tasks.cancelAll()
    s.captured.get().foreach(post => post("late"))
    Assertions.eqv(s.log.get(), Vector("late"))
  }

}
