package stui.app

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.app.internal.Loop
import stui.app.internal.Loop.Input
import stui.core.event.Event
import stui.core.frame.Regions
import stui.core.text.Line
import stui.testkit.Assertions

import java.util.concurrent.atomic.AtomicReference

/** The command laws (design doc 10 and 12, M3b): batch associativity in effect, emit ordering, and the constructors.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object CmdSpec extends Properties {

  /** An application whose messages are names and whose update appends the name and returns the command of a fixed table. */
  private def tableApp(root: Cmd[String]): StuiApp[Vector[String], String] =
    new StuiApp[Vector[String], String] {
      override def init(env: AppEnv): (Vector[String], Cmd[String])                          = (Vector.empty[String], Cmd.none)
      override def onEvent(event: Event, model: Vector[String]): Option[String]              = none[String]
      override def update(model: Vector[String], msg: String): (Vector[String], Cmd[String]) = {
        val cmd: Cmd[String] = msg match {
          case "root" => root
          case "a" => Cmd.batch(Cmd.emit("b"), Cmd.emit("c"))
          case "b" => Cmd.print(Line.raw("b"))
          case "c" => Cmd.redraw
          case "z" => Cmd.exit
          case _ => Cmd.none
        }
        (model :+ msg, cmd)
      }
      override def view(model: Vector[String]): View[Vector[String]]                         = View.of(Line.raw(""))
    }

  private val leaf: Gen[Cmd[String]] = Gen.choice1(
    Gen.constant(Cmd.none: Cmd[String]),
    Gen.element1("a", "b", "c", "x", "z").map(name => Cmd.emit(name): Cmd[String]),
    Gen.constant(Cmd.print(Line.raw("p")): Cmd[String]),
    Gen.constant(Cmd.redraw: Cmd[String]),
    Gen.constant(Cmd.exit: Cmd[String]),
  )

  /** Command trees of depth up to 3, built by iteration so no generator recurses. */
  private val tree: Gen[Cmd[String]] =
    (1 to 3).foldLeft(leaf)((gen, _) =>
      Gen.choice1(leaf, gen.list(Range.linear(0, 3)).map(children => Cmd.Batch(children.toVector): Cmd[String]))
    )

  private def run(root: Cmd[String], msg: String): Loop.Stepped[Vector[String], String] =
    Loop.step(tableApp(root), Regions.empty, Vector.empty[String], Vector(Input.Message(msg)))

  override def tests: List[Test] = List(
    property(
      "Batch is associative in its effect",
      for {
        a <- tree.forAll
        b <- tree.forAll
        c <- tree.forAll
      } yield {
        val left  = run(Cmd.Batch(Vector(Cmd.Batch(Vector(a, b)), c)), "root")
        val right = run(Cmd.Batch(Vector(a, Cmd.Batch(Vector(b, c)))), "root")
        Result.all(
          List(
            Assertions.eqv(left.model, right.model),
            Assertions.eqv(left.prints.length, right.prints.length),
            Assertions.eqv(left.redraw, right.redraw),
            Assertions.eqv(left.exit, right.exit),
          )
        )
      },
    ),
    example("emit joins the batch first in first out", testEmitOrder),
    example("the constructors build their cases", testConstructors),
    example("a task built by the constructor starts once and turns its result into a message", testTask),
  )

  def testEmitOrder: Result = Assertions.eqv(run(Cmd.none, "a").model, Vector("a", "b", "c"))

  private def isNone[A](cmd: Cmd[A]): Boolean = cmd match {
    case Cmd.None => true
    case Cmd.Emit(_) | Cmd.Batch(_) | Cmd.Exit | Cmd.Print(_) | Cmd.Redraw | Cmd.Task(_, _) => false
  }

  def testConstructors: Result = {
    val batch: Cmd[Int] = Cmd.batch(Cmd.exit, Cmd.redraw, Cmd.emit(3))
    Result.all(
      List(
        Result.assert(isNone(Cmd.none)).log("none"),
        Result
          .assert(Cmd.emit(1) match {
            case Cmd.Emit(1) => true
            case _ => false
          })
          .log("emit"),
        Result
          .assert(batch match {
            case Cmd.Batch(Vector(Cmd.Exit, Cmd.Redraw, Cmd.Emit(3))) => true
            case _ => false
          })
          .log("batch"),
        Result
          .assert(Cmd.print(Line.raw("x")) match {
            case Cmd.Print(_) => true
            case _ => false
          })
          .log("print"),
        Result
          .assert(Cmd.redraw match {
            case Cmd.Redraw => true
            case _ => false
          })
          .log("redraw"),
      )
    )
  }

  def testTask: Result = {
    val cmd    = Cmd.task[Int, String] { callback =>
      callback(7.asRight[Throwable])
      () => ()
    }(result => result.fold(_ => "failed", n => n.toString))
    val posted = new AtomicReference(Vector.empty[String])
    cmd match {
      case Cmd.Task(start, onResult) =>
        start(result => posted.updateAndGet(_ :+ onResult(result)): Unit): Unit
        Assertions.eqv(posted.get(), Vector("7"))
      case _ => Result.failure.log("not a task")
    }
  }

}
