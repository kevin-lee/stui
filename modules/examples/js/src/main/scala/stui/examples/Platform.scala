package stui.examples

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** The two members of Node's `process` the demo needs, typed just enough (`process` is a global, so `@JSGlobal` keeps the demo
  * module-kind-agnostic, the M2c recipe).
  */
@js.native
@JSGlobal("process")
private object DemoProcess extends js.Object {

  def argv: js.Array[String] = js.native

  def exit(code: Int): Unit = js.native

}

/** What the demo needs from Node: the `inline` argument from `process.argv` (the main-module initializer passes no arguments to
  * `main`, so `args` is ignored) and the process exit.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object Platform {

  /** True when the first argument after the script is `inline`. */
  def inline(@unused args: Array[String]): Boolean =
    DemoProcess.argv.toList.drop(2).headOption.exists(_.trim.equalsIgnoreCase("inline"))

  /** Ends the process with the code. */
  def exit(code: Int): Unit = DemoProcess.exit(code)

}
