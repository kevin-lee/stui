package stui.examples

/** What the demo needs from the JVM and Native platforms: the `inline` argument from the command line and the process exit.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object Platform {

  /** True when the first argument is `inline`. */
  def inline(args: Array[String]): Boolean = args.headOption.exists(_.trim.equalsIgnoreCase("inline"))

  /** Ends the process with the code. */
  def exit(code: Int): Unit = sys.exit(code)

}
