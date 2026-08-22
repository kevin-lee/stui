package stui.terminal.internal

import hedgehog.*
import hedgehog.runner.*

import java.util.Locale

/** Smoke test: passes only if sbt compiled the Java varargs interface and JNA loaded libc through both mapping styles.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object LibCSpec extends Properties {

  override def tests: List[Test] = List(
    example("isatty(0) is 0 or 1", testIsatty),
    example("varargs ioctl TIOCGWINSZ on fd 0 returns 0 or -1 without crashing", testIoctl),
  )

  private val isMac: Boolean =
    Option(System.getProperty("os.name")).exists(_.toLowerCase(Locale.ROOT).contains("mac"))

  private val tiocgwinsz: Long = if (isMac) 0x40087468L else 0x5413L

  def testIsatty: Result = {
    val rc = LibC.isatty(0)
    (rc ==== 0).or(rc ==== 1)
  }

  def testIoctl: Result = {
    val winsize = new Array[Byte](8)
    val rc      = LibC.varargs.ioctl(0, tiocgwinsz, winsize)
    (rc ==== 0).or(rc ==== -1)
  }

}
