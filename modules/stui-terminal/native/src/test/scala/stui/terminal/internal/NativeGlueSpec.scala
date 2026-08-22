package stui.terminal.internal

import hedgehog.*
import hedgehog.runner.*

import scala.scalanative.unsafe.*

/** Smoke test: passes only if sbt compiled and linked stui_terminal.c.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object NativeGlueSpec extends Properties {

  override def tests: List[Test] = List(
    example("sigwinch is a positive signal number", testSigwinch),
    example("winsize on fd 0 returns 0 or -1 without crashing", testWinsize),
  )

  def testSigwinch: Result = Result.assert(NativeGlue.sigwinch() > 0)

  def testWinsize: Result = {
    val rows = stackalloc[CInt]()
    val cols = stackalloc[CInt]()
    val rc   = NativeGlue.winsize(0, rows, cols)
    (rc ==== 0).or(rc ==== -1)
  }

}
