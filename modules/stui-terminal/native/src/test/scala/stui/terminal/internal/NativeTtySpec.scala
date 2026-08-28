package stui.terminal.internal

import hedgehog.*
import hedgehog.runner.*

/** Smoke test: the device answers without crashing. Raw mode is never entered here, the test process may share Kevin's terminal.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object NativeTtySpec extends Properties {

  override def tests: List[Test] = List(
    example("isTerminal answers", testIsTerminal),
    example("size answers with None or a non-zero size", testSize),
  )

  def testIsTerminal: Result = {
    val answer = NativeTty().isTerminal
    Result.assert(answer || !answer)
  }

  def testSize: Result = NativeTty().size() match {
    case Some(size) => Result.assert(size.width.value > 0 && size.height.value > 0)
    case None => Result.success
  }

}
