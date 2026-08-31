package stui.terminal.internal

import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions

/** Smoke test: the facade and the device answer without crashing. Raw mode is never entered here, the test Node has no tty (the
  * `JvmTtySpec` pattern).
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object NodeTtySpec extends Properties {

  override def tests: List[Test] = List(
    example("the process facade answers", testProcess),
    example("isTerminal answers", testIsTerminal),
    example("size answers with None or a non-zero size", testSize),
    example("the byte round trip preserves values", testByteRoundTrip),
  )

  def testProcess: Result = Result.assert(NodeProcess.pid > 0)

  def testIsTerminal: Result = {
    val answer = NodeTty().isTerminal
    Result.assert(answer || !answer)
  }

  def testSize: Result = NodeTty().size() match {
    case Some(size) => Result.assert(size.width.value > 0 && size.height.value > 0)
    case None => Result.success
  }

  def testByteRoundTrip: Result = {
    val bytes = Array[Byte](0, 1, 127, -1, -128, 27, 91, 65)
    Assertions.eqv(NodeBytes.toIArray(NodeBytes.toUint8Array(bytes)).toVector, bytes.toVector)
  }

}
