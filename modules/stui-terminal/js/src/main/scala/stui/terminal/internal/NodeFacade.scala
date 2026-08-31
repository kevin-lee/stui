package stui.terminal.internal

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.scalajs.js.typedarray.byteArray2Int8Array

/** The hand-written Node facade (design doc 7, the JS row, the M0 recipe): only what the backend needs from `process`, typed just
  * enough. `process` is a Node global, so `@JSGlobal` keeps the library module-kind-agnostic (no `@JSImport` anywhere). `stdin`'s
  * `on` is used for `'data'` only and Node hands the listener a `Buffer`, which is a `Uint8Array`, so the callback is typed as such
  * and bytes stay bytes (`setEncoding` is never called). `stdout`'s `on` is used for `'resize'` only (no payload), and `process`'s
  * `on` for `'exit'` and the signal names (the payload unused).
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
@js.native
trait NodeStdin extends js.Object {

  def isTTY: js.UndefOr[Boolean] = js.native

  def setRawMode(mode: Boolean): NodeStdin = js.native

  def on(event: String, callback: js.Function1[Uint8Array, Unit]): NodeStdin = js.native

  def resume(): NodeStdin = js.native

  def pause(): NodeStdin = js.native

}

@js.native
trait NodeStdout extends js.Object {

  def isTTY: js.UndefOr[Boolean] = js.native

  def columns: js.UndefOr[Int] = js.native

  def rows: js.UndefOr[Int] = js.native

  def write(data: Uint8Array): Boolean = js.native

  def on(event: String, callback: js.Function0[Unit]): NodeStdout = js.native

}

@js.native
@JSGlobal("process")
object NodeProcess extends js.Object {

  def stdin: NodeStdin = js.native

  def stdout: NodeStdout = js.native

  def env: js.Dictionary[String] = js.native

  def argv: js.Array[String] = js.native

  def pid: Int = js.native

  def on(event: String, callback: js.Function1[js.Any, Unit]): NodeProcess.type = js.native

  def exit(code: Int): Nothing = js.native

}

/** The byte conversions between the JVM-shaped arrays the library uses and Node's typed arrays, in one place. */
object NodeBytes {

  /** The bytes as a `Uint8Array` view (byte-exact, no string re-encode). */
  def toUint8Array(bytes: Array[Byte]): Uint8Array = {
    val signed: Int8Array = byteArray2Int8Array(bytes)
    new Uint8Array(signed.buffer, signed.byteOffset, signed.length)
  }

  /** A `'data'` chunk as the immutable byte array the decoder takes. */
  def toIArray(data: Uint8Array): IArray[Byte] =
    IArray.unsafeFromArray(Array.tabulate(data.length)(index => data(index).toByte))

}
