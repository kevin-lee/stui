package stui.unicode.oracle

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Minimal facades for Node's `Intl.Segmenter` and `process.versions`, enough for the differential test.
  *
  * The constructor parameters are consumed by the JavaScript constructor, never by Scala code, hence `@unused` and the scalafix
  * suppression.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
@js.native
@JSGlobal("Intl.Segmenter")
class IntlSegmenter(@unused locale: String, @unused options: js.Object) extends js.Object { // scalafix:ok UnusedConstructorParams

  def segment(input: String): js.Iterable[SegmentData] = js.native

}

@js.native
trait SegmentData extends js.Object {

  val segment: String = js.native

  val index: Int = js.native

}

@js.native
@JSGlobal("process")
object Process extends js.Object {

  val versions: ProcessVersions = js.native

}

@js.native
trait ProcessVersions extends js.Object {

  val unicode: String = js.native

  val node: String = js.native

}
