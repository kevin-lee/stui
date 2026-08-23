package stui.core.style

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions

/** Compile-time and runtime colour constructors.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object ColorSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      "rgb literals equal rgbOf of the refined values",
      Assertions.eqv(Color.rgb(255, 0, 7), Color.rgbOf(Color.Channel(255), Color.Channel(0), Color.Channel(7))),
    ),
    example("indexed literal equals indexedOf", Assertions.eqv(Color.indexed(7), Color.indexedOf(Color.Index(7)))),
    example("rgbFrom rejects 256", Result.assert(Color.rgbFrom(256, 0, 0).isLeft)),
    example("rgbFrom accepts 0..255", Assertions.eqv(Color.rgbFrom(1, 2, 3), Right(Color.rgb(1, 2, 3)))),
    example("indexedFrom rejects -1", Result.assert(Color.indexedFrom(-1).isLeft)),
    example(
      "show of Rgb contains the components",
      Result.all(List("255", "0", "7").map(component => Result.assert(Color.rgb(255, 0, 7).show.contains(component)))),
    ),
    example("named colours are distinct from Reset", Result.assert(Color.Red =!= Color.Reset)),
  )

}
