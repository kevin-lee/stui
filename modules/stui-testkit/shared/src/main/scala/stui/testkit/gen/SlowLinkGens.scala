package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.testkit.SlowLink
import stui.testkit.SlowLink.{Arrival, Timeline}

import scala.concurrent.duration.*

/** Generators for the slow-link timing model (design doc 7.5, M3c).
  *
  * @author Kevin Lee
  * @since 2026-09-06
  */
object SlowLinkGens {

  /** The bytes cut into consecutive chunks at random byte boundaries, each chunk arriving at most `maxGap` after the previous one
    * (the first after the start), zero chunks for empty bytes.
    */
  def timeline(bytes: IArray[Byte], maxGap: FiniteDuration): Gen[SlowLink.Timeline] = {
    val length = bytes.length
    for {
      cuts <- Gen.int(Range.linear(0, length)).list(Range.linear(0, 4))
      points = (0 +: cuts.sorted.distinct.toVector :+ length).distinct
      chunks = points.zip(points.drop(1)).map { case (from, until) => IArray.from(bytes.toVector.slice(from, until)) }
      gaps <- Gens.millis(Range.linear(0, maxGap.toMillis.toInt)).list(Range.linear(chunks.length, chunks.length))
    } yield {
      val (_, arrivals) = chunks.zip(gaps).foldLeft((Duration.Zero: FiniteDuration, Vector.empty[Arrival])) {
        case ((at, acc), (chunk, gap)) =>
          val next = at + gap
          (next, acc :+ Arrival(next, chunk))
      }
      Timeline.of(arrivals*)
    }
  }

  /** One arrival within the first half second carrying up to eight random bytes. */
  val arrival: Gen[Arrival] =
    for {
      at    <- Gens.millis(Range.linear(0, 500))
      bytes <- Gen.int(Range.linear(0, 255)).list(Range.linear(0, 8))
    } yield Arrival(at, IArray.from(bytes.map(_.toByte)))

}
