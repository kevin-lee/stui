package stui.unicode

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.gen.NastyGens

/** The segmentation and width laws, on nasty strings and on arbitrary UTF-16 (lone surrogates included), on every platform.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object GraphemeLawsSpec extends Properties {

  private val range: Range[Int] = Range.linear(0, 40)

  private val inputs: List[(String, Gen[String])] = List(
    "nasty" -> NastyGens.nastyString(range),
    "any"   -> NastyGens.anyString(range),
  )

  override def tests: List[Test] = inputs.flatMap {
    case (name, gen) =>
      List(
        property(s"[$name] clusters(s).mkString == s", testRoundTrip(gen)),
        property(s"[$name] width(s) == sum of the cluster widths", testWidthSum(gen)),
        property(s"[$name] segmentation is idempotent over its own output", testIdempotent(gen)),
        property(s"[$name] every cluster width is 0, 1, or 2", testClusterWidthRange(gen)),
        property(s"[$name] boundaries are strictly increasing from 0 to s.length", testBoundaries(gen)),
        property(s"[$name] count(s) == clusters(s).length", testCount(gen)),
        property(s"[$name] offset and String cluster widths agree", testOffsetForm(gen)),
      )
  }

  private def hex(s: String): String = s.map(unit => f"${unit.toInt}%04X").mkString(" ")

  def testRoundTrip(gen: Gen[String]): Property = gen.forAll.map(s => Graphemes.clusters(s).mkString ==== s)

  def testWidthSum(gen: Gen[String]): Property = gen.forAll.map { s =>
    WidthPolicy.default.width(s) ==== Graphemes.clusters(s).map(cluster => WidthPolicy.default.clusterWidth(cluster)).sum
  }

  def testIdempotent(gen: Gen[String]): Property = gen.forAll.map { s =>
    val clusters = Graphemes.clusters(s)
    clusters.flatMap(cluster => Graphemes.clusters(cluster)) ==== clusters
  }

  def testClusterWidthRange(gen: Gen[String]): Property = gen.forAll.map { s =>
    Result.all(Graphemes.clusters(s).toList.map { cluster =>
      val width = WidthPolicy.default.clusterWidth(cluster)
      Result.assert(width >= 0 && width <= 2).log(s"cluster ${hex(cluster)} has width ${width.toString}")
    })
  }

  def testBoundaries(gen: Gen[String]): Property = gen.forAll.map { s =>
    val boundaries = Graphemes.boundaries(s).toList
    Result.all(
      List(
        boundaries.headOption ==== Some(0),
        boundaries.lastOption ==== Some(s.length),
        Result
          .assert(boundaries.zip(boundaries.drop(1)).forall { case (previous, next) => previous < next })
          .log(s"not increasing: ${boundaries.mkString(", ")}"),
      )
    )
  }

  def testCount(gen: Gen[String]): Property = gen.forAll.map(s => Graphemes.count(s) ==== Graphemes.clusters(s).length)

  def testOffsetForm(gen: Gen[String]): Property = gen.forAll.map { s =>
    val boundaries = Graphemes.boundaries(s).toList
    Result.all(boundaries.zip(boundaries.drop(1)).map {
      case (start, end) =>
        WidthPolicy.default.clusterWidth(s, start, end) ==== WidthPolicy.default.clusterWidth(s.substring(start, end))
    })
  }

}
