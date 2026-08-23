package stui.testkit.gen

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.{Graphemes, WidthPolicy}

/** @author Kevin Lee
  * @since 2026-08-23
  */
object NastyGensSpec extends Properties {

  override def tests: List[Test] = List(
    example("the pools are non-empty", testPools),
    property("nastyString produces strings", testNastyString),
    property("wideCluster is one cluster of width 2", testWideCluster),
    property("narrowCluster is one cluster of width 1", testNarrowCluster),
  )

  def testPools: Result = Result.all(
    List(
      NastyGens.wide,
      NastyGens.narrow,
      NastyGens.extendedPictographic,
      NastyGens.incbConsonant,
      NastyGens.incbLinker,
      NastyGens.incbExtend,
      NastyGens.control,
    ).map(pool => Result.assert(pool.nonEmpty))
  )

  def testNastyString: Property = NastyGens.nastyString(Range.linear(0, 20)).forAll.map(s => Result.assert(s.length >= 0))

  def testWideCluster: Property = NastyGens.wideCluster.forAll.map { s =>
    Result.all(List(Graphemes.count(s) ==== 1, WidthPolicy.default.width(s) ==== 2))
  }

  def testNarrowCluster: Property = NastyGens.narrowCluster.forAll.map { s =>
    Result.all(List(Graphemes.count(s) ==== 1, WidthPolicy.default.width(s) ==== 1))
  }

}
