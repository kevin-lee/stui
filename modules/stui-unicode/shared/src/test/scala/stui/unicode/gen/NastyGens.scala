package stui.unicode.gen

import hedgehog.{Gen, Range}
import stui.unicode.corpus.GraphemeBreakCase
import stui.unicode.internal.CodePointTable
import stui.unicode.internal.CodePointTable.{Gcb, InCB}
import stui.unicode.internal.IntOps.*

/** Generators biased toward the nasty cases of segmentation and width: Hangul jamo runs, regional indicator runs, emoji ZWJ and modifier
  * sequences, Indic conjuncts, combining stacks, variation sequences, halfwidth dakuten, controls, prepends, keycaps, and lone surrogates.
  * They depend on no stui type outside this module (stui-testkit cannot be used here, it depends on stui-core which depends on stui-unicode).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object NastyGens {

  private val CodePointCount: Int = 0x110000

  private def isSurrogate(cp: Int): Boolean = cp >= 0xd800 && cp <= 0xdfff

  private val scalars: Vector[Int] = (0 until CodePointCount).filterNot(isSurrogate).toVector

  val byGcb: Map[Int, Vector[Int]] = scalars.groupBy(cp => CodePointTable.gcb(CodePointTable.record(cp)))

  val extendedPictographic: Vector[Int] = scalars.filter(cp => CodePointTable.isExtendedPictographic(CodePointTable.record(cp)))

  val incbConsonant: Vector[Int] = scalars.filter(cp => CodePointTable.incb(CodePointTable.record(cp)) === InCB.Consonant)

  val incbLinker: Vector[Int] = scalars.filter(cp => CodePointTable.incb(CodePointTable.record(cp)) === InCB.Linker)

  val incbExtend: Vector[Int] = scalars.filter(cp => CodePointTable.incb(CodePointTable.record(cp)) === InCB.Extend)

  val wide: Vector[Int] = scalars.filter(cp => CodePointTable.width(CodePointTable.record(cp)) === 2)

  val control: Vector[Int] = pool(Gcb.Control)

  def pool(gcb: Int): Vector[Int] = byGcb.getOrElse(gcb, Vector.empty[Int])

  def render(codePoints: Iterable[Int]): String = GraphemeBreakCase.render(codePoints)

  def fromPool(codePoints: Vector[Int]): Gen[Int] =
    codePoints.toList match {
      case first :: rest => Gen.element(first, rest)
      case Nil => Gen.constant(0xfffd)
    }

  /** Any non-surrogate code point. */
  val scalar: Gen[Int] = Gen.choice1(Gen.int(Range.linear(0, 0xd7ff)), Gen.int(Range.linear(0xe000, 0x10ffff)))

  /** A lone surrogate half. */
  val surrogate: Gen[Int] = Gen.int(Range.linear(0xd800, 0xdfff))

  val hangulSyllable: Gen[String] =
    for {
      l <- fromPool(pool(Gcb.L)).list(Range.linear(1, 2))
      v <- fromPool(pool(Gcb.V)).list(Range.linear(1, 2))
      t <- fromPool(pool(Gcb.T)).list(Range.linear(0, 2))
    } yield render(l ++ v ++ t)

  def regionalIndicators(count: Range[Int]): Gen[String] = fromPool(pool(Gcb.RegionalIndicator)).list(count).map(render)

  val flagPair: Gen[String] = regionalIndicators(Range.linear(2, 2))

  val emojiZwjSequence: Gen[String] =
    for {
      first     <- fromPool(extendedPictographic)
      extenders <- fromPool(pool(Gcb.Extend)).list(Range.linear(0, 1))
      rest      <- fromPool(extendedPictographic).list(Range.linear(1, 3))
    } yield render(first :: (extenders ++ rest.flatMap(ep => List(0x200d, ep))))

  val emojiModifierSequence: Gen[String] =
    for {
      base     <- fromPool(extendedPictographic)
      modifier <- Gen.int(Range.linear(0x1f3fb, 0x1f3ff))
    } yield render(List(base, modifier))

  val indicConjunct: Gen[String] = {
    val extendOrLinker = Gen.choice1(fromPool(incbExtend), fromPool(incbLinker))
    for {
      first  <- fromPool(incbConsonant)
      before <- extendOrLinker.list(Range.linear(0, 2))
      linker <- fromPool(incbLinker)
      after  <- extendOrLinker.list(Range.linear(0, 2))
      second <- fromPool(incbConsonant)
    } yield render(first :: (before ++ (linker :: after) ++ List(second)))
  }

  val combiningStack: Gen[String] =
    for {
      base  <- fromPool(pool(Gcb.Other))
      marks <- fromPool(pool(Gcb.Extend)).list(Range.linear(1, 5))
    } yield render(base :: marks)

  val variationSequence: Gen[String] =
    for {
      base     <- scalar
      selector <- Gen.element(0xfe0e, List(0xfe0f))
    } yield render(List(base, selector))

  val halfwidthDakuten: Gen[String] =
    for {
      katakana <- Gen.int(Range.linear(0xff76, 0xff9d))
      mark     <- Gen.element(0xff9e, List(0xff9f))
    } yield render(List(katakana, mark))

  /** CR, LF, CR LF, ESC, DEL, NEL, LINE SEPARATOR. */
  val controlRun: Gen[String] =
    Gen.element("\r", List("\n", "\r\n", render(List(0x1b)), render(List(0x7f)), render(List(0x85)), render(List(0x2028))))

  val prependSequence: Gen[String] =
    for {
      prepends <- fromPool(pool(Gcb.Prepend)).list(Range.linear(1, 2))
      base     <- fromPool(pool(Gcb.Other))
    } yield render(prepends ++ List(base))

  val keycap: Gen[String] = Gen.int(Range.linear('0'.toInt, '9'.toInt)).map(digit => render(List(digit, 0xfe0f, 0x20e3)))

  /** One cluster-shaped string (or a single scalar). */
  val cluster: Gen[String] = Gen.frequency1(
    3 -> scalar.map(cp => render(List(cp))),
    2 -> hangulSyllable,
    2 -> flagPair,
    1 -> regionalIndicators(Range.linear(1, 5)),
    2 -> emojiZwjSequence,
    1 -> emojiModifierSequence,
    2 -> indicConjunct,
    2 -> combiningStack,
    1 -> variationSequence,
    1 -> halfwidthDakuten,
    1 -> controlRun,
    1 -> prependSequence,
    1 -> keycap,
  )

  /** Clusters joined at nasty boundaries. */
  def text(range: Range[Int]): Gen[String] = cluster.list(range).map(_.mkString)

  /** Arbitrary UTF-16 code units, lone surrogates included. */
  def anyString(range: Range[Int]): Gen[String] = Gen.string(Gen.unicodeAll, range)

  def surrogateInjected(range: Range[Int]): Gen[String] =
    for {
      base <- text(range)
      half <- surrogate
      at   <- Gen.int(Range.linear(0, base.length))
    } yield base.substring(0, at) + render(List(half)) + base.substring(at)

  def nastyString(range: Range[Int]): Gen[String] =
    Gen.frequency1(5 -> text(range), 2 -> anyString(range), 1 -> surrogateInjected(range))

}
