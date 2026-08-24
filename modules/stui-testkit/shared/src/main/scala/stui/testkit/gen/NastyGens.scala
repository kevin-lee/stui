package stui.testkit.gen

import hedgehog.{Gen, Range}
import stui.unicode.internal.CodePointProperties
import stui.unicode.internal.CodePointProperties.{Gcb, InCB}
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec

/** Generators biased toward the nasty cases of segmentation and width: Hangul jamo runs, regional indicator runs, emoji ZWJ and modifier
  * sequences, Indic conjuncts, combining stacks, variation sequences, halfwidth dakuten, controls, prepends, keycaps, and lone surrogates.
  * A copy of stui-unicode's test-scope generator (which cannot be shared, unicode's tests cannot see testkit) over the `private[stui]`
  * property accessor. The pools are computed on first use.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
object NastyGens {

  private def isSurrogate(cp: Int): Boolean = cp >= 0xd800 && cp <= 0xdfff

  @tailrec
  private def scan(cp: Int, keep: Int => Boolean, acc: Vector[Int]): Vector[Int] =
    if (cp >= CodePointProperties.CodePointCount) acc
    else scan(cp + 1, keep, if (!isSurrogate(cp) && keep(cp)) acc :+ cp else acc)

  private lazy val scalars: Vector[Int] = scan(0, _ => true, Vector.empty[Int])

  /** Every scalar grouped by its Grapheme_Cluster_Break property. */
  lazy val byGcb: Map[Int, Vector[Int]] = scalars.groupBy(CodePointProperties.graphemeClusterBreak)

  /** The Extended_Pictographic pool. */
  lazy val extendedPictographic: Vector[Int] = scalars.filter(CodePointProperties.isExtendedPictographic)

  /** The Indic_Conjunct_Break Consonant pool. */
  lazy val incbConsonant: Vector[Int] = scalars.filter(cp => CodePointProperties.indicConjunctBreak(cp) === InCB.Consonant)

  /** The Indic_Conjunct_Break Linker pool. */
  lazy val incbLinker: Vector[Int] = scalars.filter(cp => CodePointProperties.indicConjunctBreak(cp) === InCB.Linker)

  /** The Indic_Conjunct_Break Extend pool. */
  lazy val incbExtend: Vector[Int] = scalars.filter(cp => CodePointProperties.indicConjunctBreak(cp) === InCB.Extend)

  /** Code points of width 2. */
  lazy val wide: Vector[Int] = scalars.filter(cp => CodePointProperties.width(cp) === 2)

  /** Code points of width 1 that are their own cluster (Grapheme_Cluster_Break Other). */
  lazy val narrow: Vector[Int] =
    scalars.filter(cp => CodePointProperties.width(cp) === 1 && CodePointProperties.graphemeClusterBreak(cp) === Gcb.Other)

  /** The Grapheme_Cluster_Break Control pool. */
  lazy val control: Vector[Int] = pool(Gcb.Control)

  /** The pool for the Grapheme_Cluster_Break value, empty when the table has none. */
  def pool(gcb: Int): Vector[Int] = byGcb.getOrElse(gcb, Vector.empty[Int])

  /** The string made of the given code points (surrogate halves are appended as they are). */
  def render(codePoints: Iterable[Int]): String =
    codePoints.foldLeft(new java.lang.StringBuilder)((builder, cp) => builder.appendCodePoint(cp)).toString

  /** One code point from the pool (the replacement character when the pool is empty). */
  def fromPool(codePoints: Vector[Int]): Gen[Int] =
    codePoints.toList match {
      case first :: rest => Gen.element(first, rest)
      case Nil => Gen.constant(0xfffd)
    }

  /** Any non-surrogate code point. */
  val scalar: Gen[Int] = Gen.choice1(Gen.int(Range.linear(0, 0xd7ff)), Gen.int(Range.linear(0xe000, 0x10ffff)))

  /** A lone surrogate half. */
  val surrogate: Gen[Int] = Gen.int(Range.linear(0xd800, 0xdfff))

  /** Hangul syllables composed from L, V, T, LV, and LVT jamo. */
  lazy val hangulSyllable: Gen[String] =
    for {
      l <- fromPool(pool(Gcb.L)).list(Range.linear(1, 2))
      v <- fromPool(pool(Gcb.V)).list(Range.linear(1, 2))
      t <- fromPool(pool(Gcb.T)).list(Range.linear(0, 2))
    } yield render(l ++ v ++ t)

  /** Runs of regional indicators, a count in the range. */
  def regionalIndicators(count: Range[Int]): Gen[String] = fromPool(pool(Gcb.RegionalIndicator)).list(count).map(render)

  /** Exactly two regional indicators, one flag. */
  lazy val flagPair: Gen[String] = regionalIndicators(Range.linear(2, 2))

  /** Pictographs joined with zero width joiners. */
  lazy val emojiZwjSequence: Gen[String] =
    for {
      first     <- fromPool(extendedPictographic)
      extenders <- fromPool(pool(Gcb.Extend)).list(Range.linear(0, 1))
      rest      <- fromPool(extendedPictographic).list(Range.linear(1, 3))
    } yield render(first :: (extenders ++ rest.flatMap(ep => List(0x200d, ep))))

  /** A pictograph followed by an emoji modifier. */
  lazy val emojiModifierSequence: Gen[String] =
    for {
      base     <- fromPool(extendedPictographic)
      modifier <- Gen.int(Range.linear(0x1f3fb, 0x1f3ff))
    } yield render(List(base, modifier))

  /** Indic conjuncts: consonant, linker chains, consonant. */
  lazy val indicConjunct: Gen[String] = {
    val extendOrLinker = Gen.choice1(fromPool(incbExtend), fromPool(incbLinker))
    for {
      first  <- fromPool(incbConsonant)
      before <- extendOrLinker.list(Range.linear(0, 2))
      linker <- fromPool(incbLinker)
      after  <- extendOrLinker.list(Range.linear(0, 2))
      second <- fromPool(incbConsonant)
    } yield render(first :: (before ++ (linker :: after) ++ List(second)))
  }

  /** A base scalar under a stack of combining marks. */
  lazy val combiningStack: Gen[String] =
    for {
      base  <- fromPool(pool(Gcb.Other))
      marks <- fromPool(pool(Gcb.Extend)).list(Range.linear(1, 5))
    } yield render(base :: marks)

  /** A base scalar with a variation selector (VS15 or VS16). */
  val variationSequence: Gen[String] =
    for {
      base     <- scalar
      selector <- Gen.element(0xfe0e, List(0xfe0f))
    } yield render(List(base, selector))

  /** Halfwidth katakana with a halfwidth voiced sound mark. */
  val halfwidthDakuten: Gen[String] =
    for {
      katakana <- Gen.int(Range.linear(0xff76, 0xff9d))
      mark     <- Gen.element(0xff9e, List(0xff9f))
    } yield render(List(katakana, mark))

  /** CR, LF, CR LF, ESC, DEL, NEL, LINE SEPARATOR. */
  val controlRun: Gen[String] =
    Gen.element("\r", List("\n", "\r\n", render(List(0x1b)), render(List(0x7f)), render(List(0x85)), render(List(0x2028))))

  /** A Prepend scalar before a base scalar. */
  lazy val prependSequence: Gen[String] =
    for {
      prepends <- fromPool(pool(Gcb.Prepend)).list(Range.linear(1, 2))
      base     <- fromPool(pool(Gcb.Other))
    } yield render(prepends ++ List(base))

  /** A digit keycap sequence (digit, VS16, combining enclosing keycap). */
  val keycap: Gen[String] = Gen.int(Range.linear('0'.toInt, '9'.toInt)).map(digit => render(List(digit, 0xfe0f, 0x20e3)))

  /** One cluster-shaped string (or a single scalar). */
  lazy val cluster: Gen[String] = Gen.frequency1(
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

  /** A single cluster of width 2. */
  lazy val wideCluster: Gen[String] = fromPool(wide).map(cp => render(List(cp)))

  /** A single cluster of width 1. */
  lazy val narrowCluster: Gen[String] = fromPool(narrow).map(cp => render(List(cp)))

  /** Clusters joined at nasty boundaries. */
  def text(range: Range[Int]): Gen[String] = cluster.list(range).map(_.mkString)

  /** Arbitrary UTF-16 code units, lone surrogates included. */
  def anyString(range: Range[Int]): Gen[String] = Gen.string(Gen.unicodeAll, range)

  /** Nasty text with lone surrogates spliced in. */
  def surrogateInjected(range: Range[Int]): Gen[String] =
    for {
      base <- text(range)
      half <- surrogate
      at   <- Gen.int(Range.linear(0, base.length))
    } yield base.substring(0, at) + render(List(half)) + base.substring(at)

  /** The full nasty mix: clusters, arbitrary Unicode, and surrogate-injected text. */
  def nastyString(range: Range[Int]): Gen[String] =
    Gen.frequency1(5 -> text(range), 2 -> anyString(range), 1 -> surrogateInjected(range))

}
