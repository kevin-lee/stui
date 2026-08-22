package ucdgen

/** Grapheme_Cluster_Break values as stored in the packed record (bits 0..3). The numbering is the contract with the library. */
object Gcb {

  val Other: Int = 0

  val CR: Int = 1

  val LF: Int = 2

  val Control: Int = 3

  val Extend: Int = 4

  val ZWJ: Int = 5

  val RegionalIndicator: Int = 6

  val Prepend: Int = 7

  val SpacingMark: Int = 8

  val L: Int = 9

  val V: Int = 10

  val T: Int = 11

  val LV: Int = 12

  val LVT: Int = 13

  val byName: Map[String, Int] = Map(
    "Other"              -> Other,
    "CR"                 -> CR,
    "LF"                 -> LF,
    "Control"            -> Control,
    "Extend"             -> Extend,
    "ZWJ"                -> ZWJ,
    "Regional_Indicator" -> RegionalIndicator,
    "Prepend"            -> Prepend,
    "SpacingMark"        -> SpacingMark,
    "L"                  -> L,
    "V"                  -> V,
    "T"                  -> T,
    "LV"                 -> LV,
    "LVT"                -> LVT,
  )

  val scalaNames: List[(String, Int)] = List(
    "Other"             -> Other,
    "CR"                -> CR,
    "LF"                -> LF,
    "Control"           -> Control,
    "Extend"            -> Extend,
    "ZWJ"               -> ZWJ,
    "RegionalIndicator" -> RegionalIndicator,
    "Prepend"           -> Prepend,
    "SpacingMark"       -> SpacingMark,
    "L"                 -> L,
    "V"                 -> V,
    "T"                 -> T,
    "LV"                -> LV,
    "LVT"               -> LVT,
  )

}

/** Indic_Conjunct_Break values as stored in the packed record (bits 4..5). */
object InCB {

  val None: Int = 0

  val Consonant: Int = 1

  val Extend: Int = 2

  val Linker: Int = 3

  val byName: Map[String, Int] = Map("None" -> None, "Consonant" -> Consonant, "Extend" -> Extend, "Linker" -> Linker)

  val scalaNames: List[(String, Int)] = List("None" -> None, "Consonant" -> Consonant, "Extend" -> Extend, "Linker" -> Linker)

}

/** East_Asian_Width values (generator-internal, only F and W survive into the baked width). */
object Eaw {

  val N: Int = 0

  val A: Int = 1

  val H: Int = 2

  val Na: Int = 3

  val W: Int = 4

  val F: Int = 5

  val byName: Map[String, Int] = Map("N" -> N, "A" -> A, "H" -> H, "Na" -> Na, "W" -> W, "F" -> F)

}

/** The packed per-code-point record: GCB (bits 0..3), InCB (4..5), Extended_Pictographic (6), Emoji_Presentation (7), width (8..9). */
object Record {

  private def bit(flag: Boolean): Int = if (flag) 1 else 0

  def make(gcb: Int, incb: Int, extendedPictographic: Boolean, emojiPresentation: Boolean, width: Int): Int =
    gcb | (incb << 4) | (bit(extendedPictographic) << 6) | (bit(emojiPresentation) << 7) | (width << 8)

  def gcb(record: Int): Int = record & 0xf

  def incb(record: Int): Int = (record >> 4) & 0x3

  def isExtendedPictographic(record: Int): Boolean = ((record >> 6) & 1) == 1

  def isEmojiPresentation(record: Int): Boolean = ((record >> 7) & 1) == 1

  def width(record: Int): Int = (record >> 8) & 0x3

}

/** Two-level trie: `records(leaves((index(cp >> shift) << shift) | (cp & mask)))` is the record of `cp`. */
final case class Trie(shift: Int, index: Vector[Int], leaves: Vector[Int], records: Vector[Int])

object Tables {

  /* Prepended_Concatenation_Marks that unicode-width gives width 0 (the other PCMs keep width 1). */
  val ZeroWidthPcm: Set[Int] = Set(0x0605, 0x070f, 0x0890, 0x0891, 0x08e2)

  def isSurrogate(cp: Int): Boolean = cp >= 0xd800 && cp <= 0xdfff

  /** Baked width precedence from the M1b plan (first match wins). */
  def bakedWidth(cp: Int, data: UcdData): Int = {
    val gcb = data.gcb(cp).toInt
    val eaw = data.eaw(cp).toInt
    if (isSurrogate(cp)) 1
    else if (gcb == Gcb.CR || gcb == Gcb.LF || gcb == Gcb.Control) 0
    else if (cp == 0x2d7f || cp == 0xff9e || cp == 0xff9f) 1
    else if (cp == 0x115f || cp == 0x17a4) 2
    else if (cp == 0x17d8) 2
    else if (
      data.defaultIgnorable(cp) || data.graphemeExtend(cp) || gcb == Gcb.V || gcb == Gcb.T || ZeroWidthPcm.contains(cp) ||
      (gcb == Gcb.Prepend && !data.prependedConcatenationMark(cp)) || cp == 0xa8fa
    ) 0
    else if (eaw == Eaw.W || eaw == Eaw.F) 2
    else 1
  }

  def record(cp: Int, data: UcdData): Int =
    if (isSurrogate(cp)) Record.make(Gcb.Other, InCB.None, false, false, 1)
    else
      Record.make(data.gcb(cp).toInt, data.incb(cp).toInt, data.extendedPictographic(cp), data.emojiPresentation(cp), bakedWidth(cp, data))

  def records(data: UcdData): Array[Int] = Array.tabulate(Ucd.CodePointCount)(cp => record(cp, data))

  def buildTrie(records: Array[Int]): Trie = {
    val distinct = records.distinct.toVector
    val indexOf  = distinct.zipWithIndex.toMap
    val recIdx   = records.map(indexOf)
    (5 to 9).map(shift => build(recIdx, shift, distinct)).minBy(trie => (trie.index.length + trie.leaves.length, -trie.shift))
  }

  private def build(recIdx: Array[Int], shift: Int, distinct: Vector[Int]): Trie = {
    val block             = 1 << shift
    val nBlocks           = Ucd.CodePointCount >> shift
    val (ids, uniques, _) =
      (0 until nBlocks).foldLeft((Vector.empty[Int], Vector.empty[Vector[Int]], Map.empty[Vector[Int], Int])) {
        case ((ids, uniques, seen), b) =>
          val key = recIdx.slice(b * block, (b + 1) * block).toVector
          seen.get(key) match {
            case Some(id) => (ids :+ id, uniques, seen)
            case None =>
              val id = uniques.length
              (ids :+ id, uniques :+ key, seen.updated(key, id))
          }
      }
    Trie(shift, ids, uniques.flatten, distinct)
  }

  def lookup(trie: Trie, cp: Int): Int =
    trie.records(trie.leaves((trie.index(cp >> trie.shift) << trie.shift) | (cp & ((1 << trie.shift) - 1))))

  /** Exhaustive trie-versus-array comparison plus fixed smoke assertions. Left explains the first failure(s). */
  def selfCheck(records: Array[Int], trie: Trie): Either[String, Unit] =
    (0 until Ucd.CodePointCount).find(cp => lookup(trie, cp) != records(cp)) match {
      case Some(cp) => Left(f"trie lookup differs from the record array at U+$cp%04X")
      case None => smoke(records)
    }

  private def smoke(records: Array[Int]): Either[String, Unit] = {
    def r(cp: Int): Int                 = records(cp)
    val checks: List[(String, Boolean)] = List(
      "GCB(000D) = CR"                 -> (Record.gcb(r(0x000d)) == Gcb.CR),
      "GCB(000A) = LF"                 -> (Record.gcb(r(0x000a)) == Gcb.LF),
      "GCB(001B) = Control"            -> (Record.gcb(r(0x001b)) == Gcb.Control),
      "GCB(200D) = ZWJ"                -> (Record.gcb(r(0x200d)) == Gcb.ZWJ),
      "GCB(1F1E6) = RegionalIndicator" -> (Record.gcb(r(0x1f1e6)) == Gcb.RegionalIndicator),
      "GCB(0600) = Prepend"            -> (Record.gcb(r(0x0600)) == Gcb.Prepend),
      "GCB(0903) = SpacingMark"        -> (Record.gcb(r(0x0903)) == Gcb.SpacingMark),
      "GCB(1100) = L"                  -> (Record.gcb(r(0x1100)) == Gcb.L),
      "GCB(1161) = V"                  -> (Record.gcb(r(0x1161)) == Gcb.V),
      "GCB(11A8) = T"                  -> (Record.gcb(r(0x11a8)) == Gcb.T),
      "GCB(AC00) = LV"                 -> (Record.gcb(r(0xac00)) == Gcb.LV),
      "GCB(AC01) = LVT"                -> (Record.gcb(r(0xac01)) == Gcb.LVT),
      "GCB(0300) = Extend"             -> (Record.gcb(r(0x0300)) == Gcb.Extend),
      "InCB(094D) = Linker"            -> (Record.incb(r(0x094d)) == InCB.Linker),
      "InCB(0915) = Consonant"         -> (Record.incb(r(0x0915)) == InCB.Consonant),
      "InCB(0300) = Extend"            -> (Record.incb(r(0x0300)) == InCB.Extend),
      "EP(1F600)"                      -> Record.isExtendedPictographic(r(0x1f600)),
      "EmojiPresentation(1F600)"       -> Record.isEmojiPresentation(r(0x1f600)),
      "not EmojiPresentation(263A)"    -> !Record.isEmojiPresentation(r(0x263a)),
      "width(0041) = 1"                -> (Record.width(r(0x0041)) == 1),
      "width(4E00) = 2"                -> (Record.width(r(0x4e00)) == 2),
      "width(FF76) = 1"                -> (Record.width(r(0xff76)) == 1),
      "width(FF9E) = 1"                -> (Record.width(r(0xff9e)) == 1),
      "width(0300) = 0"                -> (Record.width(r(0x0300)) == 0),
      "width(001B) = 0"                -> (Record.width(r(0x001b)) == 0),
      "width(000D) = 0"                -> (Record.width(r(0x000d)) == 0),
      "width(200D) = 0"                -> (Record.width(r(0x200d)) == 0),
      "width(FE0F) = 0"                -> (Record.width(r(0xfe0f)) == 0),
      "width(1161) = 0"                -> (Record.width(r(0x1161)) == 0),
      "width(115F) = 2"                -> (Record.width(r(0x115f)) == 2),
      "width(17D8) = 2"                -> (Record.width(r(0x17d8)) == 2),
      "width(D800) = 1"                -> (Record.width(r(0xd800)) == 1),
      "width(1F3FB) = 2"               -> (Record.width(r(0x1f3fb)) == 2),
      "width(0600) = 1"                -> (Record.width(r(0x0600)) == 1),
      "width(110BD) = 1"               -> (Record.width(r(0x110bd)) == 1),
      "width(E0020) = 0"               -> (Record.width(r(0xe0020)) == 0),
      "width(E0000) = 0"               -> (Record.width(r(0xe0000)) == 0),
    )
    val failed                          = checks.collect { case (label, false) => label }
    if (failed.isEmpty) Right(()) else Left(s"smoke assertions failed: ${failed.mkString(", ")}")
  }

  /** Run-length encoding of the record array as `START..END;GCB;INCB;EP;EMOJIPRESENTATION;WIDTH` lines (hex bounds). */
  def rawRanges(records: Array[Int]): List[String] = {
    val reversed = (1 until Ucd.CodePointCount).foldLeft(List((0, 0, records(0)))) {
      case (acc, cp) =>
        acc match {
          case (start, _, rec) :: rest if rec == records(cp) => (start, cp, rec) :: rest
          case _ => (cp, cp, records(cp)) :: acc
        }
    }
    reversed.reverse.map {
      case (start, end, rec) =>
        val ep = if (Record.isExtendedPictographic(rec)) 1 else 0
        val ex = if (Record.isEmojiPresentation(rec)) 1 else 0
        f"$start%04X..$end%04X;${Record.gcb(rec)};${Record.incb(rec)};$ep;$ex;${Record.width(rec)}"
    }
  }

}
