package stui.unicode.internal

import hedgehog.*
import hedgehog.runner.*
import stui.unicode.internal.IntOps.*

import scala.annotation.tailrec
import scala.util.Try

/** Compares the trie against the run-length encoded records the generator wrote from the parsed UCD data, for every code point.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object CodePointTableExhaustiveSpec extends Properties {

  override def tests: List[Test] = List(
    example("raw ranges cover 0..10FFFF contiguously", testContiguous),
    example("every code point's record matches the raw ranges", testRecords),
  )

  final case class RawRange(
    start: Int,
    end: Int,
    gcb: Int,
    incb: Int,
    extendedPictographic: Boolean,
    emojiPresentation: Boolean,
    width: Int,
  )

  private def hex(s: String): Either[String, Int] = Try(Integer.parseInt(s, 16)).toOption.toRight(s"not hex: '$s'")

  private def int(s: String): Either[String, Int] = Try(s.toInt).toOption.toRight(s"not an int: '$s'")

  private def parse(line: String): Either[String, RawRange] =
    line.split(';').toList match {
      case List(range, gcb, incb, ep, presentation, width) =>
        range.split("\\.\\.").toList match {
          case List(startHex, endHex) =>
            for {
              start <- hex(startHex)
              end   <- hex(endHex)
              g     <- int(gcb)
              i     <- int(incb)
              e     <- int(ep)
              p     <- int(presentation)
              w     <- int(width)
            } yield RawRange(start, end, g, i, e === 1, p === 1, w)
          case _ => Left(s"not a range: '$range'")
        }
      case _ => Left(s"malformed raw range line: '$line'")
    }

  private val ranges: Either[String, List[RawRange]] =
    RawRanges.lines.toList.foldRight(Right(Nil): Either[String, List[RawRange]]) { (line, acc) =>
      for {
        rest  <- acc
        range <- parse(line)
      } yield range :: rest
    }

  def testContiguous: Result =
    ranges match {
      case Left(error) => Result.failure.log(error)
      case Right(rs) =>
        Result.all(
          List(
            rs.headOption.map(_.start) ==== Some(0),
            rs.lastOption.map(_.end) ==== Some(0x10ffff),
            Result
              .assert(rs.zip(rs.drop(1)).forall { case (previous, next) => next.start === previous.end + 1 })
              .log("ranges are not contiguous"),
          )
        )
    }

  private def sameFlag(actual: Boolean, expected: Boolean): Boolean = if (actual) expected else !expected

  @tailrec
  private def firstMismatch(range: RawRange, cp: Int): Option[String] =
    if (cp > range.end) {
      None
    } else {
      val record = CodePointTable.record(cp)
      val same   =
        CodePointTable.gcb(record) === range.gcb &&
          CodePointTable.incb(record) === range.incb &&
          sameFlag(CodePointTable.isExtendedPictographic(record), range.extendedPictographic) &&
          sameFlag(CodePointTable.isEmojiPresentation(record), range.emojiPresentation) &&
          CodePointTable.width(record) === range.width
      if (same) firstMismatch(range, cp + 1) else Some(f"U+$cp%04X: trie record ${record.toString} differs from the raw range $range")
    }

  def testRecords: Result =
    ranges match {
      case Left(error) => Result.failure.log(error)
      case Right(rs) =>
        rs.iterator.flatMap(range => firstMismatch(range, range.start)).nextOption() match {
          case None => Result.success
          case Some(message) => Result.failure.log(message)
        }
    }

}
