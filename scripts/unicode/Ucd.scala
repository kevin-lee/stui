package ucdgen

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*
import scala.util.Try

/** One input file of the Unicode Character Database (UCD), as pinned by `scripts/unicode/ucd.manifest`. */
final case class ManifestEntry(name: String, url: String, sha256: String)

final case class Manifest(version: String, entries: List[ManifestEntry])

object Manifest {

  def parse(lines: List[String]): Either[String, Manifest] = {
    val data = lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).map(_.split('\t').toList)
    val bad  = data.filterNot {
      case List("version", _) | List(_, _, _) => true
      case _ => false
    }
    if (bad.nonEmpty) {
      Left(s"ucd.manifest: malformed lines: ${bad.map(_.mkString("\t")).mkString(" | ")}")
    } else {
      data.collectFirst { case List("version", version) => version } match {
        case None => Left("ucd.manifest: missing the 'version' line")
        case Some(version) => Right(Manifest(version, data.collect { case List(name, url, sha256) => ManifestEntry(name, url, sha256) }))
      }
    }
  }

}

/** One data line of a UCD file: the code point range and the fields after it (comments already stripped). */
final case class UcdLine(start: Int, end: Int, fields: List[String])

/** Everything the table builder needs, one entry per code point (arrays of length `Ucd.CodePointCount`). */
final case class UcdData(
  eaw: Array[Byte],
  gcb: Array[Byte],
  extendedPictographic: Array[Boolean],
  emojiPresentation: Array[Boolean],
  incb: Array[Byte],
  defaultIgnorable: Array[Boolean],
  graphemeExtend: Array[Boolean],
  prependedConcatenationMark: Array[Boolean],
  graphemeBreakTestLines: List[String],
  emojiTestLines: List[String],
)

object Ucd {

  val CodePointCount: Int = 0x110000

  val ManifestPath: Path = Paths.get("scripts", "unicode", "ucd.manifest")

  val CacheRoot: Path = Paths.get("scripts", "unicode", ".cache")

  /* EastAsianWidth.txt documents these ranges as defaulting to W for unassigned code points. Applied before the explicit lines. */
  val DefaultWideRanges: List[(Int, Int)] = List(
    (0x3400, 0x4dbf),
    (0x4e00, 0x9fff),
    (0xf900, 0xfaff),
    (0x20000, 0x2fffd),
    (0x30000, 0x3fffd),
  )

  def traverse[A, B](as: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    as.foldRight(Right(Nil): Either[String, List[B]]) { (a, acc) =>
      for {
        bs <- acc
        b  <- f(a)
      } yield b :: bs
    }

  def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  def readManifest(): Either[String, Manifest] =
    if (Files.exists(ManifestPath)) Manifest.parse(Files.readAllLines(ManifestPath, StandardCharsets.UTF_8).asScala.toList)
    else Left(s"missing ${ManifestPath.toString}, run from the repository root")

  /** Downloads every manifest entry into the cache (if absent) and verifies its SHA-256. Returns name -> cached path. */
  def fetch(manifest: Manifest): Either[String, Map[String, Path]] = {
    val dir    = CacheRoot.resolve(manifest.version)
    Files.createDirectories(dir)
    val client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    manifest.entries.foldLeft(Right(Map.empty[String, Path]): Either[String, Map[String, Path]]) { (acc, entry) =>
      acc.flatMap(paths => fetchOne(client, dir, entry).map(path => paths.updated(entry.name, path)))
    }
  }

  private def fetchOne(client: HttpClient, dir: Path, entry: ManifestEntry): Either[String, Path] = {
    val path                               = dir.resolve(entry.name)
    val bytes: Either[String, Array[Byte]] =
      if (Files.exists(path)) {
        Right(Files.readAllBytes(path))
      } else {
        println(s"downloading ${entry.url}")
        val request  = HttpRequest.newBuilder(URI.create(entry.url)).GET().build()
        val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
        if (response.statusCode() == 200) {
          Files.write(path, response.body())
          Right(response.body())
        } else {
          Left(s"${entry.url}: HTTP ${response.statusCode()}")
        }
      }
    bytes.flatMap { content =>
      val actual = sha256Hex(content)
      if (actual == entry.sha256) Right(path)
      else Left(s"${entry.name}: SHA-256 mismatch, manifest ${entry.sha256}, actual $actual (${path.toString})")
    }
  }

  def readLines(path: Path): List[String] = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toList

  /** The part of a UCD line before `#`, trimmed, or None when nothing is left. */
  def dataPart(line: String): Option[String] = {
    val cut  = line.indexOf('#')
    val data = (if (cut < 0) line else line.substring(0, cut)).trim
    Option.when(data.nonEmpty)(data)
  }

  def hex(s: String): Either[String, Int] =
    Try(Integer.parseInt(s.trim, 16)).toEither.left.map(_ => s"not a hexadecimal code point: '$s'")

  def parseRange(s: String): Either[String, (Int, Int)] =
    s.trim.split("\\.\\.").toList match {
      case List(single) => hex(single).map(cp => (cp, cp))
      case List(start, end) =>
        for {
          from <- hex(start)
          to   <- hex(end)
        } yield (from, to)
      case _ => Left(s"not a code point range: '$s'")
    }

  def ucdLines(lines: List[String]): Either[String, List[UcdLine]] =
    traverse(lines.flatMap(dataPart)) { data =>
      data.split(';').toList.map(_.trim) match {
        case range :: fields => parseRange(range).map { case (start, end) => UcdLine(start, end, fields) }
        case Nil => Left(s"empty data line: '$data'")
      }
    }

  private def fillByte(arr: Array[Byte], line: UcdLine, value: Int): Unit =
    (line.start to line.end).foreach(cp => arr(cp) = value.toByte)

  private def fillBoolean(arr: Array[Boolean], line: UcdLine): Unit =
    (line.start to line.end).foreach(cp => arr(cp) = true)

  private def named(byName: Map[String, Int], kind: String)(name: String): Either[String, Int] =
    byName.get(name).toRight(s"unknown $kind value '$name'")

  def eastAsianWidth(lines: List[String]): Either[String, Array[Byte]] =
    ucdLines(lines).flatMap { parsed =>
      val arr = Array.fill[Byte](CodePointCount)(Eaw.N.toByte)
      DefaultWideRanges.foreach { case (start, end) => fillByte(arr, UcdLine(start, end, Nil), Eaw.W) }
      traverse(parsed) { line =>
        line
          .fields
          .headOption
          .toRight(s"EastAsianWidth line without a value at U+${line.start.toHexString}")
          .flatMap(named(Eaw.byName, "East_Asian_Width"))
      }.map { values =>
        parsed.zip(values).foreach { case (line, value) => fillByte(arr, line, value) }
        arr
      }
    }

  def graphemeBreakProperty(lines: List[String]): Either[String, Array[Byte]] =
    ucdLines(lines).flatMap { parsed =>
      val arr = Array.fill[Byte](CodePointCount)(Gcb.Other.toByte)
      traverse(parsed) { line =>
        line
          .fields
          .headOption
          .toRight(s"GraphemeBreakProperty line without a value at U+${line.start.toHexString}")
          .flatMap(named(Gcb.byName, "Grapheme_Cluster_Break"))
      }.map { values =>
        parsed.zip(values).foreach { case (line, value) => fillByte(arr, line, value) }
        arr
      }
    }

  private def flags(lines: List[String], property: String): Either[String, Array[Boolean]] =
    ucdLines(lines).map { parsed =>
      val arr = Array.fill(CodePointCount)(false)
      parsed.filter(_.fields.headOption.contains(property)).foreach(line => fillBoolean(arr, line))
      arr
    }

  def emojiData(lines: List[String]): Either[String, (Array[Boolean], Array[Boolean])] =
    for {
      ep           <- flags(lines, "Extended_Pictographic")
      presentation <- flags(lines, "Emoji_Presentation")
    } yield (ep, presentation)

  def derivedCoreProperties(lines: List[String]): Either[String, (Array[Byte], Array[Boolean], Array[Boolean])] =
    for {
      parsed <- ucdLines(lines)
      incbLines = parsed.filter(_.fields.headOption.contains("InCB"))
      incbValues       <- traverse(incbLines) { line =>
                            line
                              .fields
                              .drop(1)
                              .headOption
                              .toRight(s"InCB line without a value at U+${line.start.toHexString}")
                              .flatMap(named(InCB.byName, "Indic_Conjunct_Break"))
                          }
      defaultIgnorable <- flags(lines, "Default_Ignorable_Code_Point")
      graphemeExtend   <- flags(lines, "Grapheme_Extend")
    } yield {
      val incb = Array.fill[Byte](CodePointCount)(InCB.None.toByte)
      incbLines.zip(incbValues).foreach { case (line, value) => fillByte(incb, line, value) }
      (incb, defaultIgnorable, graphemeExtend)
    }

  def propList(lines: List[String]): Either[String, Array[Boolean]] = flags(lines, "Prepended_Concatenation_Mark")

  def graphemeBreakTest(lines: List[String]): List[String] = lines.flatMap(dataPart)

  def emojiTestFullyQualified(lines: List[String]): List[String] =
    lines.flatMap(dataPart).flatMap { data =>
      data.split(';').toList.map(_.trim) match {
        case List(codePoints, "fully-qualified") => List(codePoints.split("\\s+").mkString(" "))
        case _ => Nil
      }
    }

  def load(paths: Map[String, Path]): Either[String, UcdData] = {
    def lines(name: String): Either[String, List[String]] = paths.get(name).map(readLines).toRight(s"$name is not in the manifest")
    for {
      eawLines   <- lines("EastAsianWidth.txt")
      eaw        <- eastAsianWidth(eawLines)
      gcbLines   <- lines("GraphemeBreakProperty.txt")
      gcb        <- graphemeBreakProperty(gcbLines)
      emojiLines <- lines("emoji-data.txt")
      emoji      <- emojiData(emojiLines)
      (ep, presentation) = emoji
      derivedLines <- lines("DerivedCoreProperties.txt")
      derived      <- derivedCoreProperties(derivedLines)
      (incb, di, gext) = derived
      propLines      <- lines("PropList.txt")
      pcm            <- propList(propLines)
      breakTestLines <- lines("GraphemeBreakTest.txt")
      emojiTestLines <- lines("emoji-test.txt")
    } yield UcdData(
      eaw = eaw,
      gcb = gcb,
      extendedPictographic = ep,
      emojiPresentation = presentation,
      incb = incb,
      defaultIgnorable = di,
      graphemeExtend = gext,
      prependedConcatenationMark = pcm,
      graphemeBreakTestLines = graphemeBreakTest(breakTestLines),
      emojiTestLines = emojiTestFullyQualified(emojiTestLines),
    )
  }

}
