//> using scala 3.3.8
//> using options -no-indent -deprecation -feature -Wunused:all

package ucdgen

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Regenerates the stui-unicode tables and test corpora from the pinned UCD files.
  *
  * Usage (from the repository root): `scala-cli run scripts/unicode -- fetch | generate | check`.
  *   - `fetch` downloads the manifest entries into `scripts/unicode/.cache/<version>/` and verifies their SHA-256.
  *   - `generate` fetches, parses, builds the trie, runs the self-check, and writes the generated Scala files.
  *   - `check` does the same but only compares with the committed files (exit 1 when they differ).
  */
object GenerateUnicodeTables {

  enum Mode {
    case Write
    case Check
  }

  def main(args: Array[String]): Unit = {
    val result: Either[String, String] = args.toList match {
      case List("fetch") =>
        fetch().map { case (manifest, paths) => s"fetched and verified ${paths.size.toString} files for Unicode ${manifest.version}" }
      case List("generate") => generate(Mode.Write)
      case List("check") => generate(Mode.Check)
      case _ => Left("usage: scala-cli run scripts/unicode -- fetch | generate | check")
    }
    result match {
      case Right(message) => println(message)
      case Left(error) =>
        System.err.println(s"error: $error")
        sys.exit(1)
    }
  }

  private def fetch(): Either[String, (Manifest, Map[String, Path])] =
    for {
      manifest <- Ucd.readManifest()
      paths    <- Ucd.fetch(manifest)
    } yield (manifest, paths)

  private def generate(mode: Mode): Either[String, String] =
    for {
      fetched <- fetch()
      (manifest, paths) = fetched
      data <- Ucd.load(paths)
      records = Tables.records(data)
      trie    = Tables.buildTrie(records)
      _ <- Tables.selfCheck(records, trie)
      _ <- Emit.checkEncodable(trie)
      outputs = Emit.outputs(manifest, data, records, trie)
      report <- mode match {
                  case Mode.Write => Right(writeAll(outputs))
                  case Mode.Check => check(outputs)
                }
    } yield s"$report\n${stats(data, trie)}"

  private def stats(data: UcdData, trie: Trie): String =
    List(
      s"shift=${trie.shift.toString}",
      s"records=${trie.records.length.toString}",
      s"index=${trie.index.length.toString}",
      s"leaves=${trie.leaves.length.toString}",
      s"graphemeBreakTestLines=${data.graphemeBreakTestLines.length.toString}",
      s"emojiTestFullyQualified=${data.emojiTestLines.length.toString}",
    ).mkString(" ")

  private def writeAll(outputs: List[Output]): String = {
    outputs.foreach { output =>
      Files.createDirectories(output.path.getParent)
      Files.write(output.path, output.content.getBytes(StandardCharsets.UTF_8))
    }
    s"wrote:\n${outputs.map(output => s"  ${output.path.toString}").mkString("\n")}"
  }

  private def check(outputs: List[Output]): Either[String, String] = {
    val differing = outputs.filter { output =>
      !Files.exists(output.path) || new String(Files.readAllBytes(output.path), StandardCharsets.UTF_8) != output.content
    }
    if (differing.isEmpty) Right("all generated files are up to date")
    else
      Left(s"generated output differs from the committed files:\n${differing.map(output => s"  ${output.path.toString}").mkString("\n")}")
  }

}
