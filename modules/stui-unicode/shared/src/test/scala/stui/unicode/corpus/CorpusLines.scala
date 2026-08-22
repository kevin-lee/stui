package stui.unicode.corpus

/** Joins the string-encoded chunks of a generated corpus and splits them back into lines.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
object CorpusLines {

  def split(chunks: Array[String]): Vector[String] = chunks.mkString.split('\n').toVector.filter(_.nonEmpty)

}
