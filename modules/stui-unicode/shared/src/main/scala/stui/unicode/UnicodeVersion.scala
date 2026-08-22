package stui.unicode

/** A Unicode Standard version, such as 17.0.0.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
final case class UnicodeVersion(major: Int, minor: Int, update: Int)

object UnicodeVersion {

  /** The Unicode version the bundled tables were generated from.
    *
    * A `def`, not a `val`: `CodePointTable`'s initialiser constructs a `UnicodeVersion` through this companion, so a `val` here would read
    * `CodePointTable` back while it is being initialised. On the JVM two test threads initialising the two objects from opposite ends
    * deadlock on the class-initialisation locks (observed on CI, 2026-08-22), on Scala.js the value would be null.
    */
  def current: UnicodeVersion = internal.CodePointTable.unicodeVersion

  extension (version: UnicodeVersion) {

    /** `17.0.0` */
    def render: String = s"${version.major}.${version.minor}.${version.update}"

    /** `17.0`, the form Node's `process.versions.unicode` reports. */
    def renderShort: String = s"${version.major}.${version.minor}"

  }

}
