package stui.unicode.internal

/** The raw per-code-point properties of the bundled tables, for the stui-testkit generators (`private[stui]`), so that the generated
  * `CodePointTable` keeps its `private[unicode]` visibility. Values are the same `Int` codes the table uses, no `CodePointTable` type
  * appears in a signature.
  *
  * @author Kevin Lee
  * @since 2026-08-23
  */
private[stui] object CodePointProperties {

  /** Grapheme_Cluster_Break values. */
  object Gcb {
    val Other: Int             = CodePointTable.Gcb.Other
    val CR: Int                = CodePointTable.Gcb.CR
    val LF: Int                = CodePointTable.Gcb.LF
    val Control: Int           = CodePointTable.Gcb.Control
    val Extend: Int            = CodePointTable.Gcb.Extend
    val ZWJ: Int               = CodePointTable.Gcb.ZWJ
    val RegionalIndicator: Int = CodePointTable.Gcb.RegionalIndicator
    val Prepend: Int           = CodePointTable.Gcb.Prepend
    val SpacingMark: Int       = CodePointTable.Gcb.SpacingMark
    val L: Int                 = CodePointTable.Gcb.L
    val V: Int                 = CodePointTable.Gcb.V
    val T: Int                 = CodePointTable.Gcb.T
    val LV: Int                = CodePointTable.Gcb.LV
    val LVT: Int               = CodePointTable.Gcb.LVT
  }

  /** Indic_Conjunct_Break values. */
  object InCB {
    val None: Int      = CodePointTable.InCB.None
    val Consonant: Int = CodePointTable.InCB.Consonant
    val Extend: Int    = CodePointTable.InCB.Extend
    val Linker: Int    = CodePointTable.InCB.Linker
  }

  /** The number of code points, 0x110000. */
  val CodePointCount: Int = 0x110000

  def graphemeClusterBreak(cp: Int): Int = CodePointTable.gcb(CodePointTable.record(cp))

  def indicConjunctBreak(cp: Int): Int = CodePointTable.incb(CodePointTable.record(cp))

  def isExtendedPictographic(cp: Int): Boolean = CodePointTable.isExtendedPictographic(CodePointTable.record(cp))

  def isEmojiPresentation(cp: Int): Boolean = CodePointTable.isEmojiPresentation(CodePointTable.record(cp))

  def width(cp: Int): Int = CodePointTable.width(CodePointTable.record(cp))

}
