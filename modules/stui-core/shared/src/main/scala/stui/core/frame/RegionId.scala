package stui.core.frame

import refined4s.Newtype
import refined4s.modules.cats.derivation.{CatsHash, CatsShow}

/** The identity of a hit region, chosen by the widget that draws it and compared structurally (design doc 6.4, decision D18). A list
  * tags each row it draws, a pane tags its whole area, and `Regions.at` answers mouse routing with it.
  */
type RegionId = RegionId.Type

/** @author Kevin Lee
  * @since 2026-08-29
  */
object RegionId extends Newtype[String], CatsHash[String], CatsShow[String]
