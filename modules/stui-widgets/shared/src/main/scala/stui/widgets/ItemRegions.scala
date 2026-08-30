package stui.widgets

import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.frame.RegionId

/** The hit-region scheme of the widget catalogue (design doc 6.6, M2b): a [[ListView]], [[Tabs]], or [[Table]] given a base
  * [[stui.core.frame.RegionId]] records the base over its whole target first, then `base[index]` over every drawn item, so
  * `Regions.at` on an item answers the index through [[indexOf]] and "over this widget at all" (blank rows and borders included)
  * through [[owns]]. The app decodes the routing this way until the M3 runtime owns it.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ItemRegions {

  /** The id of item `index` under `base`: `base[index]`. */
  def of(base: RegionId, index: NonNegInt): RegionId = RegionId(s"${base.value}[${index.value.toString}]")

  /** The index encoded by an id produced by [[of]] for `base`, `None` for the base itself, a foreign id, or a malformed one. */
  def indexOf(base: RegionId, id: RegionId): Option[NonNegInt] = {
    val prefix = s"${base.value}["
    val value  = id.value
    if (value.startsWith(prefix) && value.endsWith("]") && value.length > prefix.length) {
      value.substring(prefix.length, value.length - 1).toIntOption.flatMap(n => NonNegInt.from(n).toOption)
    } else {
      none[NonNegInt]
    }
  }

  /** True for the base id and for every item id under it. */
  def owns(base: RegionId, id: RegionId): Boolean = id === base || indexOf(base, id).isDefined

}
