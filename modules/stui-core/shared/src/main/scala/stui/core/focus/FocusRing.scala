package stui.core.focus

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*

/** The focus targets of an application in traversal order and the current one, if any (design doc 10, decision D25). A pure value
  * the application keeps in its model: `onEvent` reads [[FocusRing.current]] to route a key, `update` moves it with the transitions
  * in the companion, and `view` styles the focused widget from it. Generic over the application's own target type: an enum gives
  * exhaustive matches on `current`, a `RegionId` unifies click-to-focus with mouse routing. Items are distinct by `Eq` when built
  * through the companion, the current target is one of the items or nothing, and every transition is total. The ring picks the
  * widget, the widget's own state picks the item within it. Not the terminal-window focus of `Event.FocusGained`.
  *
  * @author Kevin Lee
  * @since 2026-09-02
  */
final case class FocusRing[A](items: Vector[A], current: Option[A]) derives Eq, Show, Hash

object FocusRing {

  /** No items, nothing focused. */
  def empty[A]: FocusRing[A] = FocusRing(Vector.empty[A], none[A])

  /** The items made distinct (the first occurrence kept), the first one focused (nothing when there is none). */
  def of[A: Eq](items: A*): FocusRing[A] = fromVector(items.toVector)

  /** The items made distinct (the first occurrence kept), the first one focused (nothing when there is none). */
  def fromVector[A: Eq](items: Vector[A]): FocusRing[A] = {
    val kept = distinct(items)
    FocusRing(kept, kept.headOption)
  }

  private def distinct[A: Eq](items: Vector[A]): Vector[A] =
    items.foldLeft(Vector.empty[A])((kept, item) => if (kept.exists(_ === item)) kept else kept :+ item)

  extension [A](ring: FocusRing[A]) {

    /** True when there is no item. */
    def isEmpty: Boolean = ring.items.isEmpty

    /** True when a target is focused. */
    def isFocused: Boolean = ring.current.isDefined

    /** The first item focused, unchanged when empty. */
    def first: FocusRing[A] = ring.items.headOption.fold(ring)(item => ring.copy(current = item.some))

    /** The last item focused, unchanged when empty. */
    def last: FocusRing[A] = ring.items.lastOption.fold(ring)(item => ring.copy(current = item.some))

    /** Nothing focused, the items kept. */
    def blur: FocusRing[A] = ring.copy(current = none[A])

  }

  extension [A: Eq](ring: FocusRing[A]) {

    /** True when the target is one of the items. */
    def contains(target: A): Boolean = ring.items.exists(_ === target)

    /** True when the target is the focused one (the view-side test for "render this widget as focused"). */
    def has(target: A): Boolean = ring.current.exists(_ === target)

    /** The item after the current one, wrapping to the first after the last; the first when nothing is focused or the current target
      * is not among the items; unchanged when empty.
      */
    def next: FocusRing[A] =
      if (ring.items.isEmpty) {
        ring
      } else {
        val index = ring.current.fold(-1)(target => ring.items.indexWhere(_ === target))
        val found = if (index < 0) ring.items.headOption else ring.items.lift((index + 1) % ring.items.length)
        ring.copy(current = found)
      }

    /** The item before the current one, wrapping to the last before the first; the last when nothing is focused or the current target
      * is not among the items; unchanged when empty.
      */
    def previous: FocusRing[A] =
      if (ring.items.isEmpty) {
        ring
      } else {
        val index = ring.current.fold(-1)(target => ring.items.indexWhere(_ === target))
        val found = if (index <= 0) ring.items.lastOption else ring.items.lift(index - 1)
        ring.copy(current = found)
      }

    /** The target focused when it is an item, unchanged otherwise. */
    def focus(target: A): FocusRing[A] = if (ring.contains(target)) ring.copy(current = target.some) else ring

    /** The items replaced (made distinct, the first occurrence kept) and the current target corrected: a still-present target is
      * kept, an absent one becomes the first (nothing when the new items are empty), and a blurred ring stays blurred. The correction
      * repairs an invalid current and never invents focus, so an application applies it whenever its target set changes (a pane
      * hidden in inline mode, a dialog closed).
      */
    def withItems(items: Vector[A]): FocusRing[A] = {
      val kept    = distinct(items)
      val current = ring.current.flatMap { target =>
        if kept.exists(_ === target)
        then target.some
        else kept.headOption
      }
      FocusRing(kept, current)
    }

  }

}
