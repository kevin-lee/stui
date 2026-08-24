package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*

/** The visible sides of a [[Block]]'s border.
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Borders(sides: Set[Side]) derives Eq, Show, Hash

object Borders {

  /** No border. */
  val none: Borders = Borders(Set.empty[Side])

  /** Every side. */
  val all: Borders = Borders(Side.all.toSet)

  /** Exactly the given sides. */
  def of(sides: Side*): Borders = Borders(sides.toSet)

  extension (borders: Borders) {

    /** True when the side is visible. */
    def contains(side: Side): Boolean = borders.sides.contains(side)

    /** The borders with the side made visible. */
    def add(side: Side): Borders = Borders(borders.sides + side)

    /** The borders with the side hidden. */
    def remove(side: Side): Borders = Borders(borders.sides - side)

    /** True when no side is visible. */
    def isEmpty: Boolean = borders.sides.isEmpty

  }

}
