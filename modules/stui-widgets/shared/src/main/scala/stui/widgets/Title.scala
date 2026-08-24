package stui.widgets

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import stui.core.text.Line

/** One block title: a [[Line]] (its alignment places it) and an optional position (the block's `titlesPosition` decides when absent).
  *
  * @author Kevin Lee
  * @since 2026-08-24
  */
final case class Title(line: Line, position: Option[TitlePosition]) derives Eq, Show, Hash

object Title {

  /** A title placed by the block's default position. */
  def of(line: Line): Title = Title(line, none[TitlePosition])

  /** A title on the block's first row. */
  def top(line: Line): Title = Title(line, TitlePosition.Top.some)

  /** A title on the block's last row. */
  def bottom(line: Line): Title = Title(line, TitlePosition.Bottom.some)

}
