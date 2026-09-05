package stui.app

import scala.annotation.tailrec
import scala.concurrent.duration.*

/** What an application listens to besides the terminal's events (design doc 10, decision D20, M3b): ticks only in M3b, since the
  * event stream is `StuiApp.onEvent`. `Every` fires `tag` with the scheduler's clock (a duration from an arbitrary origin) every
  * `interval`, re-armed against the previous due so the ticks do not drift. Subscriptions reconcile by key ([[SubKey]], the data
  * without the function) between updates: present in both is untouched and gets the newest tagger, new starts, missing stops, and
  * equal keys in one value share the timer with every tagger firing. An interval at or below zero counts as
  * [[Sub.minimumInterval]].
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
enum Sub[+Msg] {
  case None
  case Batch(subs: Vector[Sub[Msg]])
  case Every(interval: FiniteDuration, tag: FiniteDuration => Msg)
}

object Sub {

  /** The floor of a tick interval. */
  val minimumInterval: FiniteDuration = 1.millis

  /** Nothing. */
  val none: Sub[Nothing] = None

  /** The subscriptions together. */
  def batch[Msg](subs: Sub[Msg]*): Sub[Msg] = Batch(subs.toVector)

  /** A tick every `interval` (floored at [[minimumInterval]]) tagged with the clock. */
  def every[Msg](interval: FiniteDuration)(tag: FiniteDuration => Msg): Sub[Msg] = Every(floored(interval), tag)

  /** The interval, or [[minimumInterval]] when the given one is not positive. */
  def floored(interval: FiniteDuration): FiniteDuration = if (interval <= Duration.Zero) minimumInterval else interval

  /** The key of a tick: its floored interval. */
  def key[Msg](every: Every[Msg]): SubKey = SubKey.Every(floored(every.interval))

  extension [Msg](sub: Sub[Msg]) {

    /** The ticks in depth-first order. */
    def leaves: Vector[Every[Msg]] = {
      @tailrec
      def loop(work: Vector[Sub[Msg]], acc: Vector[Every[Msg]]): Vector[Every[Msg]] =
        work match {
          case head +: rest =>
            head match {
              case None => loop(rest, acc)
              case Batch(subs) => loop(subs ++ rest, acc)
              case every @ Every(_, _) => loop(rest, acc :+ every)
            }
          case _ => acc
        }

      loop(Vector(sub), Vector.empty[Every[Msg]])
    }

    /** The keys of the ticks. */
    def keys: Set[SubKey] = sub.leaves.map(every => key(every)).toSet

  }

}
