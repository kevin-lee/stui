package stui.app.internal

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.app.SubKey

/** Elm's subscription reconciliation as a pure function (design doc 10, decision D20, M3b): present in both is untouched, new starts,
  * missing stops. Deterministic, idempotent (`diff(next, next)` is [[Reconcile.nothing]]), and exact, the laws of 12.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
private[stui] object Reconcile {

  /** The keys to start and the keys to stop. */
  final case class Diff(starts: Set[SubKey], stops: Set[SubKey]) derives Eq, Show, Hash

  /** No change. */
  val nothing: Diff = Diff(Set.empty[SubKey], Set.empty[SubKey])

  /** The keys in `next` but not in `previous` start, the keys in `previous` but not in `next` stop. */
  def diff(previous: Set[SubKey], next: Set[SubKey]): Diff = Diff(next -- previous, previous -- next)

  extension (changes: Diff) {

    /** True when nothing starts or stops. */
    def isEmpty: Boolean = changes.starts.isEmpty && changes.stops.isEmpty

  }

}
