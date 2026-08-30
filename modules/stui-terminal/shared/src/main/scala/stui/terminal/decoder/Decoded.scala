package stui.terminal.decoder

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.event.Event

/** The result of one decoder step (design doc 7.5): the next state, the input events produced, and the terminal replies recognised
  * (design doc 7.3, consumed by the probe and dropped elsewhere).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class Decoded(state: DecoderState, events: Vector[Event], replies: Vector[Reply]) derives Eq, Show, Hash
