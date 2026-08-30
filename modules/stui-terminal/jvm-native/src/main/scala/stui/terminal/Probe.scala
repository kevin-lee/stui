package stui.terminal

import stui.core.capability.Capabilities
import stui.core.event.Event
import stui.core.spi.{Clock, TerminalOptions}
import stui.terminal.decoder.{Decoded, Decoder, DecoderInput, DecoderState, Reply}
import stui.terminal.probe.{ProbeQueries, ProbeResult}

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The startup probe on the JVM and Native (design doc 7.3, decision D15): writes the query batch, then decodes the input stream with
  * `expectingReplies` set until the DA1 sentinel answers or the deadline passes, and fails open (a timeout returns whatever arrived).
  * Keystrokes typed during the probe come back as events, with the final decoder state, so the event source continues where the probe
  * stopped and nothing is lost. `Probing.Disabled` writes nothing and returns the empty result.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object Probe {

  /** Runs the probe over the device and the raw input. */
  def run(tty: Tty, input: RawInput, options: TerminalOptions, base: Capabilities, clock: Clock): ProbeResult =
    options.probing.resolve(base.ssh) match {
      case None => ProbeResult.empty
      case Some(timeout) =>
        tty.write(ProbeQueries.batch.getBytes(StandardCharsets.UTF_8))
        val deadline = clock.monotonicNanos() + timeout.toNanos
        loop(input, clock, deadline, DecoderState.initial.expecting(true), Vector.empty[Reply], Vector.empty[Event])
    }

  @tailrec
  private def loop(
    input: RawInput,
    clock: Clock,
    deadline: Long,
    state: DecoderState,
    replies: Vector[Reply],
    events: Vector[Event],
  ): ProbeResult = {
    val sentinel  = replies.exists {
      case Reply.PrimaryDeviceAttributes(_) => true
      case Reply.CursorPosition(_) | Reply.PrivateModeReport(_, _) | Reply.TermcapReply(_, _) | Reply.VersionReply(_) |
          Reply.SecondaryDeviceAttributes(_) =>
        false
    }
    val remaining = deadline - clock.monotonicNanos()
    if (sentinel || remaining <= 0L) {
      ProbeResult(replies, events, state.expecting(false), sentinel)
    } else {
      input.poll(remaining.nanos) match {
        case Some(chunk) =>
          Decoder.step(state, DecoderInput.bytes(chunk)) match {
            case Decoded(next, produced, answered) => loop(input, clock, deadline, next, replies ++ answered, events ++ produced)
          }
        case None => loop(input, clock, deadline, state, replies, events)
      }
    }
  }

}
