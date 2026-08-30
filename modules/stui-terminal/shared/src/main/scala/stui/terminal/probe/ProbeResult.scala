package stui.terminal.probe

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import cats.syntax.all.*
import refined4s.types.numeric.NonNegInt
import stui.core.event.Event
import stui.terminal.decoder.{DecoderState, Reply}

/** What the startup probe collected (design doc 7.3): the replies in order, the input events decoded while waiting (handed to the
  * event source so keystrokes typed during the probe are not lost), the final decoder state (a partial sequence carries over), and
  * whether the DA1 sentinel arrived (false means the probe timed out and fails open).
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
final case class ProbeResult(replies: Vector[Reply], events: Vector[Event], decoder: DecoderState, sentinelSeen: Boolean)
    derives Eq,
      Show,
      Hash

object ProbeResult {

  /** No probe ran: nothing collected, the initial decoder state. */
  val empty: ProbeResult = ProbeResult(Vector.empty[Reply], Vector.empty[Event], DecoderState.initial, false)

  extension (result: ProbeResult) {

    /** The 0-based cursor row of the first Cursor Position Report, the inline entry anchor. */
    def cursorRow: Option[NonNegInt] = result
      .replies
      .collectFirst { case Reply.CursorPosition(position) => position.y }

    /** The DECRPM answer for mode 2026: `Some(true)` for set, reset, or permanently set (the mode is recognised), `Some(false)` for
      * not recognised or permanently reset, `None` when no answer arrived.
      */
    def syncOutputAnswer: Option[Boolean] = result
      .replies
      .collectFirst { case Reply.PrivateModeReport(2026, value) => value === 1 || value === 2 || value === 3 }

    /** True when a valid XTGETTCAP answer names `RGB` or `Tc`. */
    def truecolorAnswered: Boolean = result.replies.exists {
      case Reply.TermcapReply(true, entries) => entries.exists(entry => ProbeQueries.TruecolorNames.contains(entry.name))
      case Reply.TermcapReply(false, _) | Reply.CursorPosition(_) | Reply.PrivateModeReport(_, _) | Reply.VersionReply(_) |
          Reply.PrimaryDeviceAttributes(_) | Reply.SecondaryDeviceAttributes(_) =>
        false
    }

    /** The XTVERSION identity: the reply text up to the first space or `(`, in ASCII lower case (`iTerm2 3.6.11` gives `iterm2`,
      * `kitty(0.43.1)` gives `kitty`).
      */
    def identity: Option[String] = result
      .replies
      .collectFirst {
        case Reply.VersionReply(text) =>
          text.takeWhile(c => c =!= ' ' && c =!= '(').map(c => if (c >= 'A' && c <= 'Z') (c + 32).toChar else c)
      }
      .filter(_.nonEmpty)

  }

}
