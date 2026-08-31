package stui.terminal

import stui.core.capability.Capabilities
import stui.core.spi.EventSource
import stui.core.terminal.Terminal

/** What the js `PlatformTerminal.run` hands to the application: the orchestration, the push event source (no `poll` on JS by
  * construction, design doc 6.3), and the capabilities. Because nothing may block on Node, there is no returning `use` to bracket,
  * so `close()` is the application's way to end the session: it stops event delivery, closes the terminal (restore, transcript
  * flush), and pauses standard input so the process can drain. It is idempotent, and the exit hook and the signal handlers restore
  * on every other path (design doc 7.4).
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
final class TerminalSession private[terminal] (
  val terminal: Terminal,
  val events: EventSource,
  val capabilities: Capabilities,
  private val shutdown: () => Unit,
)

object TerminalSession {

  extension (session: TerminalSession) {

    /** Ends the session: event delivery stops, the terminal is closed and restored, standard input is paused. Idempotent. */
    def close(): Unit = session.shutdown()

  }

}
