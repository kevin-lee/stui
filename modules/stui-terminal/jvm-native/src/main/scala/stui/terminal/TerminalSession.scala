package stui.terminal

import stui.core.capability.Capabilities
import stui.core.spi.BlockingEventSource
import stui.core.terminal.Terminal

/** What `PlatformTerminal.run` hands to the application: the orchestration, the event source, the capabilities, and whether a
  * termination signal was recorded (true on Native after SIGTERM or SIGINT, so the loop should return, never true on the JVM, whose
  * default SIGTERM handling runs the shutdown hook and exits on its own).
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
final class TerminalSession private[terminal] (
  val terminal: Terminal,
  val events: BlockingEventSource,
  val capabilities: Capabilities,
  private val terminationFlag: () => Boolean,
)

object TerminalSession {

  extension (session: TerminalSession) {

    /** True once a termination signal was recorded (Native only). */
    def terminationRequested: Boolean = session.terminationFlag()

  }

}
