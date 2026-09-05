package stui.app

import cats.syntax.all.*
import stui.app.internal.{CallbackDriver, QueueScheduler}
import stui.app.internal.QueueScheduler.*
import stui.core.capability.CapabilitiesPatch
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.terminal.{PlatformTerminal, TerminalSession}
import stui.terminal.TerminalSession.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetTimeoutHandle

/** The Node ignition (design doc 7.4 and 10, M3b): the platform session, one due-time queue, and one `setTimeout` re-armed to the
  * earliest due, driving the shared callback driver. After `Cmd.Exit` the session is closed (restore, the transcript flush, standard
  * input paused) and `onExit` receives the final model, then the process ends by draining on its own: nothing is left to keep the
  * event loop alive. The crash path is unchanged from M2c (an exception from the application escapes the timer callback, the `'exit'`
  * hook restores, the trace lands after the restore, exit 1), and SIGTERM and SIGINT exit 143 and 130 through the platform handlers.
  * `Clock.system` is named here and nowhere else in the module.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object Stui {

  /** [[runFully]] over the process environment with no overrides. */
  def run[Model, Msg](app: StuiApp[Model, Msg], options: TerminalOptions)(onExit: Either[TerminalError, Model] => Unit): Unit =
    PlatformTerminal.run(options)(ignite(app, options, onExit))

  /** [[runFully]] with no overrides. */
  def runWith[Model, Msg](env: Map[String, String], app: StuiApp[Model, Msg], options: TerminalOptions)(
    onExit: Either[TerminalError, Model] => Unit
  ): Unit = PlatformTerminal.runWith(env, options)(ignite(app, options, onExit))

  /** Opens the terminal (the three-layer capabilities merge of design doc 7.3), starts the application, and hands the final model to
    * `onExit` once it exits, or the error when the terminal cannot be entered.
    */
  def runFully[Model, Msg](
    env: Map[String, String],
    overrides: CapabilitiesPatch,
    app: StuiApp[Model, Msg],
    options: TerminalOptions,
  )(onExit: Either[TerminalError, Model] => Unit): Unit = PlatformTerminal.runFully(env, overrides, options)(ignite(app, options, onExit))

  private def ignite[Model, Msg](app: StuiApp[Model, Msg], options: TerminalOptions, onExit: Either[TerminalError, Model] => Unit)(
    result: Either[TerminalError, TerminalSession]
  ): Unit = result match {
    case Left(error) => onExit(error.asLeft[Model])
    case Right(session) =>
      val timer                          = new AtomicReference(none[SetTimeoutHandle])
      lazy val scheduler: QueueScheduler = QueueScheduler.of(Clock.system, arm)
      def arm(due: Option[Long]): Unit   = {
        timer.getAndSet(none[SetTimeoutHandle]).foreach(handle => timers.clearTimeout(handle))
        due.foreach { d =>
          timer.set(timers.setTimeout(math.max(0L, d - scheduler.monotonicNanos()).nanos)(scheduler.drain()).some)
        }
      }
      val env                            = AppEnv(session.capabilities, options.screenMode, session.terminal.viewport.size)
      CallbackDriver.start(
        app,
        env,
        session.terminal,
        session.events,
        scheduler,
        model => {
          session.close()
          onExit(model.asRight[TerminalError])
        },
      )
  }

}
