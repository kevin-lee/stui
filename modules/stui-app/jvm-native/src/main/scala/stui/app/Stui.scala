package stui.app

import stui.app.internal.{BlockingDriver, QueueScheduler}
import stui.core.capability.CapabilitiesPatch
import stui.core.spi.{Clock, TerminalError, TerminalOptions}
import stui.terminal.{PlatformTerminal, TerminalSession}
import stui.terminal.TerminalSession.*

import scala.concurrent.duration.*

/** The JVM and Native ignition (design doc 7.4 and 10, M3b): the platform session and the blocking driver over the production
  * scheduler. `run` returns after the loop ends and the final model reaches `onExit`. An exception from the application propagates
  * out of `run` after the restore without reaching `onExit` (the M2c crash path: the trace lands after the restore, exit 1). A
  * termination signal on Native ends the loop and the platform exits with 128 plus the signal number before `onExit` would run, and
  * the JVM's shutdown hook exits 143 on its own. `Clock.system` is named here and nowhere else in the module.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
object Stui {

  /** How long a poll may block with nothing due: the bound on noticing a termination signal or a task result. */
  private val PollCap: FiniteDuration = 100.millis

  /** [[runFully]] over the process environment with no overrides. */
  def run[Model, Msg](app: StuiApp[Model, Msg], options: TerminalOptions)(onExit: Either[TerminalError, Model] => Unit): Unit =
    onExit(PlatformTerminal.run(options)(session => drive(app, options, session)))

  /** [[runFully]] with no overrides. */
  def runWith[Model, Msg](env: Map[String, String], app: StuiApp[Model, Msg], options: TerminalOptions)(
    onExit: Either[TerminalError, Model] => Unit
  ): Unit = onExit(PlatformTerminal.runWith(env, options)(session => drive(app, options, session)))

  /** Opens the terminal (the three-layer capabilities merge of design doc 7.3), runs the application to its exit, restores on every
    * path, and hands the final model to `onExit`, or the error when the terminal cannot be entered.
    */
  def runFully[Model, Msg](
    env: Map[String, String],
    overrides: CapabilitiesPatch,
    app: StuiApp[Model, Msg],
    options: TerminalOptions,
  )(onExit: Either[TerminalError, Model] => Unit): Unit =
    onExit(PlatformTerminal.runFully(env, overrides, options)(session => drive(app, options, session)))

  private def drive[Model, Msg](app: StuiApp[Model, Msg], options: TerminalOptions, session: TerminalSession): Model =
    BlockingDriver.run(
      app,
      AppEnv(session.capabilities, options.screenMode, session.terminal.viewport.size),
      session.terminal,
      session.events,
      QueueScheduler.unwoken(Clock.system),
      () => session.terminationRequested,
      PollCap,
    )

}
