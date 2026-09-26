package stui.terminal.internal

import cats.syntax.all.*
import stui.core.terminal.TeardownLatch

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.scalanative.libc.{signal as csignal, stdlib}
import scala.scalanative.posix.signal as psignal
import scala.scalanative.unsafe.*

/** The Native signal handlers (design doc 7.4, the M0 recipe): `sigaction` handlers that only store into module-level atomics
  * (signal-safety(7)), `WINCH` into the resize flag, `SIGTERM` and `SIGINT` into the termination number and the orchestration's
  * teardown latch, and an `atexit` hook that runs the current closer (idempotent, so the bracket usually got there first). They are
  * installed before the terminal is entered with the backend's exit as the closer, and attached to the terminal's latch and close
  * once it exists, so no signal between the entry (the kitty keyboard push) and the terminal is lost (M3d). After an entry that
  * fails, the handlers stay installed and only record.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
private[stui] object NativeSignals {

  /** Set by the `WINCH` handler, cleared by the event source. */
  val resized: AtomicBoolean = new AtomicBoolean(false)

  /** The first termination signal number, 0 before any. */
  val terminating: AtomicInteger = new AtomicInteger(0)

  /** The latch of the running terminal, recorded by the termination handler. */
  val latchRef: AtomicReference[Option[TeardownLatch]] = new AtomicReference(none[TeardownLatch])

  /** What the `atexit` hook runs. */
  val closer: AtomicReference[Option[() => Unit]] = new AtomicReference(none[() => Unit])

  private val installed: AtomicBoolean = new AtomicBoolean(false)

  private val winchHandler: CFuncPtr1[CInt, Unit] = CFuncPtr1.fromScalaFunction((_: CInt) => NativeSignals.resized.set(true))

  private val terminationHandler: CFuncPtr1[CInt, Unit] = CFuncPtr1.fromScalaFunction { (signal: CInt) =>
    NativeSignals.terminating.compareAndSet(0, signal): Unit
    NativeSignals.latchRef.get() match {
      case Some(latch) => latch.record(signal)
      case None => ()
    }
  }

  private val atexitHandler: CFuncPtr0[Unit] = CFuncPtr0.fromScalaFunction { () =>
    NativeSignals.closer.get() match {
      case Some(close) => close()
      case None => ()
    }
  }

  /** Forgets any previous latch, stores the closer, and installs the handlers once (before the terminal is entered). */
  def install(close: () => Unit): Unit = {
    latchRef.set(none[TeardownLatch])
    closer.set(close.some)
    if (installed.compareAndSet(false, true)) {
      handle(NativeGlue.sigwinch(), winchHandler)
      handle(csignal.SIGTERM, terminationHandler)
      handle(csignal.SIGINT, terminationHandler)
      stdlib.atexit(atexitHandler): Unit
    } else {
      ()
    }
  }

  /** Stores the terminal's latch and closer, recording into the latch a termination signal that arrived since [[install]]. */
  def attach(latch: TeardownLatch, close: () => Unit): Unit = {
    latchRef.set(latch.some)
    closer.set(close.some)
    latch.record(terminating.get())
  }

  /** The first termination signal number, if one arrived. */
  def terminationSignal: Option[Int] = Option(terminating.get()).filter(_ > 0)

  /* the C null for the unused old-action out-parameter, the M0-verified shape: letting the C library write the previous
   * action into a posixlib-sized stackalloc smashed the stack on Linux (the M2c CI smoke crash, issue 23) */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def handle(signal: CInt, handler: CFuncPtr1[CInt, Unit]): Unit = {
    val action = stackalloc[psignal.sigaction]()
    action._1 = handler
    psignal.sigemptyset(action.at2): Unit
    action._3 = 0
    psignal.sigaction(signal, action, null): Unit // scalafix:ok DisableSyntax.null
  }

}
