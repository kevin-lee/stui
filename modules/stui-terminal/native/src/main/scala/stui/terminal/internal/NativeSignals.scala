package stui.terminal.internal

import cats.syntax.all.*
import stui.core.terminal.TeardownLatch

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.scalanative.libc.{signal as csignal, stdlib}
import scala.scalanative.posix.signal as psignal
import scala.scalanative.unsafe.*

/** The Native signal handlers (design doc 7.4, the M0 recipe): `sigaction` handlers that only store into module-level atomics
  * (signal-safety(7)), `WINCH` into the resize flag, `SIGTERM` and `SIGINT` into the termination number and the orchestration's
  * teardown latch, and an `atexit` hook that closes the terminal (idempotent, so the bracket usually got there first).
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

  /** Stores the latch and the closer and installs the handlers once. */
  def install(latch: TeardownLatch, close: () => Unit): Unit = {
    latchRef.set(latch.some)
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

  private def handle(signal: CInt, handler: CFuncPtr1[CInt, Unit]): Unit = {
    val action   = stackalloc[psignal.sigaction]()
    val previous = stackalloc[psignal.sigaction]()
    action._1 = handler
    psignal.sigemptyset(action.at2): Unit
    action._3 = 0
    psignal.sigaction(signal, action, previous): Unit
  }

}
