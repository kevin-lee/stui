package stui.terminal

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.Capabilities
import stui.core.event.{Event, KeyCode, KeyEvent}
import stui.core.geometry.Size
import stui.core.spi.{Clock, Probing, TerminalOptions}
import stui.terminal.FakeTty.*
import stui.terminal.decoder.Reply
import stui.terminal.probe.{ProbeQueries, ProbeResult}
import stui.terminal.probe.ProbeResult.*
import stui.testkit.Assertions

import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.*

/** The probe loop over a fake device and a queue: the batch, the sentinel, the timeout, the disabled policy, and the carried events.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ProbeRunSpec extends Properties {

  /** A [[RawInput]] fed by tests. */
  final class QueueInput extends RawInput {
    private val queue: LinkedBlockingQueue[IArray[Byte]]             = new LinkedBlockingQueue[IArray[Byte]]()
    def push(s: String): Unit                                        = queue.put(IArray.unsafeFromArray(s.getBytes(StandardCharsets.UTF_8)))
    override def poll(timeout: FiniteDuration): Option[IArray[Byte]] = Option(queue.poll(timeout.toMillis, TimeUnit.MILLISECONDS))
  }

  private inline def sized(inline width: Int, inline height: Int): Size = Size(NonNegInt(width), NonNegInt(height))

  private val esc: String = 0x1b.toChar.toString

  private val options: TerminalOptions = TerminalOptions.alternateScreen.withProbing(Probing.fixed(2.seconds))

  override def tests: List[Test] = List(
    example("the batch is written and the DA1 sentinel ends the probe", testAnswered),
    example("no answer times out fail-open", testTimeout),
    example("a disabled probing writes nothing", testDisabled),
    example("a keystroke between replies is carried out as an event", testCarriedKey),
  )

  def testAnswered: Result = {
    val tty   = FakeTty.of(sized(80, 24))
    val input = new QueueInput
    input.push(esc + "[?2026;2$y" + esc + "P1+r524742=38" + esc + "\\" + esc + "[14;1R" + esc + "[?62;22c")
    val probe = Probe.run(tty, input, options, Capabilities.conservative, Clock.system)
    Result.all(
      List(
        Assertions.eqv(tty.output, ProbeQueries.batch),
        Result.assert(probe.sentinelSeen).log("sentinel"),
        Assertions.eqv(probe.syncOutputAnswer, true.some),
        Result.assert(probe.truecolorAnswered).log("truecolor"),
        Assertions.eqv(probe.cursorRow, NonNegInt(13).some),
        Result.assert(!probe.decoder.expectingReplies).log("expecting cleared"),
      )
    )
  }

  def testTimeout: Result = {
    val tty   = FakeTty.of(sized(80, 24))
    val input = new QueueInput
    val probe =
      Probe.run(tty, input, TerminalOptions.alternateScreen.withProbing(Probing.fixed(50.millis)), Capabilities.conservative, Clock.system)
    Result.all(
      List(
        Assertions.eqv(tty.output, ProbeQueries.batch),
        Result.assert(!probe.sentinelSeen).log("no sentinel"),
        Assertions.eqv(probe.replies, Vector.empty[Reply]),
      )
    )
  }

  def testDisabled: Result = {
    val tty   = FakeTty.of(sized(80, 24))
    val input = new QueueInput
    val probe =
      Probe.run(tty, input, TerminalOptions.alternateScreen.withProbing(Probing.Disabled), Capabilities.conservative, Clock.system)
    Result.all(
      List(
        tty.writes ==== 0,
        Assertions.eqv(probe, ProbeResult.empty),
      )
    )
  }

  def testCarriedKey: Result = {
    val tty   = FakeTty.of(sized(80, 24))
    val input = new QueueInput
    input.push(esc + "[?2026;2$y" + "q" + esc + "[?62;22c")
    val probe = Probe.run(tty, input, options, Capabilities.conservative, Clock.system)
    Result.all(
      List(
        Assertions.eqv(probe.events, Vector(Event.key(KeyEvent.press(KeyCode.char('q'))))),
        Result.assert(probe.sentinelSeen).log("sentinel"),
        Assertions.eqv(
          probe.replies,
          Vector(Reply.PrivateModeReport(2026, 2), Reply.PrimaryDeviceAttributes(Vector(62, 22))),
        ),
      )
    )
  }

}
