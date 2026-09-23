package stui.terminal.probe

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.{Capabilities, CapabilitiesPatch, ColorProfile, Multiplexer}
import stui.core.event.Event
import stui.core.geometry.Position
import stui.terminal.ansi.Sequences
import stui.terminal.decoder.{DecoderState, Reply}
import stui.testkit.Assertions

/** The pure probe parts (design doc 7.3, decision D15): the query batch, the result readers, the policy patch, and the multiplexer
  * policy rows.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object ProbeSpec extends Properties {

  private def result(replies: Reply*): ProbeResult = ProbeResult(replies.toVector, Vector.empty[Event], DecoderState.initial, true)

  private val rgbValid: Reply = Reply.TermcapReply(true, Vector(Reply.TermcapEntry("RGB", Option("8"))))

  private val tcValid: Reply = Reply.TermcapReply(true, Vector(Reply.TermcapEntry("Tc", Option.empty[String])))

  private val invalid: Reply = Reply.TermcapReply(false, Vector(Reply.TermcapEntry("Tc", Option.empty[String])))

  override def tests: List[Test] = List(
    example("the batch asks 2026, RGB and Tc, the version, DA2, the cursor, and DA1 last", testBatch),
    example("the result readers pick the right replies", testReaders),
    example("the sentinel is reached only by a DA1 reply", testSentinelReached),
    example("the sync answers map 0 to 4 as the specification says", testSyncAnswers),
    example(
      "an empty result patches nothing",
      Assertions.eqv(ProbePolicy.patch(Capabilities.conservative, ProbeResult.empty), CapabilitiesPatch.empty),
    ),
    example("a 2026 answer sets syncOutput both ways", testSyncPatch),
    example("a truecolour answer upgrades colours unless the base is Mono", testTruecolorPatch),
    example("a known identity sets truecolour and extended underlines", testIdentityPatch),
    example("identity upgrades are dropped under a multiplexer while the sync answer passes", testMultiplexerRows),
  )

  def testBatch: Result =
    Result.all(
      List(
        Assertions.eqv(ProbeQueries.xtgettcap(List("RGB", "Tc")), Sequences.Esc + "P+q524742;5463" + Sequences.Esc + "\\"),
        Assertions.eqv(
          ProbeQueries.batch,
          Sequences.Csi + "?2026$p" + Sequences.Esc + "P+q524742;5463" + Sequences.Esc + "\\" + Sequences.Csi + ">0q" +
            Sequences.Csi + ">c" + Sequences.Csi + "6n" + Sequences.Csi + "c",
        ),
        Result.assert(ProbeQueries.batch.endsWith(Sequences.Csi + "c")).log("the sentinel is last"),
      )
    )

  def testSentinelReached: Result =
    Result.all(
      List(
        Result.assert(ProbeResult.sentinelReached(Vector(Reply.PrimaryDeviceAttributes(Vector(64, 4))))).log("DA1"),
        Result
          .assert(
            ProbeResult.sentinelReached(
              Vector(Reply.PrivateModeReport(2026, 2), rgbValid, Reply.PrimaryDeviceAttributes(Vector.empty[Int]))
            )
          )
          .log("DA1 among others"),
        Result.assert(!ProbeResult.sentinelReached(Vector(Reply.PrivateModeReport(2026, 2)))).log("DECRPM alone"),
        Result.assert(!ProbeResult.sentinelReached(Vector(rgbValid))).log("XTGETTCAP alone"),
        Result.assert(!ProbeResult.sentinelReached(Vector(Reply.VersionReply("iTerm2 3.6.11")))).log("XTVERSION alone"),
        Result
          .assert(!ProbeResult.sentinelReached(Vector(Reply.SecondaryDeviceAttributes(Vector(64, 2500, 0)))))
          .log("DA2 alone"),
        Result
          .assert(!ProbeResult.sentinelReached(Vector(Reply.CursorPosition(Position(NonNegInt(0), NonNegInt(3))))))
          .log("CPR alone"),
        Result.assert(!ProbeResult.sentinelReached(Vector.empty[Reply])).log("empty"),
      )
    )

  def testReaders: Result = {
    val full = result(
      Reply.PrivateModeReport(2026, 2),
      rgbValid,
      Reply.VersionReply("iTerm2 3.6.11"),
      Reply.SecondaryDeviceAttributes(Vector(64, 2500, 0)),
      Reply.CursorPosition(Position(NonNegInt(0), NonNegInt(13))),
      Reply.PrimaryDeviceAttributes(Vector(64, 4)),
    )
    Result.all(
      List(
        Assertions.eqv(full.cursorRow, NonNegInt(13).some),
        Assertions.eqv(full.syncOutputAnswer, true.some),
        Result.assert(full.truecolorAnswered).log("RGB"),
        Assertions.eqv(full.identity, "iterm2".some),
        Assertions.eqv(result(Reply.VersionReply("kitty(0.43.1)")).identity, "kitty".some),
        Result.assert(result(tcValid).truecolorAnswered).log("Tc"),
        Result.assert(!result(invalid).truecolorAnswered).log("invalid"),
        Assertions.eqv(ProbeResult.empty.cursorRow, none[NonNegInt]),
        Assertions.eqv(ProbeResult.empty.identity, none[String]),
      )
    )
  }

  def testSyncAnswers: Result =
    Result.all(
      List(
        Assertions.eqv(result(Reply.PrivateModeReport(2026, 0)).syncOutputAnswer, false.some),
        Assertions.eqv(result(Reply.PrivateModeReport(2026, 1)).syncOutputAnswer, true.some),
        Assertions.eqv(result(Reply.PrivateModeReport(2026, 2)).syncOutputAnswer, true.some),
        Assertions.eqv(result(Reply.PrivateModeReport(2026, 3)).syncOutputAnswer, true.some),
        Assertions.eqv(result(Reply.PrivateModeReport(2026, 4)).syncOutputAnswer, false.some),
        Assertions.eqv(result(Reply.PrivateModeReport(2004, 1)).syncOutputAnswer, none[Boolean]),
        Assertions.eqv(ProbeResult.empty.syncOutputAnswer, none[Boolean]),
      )
    )

  def testSyncPatch: Result =
    Result.all(
      List(
        Assertions.eqv(
          ProbePolicy.patch(Capabilities.conservative, result(Reply.PrivateModeReport(2026, 2))),
          CapabilitiesPatch.empty.withSyncOutput(true),
        ),
        Assertions.eqv(
          ProbePolicy.patch(Capabilities.conservative, result(Reply.PrivateModeReport(2026, 0))),
          CapabilitiesPatch.empty.withSyncOutput(false),
        ),
      )
    )

  def testTruecolorPatch: Result = {
    val mono = Capabilities.conservative.withColors(ColorProfile.Mono)
    Result.all(
      List(
        Assertions.eqv(
          ProbePolicy.patch(Capabilities.conservative, result(rgbValid)),
          CapabilitiesPatch.empty.withColors(ColorProfile.Truecolor),
        ),
        Assertions.eqv(ProbePolicy.patch(mono, result(rgbValid)), CapabilitiesPatch.empty),
      )
    )
  }

  def testIdentityPatch: Result =
    Result.all(
      List(
        Assertions.eqv(
          ProbePolicy.patch(Capabilities.conservative, result(Reply.VersionReply("ghostty 1.2.3"))),
          CapabilitiesPatch.empty.withColors(ColorProfile.Truecolor).withExtendedUnderline(true),
        ),
        Assertions.eqv(
          ProbePolicy.patch(Capabilities.conservative, result(Reply.VersionReply("Alacritty 0.12.2"))),
          CapabilitiesPatch.empty.withColors(ColorProfile.Truecolor),
        ),
        Assertions.eqv(ProbePolicy.patch(Capabilities.conservative, result(Reply.VersionReply("tmux 3.7"))), CapabilitiesPatch.empty),
      )
    )

  def testMultiplexerRows: Result = {
    val underTmux = Capabilities.conservative.copy(multiplexer = Multiplexer.Tmux)
    val answers   = result(Reply.PrivateModeReport(2026, 2), rgbValid, Reply.VersionReply("ghostty 1.2.3"))
    val riches    = CapabilitiesPatch
      .empty
      .withSyncOutput(true)
      .withColors(ColorProfile.Truecolor)
      .withExtendedUnderline(true)
      .withScrollRegionsSafe(true)
      .withKittyKeyboard(true)
      .withSgrMouse(true)
    Result.all(
      List(
        Assertions.eqv(ProbePolicy.patch(underTmux, answers), CapabilitiesPatch.empty.withSyncOutput(true)),
        Assertions.eqv(MultiplexerPolicy.restrict(Multiplexer.None, riches), riches),
        Assertions.eqv(
          MultiplexerPolicy.restrict(Multiplexer.Tmux, riches),
          CapabilitiesPatch.empty.withSyncOutput(true).withSgrMouse(true),
        ),
        Assertions.eqv(
          MultiplexerPolicy.restrict(Multiplexer.Screen, riches),
          CapabilitiesPatch.empty.withSyncOutput(true),
        ),
        Assertions.eqv(
          MultiplexerPolicy.restrict(Multiplexer.Zellij, riches),
          CapabilitiesPatch.empty.withSyncOutput(true).withSgrMouse(true),
        ),
      )
    )
  }

}
