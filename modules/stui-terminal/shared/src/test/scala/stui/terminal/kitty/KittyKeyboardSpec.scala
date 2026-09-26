package stui.terminal.kitty

import hedgehog.*
import hedgehog.runner.*
import stui.core.capability.Capabilities
import stui.core.spi.{EscTimeout, KeyboardProtocol, ScreenMode, TerminalFeature, TerminalOptions}
import stui.testkit.Assertions

import scala.concurrent.duration.*

/** The kitty keyboard flags Stui pushes and the stall-aware ESC timeout (design doc 7.5, decision D30, M3d).
  *
  * @author Kevin Lee
  * @since 2026-09-27
  */
object KittyKeyboardSpec extends Properties {

  private val supported: Capabilities = Capabilities.conservative.copy(kittyKeyboard = true)

  private val options: TerminalOptions = TerminalOptions.of(ScreenMode.AlternateScreen)

  override def tests: List[Test] = List(
    example("the flags table", testFlagsFor),
    example("the ESC timeout under the disambiguate flag", testEscTimeout),
    example("the flag operations", testFlags),
  )

  def testFlagsFor: Result =
    Result.all(
      List(
        Assertions.eqv(KittyKeyboard.flagsFor(options, Capabilities.conservative), KittyFlags.none),
        Assertions.eqv(KittyKeyboard.flagsFor(options, supported), KittyFlags.Disambiguate),
        Assertions.eqv(KittyKeyboard.flagsFor(options.withKeyboard(KeyboardProtocol.Disabled), supported), KittyFlags.none),
        Assertions.eqv(
          KittyKeyboard.flagsFor(options.withFeature(TerminalFeature.KeyReleaseEvents), supported),
          KittyFlags.fromInt(7),
        ),
        Assertions.eqv(
          KittyKeyboard.flagsFor(options.withFeature(TerminalFeature.KeyReleaseEvents), Capabilities.conservative),
          KittyFlags.none,
        ),
      )
    )

  def testEscTimeout: Result =
    Result.all(
      List(
        Assertions.eqv(KittyKeyboard.escTimeout(EscTimeout.localDefault, KittyFlags.Disambiguate), EscTimeout.sshDefault),
        Assertions.eqv(KittyKeyboard.escTimeout(300.millis, KittyFlags.Disambiguate), 300.millis),
        Assertions.eqv(KittyKeyboard.escTimeout(EscTimeout.localDefault, KittyFlags.none), EscTimeout.localDefault),
      )
    )

  def testFlags: Result = {
    val seven = KittyFlags.Disambiguate.union(KittyFlags.EventTypes).union(KittyFlags.AlternateKeys)
    Result.all(
      List(
        Assertions.eqv(seven, KittyFlags.fromInt(7)),
        Assertions.eqv(seven.bits, 7),
        Result.assert(seven.contains(KittyFlags.EventTypes)).log("EventTypes"),
        Result.assert(!seven.contains(KittyFlags.AllKeys)).log("AllKeys"),
        Result.assert(KittyFlags.none.isEmpty).log("none"),
        Result.assert(!seven.isEmpty).log("seven"),
        Assertions.eqv(KittyFlags.fromInt(0xff).bits, 31),
      )
    )
  }

}
