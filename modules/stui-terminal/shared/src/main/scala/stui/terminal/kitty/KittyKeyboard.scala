package stui.terminal.kitty

import stui.core.capability.Capabilities
import stui.core.spi.{EscTimeout, KeyboardProtocol, TerminalFeature, TerminalOptions}

import scala.concurrent.duration.FiniteDuration

/** The one source of the kitty keyboard flags Stui pushes (design doc 7.5, decision D30, the kitty keyboard protocol specification):
  * the backend writes the push from [[flagsFor]] and pops exactly it, and the platform starts the decoder with the same value.
  *
  * @author Kevin Lee
  * @since 2026-09-27
  */
object KittyKeyboard {

  /** The flags to push: none when the policy is `Disabled` or the capabilities do not report the protocol (a terminal that does not
    * answer the probe's flags query gets no push), `Disambiguate` plus `EventTypes` and `AlternateKeys` when
    * `KeyReleaseEvents` is requested (so a release names the key its press named), and `Disambiguate` alone otherwise. `AllKeys` and
    * `AssociatedText` are never pushed (Ghostty 1.3.1 loses input-method commits made with Enter under them, verified 2026-09-26).
    */
  def flagsFor(options: TerminalOptions, capabilities: Capabilities): KittyFlags = options.keyboard match {
    case KeyboardProtocol.Disabled => KittyFlags.none
    case KeyboardProtocol.Automatic =>
      if (!capabilities.kittyKeyboard) KittyFlags.none
      else if (options.enabled(TerminalFeature.KeyReleaseEvents))
        KittyFlags.Disambiguate.union(KittyFlags.EventTypes).union(KittyFlags.AlternateKeys)
      else KittyFlags.Disambiguate
  }

  /** The event source's ESC timeout: under `Disambiguate` no key starts with a bare ESC, so the timeout only resolves a stalled
    * sequence and is at least [[EscTimeout.sshDefault]], an explicit shorter `EscTimeout.Fixed` included (plan refinement D10). The
    * resolved value unchanged otherwise.
    */
  def escTimeout(resolved: FiniteDuration, flags: KittyFlags): FiniteDuration =
    if (flags.contains(KittyFlags.Disambiguate)) resolved.max(EscTimeout.sshDefault) else resolved

}
