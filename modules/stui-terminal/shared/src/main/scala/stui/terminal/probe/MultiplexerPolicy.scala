package stui.terminal.probe

import stui.core.capability.{CapabilitiesPatch, Multiplexer}

/** The multiplexer policy over probe results (design doc 7.3, decision D15, plan refinement R6 of M1f), as a total function whose
  * table is this scaladoc, one test per row:
  *
  *   - synchronised output: the DECRPM answer passes under every multiplexer, because the answer is the multiplexer's own ground
  *     truth (tmux answers DECRQM 2026 and honours the mode from 3.7, tmux CHANGES).
  *   - scroll regions: never upgraded by a probe under any multiplexer (no mode to query, conservative).
  *   - the kitty keyboard protocol: never upgraded by a probe under any multiplexer, so Stui never pushes it there (tmux 3.7c
  *     answers the flags query itself with nothing, verified 2026-09-26).
  *   - identity upgrades (truecolour and extended underlines from XTGETTCAP or XTVERSION): dropped under any multiplexer, because
  *     the answering terminal is the outer one, not the multiplexer the application talks to.
  *   - SGR mouse: additionally dropped under GNU screen.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object MultiplexerPolicy {

  /** The patch with the fields the policy forbids for the multiplexer removed (the identity under `Multiplexer.None`). */
  def restrict(multiplexer: Multiplexer, patch: CapabilitiesPatch): CapabilitiesPatch = multiplexer match {
    case Multiplexer.None => patch
    case Multiplexer.Screen =>
      patch.copy(
        scrollRegionsSafe = None,
        kittyKeyboard = None,
        colors = None,
        extendedUnderline = None,
        sgrMouse = None,
      )
    case Multiplexer.Tmux | Multiplexer.Zellij | Multiplexer.Other =>
      patch.copy(scrollRegionsSafe = None, kittyKeyboard = None, colors = None, extendedUnderline = None)
  }

}
