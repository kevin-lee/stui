package stui.terminal.internal

import scala.scalanative.unsafe.*

/** Bindings to the C glue in src/main/resources/scala-native/stui_terminal.c.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
@extern
private[stui] object NativeGlue {

  @name("stui_terminal_sigwinch")
  def sigwinch(): CInt = extern

  @name("stui_terminal_winsize")
  def winsize(fd: CInt, rows: Ptr[CInt], cols: Ptr[CInt]): CInt = extern

}
