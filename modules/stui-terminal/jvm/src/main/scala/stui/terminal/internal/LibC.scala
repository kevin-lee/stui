package stui.terminal.internal

import com.sun.jna.{Native, Platform}

/** libc bindings for the JVM backend. Non-variadic functions are direct-mapped, variadic ones go through [[LibCVarargs]].
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
private[stui] object LibC {

  Native.register(Platform.C_LIBRARY_NAME)

  @native def isatty(fd: Int): Int

  val varargs: LibCVarargs = Native.load(Platform.C_LIBRARY_NAME, classOf[LibCVarargs])

}
