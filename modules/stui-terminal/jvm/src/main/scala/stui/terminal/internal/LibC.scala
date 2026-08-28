package stui.terminal.internal

import com.sun.jna.{Native, Platform}

/** libc bindings for the JVM backend. Non-variadic functions are direct-mapped, variadic ones go through [[LibCVarargs]]. The termios
  * functions take an opaque byte blob (256 bytes is enough on every supported platform), so no per-OS struct layout exists (M0
  * finding).
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
private[stui] object LibC {

  Native.register(Platform.C_LIBRARY_NAME)

  @native def isatty(fd: Int): Int

  @native def tcgetattr(fd: Int, termios: Array[Byte]): Int

  @native def tcsetattr(fd: Int, optionalActions: Int, termios: Array[Byte]): Int

  @native def cfmakeraw(termios: Array[Byte]): Unit

  val varargs: LibCVarargs = Native.load(Platform.C_LIBRARY_NAME, classOf[LibCVarargs])

}
