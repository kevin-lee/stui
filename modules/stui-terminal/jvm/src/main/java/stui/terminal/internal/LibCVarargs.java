package stui.terminal.internal;

import com.sun.jna.Library;

/**
 * Variadic C functions must be bound through JNA interface mapping with true Java varargs so that
 * JNA marks the call variadic for libffi. The Apple arm64 ABI passes variadic arguments on the
 * stack, and a fixed-arity direct mapping silently returns garbage (M0 finding, 2026-08-20).
 * Scala varargs compile to Seq and do not qualify, which is why this interface is written in Java.
 * Every variadic libc function the JVM backend needs goes through this interface.
 */
public interface LibCVarargs extends Library {
  int ioctl(int fd, long request, Object... args);
}
