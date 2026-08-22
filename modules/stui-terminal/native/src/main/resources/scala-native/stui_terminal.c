/* C glue for the Scala Native backend.
 * TIOCGWINSZ and SIGWINCH are not exposed by Scala Native's posixlib (M0 finding),
 * so this file is compiled and linked automatically by the Scala Native toolchain
 * from src/main/resources/scala-native/.
 */
#include <sys/ioctl.h>
#include <signal.h>

int stui_terminal_sigwinch(void) { return SIGWINCH; }

int stui_terminal_winsize(int fd, int* rows, int* cols) {
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) != 0) return -1;
  *rows = ws.ws_row;
  *cols = ws.ws_col;
  return 0;
}
