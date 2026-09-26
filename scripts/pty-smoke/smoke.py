#!/usr/bin/env python3
"""The CI demo smoke (M2c issue 23, extended in M3d issue 36): runs a stui demo command under a fresh pseudo-terminal, plays the
terminal's part in the startup probe, waits for the demo's header marker, presses a key, ends the demo along one exit path, and
asserts the exit status, the kitty keyboard push-and-pop balance, and sane terminal modes afterwards.

The probe modes:
  none    the probe is never answered, so the fail-open path runs (no kitty keyboard push may appear)
  legacy  DA1 only, a terminal without the kitty keyboard protocol (no push may appear)
  kitty   the kitty flags answer and DA1, a terminal with the protocol (exactly one push after the entry and one pop before the
          exit's reset, on every exit path)

The exit paths: q (status 0), ctrl-c (the Ctrl-C key, as `CSI 99;5u` under kitty and the 0x03 byte otherwise, status 0), crash
(the demo's `!` key, the trace after the restore, status 1), and sigterm (status 143). For an answered probe the environment gets
SSH_CONNECTION so the demo's probe waits its ssh deadline (1 s) and a slow CI runner cannot miss it (plan refinement D9 of M3d).

Usage:
    python3 scripts/pty-smoke/smoke.py [--probe none|legacy|kitty] [--exit q|ctrl-c|crash|sigterm] [--cols 80] [--rows 30]
                                       [--timeout 60] [--marker "stui M2c demo"] -- <command...>

Exits 0 when every check passes, 1 otherwise with a [FAIL] line per failed check.
"""

import argparse
import fcntl
import os
import pty
import re
import select
import signal
import struct
import sys
import termios
import time

FAILURES = []

# escape sequences stripped before text assertions (CSI / OSC / DCS-APC-PM-SOS / charset / other ESC forms)
STRIP = re.compile(
    rb"\x1b(?:\[[0-9;:?<>=]*[A-Za-z]|\][^\x07\x1b]*(?:\x07|\x1b\\)|[PX^_][^\x1b]*\x1b\\|[()][0-9A-Za-z]|.)"
)

DA1_QUERY = b"\x1b[c"
KITTY_QUERY = b"\x1b[?u"
ANSWERS = {"legacy": b"\x1b[?62;22c", "kitty": b"\x1b[?0u\x1b[?62;22c"}
EXPECTED_STATUS = {"q": 0, "ctrl-c": 0, "crash": 1, "sigterm": 143}
# a kitty push ends in `u`, which tells it apart from the probe's own `CSI > 0 q` (XTVERSION) and `CSI > c` (DA2)
PUSH = re.compile(rb"\x1b\[>[0-9]*u")
PUSH_ONE = b"\x1b[>1u"
POP = b"\x1b[<u"
ALTERNATE_ENTER = b"\x1b[?1049h"
ALTERNATE_EXIT = b"\x1b[?1049l"
REGION_RESET = b"\x1b7\x1b[r\x1b8"


def report(name, ok, detail=""):
    mark = "PASS" if ok else "FAIL"
    print(f"  [{mark}] {name}" + (f" :: {detail}" if (detail and not ok) else ""))
    if not ok:
        FAILURES.append(name)


def stripped(raw: bytes) -> str:
    return STRIP.sub(b"", raw).decode("utf-8", "replace")


def main():
    parser = argparse.ArgumentParser(description="stui demo PTY smoke")
    parser.add_argument("--probe", choices=["none", "legacy", "kitty"], default="none")
    parser.add_argument("--exit", choices=["q", "ctrl-c", "crash", "sigterm"], default="q")
    parser.add_argument("--cols", type=int, default=80)
    parser.add_argument("--rows", type=int, default=30)
    parser.add_argument("--timeout", type=float, default=60.0, help="seconds to wait for the marker and for the exit")
    parser.add_argument("--marker", default="stui M2c demo", help="text that must appear in the stripped output")
    parser.add_argument("command", nargs="+", help="the demo command, after --")
    args = parser.parse_args()

    print(f"smoke: probe={args.probe} exit={args.exit} :: {' '.join(args.command)}")

    env = dict(os.environ)
    env["LANG"] = "en_US.UTF-8"
    env["TERM"] = "xterm-256color"
    env.pop("LC_ALL", None)
    env.pop("LC_CTYPE", None)
    if args.probe != "none":
        env["SSH_CONNECTION"] = "pty-smoke 0 pty-smoke 0"

    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(args.command[0], args.command, env)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", args.rows, args.cols, 0, 0))

    buf = b""
    answered = False

    def pump(seconds):
        nonlocal buf, answered
        end = time.time() + seconds
        while time.time() < end:
            ready, _, _ = select.select([fd], [], [], 0.05)
            if ready:
                try:
                    chunk = os.read(fd, 65536)
                except OSError:
                    return
                if not chunk:
                    return
                buf += chunk
                # answer the probe the moment its DA1 sentinel query appears (DA2's `ESC [ > c` never matches), once
                if args.probe != "none" and not answered and DA1_QUERY in buf:
                    answered = True
                    try:
                        os.write(fd, ANSWERS[args.probe])
                    except OSError:
                        pass

    # 1. the demo draws its header
    deadline = time.time() + args.timeout
    while time.time() < deadline and args.marker not in stripped(buf):
        pump(0.2)
    report("the demo header appeared", args.marker in stripped(buf), f"marker {args.marker!r} not seen")

    # 2. the probe asks the kitty flags before its DA1 sentinel (the kitty keyboard protocol's own detection)
    asks = KITTY_QUERY in buf and DA1_QUERY in buf and buf.index(KITTY_QUERY) < buf.index(DA1_QUERY)
    report("the probe asks the kitty flags before DA1", asks, "no `CSI ? u` before `CSI c`")

    # 3. a key is accepted (a soft pump, layout is the local driver's business); a dead child makes the write fail with EIO
    def send(data):
        try:
            os.write(fd, data)
        except OSError:
            pass

    send(b"2")
    pump(0.5)

    # 4. the exit path
    if args.exit == "q":
        send(b"q")
    elif args.exit == "ctrl-c":
        send(b"\x1b[99;5u" if args.probe == "kitty" else b"\x03")
    elif args.exit == "crash":
        send(b"!")
    else:
        os.kill(pid, signal.SIGTERM)
    exit_code = None
    deadline = time.time() + args.timeout
    while time.time() < deadline:
        waited, status = os.waitpid(pid, os.WNOHANG)
        if waited:
            exit_code = os.waitstatus_to_exitcode(status)
            break
        pump(0.1)
    if exit_code is None:
        os.kill(pid, signal.SIGKILL)
        os.waitpid(pid, 0)
    pump(0.3)
    expected = EXPECTED_STATUS[args.exit]
    report(f"{args.exit} exited with status {expected}", exit_code == expected, f"exit code {exit_code}")

    # 5. the kitty keyboard balance: one push after the entry and one pop before the exit's reset, or none at all
    pushes = len(PUSH.findall(buf))
    pops = buf.count(POP)
    if args.probe == "kitty":
        balanced = pushes == 1 and PUSH_ONE in buf and pops == 1 and buf.index(PUSH_ONE) < buf.index(POP)
        report("exactly one kitty push and one pop, in that order", balanced, f"pushes {pushes}, pops {pops}")
        if ALTERNATE_ENTER in buf:
            placed = (
                balanced
                and buf.index(ALTERNATE_ENTER) < buf.index(PUSH_ONE)
                and ALTERNATE_EXIT in buf
                and buf.index(POP) < buf.rindex(ALTERNATE_EXIT)
            )
            report("the push follows the alternate-screen entry and the pop precedes its exit", placed)
        else:
            placed = balanced and REGION_RESET in buf and buf.index(POP) < buf.rindex(REGION_RESET)
            report("the pop precedes the inline exit's region reset", placed)
    else:
        report("no kitty push or pop without a flags answer", pushes == 0 and pops == 0, f"pushes {pushes}, pops {pops}")

    # 6. the terminal came back sane (the M0 post-exit tcgetattr-on-master technique)
    try:
        attributes = termios.tcgetattr(fd)
        lflag = attributes[3]
        sane = bool(lflag & termios.ECHO) and bool(lflag & termios.ICANON)
        report("ECHO and ICANON are restored", sane, f"lflag {lflag:#x}")
    except OSError as error:
        report("ECHO and ICANON are restored", False, f"tcgetattr failed: {error}")
    os.close(fd)

    if FAILURES:
        print(f"FAILED: {len(FAILURES)} check(s): {', '.join(FAILURES)}")
        print(f"--- output tail (escapes stripped, {len(buf)} raw bytes) ---")
        print(stripped(buf)[-3000:])
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
