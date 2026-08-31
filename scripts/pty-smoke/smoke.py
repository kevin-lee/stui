#!/usr/bin/env python3
"""The M2c CI demo smoke (issue 23): runs a stui demo command under a fresh pseudo-terminal, waits for the demo's header
marker, presses a key, quits with `q`, and asserts a clean exit with sane terminal modes afterwards.

Clean-exit only by design: the four-exit-path restore matrix (crash, Ctrl-C byte, SIGTERM, both screen modes with the probe
answered) is the local PTY driver's job for M2c and the in-repo harness's job in M4. This smoke is the seed that harness
grows from. The probe is deliberately never answered, so the fail-open path is what CI exercises.

Usage:
    python3 scripts/pty-smoke/smoke.py [--cols 80] [--rows 30] [--timeout 60] [--marker "stui M2c demo"] -- <command...>

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


def report(name, ok, detail=""):
    mark = "PASS" if ok else "FAIL"
    print(f"  [{mark}] {name}" + (f" :: {detail}" if (detail and not ok) else ""))
    if not ok:
        FAILURES.append(name)


def stripped(raw: bytes) -> str:
    return STRIP.sub(b"", raw).decode("utf-8", "replace")


def main():
    parser = argparse.ArgumentParser(description="stui demo PTY smoke")
    parser.add_argument("--cols", type=int, default=80)
    parser.add_argument("--rows", type=int, default=30)
    parser.add_argument("--timeout", type=float, default=60.0, help="seconds to wait for the marker and for the exit")
    parser.add_argument("--marker", default="stui M2c demo", help="text that must appear in the stripped output")
    parser.add_argument("command", nargs="+", help="the demo command, after --")
    args = parser.parse_args()

    env = dict(os.environ)
    env["LANG"] = "en_US.UTF-8"
    env["TERM"] = "xterm-256color"
    env.pop("LC_ALL", None)
    env.pop("LC_CTYPE", None)

    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(args.command[0], args.command, env)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", args.rows, args.cols, 0, 0))

    buf = b""

    def pump(seconds):
        nonlocal buf
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

    # 1. the demo draws its header (the probe is never answered: the fail-open deadline passes first)
    deadline = time.time() + args.timeout
    while time.time() < deadline and args.marker not in stripped(buf):
        pump(0.2)
    report("the demo header appeared", args.marker in stripped(buf), f"marker {args.marker!r} not seen")

    # 2. a key is accepted (a soft pump, layout is the local driver's business)
    os.write(fd, b"2")
    pump(0.5)

    # 3. q quits cleanly
    os.write(fd, b"q")
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
    report("q exited with status 0", exit_code == 0, f"exit code {exit_code}")

    # 4. the terminal came back sane (the M0 post-exit tcgetattr-on-master technique)
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
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
