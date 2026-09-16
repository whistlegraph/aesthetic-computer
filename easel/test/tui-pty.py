"""Real PTY checks with offline engine fixtures; run with python3 test/tui-pty.py."""
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import shutil
import struct
import subprocess
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory(prefix="easel-pty-") as temporary:
    folder = Path(temporary)
    piece = folder / "existing.mjs"
    piece_source = "export function paint({wipe}) { wipe(70,50,100); }\n"
    piece.write_text(piece_source)
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 100, 0, 0))
    fake_bin = folder / "bin"
    fake_bin.mkdir()
    opener = fake_bin / "open"
    opener.write_text('#!/bin/sh\nprintf "%s" "$1" > "$EASEL_BROWSER_LOG"\n')
    opener.chmod(0o755)
    env = dict(os.environ, TERM="xterm-256color", NO_COLOR="1",
               SLAB_HOME=str(folder / "slab"), EASEL_TEST_LOG=str(folder / "engines.jsonl"),
               EASEL_HISTORY_DIR=str(folder / "history"),
               EASEL_BROWSER_LOG=str(folder / "browser.txt"), PATH=str(fake_bin) + ":" + os.environ["PATH"])
    child = subprocess.Popen([shutil.which("node"), "--import", str(ROOT / "test/tui-fixture.mjs"),
                              str(ROOT / "src/tui.mjs"), "--cwd", str(folder), "--piece", str(piece), "--backend", "ac", "--no-autopublish"],
                             stdin=slave, stdout=slave, stderr=slave, env=env)
    os.close(slave)
    output = bytearray()
    def read_for(seconds=0.15):
        until = time.monotonic() + seconds
        while time.monotonic() < until:
            if select.select([master], [], [], max(0, until-time.monotonic()))[0]:
                try:
                    data = os.read(master, 65536)
                except OSError:
                    break
                if not data:
                    break
                output.extend(data)
        return output.decode("utf-8", errors="replace")
    def send(text):
        offset = len(output)
        os.write(master, text.encode())
        read_for(0.3)
        return output[offset:].decode("utf-8", errors="replace")
    try:
        read_for(0.8)
        send("\x1b")  # dismiss splash
        assert "make a piece" in send("/about\r")
        send("\x1b")
        assert "make a piece" in send("\x1b[<0;3;21M")  # click EASEL
        send("\x1b")
        profile_result = send("\x1b[<0;10;21M")  # click @tester
        deadline = time.monotonic() + 2
        while not (folder / "browser.txt").exists() and time.monotonic() < deadline:
            read_for(0.05)
        assert (folder / "browser.txt").exists(), profile_result[-5000:]
        assert (folder / "browser.txt").read_text() == "https://aesthetic.computer/@tester"
        send("remember cobalt dots\r")
        result = send("/backend codex\r")
        assert "current piece and recent conversation carried over" in result
        assert "STALE_CALLBACK_BUG" not in result
        records = [json.loads(line) for line in (folder / "engines.jsonl").read_text().splitlines()]
        assert "remember cobalt dots" in records[-1]["context"]
        result = send("/model broken\r")
        assert "Fixture switch failed" in result
        assert "STALE_CALLBACK_BUG" not in result
        assert "I remember still here" in send("still here\r")
        # A metered turn puts the electricity estimate on the gauge row, and
        # /energy prints the working: the same tokens across every model.
        assert "Wh" in read_for(0.2)
        energy = send("/energy\r")
        assert "Same conversation, other models" in energy
        assert "Estimated from active parameters" in energy
        offset = len(output)
        send("/performance 10\r")
        read_for(1.5)
        assert "Excludes browser rendering" in output[offset:].decode("utf-8", errors="replace")
        for index in range(24):
            send(f"line {index}\r")
        result = send("\x1b[5~")
        assert "lines above" in result and "AESEL" in result
        result = send("\x1b[F")
        assert "line 23" in result
        send("/mouse off\r")
        assert b"\x1b[?1003l" in output
        send("/quit\r")
        child.wait(timeout=5)
        assert child.returncode == 0, child.returncode
        assert piece.read_text() == piece_source
        print("PTY passed: about/profile clicks, engine handoff/recovery, stale callbacks, energy meter, internal scroll, benchmark, existing piece preservation, mouse cleanup")
    finally:
        if child.poll() is None:
            child.terminate()
            child.wait(timeout=5)
        os.close(master)
