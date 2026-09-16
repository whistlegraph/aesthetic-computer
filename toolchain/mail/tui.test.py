"""Real-terminal smoke test using synthetic letters only (macOS/Linux)."""
import fcntl
import os
from pathlib import Path
import pty
import select
import signal
import struct
import subprocess
import termios
import time
import unittest


class TerminalTest(unittest.TestCase):
    def test_read_reply_review_send_resize_and_exit(self):
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
        child = subprocess.Popen(
            ["node", str(Path(__file__).with_name("ac-mail.mjs")), "--demo"],
            stdin=slave, stdout=slave, stderr=slave,
        )
        os.close(slave)

        def screen(needle):
            data = b""
            deadline = time.monotonic() + 5
            while time.monotonic() < deadline:
                if select.select([master], [], [], 0.1)[0]:
                    try:
                        data += os.read(master, 65536)
                    except OSError:
                        break
                    if needle.encode() in data:
                        return data
            self.fail(f"Terminal did not display {needle!r}")

        try:
            screen("@neighbor")
            os.write(master, b"\r")
            screen("0 unread")
            os.write(master, b"c")
            screen("re: Hello")
            os.write(master, b"terminal hello\x13")
            screen("y send")
            # Merely reviewing does not send. Resize while reviewing, then
            # return to the draft and confirm its body survives.
            fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", 12, 36, 0, 0))
            child.send_signal(signal.SIGWINCH)
            screen("Send to @friend?")
            os.write(master, b"\x1b")
            screen("terminal hello")
            fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
            child.send_signal(signal.SIGWINCH)
            screen("Ctrl+S review")
            os.write(master, b"\x13")
            screen("y send")
            os.write(master, b"y")
            screen("Demo letter sent. No real delivery.")
            os.write(master, b"q")
            screen("\x1b[?1049l")
            self.assertEqual(child.wait(timeout=3), 0)
        finally:
            if child.poll() is None:
                child.terminate()
                child.wait(timeout=3)
            os.close(master)


if __name__ == "__main__":
    unittest.main()
