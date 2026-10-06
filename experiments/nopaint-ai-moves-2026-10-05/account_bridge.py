"""One persistent, private AC client per local No Paint server."""
import atexit
import json
import queue
import subprocess
import threading
import time
import uuid
from pathlib import Path

from move_control import check_cancel


class AccountError(RuntimeError):
    def __init__(self, message, code=None):
        super().__init__(message)
        self.code = code


class AccountBridge:
    def __init__(self, command=None):
        self.command = command or ["node", str(Path(__file__).with_name("ac_account.mjs")), "--serve"]
        self.lock = threading.RLock()
        self.process = None
        self.pending = {}

    def _failed(self, process):
        with self.lock:
            if self.process is process:
                self.process = None
            for key, (owner, inbox) in list(self.pending.items()):
                if owner is process:
                    del self.pending[key]
                    inbox.put({"error": "AC connection interrupted. Retrying…", "code": "offline"})

    def _read(self, process):
        try:
            for line in process.stdout:
                response = json.loads(line)
                result = response["result"]
                if not isinstance(result, dict):
                    raise ValueError("Invalid account response")
                with self.lock:
                    waiting = self.pending.pop(response.get("id"), None)
                if waiting and waiting[0] is process:
                    waiting[1].put(result)
        except (ValueError, KeyError, OSError):
            pass
        finally:
            self._failed(process)
            if process.poll() is None:
                process.terminate()
            process.wait()
            process.stdout.close()
            process.stdin.close()

    def call(self, data, timeout=90, cancel=None):
        check_cancel(cancel)
        key, inbox = uuid.uuid4().hex, queue.Queue(maxsize=1)
        with self.lock:
            if self.process is None or self.process.poll() is not None:
                self.process = subprocess.Popen(self.command, stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True, bufsize=1)
                threading.Thread(target=self._read, args=(self.process,), daemon=True).start()
            process = self.process
            self.pending[key] = (process, inbox)
            try:
                process.stdin.write(json.dumps({"id": key, "input": data})+"\n")
                process.stdin.flush()
            except (BrokenPipeError, OSError):
                self._failed(process)
        deadline = time.monotonic()+timeout
        try:
            while True:
                check_cancel(cancel)
                remaining = deadline-time.monotonic()
                if remaining <= 0:
                    raise AccountError("AC connection timed out. Retrying…", "offline")
                try:
                    result = inbox.get(timeout=min(.15, remaining))
                except queue.Empty:
                    continue
                if result.get("error"):
                    raise AccountError(result["error"], result.get("code"))
                return result
        finally:
            # A cancelled move may still settle on AC. Discard its late reply;
            # keep other requests connected and never replay the paid request.
            with self.lock:
                self.pending.pop(key, None)

    def close(self):
        with self.lock:
            process = self.process
            if process is None:
                return
            self._failed(process)
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


client = AccountBridge()
atexit.register(client.close)
