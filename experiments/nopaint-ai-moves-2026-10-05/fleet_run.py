"""Full-image moves and genuine previews from one private fleet worker."""
import base64
import io
import json
import os
from pathlib import Path
import queue
import shlex
import subprocess
import threading
import time
import uuid
from PIL import Image
from local_run import HERE, sha, write
from move_control import MoveCancelled, check_cancel
from paint_region import encode_mask

MODEL = "stable-diffusion-v1-5/stable-diffusion-v1-5"
REVISION = "451f4fe16113bff5a5d2269ed5ad43b0592e9a14"
OFFLINE_UNTIL = 0


def configuration():
    path = Path(os.environ.get("NOPAINT_FLEET_CONFIG", HERE / "local/fleet-worker.json"))
    try:
        value = json.loads(path.read_text())
        return value if value.get("enabled") else None
    except (OSError, ValueError):
        return None


def available():
    return bool(configuration()) and time.monotonic() >= OFFLINE_UNTIL


def decode(value, path):
    raw = base64.b64decode(value, validate=True)
    with Image.open(io.BytesIO(raw)) as image:
        if image.format != "PNG" or image.size != (256, 256) or image.mode != "RGB":
            raise ValueError("Fleet returned an invalid image")
        image.load()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(raw)


class Worker:
    def __init__(self, command=None):
        self.command = command
        self.process = None
        self.messages = None
        self.log = None

    def start(self, cancel=None):
        if self.process and self.process.poll() is None:
            return
        self.close()
        config = configuration()
        if not self.command and not config:
            raise RuntimeError("Fleet model is not configured")
        command = self.command
        if command is None:
            root = config["root"]
            remote = "exec nice -n 10 " + shlex.join([root+"/.venv/bin/python", "-u", root+"/fleet_worker.py"])
            command = ["ssh", "-T", "-i", str(Path(config["key"]).expanduser()),
                       "-o", "BatchMode=yes", "-o", "ConnectTimeout=8",
                       "-o", "ServerAliveInterval=10", "-o", "ServerAliveCountMax=2",
                       config["target"], remote]
        self.messages = queue.Queue(maxsize=32)
        messages = self.messages
        log_path = HERE / "local/fleet-worker.log"
        log_path.parent.mkdir(parents=True, exist_ok=True)
        self.log = log_path.open("a")
        self.process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        stderr=self.log, text=True, bufsize=1)
        output = self.process.stdout

        def read():
            try:
                while line := output.readline(1_000_001):
                    if len(line) > 1_000_000:
                        raise ValueError("Fleet response too large")
                    messages.put(json.loads(line), timeout=5)
            except Exception:
                pass
            finally:
                try:
                    messages.put({"type": "disconnected"}, timeout=5)
                except queue.Full:
                    pass
        threading.Thread(target=read, daemon=True).start()
        try:
            deadline = time.monotonic()+90
            while True:
                check_cancel(cancel)
                if time.monotonic() >= deadline:
                    raise TimeoutError("Fleet startup timed out")
                try:
                    ready = messages.get(timeout=.1)
                    break
                except queue.Empty:
                    pass
            if ready != {"type": "ready", "protocol": 2, "model": MODEL, "revision": REVISION}:
                raise RuntimeError("Fleet worker is unavailable or uses a different model")
        except Exception:
            self.close()
            raise

    def send(self, value):
        self.process.stdin.write(json.dumps(value, separators=(",", ":"))+"\n")
        self.process.stdin.flush()

    def close(self):
        if self.process:
            process, self.process = self.process, None
            try:
                process.stdin.close()  # EOF cancels the worker and releases its GPU lease.
            except BrokenPipeError:
                pass
            if process.poll() is None:
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.terminate()
                    try:
                        process.wait(timeout=3)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()
            process.stdout.close()
        if self.log:
            self.log.close()
            self.log = None


def connect():
    # Delay SSH/model startup until move() so it is measured and cancellable.
    return Worker(), None


def move(worker, embed, before, folder, step, strength, seed, cold=False, observe=None, cancel=None, mask=None):
    global OFFLINE_UNTIL
    check_cancel(cancel)
    started = time.monotonic()
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    if output.exists() or receipt.exists():
        raise ValueError("Move already exists")
    request_id = str(uuid.uuid4())
    row = {"status": "running", "request_id": request_id, "location": "Poorslice · fleet",
           "identity": {"model": MODEL, "revision": REVISION, "input_sha256": sha(before),
                        "seed": seed, "strength": strength, "size": [256, 256]}}
    if mask:
        row["identity"]["mask_sha256"] = sha(mask)
    write(receipt, row)
    cancelled_at = None
    try:
        worker.start(cancel)
        check_cancel(cancel)
        worker.send({"action": "move", "id": request_id, "strength": strength,
                     "seed": seed, "image": base64.b64encode(before.read_bytes()).decode(),
                     **({"mask": encode_mask(mask)} if mask else {})})
        while True:
            now = time.monotonic()
            if cancel and cancel.is_set() and cancelled_at is None:
                worker.send({"action": "cancel", "id": request_id})
                cancelled_at = now
            if now-started > 180 or (cancelled_at is not None and now-cancelled_at > 15):
                raise TimeoutError("Fleet move timed out")
            try:
                message = worker.messages.get(timeout=.1)
            except queue.Empty:
                continue
            if message["type"] == "disconnected":
                raise RuntimeError("Fleet worker disconnected")
            if message.get("id") != request_id:
                continue
            if message["type"] == "preview":
                if cancelled_at is not None:
                    continue
                frame = message["frame"]
                index = frame["index"]
                if type(index) is not int or not 0 <= index <= 32:
                    raise ValueError("Invalid fleet preview")
                path = folder / f"{step:03}-frames" / f"{index:03}.png"
                decode(message["image"], path)
                if observe:
                    # The game may reject between receipt and callback.
                    try:
                        observe({**frame, "image": str(path.relative_to(HERE))})
                    except MoveCancelled:
                        worker.send({"action": "cancel", "id": request_id})
                        cancelled_at = time.monotonic()
            elif message["type"] in ("result", "cancelled", "error"):
                if cancelled_at is not None or message["type"] == "cancelled":
                    raise MoveCancelled("Move rejected")
                check_cancel(cancel)
                if message["type"] == "error":
                    raise RuntimeError(message.get("error", "Fleet generation failed"))
                remote = message["receipt"]
                identity = remote["identity"]
                if any(identity.get(k) != v for k, v in row["identity"].items()):
                    raise ValueError("Fleet returned a result for different input or settings")
                decode(message["image"], output)
                if sha(output) != remote["output_sha256"]:
                    raise ValueError("Fleet output checksum mismatch")
                row.update(status="complete", seconds=time.monotonic()-started,
                           output_sha256=sha(output), remote=remote)
                write(receipt, row)
                return output
    except MoveCancelled:
        row.update(status="rejected", seconds=time.monotonic()-started)
        write(receipt, row)
        raise
    except Exception:
        worker.close()
        OFFLINE_UNTIL = time.monotonic()+60
        row.update(status="failed", seconds=time.monotonic()-started)
        write(receipt, row)
        raise RuntimeError("Poorslice is unavailable. Local models remain ready.") from None
