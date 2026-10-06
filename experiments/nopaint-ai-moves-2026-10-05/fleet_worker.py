#!/usr/bin/env python3
"""One warm image model over an authenticated SSH stdin/stdout connection."""
import base64
import fcntl
import io
import json
import os
import sys
import tempfile
import threading
import uuid
from pathlib import Path
from PIL import Image
from move_control import MoveCancelled
from paint_region import decode_mask, save_mask

LIMIT = 1_000_000


def validate(message):
    request_id = str(uuid.UUID(message["id"]))
    if request_id != message["id"] or message.get("action") != "move":
        raise ValueError("Invalid move")
    if message.get("strength") not in (.25, .5, .75):
        raise ValueError("Invalid strength")
    seed = message.get("seed")
    if type(seed) is not int or not 0 <= seed < 2**31:
        raise ValueError("Invalid seed")
    if message.get("mask") is not None:
        decode_mask(message["mask"])
    raw = base64.b64decode(message["image"], validate=True)
    with Image.open(io.BytesIO(raw)) as image:
        if image.format != "PNG" or image.size != (256, 256) or image.mode != "RGB":
            raise ValueError("Expected full 256x256 RGB PNG")
        image.load()
    return raw


def serve(load, move, root, model, revision, source=None, sink=None):
    source, sink = source or sys.stdin, sink or sys.stdout
    lock, output_lock = threading.Lock(), threading.Lock()
    active = None
    pool = []

    def send(value):
        with output_lock:
            sink.write(json.dumps(value, separators=(",", ":"))+"\n")
            sink.flush()

    # Refuse a second process before loading another model on the same GPU.
    root.mkdir(parents=True, exist_ok=True)
    with (root / "worker.lock").open("a") as lease:
        try:
            fcntl.flock(lease, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            send({"type": "error", "error": "Fleet model is in use"})
            return
        pipe, embed = load("fleet")
        send({"type": "ready", "protocol": 2, "model": model, "revision": revision})

        def work(message, raw, cancel):
            nonlocal active
            request_id = message["id"]
            try:
                with tempfile.TemporaryDirectory(prefix="move-", dir=root) as folder:
                    folder = Path(folder)
                    before = folder / "input.png"
                    before.write_bytes(raw)
                    mask = save_mask(message.get("mask"), folder / "masks")

                    def observe(frame):
                        # The existing generator records paths relative to its source root.
                        image = (root.parent.parent / frame["image"]).read_bytes()
                        send({"type": "preview", "id": request_id,
                              "frame": {k: v for k, v in frame.items() if k != "image"},
                              "image": base64.b64encode(image).decode()})

                    output = move(pipe, embed, before, folder / "proposals", 1,
                                  message["strength"], message["seed"], observe=observe, cancel=cancel,
                                  **({"mask": mask} if mask else {}))
                    receipt = json.loads(output.with_suffix(".json").read_text())
                    result = {"type": "result", "id": request_id,
                              "image": base64.b64encode(output.read_bytes()).decode(),
                              "receipt": {k: receipt[k] for k in ("identity", "total_seconds", "actual_denoise_steps", "output_sha256")}}
            except MoveCancelled:
                result = {"type": "cancelled", "id": request_id}
            except Exception:
                # Do not return local paths, input pixels, or traceback details.
                result = {"type": "error", "id": request_id, "error": "Fleet image generation failed"}
                import traceback
                traceback.print_exc(file=sys.stderr)
            with lock:
                active = None
            send(result)

        try:
            while True:
                line = source.readline(LIMIT+1)
                if not line:
                    break
                if len(line) > LIMIT:
                    raise ValueError("Request too large")
                message = json.loads(line)
                if message.get("action") == "cancel":
                    with lock:
                        if active and active[0] == message.get("id"):
                            active[1].set()
                    continue
                raw = validate(message)
                with lock:
                    if active:
                        send({"type": "error", "id": message["id"], "error": "Fleet model is in use"})
                        continue
                    cancel = threading.Event()
                    active = (message["id"], cancel)
                thread = threading.Thread(target=work, args=(message, raw, cancel))
                pool = [t for t in pool if t.is_alive()] + [thread]
                thread.start()
        finally:
            with lock:
                if active:
                    active[1].set()
            for thread in pool:
                thread.join()


if __name__ == "__main__":
    os.environ["HF_HUB_OFFLINE"] = "1"
    os.environ["TRANSFORMERS_OFFLINE"] = "1"
    protocol = sys.stdout
    sys.stdout = sys.stderr  # Library/model logging must never enter the protocol.
    from evolve_run import load, move, MODEL, REVISION
    from local_run import HERE
    serve(load, move, HERE / "local" / "fleet", MODEL, REVISION, sink=protocol)
