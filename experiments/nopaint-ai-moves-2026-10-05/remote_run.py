"""Opt-in fal queue adapter. No calls unless both server flags are present."""
import base64
import io
import json
import os
import threading
import time
from urllib.error import HTTPError
from urllib.parse import urlsplit
from urllib.request import Request, HTTPRedirectHandler, build_opener

from PIL import Image
from engines import ENGINES, available
from local_run import HERE, sha, write
from move_control import MoveCancelled, check_cancel

MODEL = ENGINES["fal-klein"]["model"]
PROMPT = ("Make exactly one {amount} abstract painting move on this image: "
          "change a texture, color relationship, shape, or spatial arrangement. "
          "Preserve most of the existing image. Return the complete updated image. "
          "Do not add text, borders, or a depicted scene.")


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def request(url, method="GET", payload=None, key=None):
    target = urlsplit(url)
    if target.scheme != "https" or target.username or target.password or target.port not in (None, 443):
        raise ValueError("Unexpected fal URL")
    if key and target.hostname != "queue.fal.run":
        raise ValueError("Credentials are restricted to the fal queue")
    if not key and not (target.hostname == "fal.media" or (target.hostname or "").endswith(".fal.media")):
        raise ValueError("Unexpected image download host")
    headers = {"Content-Type": "application/json"}
    if key:
        headers["Authorization"] = "Key " + key
    data = json.dumps(payload).encode() if payload is not None else None
    try:
        with build_opener(NoRedirect).open(Request(url, data=data, headers=headers, method=method), timeout=15) as response:
            body = response.read(16 * 1024 * 1024 + 1)
            if len(body) > 16 * 1024 * 1024:
                raise ValueError("Remote image response too large")
            return json.loads(body) if key else body
    except HTTPError as error:
        # Avoid echoing private input, signed output URLs, or credentials.
        raise RuntimeError(f"Fal HTTP {error.code}") from None


def move(pipe, embed, before, folder, step, strength, seed, cold=False, observe=None,
         cancel=None, transport=None, poll_seconds=.5):
    if not available("fal-klein"):
        raise ValueError("Fal is disabled on this server")
    transport = transport or request
    cancel = cancel if cancel is not None else threading.Event()
    check_cancel(cancel)
    key = os.environ["FAL_KEY"]
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    if output.exists() or receipt.exists():
        raise ValueError("Move already exists")
    source = Image.open(before).convert("RGB")
    if source.size != (256, 256):
        raise ValueError("Expected full 256x256 input")
    amount = {.25: "small", .5: "medium", .75: "large"}[strength]
    payload = {"prompt": PROMPT.format(amount=amount), "seed": seed,
               "num_inference_steps": 4, "num_images": 1, "output_format": "png",
               "sync_mode": True, "enable_safety_checker": True,
               "image_size": {"width": 256, "height": 256},
               "image_urls": ["data:image/png;base64," + base64.b64encode(before.read_bytes()).decode()]}
    started = time.monotonic()
    row = {"status": "running", "step": step,
           "input": str(before.relative_to(HERE)), "output": str(output.relative_to(HERE)),
           "identity": {"model": MODEL, "input_sha256": sha(before), "seed": seed,
                        "strength": strength, "prompt": payload["prompt"], "size": [256, 256]},
           "previews": False}
    write(receipt, row)
    handle = None
    try:
        check_cancel(cancel)
        # No client retries for submission: an ambiguous timeout may already be billed.
        handle = transport("https://queue.fal.run/" + MODEL, "POST", payload, key)
        row["request_id"] = handle["request_id"]
        write(receipt, row)
        while True:
            check_cancel(cancel)
            if time.monotonic()-started > 180:
                raise TimeoutError("Fal move exceeded three minutes")
            status = transport(handle["status_url"], key=key)
            check_cancel(cancel)
            if status["status"] == "COMPLETED":
                if status.get("error"):
                    raise RuntimeError("Fal could not complete this move")
                break
            if status["status"] not in ("IN_QUEUE", "IN_PROGRESS"):
                raise RuntimeError("Unexpected fal request status")
            cancel.wait(poll_seconds)
        result = transport(handle["response_url"], key=key)
        check_cancel(cancel)
        url = result["images"][0]["url"]
        if url.startswith("data:image/") and ";base64," in url:
            raw = base64.b64decode(url.split(",", 1)[1], validate=True)
        else:
            raw = transport(url)
        check_cancel(cancel)
        image = Image.open(io.BytesIO(raw)).convert("RGB")
        native_size = list(image.size)
        if image.size != (256, 256):
            image = image.resize((256, 256), Image.Resampling.LANCZOS)
        image.save(output)
        check_cancel(cancel)
        row.update(status="complete", total_seconds=time.monotonic()-started,
                   native_size=native_size, returned_size=[256, 256], output_sha256=sha(output))
        write(receipt, row)
        return output
    except Exception as error:
        row.update(status="cancelled" if isinstance(error, MoveCancelled) else "failed",
                   seconds=time.monotonic()-started, error=str(error)[:300])
        if handle:
            try:
                transport(handle["cancel_url"], "PUT", key=key)
                row["cancellation_requested"] = True
            except Exception:
                row["cancellation_requested"] = False
        # A running remote app can still finish and charge after cancellation.
        write(receipt, row)
        raise
