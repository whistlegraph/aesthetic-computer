"""Handle-linked AC remote moves, with no browser-visible tokens or fal key."""
import base64
import io
import json
import threading
import time
import uuid

from PIL import Image
from local_run import HERE, sha, write
from move_control import MoveCancelled, check_cancel
from account_bridge import AccountError, client


def bridge(data, timeout=90, cancel=None):
    return client.call(data, timeout=timeout, cancel=cancel)


class Account:
    def __init__(self):
        self.lock = threading.Lock()
        self.value = {"connected": False}
        self.working = False
        self.epoch = 0

    def snapshot(self):
        with self.lock:
            return {**self.value, "working": self.working}

    def update(self, login=False):
        with self.lock:
            if self.working or (not login and not (self.value.get("connected") or self.value.get("reconnecting"))):
                return
            self.working = True
            epoch = self.epoch
            previous = self.value.copy()

        def work():
            try:
                result = bridge({"action": "login" if login else "status"}, timeout=330 if login else 90)
            except Exception as error:
                offline = getattr(error, "code", None) == "offline"
                result = ({**previous, "stale": True, "reconnecting": True} if offline else {"connected": False})
                result.update({"error": str(error)[:200], "error_code": getattr(error, "code", None)})
            with self.lock:
                if epoch == self.epoch:
                    self.value = result
                    self.working = False
        threading.Thread(target=work, daemon=True).start()

    def disconnect(self):
        # Disconnect only this game; other AC apps keep their shared sign-in.
        with self.lock:
            self.epoch += 1
            self.working = False
            self.value = {"connected": False}


def move(pipe, embed, before, folder, step, strength, seed, cold=False, observe=None,
         cancel=None, account=None, identity=None, engine="ac-klein"):
    identity = identity or account.snapshot()
    offer = next((item for item in identity.get("models", []) if item["id"] == engine), None) or (identity.get("remote") if engine == "ac-klein" else None) or {}
    if not identity.get("connected") or not offer.get("available"):
        raise ValueError("Sign in with AC and choose an available remote model")
    check_cancel(cancel)
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    if receipt.exists() or output.exists():
        raise ValueError("Move already exists")
    request_id = str(uuid.uuid4())
    row = {"status": "running", "request_id": request_id, "handle": identity["handle"],
           "quoted_braincells": offer["braincells"],
           "identity": {"model": offer["model"], "input_sha256": sha(before),
                        "seed": seed, "strength": strength, "quote": offer["quote"], "size": [256, 256]}}
    write(receipt, row)
    started = time.monotonic()
    request = {"action": "move", "before": str(before), "requestId": request_id,
                          "seed": seed, "strength": strength, "quote": offer["quote"],
                          "account_id": identity["account_id"], "model": engine}
    try:
        result = bridge(request, timeout=260, cancel=cancel)
        check_cancel(cancel)
        image = Image.open(io.BytesIO(base64.b64decode(result["image"], validate=True))).convert("RGB")
        if image.size != (256, 256):
            raise RuntimeError("AC returned an invalid canvas size")
        image.save(output)
        check_cancel(cancel)
        row.update(status="complete", seconds=time.monotonic()-started, billing=result["billing"], output_sha256=sha(output),
                   prompt=result.get("prompt"), prompt_version=result.get("prompt_version"),
                   provider_cost_usd=result.get("provider_cost_usd"))
        write(receipt, row)
        return output
    except Exception as error:
        row.update(status="rejected" if isinstance(error, MoveCancelled) else "failed",
                   seconds=time.monotonic()-started, error=str(error)[:200],
                   billing_pending=True)
        write(receipt, row)
        raise
    finally:
        account.update()
