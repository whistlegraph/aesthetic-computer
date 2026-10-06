#!/usr/bin/env python3
"""Loopback-only No/Paint game with real multi-step image previews."""
import argparse
import concurrent.futures
import datetime
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import secrets
import statistics
import threading
import time
import uuid
from urllib.parse import urlsplit, parse_qs
from PIL import Image

os.environ["HF_HUB_OFFLINE"] = "1"
os.environ["TRANSFORMERS_OFFLINE"] = "1"

from local_run import HERE, sha, write
from engines import ENGINES, available, catalog, remote_offer, register_image_models
from move_control import MoveCancelled, check_cancel
from ac_run import Account, bridge
from openrouter_catalog import ModelBrowser
from paint_region import Region, save_mask, encode_mask
from painting_tracks import PaintingTracks

MODEL_BROWSER = ModelBrowser()


class Conflict(Exception):
    pass


class Game:
    def __init__(self, generator=None, resume=None, engine=None, verify_engine=False, account=None):
        stamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S") + "-" + secrets.token_hex(3)
        self.folder = Path(resume).resolve() if resume else HERE / "local" / "play" / stamp
        self.folder.mkdir(parents=True, exist_ok=bool(resume))
        self.lock = threading.RLock()
        self.pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        self.generator = generator
        self.account = account or Account()
        self.selection = engine or "random"
        self.engine = engine if engine and engine != "random" else "evolve"
        self.cost = {"braincells": 0, "status": "free"}
        self.loaded_engine = None
        self.job = None
        self.quote = None
        self.quote_identity = None
        self.worker_running = False
        self.verify_engine = verify_engine
        self.move = None
        self.pipe = self.embed = None
        self.ready = False
        self.busy = True
        self.error = None
        self.revision = 0
        self.candidate = None
        self.history = [] if resume else [self.fresh_noise()]
        self.mask = self.mask_bits = None
        self.publication = {"busy": False}
        self.upscale = {"busy": False}
        self.pending_done = None
        self.images = {}
        self.attempt = 0
        self.strength = .25
        self.events = []
        self.trace_id = None
        self.frames = []
        self.trace_verified = None
        self.started = time.monotonic()
        if resume:
            saved = json.loads((self.folder / "session.json").read_text())
            self.history = [HERE / p for p in saved["history"]]
            self.mask = HERE / saved["mask"] if saved.get("mask") else None
            self.mask_bits = encode_mask(self.mask) if self.mask else None
            self.events = saved["events"]
            self.publication = {**saved.get("publication", {}), "busy": False}
            self.pending_done = saved.get("pending_done")
            last = self.events[-1] if self.events else {}
            self.revision = saved.get("revision", last.get("revision", 0))+1
            self.candidate = saved.get("candidate", last.get("candidate") if last.get("action") == "proposal" else None)
            self.selection = engine or saved.get("selection", saved.get("engine", "random"))
            self.engine = engine if engine and engine != "random" else saved.get("engine", "evolve")
            self.cost = saved.get("cost", self.cost)
            if self.engine not in ENGINES:
                ENGINES[self.engine] = saved.get("engine_info") or (self.candidate or {}).get("model") or {"name": self.engine, "location": "AC cloud", "model": self.engine, "previews": False}
            self.strength = saved.get("strength", (self.candidate or {}).get("strength", .25))
            self.attempt = max([saved.get("attempt", 0)] + [int(p.stem) for p in (self.folder / "proposals").glob("*.*") if p.stem.isdigit()])
            if self.candidate:
                receipt = self.folder / "proposals" / f"{self.candidate['id']:03}.json"
                identity = json.loads(receipt.read_text()).get("identity", {}) if receipt.exists() else {}
                previous_engine = next((key for key, value in ENGINES.items() if value["model"] == identity.get("model")), self.engine)
                self.candidate.setdefault("engine", previous_engine)
                self.candidate.setdefault("model", ENGINES[previous_engine])
                self.cost = self.settled_cost(self.candidate["engine"], self.candidate["id"])
                self.candidate["cost"] = self.cost
            for p in [*self.history, *self.folder.rglob("*.png")]:
                self.image_url(p)
        self.tracks = PaintingTracks(self.folder, self.events)
        self.pool.submit(self.initialize)

    def fresh_noise(self):
        path = self.folder / "starts" / secrets.token_hex(8) / "noise.png"
        path.parent.mkdir(parents=True, exist_ok=True)
        Image.frombytes("RGB", (256, 256), secrets.token_bytes(256*256*3)).save(path)
        return path

    def image_url(self, path):
        key = sha(path)
        self.images[key] = path
        return "/image/" + key + ".png"

    def state(self):
        with self.lock:
            account = self.account.snapshot()
            if self.ready and not self.busy and self.quote and not available(self.quote["engine"], account):
                self.prepare_quote()
                self.revision += 1
                self.save("availability")
            if self.ready and not self.busy and not self.error and self.quote is None and available(self.selection, account):
                self.prepare_quote()
                self.revision += 1
                self.save("availability")
            return {"revision": self.revision, "ready": self.ready, "busy": self.busy,
                    "error": self.error, "accepted": self.image_url(self.history[-1]),
                    "mask": self.mask_bits,
                    "publication": {**self.publication, "pending": bool(self.pending_done)},
                    "upscale": {**self.upscale, "elapsed": round(time.monotonic() - self.upscale_started, 1) if self.upscale.get("busy") else self.upscale.get("seconds")},
                    "accepted_count": len(self.history)-1, "can_undo": len(self.history)>1,
                    "candidate": self.candidate, "strength": self.strength, "quote": self.quote,
                    "before": self.image_url(self.job["before"] if self.busy and self.job else self.history[max(0, len(self.history)-2)]),
                    "elapsed": round(time.monotonic()-self.started, 1) if self.busy else None,
                    "engine": self.engine, "selection": self.selection,
                    "models": catalog(account), "cost": self.cost,
                    "account": {key: value for key, value in account.items() if key != "account_id"},
                    "model": ENGINES[self.engine],
                    "generation": self.job["number"] if self.job else (self.candidate or {}).get("id"),
                    "can_reject": bool(not self.publication["busy"] and self.ready and not self.pending_done and (len(self.history)>1 or self.busy)),
                    "trace": {"id": self.trace_id, "count": len(self.frames),
                              "latest": self.frames[-1] if self.frames else None,
                              "verified": self.trace_verified}}

    def observe(self, frame, job=None):
        with self.lock:
            if job is not None:
                check_cancel(job["cancel"])
                if self.job is not job:
                    raise MoveCancelled("Stale preview")
            if frame.get("image"):
                frame = {**frame, "url": self.image_url(HERE / frame["image"])}
            self.frames.append(frame)
            self.tracks.frame(job["number"] if job else None, frame)

    def trace(self):
        with self.lock:
            return {"id": self.trace_id, "frames": list(self.frames), "verified": self.trace_verified}

    def save_trace(self):
        write(self.folder / "traces" / f"{self.trace_id}.json", self.trace())

    def save(self, action, **detail):
        before = self.events[-1].get("accepted") if self.events else str(self.history[-1].relative_to(HERE))
        number = detail.get("rejected_generation") or (self.job or {}).get("number")
        self.events.append({"action": action, "revision": self.revision,
                            "at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
                            "accepted": str(self.history[-1].relative_to(HERE)),
                            "candidate": self.candidate, "engine": self.engine,
                            "selection": self.selection, "cost": self.cost,
                            "generation": self.job["number"] if self.job else None,
                            "before": before, "strength": self.strength, "quote": self.quote,
                            "mask": str(self.mask.relative_to(HERE)) if self.mask else None,
                            "error": self.error,
                            "move": {key: self.job[key] for key in ("number", "engine", "strength", "seed")} if self.job else None,
                            "receipt": str((self.folder / "proposals" / f"{number:03}.json").relative_to(HERE)) if number else None,
                            "trace": str((self.folder / "traces" / f"{number:03}.json").relative_to(HERE)) if number else None,
                            "publication": self.publication if action == "done" else None, **detail})
        self.tracks.record(self.events[-1])
        # Replace atomically so a restart cannot read a half-written session.
        temporary = self.folder / "session.tmp"
        write(temporary, {"history": [str(p.relative_to(HERE)) for p in self.history],
                          "mask": str(self.mask.relative_to(HERE)) if self.mask else None,
                          "publication": self.publication, "pending_done": self.pending_done,
                          "candidate": self.candidate, "revision": self.revision,
                          "strength": self.strength, "selection": self.selection, "cost": self.cost,
                          "engine": self.engine, "engine_info": ENGINES[self.engine], "attempt": self.attempt, "events": self.events})
        temporary.replace(self.folder / "session.json")

    def track_list(self):
        with self.lock:
            return {"paintings": [{**row, "image": self.image_url(HERE / row["final"])} for row in self.tracks.list()]}

    def track_read(self, identifier):
        with self.lock:
            result = self.tracks.read(identifier)
            steps = []
            for event in result["events"]:
                if event.get("kind") != "decision" or event.get("action") not in ("ready", "resume", "paint", "no", "undo", "restart", "done", "crop", "error"):
                    continue
                if event["action"] == "resume" and steps:
                    continue
                path = event.get("accepted")
                if not path or not (HERE / path).is_file():
                    continue
                candidate = event.get("candidate") or {}
                receipt = self.folder / "proposals" / f"{candidate.get('id', 0):03}.json"
                detail = json.loads(receipt.read_text()) if candidate and receipt.exists() else {}
                steps.append({"id": str(len(steps)), "action": event["action"], "at": event.get("at"),
                    "image": self.image_url(HERE / path), "model": candidate.get("model", {}).get("name") or event.get("engine"),
                    "seconds": candidate.get("seconds"), "braincells": (event.get("cost") or {}).get("braincells") if event["action"] == "paint" else None,
                    "error": event.get("error"),
                    "prompt": detail.get("prompt"), "operation": detail.get("identity", {}).get("operation"),
                    "receipt": detail or None})
            return {**result, "steps": steps, "folder": str(self.tracks.folder / identifier)}

    def ensure_engine(self, engine):
        if engine == self.loaded_engine:
            return
        if not available(engine, self.account.snapshot()):
            raise ValueError("Choose an available model, or sign in with AC for remote moves.")
        previous_engine, self.loaded_engine = self.loaded_engine, None
        if self.generator is None:
            # Only one large local pipeline is resident on this 8 GB machine.
            if self.pipe is not None:
                if previous_engine == "fleet-evolve":
                    self.pipe.close()
                    self.pipe = self.embed = self.move = None
                else:
                    import gc
                    import torch
                    self.pipe = self.embed = self.move = None
                    gc.collect()
                    torch.mps.empty_cache()
            if engine.startswith("ac-"):
                from ac_run import move
                self.move = move
            elif engine == "fal-klein":
                from remote_run import move
                self.move = move
            elif engine == "fleet-evolve":
                from fleet_run import connect, move
                self.move = move
                self.pipe, self.embed = connect()
            elif engine.startswith("classic"):
                from functools import partial
                from primitives import move
                self.move = partial(move, family=engine)
            else:
                if engine == "evolve":
                    from evolve_run import load, move
                else:
                    from local_run import load, move
                self.move = move
                self.pipe, self.embed = load("play-" + engine)
        self.loaded_engine = engine

    def initialize(self):
        # Normal opening/resuming never start inference. The explicit diagnostic
        # flag retains its local preview benchmark, outside painting history.
        if self.verify_engine:
            try:
                self.ensure_engine("evolve")
                from evolve_run import benchmark
                folder = benchmark(self.pipe, self.embed)
                self.trace_verified = json.loads((folder / "verification.json").read_text())
            except Exception as error:
                self.fail(error)
                return
        with self.lock:
            self.ready = True
            self.busy = False
            self.candidate = None
            self.revision += 1
            self.prepare_quote()
            self.save("resume" if self.events else "ready")

    def estimate_seconds(self, engine):
        samples = []
        seen = set()
        for event in reversed(self.events):
            candidate = event.get("candidate") or {}
            number = candidate.get("id")
            if number in seen or candidate.get("engine") != engine or candidate.get("strength") != self.strength:
                continue
            seen.add(number)
            seconds = candidate.get("seconds")
            if isinstance(seconds, (int, float)) and seconds > 0:
                samples.append(seconds)
            if len(samples) == 7:
                break
        return round(statistics.median(samples), 1) if samples else None

    def prepare_quote(self):
        """Choose the next move without loading a model or submitting work."""
        identity = self.account.snapshot()
        choices = [model["id"] for model in catalog(identity) if model["available"]]
        if self.selection == "random":
            self.engine = secrets.choice(choices)
        elif self.selection in choices:
            self.engine = self.selection
        else:
            self.engine = self.selection
            self.quote = self.quote_identity = None
            self.cost = {"braincells": None, "status": "unavailable"}
            return
        offer = remote_offer(self.engine, identity)
        amount = offer.get("braincells") if self.engine.startswith("ac-") else 0
        self.quote = {"id": str(uuid.uuid4()), "engine": self.engine,
                      "input": self.image_url(self.history[-1]), "strength": self.strength,
                      "braincells": amount, "estimated_seconds": self.estimate_seconds(self.engine),
                      "price": offer.get("quote"), "seed": secrets.randbelow(2**31)}
        self.quote_identity = identity
        self.cost = {"braincells": amount, "status": "quoted" if amount else "free"}

    def fail(self, error):
        with self.lock:
            self.busy = False
            self.error = str(error)[:300]
            if self.job and self.job["engine"].startswith("ac-"):
                # A lost provider response does not prove that no charge occurred.
                self.cost = ({"braincells": None, "status": "pending"} if self.job["started"]
                             else {"braincells": 0, "status": "free"})
            self.revision += 1
            self.save("error")

    def settled_cost(self, engine, number, quoted=None):
        if not engine.startswith("ac-"):
            return {"braincells": 0, "status": "free"}
        receipt = self.folder / "proposals" / f"{number:03}.json"
        row = json.loads(receipt.read_text()) if receipt.exists() else {}
        charged = (row.get("billing") or {}).get("braincells")
        if row.get("status") == "complete" and isinstance(charged, int):
            return {"braincells": charged, "status": "charged"}
        amount = row.get("quoted_braincells", quoted)
        return {"braincells": amount, "status": "quoted" if amount is not None else "pending"}

    def begin(self, quote_id, hint=None):
        """Only Paint can confirm the exact displayed offer and start work."""
        if self.busy or not self.quote or quote_id != self.quote["id"]:
            raise Conflict("The move changed. Review the current cost before painting.")
        quote = self.quote
        identity = self.account.snapshot()
        offer = remote_offer(quote["engine"], identity)
        if not available(quote["engine"], identity) or (quote["engine"].startswith("ac-") and (
                identity.get("account_id") != self.quote_identity.get("account_id") or
                offer.get("quote") != quote["price"] or offer.get("braincells") != quote["braincells"])):
            self.prepare_quote()
            self.revision += 1
            self.save("requote")
            raise Conflict("Availability or price changed. Review the new quote before painting.")
        if quote["input"] != self.image_url(self.history[-1]):
            raise Conflict("The painting changed. Review the current move.")
        if self.trace_id and self.frames:
            self.save_trace()
        self.attempt += 1
        self.started = time.monotonic()
        self.engine = quote["engine"]
        self.job = {"number": self.attempt, "engine": self.engine,
                    "before": self.history[-1], "strength": quote["strength"],
                    "mask": self.mask, "seed": quote["seed"], "cancel": threading.Event(),
                    "started": False, "cost": self.cost.copy(), "hint": hint}
        if self.engine.startswith("ac-"):
            self.job["account"] = self.quote_identity
        self.quote = self.quote_identity = None
        self.candidate = self.error = None
        self.busy = True
        self.frames = []
        self.trace_id = f"{self.attempt:03}"
        self.trace_verified = None
        self.revision += 1
        self.save("confirm")
        if not self.worker_running:
            self.worker_running = True
            self.pool.submit(self.propose)

    def propose(self):
        while True:
            with self.lock:
                job = self.job
                if job is None:
                    self.worker_running = False
                    return
            try:
                check_cancel(job["cancel"])
                self.ensure_engine(job["engine"])
                check_cancel(job["cancel"])
                with self.lock:
                    job["started"] = True
                    self.ready = True
                before, number = job["before"], job["number"]
                region = Region(before, job["mask"], job["engine"], self.folder / "regions" / f"{number:03}")
                observe = lambda frame, job=job: self.observe(region.preview(frame), job)
                args = (region.input, self.folder / "proposals", number, job["strength"], job["seed"])
                if self.generator:
                    output = self.generator(*args, observe=observe, cancel=job["cancel"], engine=job["engine"], **region.kwargs)
                else:
                    output = self.move(self.pipe, self.embed, *args, cold=number == 1,
                                       observe=observe, cancel=job["cancel"],
                                       **region.kwargs,
                                       **({"account": self.account, "identity": job["account"], "engine": job["engine"], "hint": job["hint"]} if job["engine"].startswith("ac-") else {}))
                output = region.finish(output)
                with self.lock:
                    check_cancel(job["cancel"])
                    if self.job is not job:
                        raise MoveCancelled("Stale result")
                    self.cost = self.settled_cost(job["engine"], number, job["cost"]["braincells"])
                    self.candidate = {"id": number, "url": self.image_url(output),
                                      "input": self.image_url(before), "seed": job["seed"],
                                      "strength": job["strength"], "engine": job["engine"],
                                      "model": ENGINES[job["engine"]],
                                      "cost": self.cost,
                                      "seconds": time.monotonic()-self.started}
                    self.history.append(output)
                    self.busy = False
                    self.error = None
                    self.revision += 1
                    self.save_trace()
                    self.save("paint")
                    self.prepare_quote()
                    self.save("quote")
            except MoveCancelled:
                pass
            except Exception as e:
                with self.lock:
                    if self.job is job:
                        self.fail(e)
            with self.lock:
                if self.job is job:
                    self.worker_running = False
                    return
                # No extra executor tasks: rapid rejections coalesce here.

    def done(self, data):
        with self.lock:
            if self.upscale.get("busy"):
                raise Conflict("Wait for the upscale to finish.")
            if self.publication["busy"] or self.busy or data.get("revision") != self.revision or data.get("accepted") != self.image_url(self.history[-1]):
                raise Conflict("The painting changed. Try Done again.")
            identity = self.account.snapshot()
            if not identity.get("connected"):
                raise ValueError("Sign in with AC before Done.")
            if not self.pending_done:
                operation = str(uuid.uuid4())
                folder = self.folder / "done" / operation
                folder.mkdir(parents=True, mode=0o700)
                write(folder / "manifest.json", {"id": operation, "account_id": identity["account_id"],
                    "createdAt": datetime.datetime.now(datetime.timezone.utc).isoformat(),
                    "history": [{"path": str(p), "sha256": sha(p)} for p in self.history]})
                self.pending_done = str(folder.relative_to(HERE))
            folder = HERE / self.pending_done
            manifest = json.loads((folder / "manifest.json").read_text())
            if manifest["account_id"] != identity["account_id"]:
                raise ValueError("Sign into the account that started this Done.")
            self.publication = {"busy": True}
            self.revision += 1
            self.save("done-start")
            self.pool.submit(self.publish, folder, identity)
            return self.state()

    def publish(self, folder, identity):
        try:
            result = bridge({"action": "done", "folder": str(folder), "account_id": identity["account_id"]}, timeout=300)
            if not result.get("verified"):
                raise RuntimeError("Painting is not yet verified. Try Done again.")
            with self.lock:
                self.publication = {"busy": False, "code": result["code"], "url": result["route"]}
                self.pending_done = None
                self.history = [self.fresh_noise()]
                self.mask = self.mask_bits = None
                self.candidate = None
                self.revision += 1
                self.save("done")
                self.prepare_quote()
                self.save("quote")
        except Exception as error:
            with self.lock:
                self.publication = {"busy": False, "error": str(error)[:300]}
                self.revision += 1
                self.save("done-error")

    def start_upscale(self, data):
        from upscale import MODEL
        with self.lock:
            scale = data.get("scale")
            if type(scale) is not int or scale not in (2, 4):
                raise ValueError("Choose 2× or 4× upscale.")
            if (self.busy or self.worker_running or self.publication["busy"] or self.upscale.get("busy") or
                not self.ready or self.pending_done or data.get("accepted") != self.image_url(self.history[-1])):
                raise Conflict("The painting changed or is busy. Try Upscale again.")
            source = self.history[-1]
            identifier = uuid.uuid4().hex
            output = self.folder / "upscales" / f"{identifier}-{scale}x.png"
            self.upscale_started = time.monotonic()
            self.upscale = {"id": identifier, "busy": True, "progress": 0, "scale": scale,
                            "model": MODEL, "braincells": 0, "input": self.image_url(source)}
            self.revision += 1
            self.save("upscale-start", upscale=self.upscale.copy())
            self.pool.submit(self.run_upscale, source, output, scale)
            return self.state()

    def run_upscale(self, source, output, scale):
        from upscale import upscale_image
        def progress(value):
            with self.lock:
                self.upscale["progress"] = value
        try:
            result = upscale_image(source, output, scale, progress)
            with self.lock:
                self.upscale.update(busy=False, progress=1, seconds=result["seconds"], url=self.image_url(output))
                self.revision += 1
                self.save("upscale", upscale=self.upscale.copy(), output=str(output.relative_to(HERE)))
        except Exception as error:
            with self.lock:
                self.upscale.update(busy=False, error=str(error)[:300])
                self.revision += 1
                self.save("upscale-error", upscale=self.upscale.copy())

    def act(self, data):
        with self.lock:
            if self.upscale.get("busy"):
                raise Conflict("Wait for the upscale to finish.")
            if self.publication["busy"] or (self.pending_done and data.get("action") != "restart"):
                raise Conflict("Finish Done before changing this painting. Your canvas is saved.")
            action = data.get("action")
            if action not in ("no", "paint", "undo", "restart", "retry", "strength", "engine", "mask", "crop"):
                raise ValueError("Unknown action")
            generation = self.job["number"] if self.job else (self.candidate or {}).get("id")
            # A No sent just before completion still rejects that exact move.
            same_no = action == "no" and generation is not None and data.get("generation") == generation
            same_mask = action in ("mask", "crop") and data.get("accepted") == self.image_url(self.history[-1])
            if action in ("mask", "crop") and not same_mask:
                raise Conflict("The painting changed. Brush the current image.")
            if (not (same_no or same_mask) and data.get("revision") != self.revision) or (self.busy and not (same_no or same_mask)):
                raise Conflict("The canvas changed. Try again.")
            if action == "crop":
                box = data.get("box")
                if (not isinstance(box, list) or len(box) != 4 or
                    any(type(n) is not int for n in box) or
                    not (0 <= box[0] < box[2] <= 256 and 0 <= box[1] < box[3] <= 256)):
                    raise ValueError("Select a crop inside the painting.")
                output = self.folder / "crops" / (uuid.uuid4().hex + ".png")
                output.parent.mkdir(parents=True, exist_ok=True)
                Image.open(self.history[-1]).convert("RGB").crop(box).resize((256, 256), Image.Resampling.NEAREST).save(output)
                self.history.append(output)
                self.mask = self.mask_bits = None
            elif action == "mask":
                self.mask = save_mask(data.get("mask"), self.folder / "masks")
                self.mask_bits = encode_mask(self.mask) if self.mask else None
            elif action == "paint":
                # A hint steers cloud moves; local engines ignore it.
                hint = data.get("hint")
                if hint is not None and (not isinstance(hint, str) or len(hint) > 200):
                    raise ValueError("Keep the hint under 200 characters.")
                self.begin(data.get("quote"), (hint or "").strip() or None)
                return self.state()
            elif action == "no":
                if self.busy:
                    # The in-flight image is the forward step: No returns to
                    # its input. An already submitted cloud request may charge.
                    self.job["cancel"].set()
                elif len(self.history) > 1:
                    self.history.pop()
                else:
                    raise Conflict("Already at the first painting.")
            elif action == "undo":
                if len(self.history) < 2:
                    raise Conflict("There is no accepted move to undo.")
                self.history.pop()
            elif action == "restart":
                start = data.get("start")
                if start not in ("base", "blank", "noise"):
                    raise ValueError("Unknown starting image")
                self.history = [self.fresh_noise() if start == "noise" else HERE / "starts" / (start + ".png")]
                self.mask = self.mask_bits = None
                self.pending_done = None
                self.publication.pop("error", None)
            elif action == "strength":
                strength = data.get("strength")
                if strength not in (.25, .5, .75):
                    raise ValueError("Unknown strength")
                self.strength = strength
            elif action == "engine":
                engine = data.get("engine")
                if engine != "random" and engine not in ENGINES:
                    raise ValueError("Choose a model from the image model catalog.")
                self.selection = engine
            elif action == "retry" and not self.error:
                raise Conflict("No failed move to retry.")
            rejected_generation = generation if action in ("no", "undo") else None
            if self.job:
                self.job["cancel"].set()
            if self.trace_id and self.frames:
                self.save_trace()
            self.job = None
            self.busy = False
            self.error = None
            self.candidate = None
            self.frames = []
            self.trace_id = None
            self.revision += 1
            self.prepare_quote()
            self.save(action, rejected_generation=rejected_generation,
                      crop=data.get("box") if action == "crop" else None,
                      start=data.get("start") if action == "restart" else None)
            return self.state()


class Handler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        pass

    def respond(self, code, content, kind="application/json", extra=None):
        if isinstance(content, dict):
            content = json.dumps(content).encode()
        if isinstance(content, str):
            content = content.encode()
        self.send_response(code)
        self.send_header("Content-Type", kind)
        self.send_header("Content-Length", str(len(content)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Content-Security-Policy", "default-src 'self'; img-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'; frame-ancestors 'none'")
        for key, value in (extra or {}).items():
            self.send_header(key, value)
        self.end_headers()
        self.wfile.write(content)

    def valid_host(self):
        return self.headers.get("Host") == f"127.0.0.1:{self.server.server_port}"

    def do_GET(self):
        if not self.valid_host():
            return self.respond(403, {"error": "Use the loopback URL"})
        route = urlsplit(self.path).path
        game = self.server.game
        if route == "/":
            return self.respond(200, (HERE / "play.html").read_bytes(), "text/html; charset=utf-8")
        if route == "/api/state":
            return self.respond(200, game.state())
        if route == "/api/models":
            try:
                model = parse_qs(urlsplit(self.path).query).get("model", [None])[0]
                result = MODEL_BROWSER.read(model)
                if model is None:
                    with game.lock:
                        register_image_models(result["models"])
                return self.respond(200, result)
            except ValueError as error:
                return self.respond(400, {"error": str(error)})
            except Exception:
                return self.respond(503, {"error": "OpenRouter catalog unavailable. Try again."})
        if route == "/api/trace":
            return self.respond(200, game.trace())
        if route == "/api/tracks":
            return self.respond(200, game.track_list())
        if route == "/api/track":
            try:
                return self.respond(200, game.track_read(parse_qs(urlsplit(self.path).query).get("id", [""])[0]))
            except ValueError as error:
                return self.respond(404, {"error": str(error)})
        if route == "/download":
            with game.lock:
                content = game.history[-1].read_bytes()
            stamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S-%f")[:-3]
            return self.respond(200, content, "image/png", {"Content-Disposition": f'attachment; filename="nopaint-{stamp}.png"'})
        if route.startswith("/image/") and route.endswith(".png"):
            key = route[len("/image/"):-4]
            with game.lock:
                image = game.images.get(key)
            if image:
                return self.respond(200, image.read_bytes(), "image/png")
        self.respond(404, {"error": "Not found"})

    def do_POST(self):
        origin = f"http://127.0.0.1:{self.server.server_port}"
        if not self.valid_host() or self.headers.get("Origin") not in (None, origin):
            return self.respond(403, {"error": "Use the local game page"})
        if self.path not in ("/api/action", "/api/account", "/api/done", "/api/upscale"):
            return self.respond(404, {"error": "Not found"})
        if self.headers.get("Content-Type", "").split(";")[0] != "application/json":
            return self.respond(415, {"error": "Expected JSON"})
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if not 0 < length < 32768:
                raise ValueError("Invalid request size")
            data = json.loads(self.rfile.read(length))
            if not isinstance(data, dict):
                raise ValueError("Expected an object")
            if self.path == "/api/done":
                return self.respond(200, self.server.game.done(data))
            if self.path == "/api/upscale":
                return self.respond(200, self.server.game.start_upscale(data))
            if self.path == "/api/account":
                game = self.server.game
                action = data.get("action")
                if action == "connect":
                    game.account.update(login=True)
                elif action == "refresh":
                    game.account.update()
                elif action == "disconnect":
                    with game.lock:
                        if game.busy or game.publication["busy"]:
                            raise Conflict("Wait for the current move before signing out.")
                        game.account.disconnect()
                else:
                    raise ValueError("Unknown account action")
                return self.respond(200, game.state())
            self.respond(200, self.server.game.act(data))
        except Conflict as e:
            self.respond(409, {"error": str(e)})
        except (ValueError, TypeError) as e:
            self.respond(400, {"error": str(e)})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=8767)
    parser.add_argument("--resume", type=Path, help="Resume a saved local/play session")
    parser.add_argument("--engine", choices=("random", *ENGINES), default=None)
    parser.add_argument("--verify-engine", action="store_true", help="Verify previews against an unobserved run before play")
    args = parser.parse_args()
    server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    server.game = Game(resume=args.resume, engine=args.engine, verify_engine=args.verify_engine)
    write(HERE / "local/play-server.json", {"pid": os.getpid(), "port": args.port,
                                            "session": str(server.game.folder.relative_to(HERE))})
    print(f"No Paint: http://127.0.0.1:{args.port}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
        with server.game.lock:
            if server.game.job:
                server.game.job["cancel"].set()
        server.game.pool.shutdown(wait=True)
        if server.game.loaded_engine == "fleet-evolve" and server.game.pipe:
            server.game.pipe.close()


if __name__ == "__main__":
    main()
