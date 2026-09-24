#!/usr/bin/env python3
"""run-fleet-nsscore.py — a song folder's own conductor for a ring .nsscore.

Copied into a folder as run-fleet.py beside one .nsscore, it makes that
folder a venue setlist item (toolchain/mcp/ac-venue-mcp.mjs prefers a
folder's run-fleet.py: no arguments preflights, --run performs). The seats
keep whatever piece they are on (blueberry's notespatial-controls wrapper
or the stock spatial-rehearsal piece): the score is put in
/pieces/spatial-rehearsal.nsscore, the piece is re-jumped so it loads, the
rehearsal tool cues it with clock probes, the run is watched to its end,
and the score that was there before is put back. A receipt run-<id>.json
lands in the folder for venue_result.

  python3 run-fleet.py            preflight: seats reachable, idle, mic closed
  python3 run-fleet.py --run      stage, cue, watch, restore, receipt
  python3 run-fleet.py --run --keep    leave the score on the seats afterwards

Env: TRIO_FLEET (fleet.json; default ~/.ac-os/culturehub/fleet.json),
TRIO_REPO (the aesthetic-computer checkout; default ~/aesthetic-computer),
NSSCORE (the score; default the one .nsscore in this folder).
"""
import glob, hashlib, json, os, signal, subprocess, sys, time, urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
FLEET = Path(os.environ.get("TRIO_FLEET") or Path.home() / ".ac-os/culturehub/fleet.json")
REPO = Path(os.environ.get("TRIO_REPO") or Path.home() / "aesthetic-computer")
RUN = "--run" in sys.argv
KEEP = "--keep" in sys.argv
SCORE_FILE = os.environ.get("NSSCORE")
PIECE_FILE = "spatial-rehearsal.nsscore"
RUN_ID = f"nsscore-{int(time.time())}"
scores = [Path(SCORE_FILE)] if SCORE_FILE else [Path(p) for p in sorted(glob.glob(str(HERE / "*.nsscore")))]
if len(scores) != 1: sys.exit(f"expected one .nsscore in {HERE} (or NSSCORE=path), found {len(scores)}")
SCORE_PATH = scores[0]
SCORE_BYTES = SCORE_PATH.read_bytes()
SCORE = json.loads(SCORE_BYTES)
SCORE_HASH = hashlib.sha256(SCORE_BYTES).hexdigest()
EVENTS = sorted(e["t"] for lane in SCORE["lanes"] for e in lane["events"])

def http(host, path, body=None, timeout=8):
    req = urllib.request.Request(f"http://{host}{path}", data=body, method="PUT" if body is not None else "GET")
    with urllib.request.urlopen(req, timeout=timeout) as r: return r.read()
def post(host, path, timeout=8):
    req = urllib.request.Request(f"http://{host}{path}", data=b"", method="POST")
    with urllib.request.urlopen(req, timeout=timeout) as r: return r.read()
def status(host):
    return json.loads(http(host, "/pieces/spatial-rehearsal-status.json", timeout=4))
def piece(host):
    return json.loads(http(host, "/status", timeout=4)).get("piece")
def log(msg): print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

fleet = json.loads(FLEET.read_text())
hosts = [row[0] for row in fleet]
labels = {row[0]: (row[2] if len(row) > 2 else row[0]) for row in fleet}

def readiness():
    seats, problems = {}, []
    for h in hosts:
        try:
            st = status(h); pc = piece(h)
        except Exception as e: problems.append(f"{h} ({labels[h]}): unreachable: {e}"); continue
        seats[h] = {"seat": st.get("seat"), "seats": st.get("seats"), "phase": st.get("phase"), "piece": pc, "scoreDuration": st.get("scoreDuration")}
        mic = st.get("microphone") or {}
        if st.get("error"): problems.append(f"{h}: error {st['error']}")
        if mic.get("hot") or mic.get("recording"): problems.append(f"{h}: microphone open")
        if st.get("phase") not in ("ready", "idle", "stopped", "armed", None): problems.append(f"{h}: phase {st.get('phase')}")
    seatnums = [s["seat"] for s in seats.values()]
    if len(set(seatnums)) != len(seatnums): problems.append(f"duplicate seats: {seatnums}")
    if len(seats) != SCORE.get("seats", len(hosts)): problems.append(f"{len(seats)} seats up, score wants {SCORE.get('seats')}")
    return seats, problems

seats, problems = readiness()
ordered = [h for h, s in sorted(seats.items(), key=lambda kv: (kv[1]["seat"] if kv[1]["seat"] is not None else 99))]
log(f"{SCORE.get('name')}: {SCORE['dur']:.1f} s, {len(EVENTS)} events, sha {SCORE_HASH[:12]}")
for h in ordered: log(f"  seat {seats[h]['seat'] + 1 if seats[h]['seat'] is not None else '?'} {h} {labels[h]}: {seats[h]['piece']} {seats[h]['phase']} (score {seats[h]['scoreDuration']})")
for p in problems: log("  ! " + p)
if not RUN:
    print("READY" if not problems else "NOT READY"); sys.exit(0 if not problems else 1)
if problems: sys.exit("not ready; refusing to run")

receipt = {"runId": RUN_ID, "score": SCORE_PATH.name, "scoreHash": SCORE_HASH, "title": SCORE.get("name"), "duration": SCORE["dur"], "events": len(EVENTS),
           "seats": [{"host": h, "seat": seats[h]["seat"], "label": labels[h], "piece": seats[h]["piece"]} for h in ordered],
           "startedAt": time.time(), "completed": False, "error": None, "seatWarnings": [], "samples": [], "cleanup": {}}
def write_receipt(): (HERE / f"run-{RUN_ID}.json").write_text(json.dumps(receipt, indent=2) + "\n")
write_receipt()

def put_verified(h, name, data):
    for attempt in (1, 2):
        http(h, f"/pieces/{name}", data, timeout=30)
        back = http(h, f"/pieces/{name}", timeout=30)
        if back == data: return
        log(f"{h}: readback differs for {name} ({len(back)}/{len(data)} bytes), attempt {attempt}")
    raise RuntimeError(f"{h}: could not stage {name}")

# 1. what was there (put back at the end unless --keep)
previous = {}
for h in ordered:
    try: previous[h] = http(h, f"/pieces/{PIECE_FILE}", timeout=30)
    except Exception as e: log(f"{h}: no previous score ({e})"); previous[h] = None
prev_hashes = {h: hashlib.sha256(b).hexdigest()[:12] for h, b in previous.items() if b}
receipt["cleanup"]["previousScore"] = sorted(set(prev_hashes.values()))
if previous and any(b and b != SCORE_BYTES for b in previous.values()):
    for hsh in set(prev_hashes.values()):
        src = next(b for h, b in previous.items() if b and hashlib.sha256(b).hexdigest()[:12] == hsh)
        (HERE / f"previous-{hsh}.nsscore").write_bytes(src)

def restore(reason):
    if KEEP: receipt["cleanup"]["restored"] = "kept"; return
    restored = []
    for h in ordered:
        b = previous.get(h)
        if not b or b == SCORE_BYTES: continue
        try: put_verified(h, PIECE_FILE, b); post(h, f"/jump/{seats[h]['piece'] or 'spatial-rehearsal'}"); restored.append(h)
        except Exception as e: receipt["seatWarnings"].append(f"{h}: restore failed: {e}")
    receipt["cleanup"]["restored"] = f"{len(restored)} seats ({reason})"
    log(f"restored previous score on {len(restored)} seats ({reason})")

def stop_all():
    for h in ordered:
        try: http(h, "/pieces/spatial-rehearsal-command.json", json.dumps({"id": f"{RUN_ID}-stop", "action": "stop"}).encode())
        except Exception: pass

def on_sigint(sig, frame):
    log("interrupted: stopping seats"); stop_all(); receipt["error"] = "interrupted"; restore("interrupted"); receipt["finishedAt"] = time.time(); write_receipt(); sys.exit(130)
signal.signal(signal.SIGINT, on_sigint); signal.signal(signal.SIGTERM, on_sigint)

try:
    # 2. stage and reload
    for h in ordered: put_verified(h, PIECE_FILE, SCORE_BYTES); log(f"{h}: staged {len(SCORE_BYTES)} bytes, readback verified")
    for h in ordered: post(h, f"/jump/{seats[h]['piece'] or 'spatial-rehearsal'}")
    deadline = time.time() + 40
    while time.time() < deadline:
        loaded = {}
        for h in ordered:
            try: st = status(h); loaded[h] = abs(float(st.get("scoreDuration") or 0) - SCORE["dur"]) < 0.01 and st.get("phase") in ("ready", "armed", "idle")
            except Exception: loaded[h] = False
        if all(loaded.values()): break
        time.sleep(1)
    else: raise RuntimeError("seats did not report the new score: " + ", ".join(h for h, ok in loaded.items() if not ok))
    log("all seats loaded the score")
    # 3. cue through the rehearsal tool (clock probes, prepare, play)
    cue = subprocess.run(["node", str(REPO / "fedac/native/tools/spatial-rehearsal.mjs"), "cue", *ordered], capture_output=True, text=True, timeout=120)
    receipt["cue"] = (cue.stdout + cue.stderr).strip().splitlines()[-8:]
    if cue.returncode: raise RuntimeError("cue failed: " + " | ".join(receipt["cue"]))
    log("cued: " + (receipt["cue"][0] if receipt["cue"] else ""))
    # 4. watch to the end
    t0 = time.time(); ended_at = None
    while time.time() - t0 < SCORE["dur"] + 30:
        time.sleep(5)
        native = []
        for h in ordered:
            try:
                st = status(h); sct = float(st.get("scoreTime") or 0)
                native.append({"receiverId": labels[h], "host": h, "phase": st.get("phase"), "scoreTime": round(sct, 2), "eventsStarted": sum(1 for t in EVENTS if t <= sct), "eventCount": len(EVENTS), "maxFrameGap": st.get("maxFrameGap", 0)})
            except Exception as e: native.append({"receiverId": labels[h], "host": h, "phase": "unreachable", "error": str(e)})
        receipt["samples"].append({"t": round(time.time() - t0, 1), "native": native}); write_receipt()
        phases = {n["phase"] for n in native}
        if native and phases <= {"ready", "idle", "stopped", "armed"} and time.time() - t0 > 20: ended_at = time.time(); break
    receipt["completed"] = ended_at is not None
    if not receipt["completed"]: receipt["error"] = "seats did not return to ready"; stop_all()
    else: log(f"run ended after {ended_at - t0:.0f} s")
except Exception as e:
    receipt["error"] = str(e); log("error: " + str(e)); stop_all()
finally:
    restore("end of run"); receipt["finishedAt"] = time.time(); write_receipt()
    log(f"receipt run-{RUN_ID}.json: completed={receipt['completed']} error={receipt['error']}")
sys.exit(0 if receipt["completed"] else 1)
