#!/usr/bin/env python3
"""lock-lights-to-audio.py — make a capture's key lights land on its notes.

    python lock-lights-to-audio.py base.mp4 capture.json out.mp4 events.json
        [--cut A,B] [--letters t,t,…] [--enter T] [--perf performance.json]

A headless capture records sound sample-exact, but the picture shows each
key light a variable 50–170 ms after its note. The sound is the truth. This
finds each pressed note three ways (the press in capture.json, its attack in
the audio, its light in the picture), then re-times the picture between
notes so that every light starts on its attack. Between notes
the screen is still, so the stretch does not show.

It writes the re-timed video (audio untouched apart from --cut, which drops
a dead stretch from both) and an events file for the hand scene: the tap,
typed-letter times, and each note's time and screen position.
Needs numpy and ffmpeg.
"""
import subprocess, sys, json, os, tempfile, shutil, numpy as np

src, cap_json, out_mp4, out_events = sys.argv[1:5]
opt = lambda k, d=None: sys.argv[sys.argv.index(k) + 1] if k in sys.argv else d
CUT = tuple(float(v) for v in opt("--cut").split(",")) if opt("--cut") else None
FPS = 30
W, H = 270, 480

raw = subprocess.run(["ffmpeg", "-v", "error", "-i", src, "-vf", f"fps={FPS},scale={W}:{H}", "-f", "rawvideo", "-pix_fmt", "rgb24", "-"],
                     capture_output=True).stdout
fr = np.frombuffer(raw, np.uint8).reshape(-1, H, W, 3).astype(np.int16)
N = len(fr)
cap = json.load(open(cap_json))
# --perf FILE: the performance that was played. Actions tagged "note" are
# the notes to lock; every other touch, drag and typed key is carried into
# the events file on the same re-timed clock.
PERF = json.load(open(opt("--perf"))) if opt("--perf") else None
TAG = {a["id"]: a["tag"] for a in PERF if "id" in a and a.get("tag")} if PERF else {}
is_note = (lambda g: TAG.get(g["id"]) == "note") if PERF else (lambda g: True)
presses = sorted([g for g in cap["gestures"] if g["kind"] == "down" and is_note(g)], key=lambda g: g["t"])
taps = [g for g in cap["gestures"] if g["kind"] == "tap"]
NOTES_END = presses[-1]["t"] + 1.0 if presses else 0.0   # after this the screen is some other piece

notes = []
if presses:
    # sound: attacks
    pcm = subprocess.run(["ffmpeg", "-v", "error", "-i", src, "-ac", "1", "-ar", "48000", "-f", "f32le", "-"], capture_output=True).stdout
    x = np.frombuffer(pcm, np.float32); sr = 48000; win, hop = 480, 96
    frames = np.lib.stride_tricks.sliding_window_view(x, win)[::hop] * np.hanning(win)
    spec = np.abs(np.fft.rfft(frames, axis=1)); wgt = np.arange(spec.shape[1])
    flux = np.clip(np.diff((spec * wgt).sum(1), prepend=0), 0, None); ft = np.arange(len(flux)) * hop / sr
    t_first = presses[0]["t"] - 0.2
    thr = 0.12 * flux[(ft > t_first) & (ft < NOTES_END)].max()
    peaks = [ft[i] for i in range(1, len(flux) - 1) if ft[i] > t_first and flux[i] > thr and flux[i] >= flux[i - 1] and flux[i] >= flux[i + 1]]

    # picture: a key is lit when many pixels differ from the resting keyboard
    ref = fr[int((presses[0]["t"] - 0.35) * FPS)]
    diff = lambda f: np.abs(f - ref).sum(2) > 60
    count = np.array([int(diff(f).sum()) for f in fr])

    # Every light as its own event: a rise from dark, or the lit patch jumping
    # to another key while still lit.
    events = []                                    # (first frame, last frame)
    on = False
    for f in range(int((presses[0]["t"] - 0.1) * FPS), min(N, int(NOTES_END * FPS))):
        hot = count[f] > 800
        if hot and not on: events.append([f, f])
        elif hot and on:
            a = np.nonzero(diff(fr[f]))[1]; b = np.nonzero(diff(fr[f - 1]))[1]
            if abs(np.median(a) - np.median(b)) > 14: events.append([f, f])
            else: events[-1][1] = f
        on = hot
    # The sound follows its press by a steady amount; find it from the first note.
    first_peak = next(a for a in peaks if a > presses[0]["t"] + 0.02)
    sound_lat = first_peak - presses[0]["t"]
    if len(events) != len(presses): print(f"  note: {len(events)} lights for {len(presses)} presses; pairing by order where possible")
    notes = []
    lat = []
    for k, p in enumerate(presses):
        want = p["t"] + sound_lat
        attack = min(peaks, key=lambda a: abs(a - want))
        if abs(attack - want) > 0.09: print(f"  press {k} at {p['t']:.2f}s: no attack near {want:.2f}s, skipped"); continue
        if len(events) == len(presses): start, end = events[k]
        else:
            after = [e for e in events if e[0] / FPS >= attack - 0.05]
            if not after: continue
            start, end = after[0]
        best = max(range(start, end + 1), key=lambda f: count[f])
        ys, xs = np.nonzero(diff(fr[best]))
        notes.append({"press": p["t"], "attack": attack, "light": start / FPS, "dark": (end + 1) / FPS,
                      "x": float(xs.mean() / W), "y": float((ys.min() + 0.72 * (ys.max() - ys.min())) / H)})
        lat.append((start / FPS - attack) * 1000)
    lat = np.array(lat)
    print(f"{len(notes)} of {len(presses)} notes found · light after attack before the fix: median {np.median(lat):+.0f} ms, range {lat.min():+.0f}..{lat.max():+.0f} ms")
else:
    print("no notes in this performance: the picture keeps its own timing")

# Re-time: output time -> source time, through anchors where each light's
# first frame is shown at its attack.
anchors = [(0.0, 0.0)]
for n in notes:
    o = n["attack"]; s = n["light"]
    if o > anchors[-1][0] + 0.02 and s > anchors[-1][1]: anchors.append((o, s))
dur = N / FPS
anchors.append((dur + (anchors[-1][0] - anchors[-1][1]), dur))
def source_time(o):
    for (o0, s0), (o1, s1) in zip(anchors, anchors[1:]):
        if o0 <= o <= o1: return s0 + (s1 - s0) * (o - o0) / max(1e-6, o1 - o0)
    return min(dur, o)
out_dur = min(dur, anchors[-1][0])

def out_time(s):                               # the inverse: when a source moment is shown
    for (o0, s0), (o1, s1) in zip(anchors, anchors[1:]):
        if s0 <= s <= s1: return o0 + (o1 - o0) * (s - s0) / max(1e-6, s1 - s0)
    return s
# A touch shows its effect on screen this long after the capture logged it.
RESP = float(np.median([n["light"] - n["press"] for n in notes])) if notes else 0.32
shown = lambda g_t: out_time(g_t + RESP)

def keep(t):                                   # output times that survive the cut
    return not (CUT and CUT[0] <= t < CUT[1])
shift = lambda t: t - (CUT[1] - CUT[0]) if CUT and t >= CUT[1] else t

tmp = tempfile.mkdtemp(prefix="lock-lights-")
try:
    subprocess.run(["ffmpeg", "-v", "error", "-i", src, "-vf", f"fps={FPS}", "-start_number", "0", f"{tmp}/s%05d.png"], check=True)
    have = len([f for f in os.listdir(tmp) if f.startswith("s")])
    lines = []; n_out = 0
    for i in range(int(out_dur * FPS)):
        o = i / FPS
        if not keep(o): continue
        sidx = min(have - 1, max(0, int(round(source_time(o) * FPS))))
        lines.append(f"file '{tmp}/s{sidx:05d}.png'\nduration {1 / FPS:.6f}"); n_out += 1
    open(f"{tmp}/list.txt", "w").write("\n".join(lines) + "\n")
    if CUT:
        af = f"[1:a]atrim=0:{CUT[0]},asetpts=PTS-STARTPTS[a0];[1:a]atrim={CUT[1]}:{out_dur},asetpts=PTS-STARTPTS[a1];[a0][a1]concat=n=2:v=0:a=1,loudnorm=I=-14:TP=-1.5:LRA=11[a]"
    else:
        af = f"[1:a]atrim=0:{out_dur},loudnorm=I=-14:TP=-1.5:LRA=11[a]"
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-f", "concat", "-safe", "0", "-i", f"{tmp}/list.txt", "-i", src,
                    "-filter_complex", af, "-map", "0:v", "-map", "[a]", "-r", str(FPS), "-c:v", "libx264", "-crf", "12",
                    "-preset", "fast", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-ar", "48000", "-shortest", out_mp4], check=True)
finally:
    shutil.rmtree(tmp, ignore_errors=True)

ev = {"fps": FPS, "cut": list(CUT) if CUT else None, "duration": round(n_out / FPS, 3),
      "keys": [{"t": round(shift(n["attack"]), 4), "up": round(shift(n["attack"] + (n["dark"] - n["light"])), 4),
                "x": round(n["x"], 4), "y": round(n["y"], 4)} for n in notes if keep(n["attack"])]}
if PERF:
    # other touches and drags, on the re-timed clock
    ev["touches"] = [{"t": round(shift(shown(g["t"])), 3), "x": g["x"], "y": g["y"], "tag": TAG.get(g["id"], "")}
                     for g in taps if TAG.get(g["id"]) != "prime" and keep(shown(g["t"]))]
    drags = {}
    for g in cap["gestures"]:
        if TAG.get(g["id"]) == "draw" and g["kind"] in ("down", "move", "up"):
            drags.setdefault(g["id"], []).append([round(shift(shown(g["t"])), 3), g["x"], g["y"]])
    ev["drags"] = [sorted(v) for v in drags.values()]
    # typed keys: the performance's clock runs a touch ahead of the video's
    pt = {a["id"]: a["t"] / 1000 for a in PERF if a.get("type") == "tap"}
    lead = float(np.median([pt[g["id"]] - g["t"] for g in taps if g["id"] in pt])) if taps else 0.0
    typed = lambda a: round(shift(out_time(a["t"] / 1000 - lead + 0.06)), 3)
    ev["letters"] = [typed(a) for a in PERF if a.get("tag") == "letter"]
    ev["enters"] = [typed(a) for a in PERF if a.get("tag") == "enter"]
else:
    if taps:
        first = min(taps, key=lambda g: g["t"])
        ev["tap"] = {"t": round(shift(first["t"]), 3), "x": first["x"], "y": first["y"]}
    if opt("--letters"): ev["letters"] = [round(shift(float(v)), 3) for v in opt("--letters").split(",")]
    if opt("--enter"): ev["enter"] = round(shift(float(opt("--enter"))), 3)
json.dump(ev, open(out_events, "w"))
print(f"wrote {out_mp4} ({n_out / FPS:.2f}s) and {out_events} ({len(ev['keys'])} notes)")
