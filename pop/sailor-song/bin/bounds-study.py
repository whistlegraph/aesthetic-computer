#!/usr/bin/env python3
"""bounds-study.py — what the hand-drawn word bounds teach the aligner.

Compares src/word-bounds.json (SyllaWizard, take seconds — the ear's answer)
against the automatic answers for the same words:
  FA    src/.word-times/forced-align.json      MMS forced alignment (bin/forced-align.py)
  AUTO  <words-aligned.json made WITHOUT bounds> word-times.py --fa's full pipeline
        (pass it with --auto; default: src/.word-times/words-aligned-auto.json)
and against her own energy envelope (src/vox/vocals-natural.wav, the clock the
clips were cut from), to read off the thresholds the ear actually uses:
  · how many dB under the word's peak an onset sits (the consonant attack)
  · how many dB under the peak a word is released (the held-vowel tail)
  · how far a hand onset is from the nearest detected vocal onset (snap window)
  · the gap that separates "same phrase" from "breath" (the phrase threshold)
Then a quality pass over the hand bounds themselves: onsets that start late
(an energy attack shortly before them), ends cut inside a loud vowel, words
shorter than a syllable can be, and tiny gaps that are really contiguous.

  pop/.venv/bin/python pop/sailor-song/bin/bounds-study.py            # report
  pop/.venv/bin/python pop/sailor-song/bin/bounds-study.py --apply    # also nudge
      snaps a hand onset ≤ --snap-ms (default 25) onto the detected attack and
      writes it back through the SyllaWizard files (sylls-NN / words-NN, clip ms)

Writes src/.word-times/bounds-study.json (every word's numbers) for the platter.
"""
import argparse, json, os, sys
import numpy as np, librosa, soundfile as sf

ap = argparse.ArgumentParser()
ap.add_argument("--auto", default=None)
ap.add_argument("--apply", action="store_true")
ap.add_argument("--snap-ms", type=float, default=0, help="also snap onsets this close to a detected attack (0 = off)")
a = ap.parse_args()
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src"); SY = os.path.join(SRC, "sylla"); TMP = os.path.join(SRC, ".word-times")
hand = json.load(open(os.path.join(SRC, "word-bounds.json")))["words"]
fa = json.load(open(os.path.join(TMP, "forced-align.json")))
auto_path = a.auto or os.path.join(TMP, "words-aligned-auto.json")
auto = json.load(open(auto_path)) if os.path.exists(auto_path) else None
lyric = [w for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("[") for w in l.split()]

# ── her envelope: 10 ms RMS in dB, detected attacks (backtracked), the floor ──
y, sr = librosa.load(os.path.join(SRC, "vox", "vocals-natural.wav"), sr=22050, mono=True)
hop = int(sr * 0.01)
e = librosa.feature.rms(y=y, frame_length=hop * 3, hop_length=hop)[0]
edb = (20 * np.log10(e + 1e-6)).astype(float)
t = np.arange(len(edb)) * 0.01
floor = float(np.percentile(edb[edb > -80], 20))
env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=256)
on = librosa.onset.onset_detect(onset_envelope=env, sr=sr, hop_length=256, backtrack=True, units="time", delta=0.05)
def db_at(s): return float(np.interp(s, t, edb))
def peak_in(s0, s1):
    i0, i1 = int(s0 / 0.01), max(int(s0 / 0.01) + 1, int(s1 / 0.01)); return float(edb[i0:i1].max())
def near_on(s): d = np.abs(on - s); k = int(np.argmin(d)); return float(on[k]), float(on[k] - s)

rows = []
for k, h in enumerate(hand):
    i = h["i"]; f = fa[i] if i < len(fa) else None; u = auto[i] if auto and i < len(auto) else None
    pk = peak_in(h["from"], h["to"])
    prev = hand[k - 1] if k and hand[k - 1]["i"] == i - 1 else None
    nxt = hand[k + 1] if k + 1 < len(hand) and hand[k + 1]["i"] == i + 1 else None
    o, dv = near_on(h["from"])
    row = {"i": i, "text": h["text"], "from": h["from"], "to": h["to"], "dur": round(h["to"] - h["from"], 3),
           "on_db_under_peak": round(pk - db_at(h["from"]), 1),
           "off_db_under_peak": round(pk - db_at(h["to"]), 1),
           "peak_db_over_floor": round(pk - floor, 1),
           "nearest_attack_ms": round(dv * 1000),
           "gap_before": None if not prev else round(h["from"] - prev["to"], 3),
           "gap_after": None if not nxt else round(nxt["from"] - h["to"], 3),
           "fa_on_ms": None if not f else round((h["from"] - f["from"]) * 1000),
           "fa_off_ms": None if not f else round((h["to"] - f["to"]) * 1000),
           "auto_on_ms": None if not u else round((h["from"] - u["fromMs"] / 1000) * 1000),
           "auto_off_ms": None if not u else round((h["to"] - u["toMs"] / 1000) * 1000)}
    rows.append(row)
json.dump({"floor_db": round(float(floor), 1), "words": rows}, open(os.path.join(TMP, "bounds-study.json"), "w"), indent=1)

def q(xs, ps=(10, 25, 50, 75, 90)):
    xs = np.array([x for x in xs if x is not None], float)
    return ", ".join(f"p{p} {np.percentile(xs, p):+.0f}" for p in ps) if len(xs) else "-"
def within(xs, ms):
    xs = [x for x in xs if x is not None]; return f"{sum(abs(x) <= ms for x in xs)}/{len(xs)}"
print(f"{len(rows)} hand words · floor {floor:.1f} dB · {len(on)} detected attacks")
print("\n── the ear vs the machines (hand − machine, ms; + = hand later) ──")
print(f"FA   onsets  {q([r['fa_on_ms'] for r in rows])}   within 40: {within([r['fa_on_ms'] for r in rows], 40)}")
print(f"FA   ends    {q([r['fa_off_ms'] for r in rows])}   within 40: {within([r['fa_off_ms'] for r in rows], 40)}")
if auto:
    print(f"AUTO onsets  {q([r['auto_on_ms'] for r in rows])}   within 40: {within([r['auto_on_ms'] for r in rows], 40)}")
    print(f"AUTO ends    {q([r['auto_off_ms'] for r in rows])}   within 40: {within([r['auto_off_ms'] for r in rows], 40)}")
print("\n── the ear vs her envelope ──")
print(f"onset sits under the word's peak by   {q([r['on_db_under_peak'] for r in rows])} dB")
print(f"release sits under the word's peak by {q([r['off_db_under_peak'] for r in rows])} dB")
print(f"hand onset → nearest detected attack   {q([r['nearest_attack_ms'] for r in rows])} ms · within 40: {within([r['nearest_attack_ms'] for r in rows], 40)} · within 100: {within([r['nearest_attack_ms'] for r in rows], 100)}")
gaps = [r["gap_after"] for r in rows if r["gap_after"] is not None]
print(f"gap to the next word: contiguous (≤15 ms) {sum(g <= 0.015 for g in gaps)}, 15–150 ms {sum(0.015 < g <= 0.15 for g in gaps)}, 150–350 ms {sum(0.15 < g <= 0.35 for g in gaps)}, >350 ms (breath) {sum(g > 0.35 for g in gaps)} of {len(gaps)}")
print(f"durations: {q([r['dur'] * 1000 for r in rows])} ms · shortest: " + ", ".join(f"{r['text']}@{r['from']:.2f} {r['dur']*1000:.0f}" for r in sorted(rows, key=lambda r: r['dur'])[:5]))

# ── which onset policy would have matched the ear? (scored against the hand onsets) ──
print("\n── onset policies scored against the ear (share of words within 40 / 80 ms) ──")
def score(pred):
    d = [abs(r["from"] - p) * 1000 for r, p in zip(rows, pred) if p is not None]
    return f"within 40: {sum(x <= 40 for x in d):3d}/{len(d)}   within 80: {sum(x <= 80 for x in d):3d}/{len(d)}   median {np.median(d):.0f} ms"
def snapped(src, win):
    out = []
    for r in rows:
        s = src(r)
        if s is None: out.append(None); continue
        o, dv = near_on(s); out.append(o if abs(dv) <= win else s)
    return out
fa_on = lambda r: fa[r["i"]]["from"] if r["i"] < len(fa) else None
au_on = lambda r: auto[r["i"]]["fromMs"] / 1000 if auto and r["i"] < len(auto) else None
print(f"FA raw                    {score([fa_on(r) for r in rows])}")
for win in (0.04, 0.08, 0.12, 0.2):
    print(f"FA → attack within {int(win*1000):3d} ms  {score(snapped(fa_on, win))}")
if auto:
    print(f"AUTO (shape pass)         {score([au_on(r) for r in rows])}")
    print(f"AUTO → attack within 100  {score(snapped(au_on, 0.1))}")

# ── quality pass ──
print("\n── quality pass on the hand bounds ──")
late, cut, short, seams, snaps = [], [], [], [], []
for r in rows:
    # an attack 30–120 ms BEFORE the hand onset, with the hand onset already loud: the consonant was left out
    if -0.12 <= r["nearest_attack_ms"] / 1000 <= -0.03 and r["on_db_under_peak"] < 6: late.append(r)
    # released while still within 6 dB of the peak and nothing follows for 150 ms: a held note cut short
    if r["off_db_under_peak"] < 6 and (r["gap_after"] is None or r["gap_after"] > 0.15) and r["peak_db_over_floor"] > 12: cut.append(r)
    if r["dur"] < 0.06: short.append(r)
    if r["gap_after"] is not None and 0 < r["gap_after"] <= 0.03: seams.append(r)
    if 0 < abs(r["nearest_attack_ms"]) <= a.snap_ms: snaps.append(r)
def show(label, rs, fmt):
    print(f"{label} ({len(rs)}):" + ("" if rs else " none"))
    for r in rs[:14]: print("   " + fmt(r))
    if len(rs) > 14: print(f"   … {len(rs) - 14} more")
show("onset misses an attack just before it", late, lambda r: f"{r['text']!r}@{r['from']:.2f}  attack {r['nearest_attack_ms']:+d} ms, onset {r['on_db_under_peak']} dB under peak")
show("released inside a loud vowel", cut, lambda r: f"{r['text']!r}@{r['from']:.2f}  end {r['off_db_under_peak']} dB under peak, {r['gap_after'] or 999:.2f} s before the next word")
show("shorter than 60 ms", short, lambda r: f"{r['text']!r}@{r['from']:.2f}  {r['dur']*1000:.0f} ms")
show("tiny gaps (≤30 ms) that read as contiguous", seams, lambda r: f"{r['text']!r}@{r['from']:.2f}  {r['gap_after']*1000:.0f} ms")
show(f"onsets within {a.snap_ms:.0f} ms of a detected attack (snap candidates)", snaps, lambda r: f"{r['text']!r}@{r['from']:.2f}  {r['nearest_attack_ms']:+d} ms")

# ── nudges: the onset moves back to the attack it missed; a loud line-final release runs out to the fall ──
nudges = {}                                   # word index → (new from | None, new to | None)
for r in late: nudges[r["i"]] = (round(r["from"] + r["nearest_attack_ms"] / 1000, 3), None)
for r in cut:
    s = r["to"]
    while s < t[-1] - 0.02 and db_at(s) > peak_in(r["from"], r["to"]) - 12: s += 0.01
    nudges[r["i"]] = (nudges.get(r["i"], (None, None))[0], round(min(s, r["to"] + 1.0), 3))
for r in snaps:
    if r["i"] not in nudges: nudges[r["i"]] = (round(r["from"] + r["nearest_attack_ms"] / 1000, 3), None)
if a.apply and nudges:
    spec = json.load(open(os.path.join(SY, "spec.json")))
    moved = []
    for tk in spec["takes"]:
        wf = os.path.join(SY, f"words-{tk['id']}.json"); sp = os.path.join(SY, f"sylls-{tk['id']}.json")
        if not os.path.exists(sp): continue
        seed = json.load(open(wf)); off = seed["offsetSec"]; sy = json.load(open(sp))
        by_i = {seed["wordIndex"][wi]: wi for wi in range(len(seed["wordIndex"]))}
        dirty = False
        for i, (nf, nt) in nudges.items():
            if i not in by_i: continue
            wi = by_i[i]; mine = [s for s in sy["sylls"] if s["wi"] == wi]
            if not mine: continue
            text = seed["words"][wi]["text"] if wi < len(seed["words"]) else "?"
            if nf is not None:
                s0 = min(mine, key=lambda s: s["fromMs"]); old = s0["fromMs"]; new = round((nf - off) * 1000)
                s0["fromMs"] = new; seed["words"][wi]["fromMs"] = new
                prevs = [s for s in sy["sylls"] if s["wi"] == wi - 1]       # a joined neighbour's end rides along
                if prevs:
                    p = max(prevs, key=lambda s: s["toMs"])
                    if abs(p["toMs"] - old) <= 15: p["toMs"] = new; seed["words"][wi - 1]["toMs"] = new
                moved.append(f"{text}@{nf:.2f} onset {new - old:+d} ms"); dirty = True
            if nt is not None:
                s1 = max(mine, key=lambda s: s["toMs"]); old = s1["toMs"]; new = round((nt - off) * 1000)
                s1["toMs"] = new; seed["words"][wi]["toMs"] = new
                moved.append(f"{text}@{off + old / 1000:.2f} release {new - old:+d} ms"); dirty = True
        if dirty:
            json.dump(sy, open(sp, "w"), indent=2, sort_keys=True); json.dump(seed, open(wf, "w"), indent=1)
    print(f"\napplied {len(moved)} nudges — rerun sylla-collect.py + word-times.py --fa (and relaunch SyllaWizard):")
    for m in moved: print("   " + m)
