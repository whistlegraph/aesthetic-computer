#!/usr/bin/env python3
"""notes.py — FIGURE OUT THE NOTES (the flwe move, in the xpld frame).

Derives the xpld scale FROM THE DATA and charts every sung word onto the
130.8 BPM clickvox grid. The take's own frame: tonic 188.3 Hz — F#3
about +31 cents sharp of concert; all five takes cluster 187.3-190.6 Hz
(takes.json), so the frame is stable across two months.

The scale: key.json's frame-level derivation is the scale of record —
1 2 3 5 6 b7 in F#: F# G# A# C# D# E, MIXOLYDIAN WITH NO 4TH. This
pass re-probes it (pyin over the sung region folded to pitch classes in
the 188.3 Hz frame) as a cross-check: the top four ranked degrees must
be the tonal spine 1 6 3 b7 (they are — tonic, both thirds of the
mixolydian sixth chord, and the closing b7; the remaining degrees sit
in a band the probe's resolution can't order). The b7 matters: the song
ENDS on it ("do you see" -> E3, the question left open) and no chart
pin is allowed to resolve it to the tonic.

Per word: the pitch is RE-MEASURED here, not read off melody.json — its
whole-word medians blur a scooped onset into the note (the flwe fix).
Each word's f0 is the median of the LAST 60% of its confidently-voiced
pyin frames — the sustain, past the scoop. Target = nearest scale tone
in the tonic frame, any octave.

Per phrase (clickvox.json's grid groups): word onsets are distributed
derive_units-style — the phrase span scales to a whole number of beats,
each word's duration quantizes to 8ths (min a half-beat) — so onsets
land on-grid by construction and the chart is a score, not a
transcription.

THE DUBS (takes.py's better_elsewhere, with the noise column read):
four phrases carry a "dub" block — where the SAMPLE BANK should carve
the phrase from instead:

  Locking the doorway little doggy   -> 7258670… (the quiet July room
        beats the flagged 7278943… on every column: more voiced, closer
        to the grid, ten times less hiss)
  Safely in a bubble that surrounds  -> 7278943… (only take that matched;
        more voiced, closer to grid — mind its HF floor)
  Gee defends me in my bubble        -> 7278943… ("This emoji defends
        me" — the pun aligning with itself)
  do you see                         -> 7258670… (the cleanest close:
        grid_dev 21 vs 30, room 10x quieter)

The RENDER does not dub: measured here, all four alternates sing those
phrases in a different register/contour (e.g. 7278943's 'Safely in a
bubble' orbits 270-380 Hz where the spine sings ~165-250) — regulating
that onto the spine's chart would mean 500-1600-cent pulls, which is no
longer intonation repair. So the chart's src stays the primary
(unbroken-take philosophy) and each dub block records the alternate's
span, its whisper words DP-aligned (monotonic best-similarity) to the
chart words, and hz_dub — the sustain median of what that take actually
sings — for the bank's manifest and any future arrangement that wants
the alternate color. Every phrase carries tail_end_s: where the next
phrase opens in the take, so a phrase tail never plays the next
phrase's opening (halo3's lesson).

Writes analysis/notes.json (the word-level receipt) and
analysis/melody-chart.json (the general melody chart — what the
aesthetivox renders and any future arrangement reads).

  ../../.venv/bin/python3 bin/notes.py
"""
import json, os, re
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
ANA = os.path.join(LANE, "analysis")
RAW = os.path.join(ANA, "whisper-raw")
PRIMARY = "7275499036398865706"
SRC = os.path.join(LANE, "source", f"xpld-{PRIMARY}.wav")

TONIC = 188.3                    # F#3 +31 cents — the take's own frame
TONIC_MIDI = 54                  # F#3, for naming in the tonic frame
BPM = 130.8
BEAT = 60.0 / BPM
SONG = (6.0, 99.2)               # the primary's sung region

NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
DEGREE_NAMES = ["1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"]

# the audio dubs: chart phrase text -> take id (see docstring)
DUBS = {
    "Locking the doorway little doggy": "7258670360357276970",
    "Safely in a bubble that surrounds me": "7278943795482283307",
    "Gee defends me in my bubble": "7278943795482283307",
    "do you see": "7258670360357276970",
}


def st_to_name(st):
    """Note name of `st` semitones from tonic, in the tonic frame
    (where F#3 means 188.3 Hz, +31c sharp of concert)."""
    k = TONIC_MIDI + int(round(st))
    return f"{NOTE_NAMES[k % 12]}{k // 12 - 1}"


# ── the scale, from the frames ────────────────────────────────────────
y, sr = librosa.load(SRC, sr=22050, mono=True)
song = y[int(SONG[0] * sr):int(SONG[1] * sr)]
f0, vf, vp = librosa.pyin(song, sr=sr, fmin=60, fmax=600,
                          frame_length=2048, hop_length=256)
v = f0[vf & (vp > 0.6)]
v = v[np.isfinite(v)]
pc = np.mod(1200.0 * np.log2(v / TONIC), 1200.0)
mass = np.zeros(12)
for d in range(12):
    dev = np.mod(pc - d * 100 + 600, 1200) - 600
    mass[d] = np.sum(np.abs(dev) < 50)
mass /= mass.sum()
order = np.argsort(mass)[::-1]
# The scale of record is key.json's frame-level derivation (1 2 3 5 6 b7);
# this pass re-probes as a cross-check: the top of the ranking must be the
# same tonal spine (1 6 3 b7 — tonic, both thirds of the mixolydian sixth
# chord, and the closing b7) or something moved under us.
SCALE = list(json.load(open(os.path.join(ANA, "key.json")))["scale_semitones"])
spine = [int(d) for d in order[:4]]
assert set(spine) <= set(SCALE), \
    f"scale cross-check failed: probe spine {spine} not inside {SCALE}"

# ── per-word targets (re-measured: sustain median, not whole-word) ────
melody = json.load(open(os.path.join(ANA, "melody.json")))
click = json.load(open(os.path.join(ANA, "clickvox.json")))
sung = [w for w in melody["words"] if w.get("sung")]

# one pyin pass over the whole take, wide enough for the real range
# (F#2 fry at the name-drop, G4 at the Air-pockets peak)
f0w, vfw, vpw = librosa.pyin(y, sr=sr, fmin=60, fmax=600,
                             frame_length=2048, hop_length=256)
tw = librosa.times_like(f0w, sr=sr, hop_length=256)

# One pin: the closing "see" measures st -2.7 — a coin-flip between D#3
# (-3, the 6th) and E3 (-2, the b7) that falls 30c on the D# side only
# because the approach droops. melody.json's whole-word median hears E3
# -20c and key.json's cadence table has the final "do you see" landing
# on E3 with "star" and "oh" — the b7 question the song refuses to
# resolve. Pinned to -2, NOT to the tonic.
PINS = {("see", 98.54): -2.0}

steps = np.array([s + 12 * o for o in range(-3, 4) for s in SCALE], float)


def sustain_hz(f0a, vfa, vpa, ta, start, end, fallback=None):
    """Median of the last 60% of a span's confidently-voiced frames."""
    m = (ta >= start) & (ta <= max(end, start + 0.08)) & vfa & (vpa > 0.5)
    vv = f0a[m]
    vv = vv[np.isfinite(vv)]
    if len(vv) < 3:
        return fallback
    return float(np.median(vv[int(len(vv) * 0.4):]))


ANCHOR = click["anchor_sec"]
notes = []
for w in sung:
    entry = {
        "word": w["word"], "start": w["start"], "end": w["end"],
        "beat_measured": round((w["start"] - ANCHOR) / BEAT, 2),
        "dur_beats_measured": round((w["end"] - w["start"]) / BEAT, 2),
    }
    hz = sustain_hz(f0w, vfw, vpw, tw, w["start"], w["end"],
                    fallback=w.get("hz"))
    if hz:
        st = 12.0 * np.log2(hz / TONIC)
        tgt = PINS.get((w["word"], w["start"]),
                       float(steps[np.argmin(np.abs(steps - st))]))
        entry.update(
            hz_measured=round(hz, 1),
            st_measured=round(st, 2),
            st_target=tgt,
            note=st_to_name(tgt),
            degree=DEGREE_NAMES[int(tgt) % 12],
            cents_from_tonic=int(round(tgt * 100)),
            cents_err=int(round((st - tgt) * 100)),
            pinned=(w["word"], w["start"]) in PINS or None,
        )
        entry = {k_: v_ for k_, v_ in entry.items() if v_ is not None}
    notes.append(entry)

# ── the chart: phrases onto the grid, derive_units-style ──────────────
chart = []
for p in click["phrases"]:
    ws = [n for n in notes if p["t0"] - 0.01 <= n["start"] < p["t1"]]
    if not ws:
        continue
    span = ws[-1]["end"] - ws[0]["start"]
    beats_total = max(1, round(span / BEAT))
    kk = beats_total * BEAT / span
    acc, units = 0.0, []
    for i, w in enumerate(ws):
        end = ws[i + 1]["start"] if i + 1 < len(ws) else w["end"]
        d = (end - w["start"]) * kk / BEAT
        dq = max(0.5, round(d * 2) / 2.0)
        u = {"word": w["word"], "beat": round(acc, 2), "dur_beats": dq,
             "src": [w["start"], w["end"]]}
        for key in ("note", "degree", "st_target", "cents_from_tonic",
                    "hz_measured", "cents_err"):
            if key in w:
                u[key] = w[key]
        units.append(u)
        acc += dq
    chart.append({
        "phrase": " ".join(w["word"] for w in ws),
        "beat": p["beat"],
        "beats_total": round(acc, 2),
        "t0": p["t0"], "t1": p["t1"],
        "take": PRIMARY,
        "words": units,
    })

# tail_end_s: the next phrase's opening in the PRIMARY timeline
for i, ph in enumerate(chart):
    if i + 1 < len(chart):
        ph["tail_end_s"] = chart[i + 1]["words"][0]["src"][0]

# ── the dubs: move src spans to the better take ───────────────────────
def load_words(path):
    """whisper.cpp -ml 1 tokens -> words (takes.py's parser)."""
    segs = json.loads(open(path, "rb").read().decode(
        "utf-8", errors="replace"))["transcription"]
    words = []
    for s in segs:
        txt = s["text"]
        if not txt.strip() or txt.strip() in ".,!?":
            continue
        t0, t1 = s["offsets"]["from"] / 1000, s["offsets"]["to"] / 1000
        if words and not txt.startswith(" "):
            words[-1]["word"] += txt
            words[-1]["end"] = t1
        else:
            words.append({"word": txt.strip(), "start": t0, "end": t1})
    return [w for w in words if re.sub(r"[^a-z]", "", w["word"].lower())]


def word_sim(a, b):
    import difflib
    a = re.sub(r"[^a-z]", "", a.lower())
    b = re.sub(r"[^a-z]", "", b.lower())
    return difflib.SequenceMatcher(None, a, b).ratio()


def align(chart_words, dub_words):
    """Monotonic DP: each chart word -> a distinct, increasing dub word,
    maximizing summed similarity (tiny early-match bias)."""
    n, m = len(chart_words), len(dub_words)
    NEG = -1e9
    best = np.full((n, m), NEG)
    back = np.full((n, m), -1, dtype=int)
    for j in range(m):
        best[0, j] = word_sim(chart_words[0], dub_words[j]) - 0.001 * j
    for i in range(1, n):
        run = NEG
        arg = -1
        for j in range(i, m):
            if best[i - 1, j - 1] > run:
                run, arg = best[i - 1, j - 1], j - 1
            best[i, j] = run + word_sim(chart_words[i], dub_words[j]) \
                - 0.001 * j
            back[i, j] = arg
    j = int(np.argmax(best[n - 1]))
    picks = [0] * n
    for i in range(n - 1, -1, -1):
        picks[i] = j
        j = back[i, j] if i else -1
    return picks


takes_json = json.load(open(os.path.join(ANA, "takes.json")))
dub_cache = {}
for ph in chart:
    tid = DUBS.get(ph["phrase"])
    if tid is None:
        continue
    if tid not in dub_cache:
        yd, srd = librosa.load(os.path.join(LANE, "source", f"xpld-{tid}.wav"),
                               sr=22050, mono=True)
        f0d, vfd, vpd = librosa.pyin(yd, sr=srd, fmin=60, fmax=600,
                                     frame_length=2048, hop_length=256)
        dub_cache[tid] = dict(
            words=load_words(os.path.join(RAW, f"xpld-{tid}.json")),
            f0=f0d, vf=vfd, vp=vpd,
            t=librosa.times_like(f0d, sr=srd, hop_length=256))
    dc = dub_cache[tid]
    row = next(r for r in takes_json["phrases"] if r["phrase"] == ph["phrase"])
    mt0, mt1 = row["matches"][tid]["t"]
    cand = [w for w in dc["words"] if mt0 - 0.6 <= w["start"] <= mt1 + 0.6]
    picks = align([u["word"] for u in ph["words"]],
                  [w["word"] for w in cand])
    dwords = []
    for u, j in zip(ph["words"], picks):
        dw = cand[j]
        d = {"word": u["word"], "dub_word": dw["word"],
             "src": [round(dw["start"], 3), round(dw["end"], 3)]}
        hz = sustain_hz(dc["f0"], dc["vf"], dc["vp"], dc["t"],
                        dw["start"], dw["end"])
        if hz:
            st = 12.0 * np.log2(hz / TONIC)
            near = float(steps[np.argmin(np.abs(steps - st))])
            d.update(hz_dub=round(hz, 1), note_dub=st_to_name(near),
                     st_dub=round(st, 2))
            if "st_target" in u:
                d["cents_from_chart"] = int(round(
                    (st - u["st_target"]) * 100))
        dwords.append(d)
    t0d = min(d["src"][0] for d in dwords)
    t1d = max(d["src"][1] for d in dwords)
    nxt = [w["start"] for w in dc["words"] if w["start"] > t1d + 0.05]
    ph["dub"] = {
        "take": tid,
        "matched_text": row["matches"][tid]["text"],
        "t": [round(t0d, 3), round(t1d, 3)],
        "tail_end_s": round(min(nxt), 3) if nxt else None,
        "why": row["matches"][tid],
        "words": dwords,
    }

receipt = {
    "source": os.path.basename(SRC),
    "tonic_hz": TONIC,
    "tonic_note": "F#3 +31c",
    "bpm": BPM,
    "method": ("scale of record = key.json's frame-level derivation, "
               "cross-checked here by a pyin probe (vp>0.6) over 6-99.2s "
               "folded to pitch classes in the 188.3 Hz frame (top-4 "
               "ranked degrees must be the 1 6 3 b7 spine); word targets "
               "= nearest scale tone to the sustain median (last 60% of "
               "confidently-voiced frames)"),
    "degree_mass": {DEGREE_NAMES[d]: round(float(mass[d]), 3)
                    for d in range(12)},
    "scale_semitones": SCALE,
    "scale_degrees": [DEGREE_NAMES[d] for d in SCALE],
    "scale_note": ("1 2 3 5 6 b7 — F# G# A# C# D# E: mixolydian with no "
                   "4th; the b7 (E) carries the closing question and the "
                   "final 'do you see' stays on it, unresolved"),
    "voiced_frames": int(len(v)),
    "dubs": DUBS,
    "words": notes,
}
json.dump(receipt, open(os.path.join(ANA, "notes.json"), "w"), indent=1)
json.dump({
    "source": os.path.basename(SRC),
    "primary": PRIMARY,
    "tonic_hz": TONIC, "bpm": BPM, "anchor_sec": ANCHOR,
    "scale_semitones": SCALE,
    "note": ("the general melody chart: each phrase starts on its clickvox "
             "beat; word onsets are on-grid by construction (8th-note "
             "quantize, min half a beat); src is the primary take's span "
             "(the render never dubs — the alternates sing these phrases "
             "in another register); a 'dub' block marks where the sample "
             "bank carves the phrase from instead; tail_end_s = next "
             "phrase's opening in the primary"),
    "phrases": chart,
}, open(os.path.join(ANA, "melody-chart.json"), "w"), indent=1)

print(f"scale: {[DEGREE_NAMES[d] for d in SCALE]}  (mass "
      f"{[round(float(mass[d]), 3) for d in SCALE]})")
for ph in chart:
    tag = "" if ph["take"] == PRIMARY else f"  DUB<{ph['take'][:7]}…>"
    line = " ".join(f"{u['word']}:{u.get('note','?')}({u['dur_beats']})"
                    for u in ph["words"])
    print(f"beat {ph['beat']:3d} [{ph['beats_total']:4.1f}b]{tag} {line}")
print("wrote analysis/notes.json + analysis/melody-chart.json")
