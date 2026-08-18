# harvest.py — fetch, transcribe, measure and slice the `fact` whistlegraph.
#
# "factory 🏭 cookie-cutter🎄personalities" — the whistlegraph index tags
# TWENTY-ONE posts with the `fact` work. Three feed this lane:
#
#   6925546179275099397  2021-02-04  the original, 50.8M views — and the
#                        only take with the count-in ("a one, two, ready,
#                        and...") — a machine being switched on
#   6928682624529485062  2021-02-13  "factory cookie cutter personalities"
#                        — a darker, slower pressing (cutter lands on B2)
#   7030651123325308165  2021-11-15  the caption-with-the-whole-poem take
#                        — brighter and quicker (factory on C#4)
#
# TikTok blocks this IP, so mp4s come from the AC asset mirror that
# posts.json already points at. Words are timestamped with whisper.cpp
# (whisper-cli, ggml-small, word-level -ml 1), edges refined by RMS trim,
# f0 measured with librosa.pyin — same probe chain as pop/cult/alt/.
#
# What the probe found, and what the render is built on:
#   · tempo — median syllable IOI 0.299 s on the original = eighth notes
#     at 100.3 BPM; librosa.beat_track says 101.4 / 100.4 on the two 2021-02
#     takes. The chant lives at 100 BPM, and each poem line spans almost
#     exactly two bars there (4.15 / 4.86 / 4.82 s against 4.80).
#   · key — the chant's pitch set is D·F·G·A·Bb·C rooted hard on D3
#     (~147 Hz: factory, cookie, break, ning, I all sit there). D minor.
#   · the poem — factory / cookie cutter / personalities // we must break
#     free from the states that we're in // spinning away, I hear a bird.
#
# Writes 48 kHz mono PCM16 WAVs to samples/ (trimmed, normalised to 0.90,
# cosine-ramped so no slice can click) and the receipt to harvest.json.
#
#   cd pop/factory && ../.venv/bin/python bin/harvest.py

import json, os, subprocess, sys
import numpy as np, librosa, soundfile as sf

SR = 48000
MIRROR = "https://assets.aesthetic.computer/whistlegraph/index/posts"
WHISPER_MODEL = os.path.expanduser("~/.whisper-models/ggml-small.bin")

os.makedirs("source/txt", exist_ok=True)
os.makedirs("samples", exist_ok=True)

# ── fetch + decode + transcribe (idempotent; every CUTS id) ───────────
def fetch(id):
    mp4, wav, txt = f"source/{id}.mp4", f"source/{id}.wav", f"source/txt/{id}.json"
    if not os.path.exists(mp4):
        print(f"→ fetching {id}.mp4 from the AC mirror")
        subprocess.run(["curl", "-sL", "-o", mp4, f"{MIRROR}/{id}.mp4"], check=True)
    if not os.path.exists(wav):
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", mp4,
                        "-ac", "1", "-ar", str(SR), wav], check=True)
    if not os.path.exists(txt):
        print(f"→ whisper {id}")
        k16 = f"source/{id}-16k.wav"
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", wav,
                        "-ar", "16000", k16], check=True)
        subprocess.run(["whisper-cli", "-m", WHISPER_MODEL, "-l", "en",
                        "-ml", "1", "-oj", "-of", f"source/txt/{id}",
                        "-f", k16, "--no-prints"],
                       check=True, capture_output=True)
        os.unlink(k16)

# ── probes (pop/cult/alt/analyze.py's, verbatim in spirit) ────────────
def rms_env(y, hop=256):
    return librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0], hop

def refine(y, t0, t1, pad=0.06, thresh_ratio=0.10):
    a = max(0, int((t0 - pad) * SR)); b = min(len(y), int((t1 + pad) * SR))
    seg = y[a:b]
    if len(seg) < 1024: return t0, t1
    e, hop = rms_env(seg)
    peak = e.max()
    if peak <= 0: return t0, t1
    idx = np.where(e >= peak * thresh_ratio)[0]
    if len(idx) == 0: return t0, t1
    return (a + idx[0] * hop) / SR, (a + min(len(seg), (idx[-1] + 2) * hop)) / SR

# @jeffrey: "the vocals keep cutting off". The 10% RMS trim above is right
# for a START edge and wrong for an END: it amputates a vowel's decay at
# about -20 dB. So the end edge FOLLOWS THE DECAY instead — walk forward
# from the refined end while energy keeps falling, stop when it reaches
# the noise floor (3% of peak) or when it turns back UP (the next word's
# onset — these takes are continuous chant, and bleeding the next attack
# into a stamp would double-hit it).
def follow_decay(y, t_end, max_ext=0.30, floor_ratio=0.03):
    a = int(t_end * SR); b = min(len(y), int((t_end + max_ext) * SR))
    seg = y[max(0, a - 2048):b]
    if len(seg) < 2048: return t_end
    e, hop = rms_env(seg)
    start_fr = 2048 // hop
    peak = e[:start_fr + 1].max()
    if peak <= 0: return t_end
    lo = e[start_fr]
    ext = 0
    for f in range(start_fr + 1, len(e)):
        if e[f] < peak * floor_ratio: break          # decay reached the floor
        lo = min(lo, e[f])
        if e[f] > lo * 1.8 and f - start_fr > 3: break   # a new onset — back off
        ext = f - start_fr
    return t_end + ext * hop / SR

def f0_of(y, s, t, fmin=70, fmax=600):
    seg = y[int(s * SR):int(t * SR)]
    if len(seg) < 2048: return None, 0.0
    f0, _, _ = librosa.pyin(seg, fmin=fmin, fmax=fmax, sr=SR, frame_length=2048)
    v = f0[~np.isnan(f0)]
    if len(v) == 0: return None, 0.0
    return float(np.median(v)), float(len(v) / len(f0))

# Trim inside the span, normalise to 0.90 and cosine-ramp head and tail —
# pop/cult/bin/slice.mjs's dress(). The tail ramp is 45 ms now, not 12:
# the slice already ends where the decay ends (follow_decay), so the ramp's
# job is to land that decay, not to amputate it.
def write(name, y, s, t):
    a = max(0, int((s - 0.004) * SR)); b = min(len(y), int((t + 0.020) * SR))
    seg = np.copy(y[a:b])
    peak = np.abs(seg).max()
    if peak > 1e-6: seg *= 0.90 / peak
    ramp = int(0.006 * SR); tail = min(int(0.045 * SR), len(seg) // 3)
    seg[:ramp] *= 0.5 - 0.5 * np.cos(np.pi * np.arange(ramp) / ramp)
    seg[-tail:] *= 0.5 + 0.5 * np.cos(np.pi * np.arange(tail) / tail)
    sf.write(f"samples/{name}.wav", seg, SR, subtype="PCM_16")
    return len(seg) / SR

# ── the cuts ──────────────────────────────────────────────────────────
# Spans start from whisper's word boundaries, then refine() trims them to
# the energy. Take a = the original (count-in + chant), b = the 02-13
# pressing (low), c = the 2021-11 pressing (bright).
CUTS = {
  "6925546179275099397": [
    ("count-in",       "a one, two, ready, and", 0.10, 2.94),
    ("a-one",          "a one",                  0.10, 0.56),
    ("two",            "two",                    0.88, 1.22),
    ("ready",          "ready",                  1.47, 2.20),
    ("and",            "and",                    2.34, 2.94),
    ("factory",        "factory",                3.13, 3.91),
    ("cookie",         "cookie",                 3.91, 4.58),
    ("cutter",         "cutter",                 4.58, 5.28),
    ("personalities",  "personalities",          5.28, 7.28),
    ("line1",          "factory cookie cutter personalities", 3.13, 7.28),
    ("we",             "we",                     7.62, 8.19),
    ("must",           "must",                   8.19, 8.42),
    ("break",          "break",                  8.42, 9.02),
    ("free",           "free",                   9.02, 9.54),
    ("break-free",     "break free",             8.42, 9.54),
    ("from-the-states","from the states",        9.52, 11.14),
    ("that-were-in",   "that we're in",         11.14, 12.55),
    ("line2",          "we must break free from the states that we're in", 7.62, 12.55),
    ("spinning",       "spinning",              13.85, 15.28),
    ("away",           "away",                  15.28, 16.18),
    ("spinning-away",  "spinning away",         13.85, 16.18),
    ("i-hear",         "I hear",                16.52, 17.70),
    ("a-bird",         "a bird",                17.68, 18.70),
    ("bird",           "bird",                  17.82, 18.70),
    ("line3",          "spinning away, I hear a bird", 13.85, 18.70),
    ("chant-full",     "the whole poem",         3.13, 18.70),
  ],
  "6928682624529485062": [
    ("factory-b",       "factory",               0.22, 1.00),
    ("cookie-b",        "cookie",                1.00, 1.94),
    ("cutter-b",        "cutter",                1.94, 2.74),
    ("personalities-b", "personalities",         2.74, 4.72),
    ("line1-b",         "factory cookie cutter personalities", 0.22, 4.72),
    ("line2-b",         "we must break free from the states that we're in", 4.72, 10.56),
    ("bird-b",          "bird (long, low)",     15.23, 17.12),
  ],
  "7030651123325308165": [
    ("factory-c",       "factory",               0.33, 1.11),
    ("cutter-c",        "cutter",                1.78, 2.45),
    ("personalities-c", "personalities",         2.45, 3.90),
    ("line1-c",         "factory cookie cutter personalities", 0.33, 3.90),
    ("spinning-away-c", "spinning away",         9.63, 12.15),
    ("bird-c",          "bird",                 13.83, 14.67),
  ],
  # ── the new dies (bin/survey.py's receipt is survey.json) ───────────
  # d — the 6.7M bright pressing; its "bird" holds for two seconds
  "7029459939408006405": [
    ("factory-d",       "factory",               0.31, 0.87),
    ("cookie-d",        "cookie",                0.87, 1.62),
    ("cutter-d",        "cutter",                1.62, 2.36),
    ("personalities-d", "personalities",         2.36, 4.00),
    ("line2-d",         "we must break free from the states that we're in", 4.00, 9.00),
    ("line3-d",         "spinning away I hear a bird (held)", 9.00, 15.00),
  ],
  # e — the 2024 pressing, LOW (f0 ~134 Hz) and slow; "personality"
  "7427774349865700639": [
    ("factory-e",       "factory (long)",        0.48, 2.00),
    ("cookie-e",        "cookie",                2.00, 2.72),
    ("cutter-e",        "cutter",                2.72, 3.44),
    ("personalities-e", "personality",           3.44, 5.76),
    ("line3-e",         "spinning away, I hear a bird (low)", 12.32, 18.06),
  ],
  # f — 2026, "mhm yaaa factoreyy :) cookay cubber" (f0 ~392 Hz): the
  # mislabeled die. OUT OF SPEC's k=6 copy.
  "7637680702124805407": [
    ("factory-f",       "factoreyy",             1.23, 1.84),
    ("cookie-f",        "cookay",                1.84, 3.49),
    ("cutter-f",        "cubber",                3.49, 5.00),
    ("personalities-f", "personality",           5.00, 7.80),
  ],
  # g — "grab a pen and follow along": a spoken intro, a 3.5 s stretched
  # "spinning", and a chain of birds
  "7079083421674343722": [
    ("heres-the-factory", "here's the factory",  0.17, 2.23),
    ("spinning-g",       "spinning (stretched)", 18.89, 22.35),
    ("bird-chain-g",     "a bird, a bird, a bird, a bird", 26.42, 31.04),
  ],
  # h — the 29 s take that keeps saying bird
  "7079471062013988139": [
    ("bird-chain-h",     "a bird, a bird, a bird", 22.44, 29.16),
  ],
  # the 58 s talk — intercom material, spoken-treated in the score
  "7019362782122380550": [
    ("intercom-wondering", "some of you are probably wondering what a whistlegraph is", 0.07, 2.40),
    ("intercom-sameway",   "it has a score and it can be performed the same way every time", 21.14, 25.36),
  ],
}

report = {}
for id, cuts in CUTS.items():
    fetch(id)
    y, _ = librosa.load(f"source/{id}.wav", sr=SR, mono=True)
    dur = len(y) / SR
    # tempo, two ways — receipts for the 100 BPM decision
    oenv = librosa.onset.onset_strength(y=y, sr=SR, hop_length=256)
    bpm = float(np.atleast_1d(librosa.beat.beat_track(
        onset_envelope=oenv, sr=SR, hop_length=256)[0])[0])
    onsets = librosa.onset.onset_detect(onset_envelope=oenv, sr=SR,
                                        hop_length=256, units="time")
    iois = np.diff(onsets); iois = iois[(iois > 0.15) & (iois < 1.5)]
    ioi = float(np.median(iois)) if len(iois) else None
    words = json.load(open(f"source/txt/{id}.json"))["transcription"]
    segs = []
    for name, label, t0, t1 in cuts:
        s, t = refine(y, t0, t1)
        s = max(s, t0 - 0.06); t = min(t, t1 + 0.08)
        t = follow_decay(y, t)               # ring to the natural end
        f0, vfrac = f0_of(y, s, t)
        wdur = write(name, y, s, t)
        note = librosa.hz_to_note(f0) if f0 else None
        segs.append(dict(file=f"samples/{name}.wav", words=label,
            start=round(s, 3), end=round(t, 3), dur=round(wdur, 3),
            median_f0_hz=round(f0, 1) if f0 else None,
            note=note, voiced_frac=round(vfrac, 2)))
        print(f"  {name:18s} {wdur:5.2f}s  f0={f0 or 0:6.1f} ({note or '—':4s}) "
              f"v={vfrac:.2f}  \"{label}\"")
    report[id] = dict(
        source_mp4=f"{MIRROR}/{id}.mp4",
        duration=round(dur, 2),
        beat_track_bpm=round(bpm, 1),
        median_onset_ioi_s=round(ioi, 3) if ioi else None,
        ioi_as_eighths_bpm=round(60 / ioi / 2, 1) if ioi else None,
        transcript=" ".join(w["text"] for w in words).strip(),
        transcription_method="whisper.cpp (whisper-cli) ggml-small, word-level -ml 1",
        word_timestamps=[dict(t=w["text"],
            start=w["offsets"]["from"] / 1000, end=w["offsets"]["to"] / 1000)
            for w in words if w["text"].strip()],
        samples=segs)

json.dump(dict(
    work="fact",
    poem="factory / cookie cutter / personalities / we must break free from "
         "the states that we're in / spinning away, I hear a bird",
    measured=dict(bpm=100, key="D minor", chant_root_hz=147.0,
        note="syllables are eighths at 100.3 BPM (median IOI 0.299 s); each "
             "poem line spans ~2 bars at 100; pitch set D F G A Bb C on D3"),
    takes=report), open("harvest.json", "w"), indent=2)
print("WROTE harvest.json")
