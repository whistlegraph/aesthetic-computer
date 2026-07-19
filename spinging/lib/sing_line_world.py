#!/usr/bin/env python3
"""
sing_line_world.py — spinging's line-continuous singing engine (round 3).

Adopted from pop/menuband/bin/sing_line_world.py (round 2) as the spinging
core. What round 3 adds, per jeffrey's review of round 2:

  A · GROUND TRUTH FROM TEXT — the plan now carries a choral score sidecar
      (spinging/lib/notation.mjs): per note the expected syllable and its
      {onset, nucleus, coda} phoneme classes from curated IPA (Wiktionary
      first). Segmentation is GUIDED: expected onset consonants anchor burst
      detection, the expected nucleus anchors the vowel, expected codas set
      how much tail to keep. No more guessing phonemes from spectra alone.

  B · GOALPOST SHAPES — reference feature bands (spinging/cache/
      goalposts.json, measured on real sung acapellas) parameterize glides,
      drift and vibrato, and the rendered lead is re-analyzed and gated
      against the p10–p90 bands (conformance report in the stats JSON).

  C · SOUND FIXES
      · --harmony h (beta = 1-h): contour retention is a first-class knob;
        round 3 defaults to ~0.875 lock.
      · de-kermit: harvest f0 clamped to the speaker's real range (60–300 Hz)
        so CheapTrick never sees octave errors; per-line octave transposition
        chosen to MINIMIZE mean |f0 shift| from the spoken take; the formant
        envelope is never touched by f0 moves.
      · consonant→vowel: the first ~50 ms of every vowel (formant transition /
        VOT region) is mapped 1:1 unstretched; f0 glides from the NATURAL
        onset pitch into the plateau over 40–80 ms; only the steady-state
        nucleus middle is stretched — with a strictly MONOTONIC source map
        (interpolate, never tile → no stutter).
      · no pre-transition dip: contour retention tapers to 0 over the last
        80 ms of each note and the first 40 ms of the next; inter-note glides
        are clamped between the two plateaus and forced monotonic; scoops at
        phrase starts only.
      · glitch gate: strictly continuous frame timeline, energy smoothing at
        every composite seam, and a click scan (waveform discontinuity +
        spectral-flux spikes) reported per line.
      · angelic: tight pure plateaus (drift ~8¢), gentle vibrato from the
        reference bands, airy sustains (raised HF aperiodicity on holds), and
        a QUIET self-choir under the lead (unison pair + octave + fifth,
        ±8–15¢, 10–25 ms spread, −10…−13 dB) — still jeffrey singing lead.

Driven by sing-jingle.mjs (pop/menuband/bin) with a JSON plan:

  plan.json = {
    "line_wav": …, "out_wav": …, "lead_wav": …, "phoneme_sidecar": …,
    "score": …line score sidecar from notation.mjs…,
    "goalposts": …spinging/cache/goalposts.json…,
    "line_t0": …, "line_t1": …,
    "harmony": 0.875, "seed": 7,
    "f0_floor": 60, "f0_ceil": 300, "octave_opt": true,
    "choir": true,
    "tweaks": { "drift_scale":1, "glide_scale":1, "vib_depth_scale":1,
                "beta_scale":1, "air_scale":1 },
    "words": [ { "w": "menu", "wordIndex": 0, "srcFromMs": …, "srcToMs": …,
                 "slots": [{"t":…,"dur":…,"midi":…}, …],
                 "hardEnd": …, "phraseStart": true }, … ]
  }

Prints ONE JSON line of stats: per-word map, f0 continuity, line transpose,
conformance report, click scan.
"""
import json
import os
import sys

import numpy as np
import soundfile as sf
import pyworld as pw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vocal_shapes import (FRAME_S, segment_notes, note_features, frame_rms_hf,
                          conformance, click_scan, hz_to_midi as vs_hz_to_midi)

MAX_ONSET_S = 0.22
MAX_CODA_S = 0.26
MAX_BREATH_S = 0.45
GLIDE_SIGMA_S = 0.020
SCOOP_S = 0.07
SCOOP_ST = 0.8
DRIFT_CENTS = 8.0
VIB_HOLD_S = 0.6
VIB_ONSET_S = 0.25
BETA_END_TAPER_S = 0.08     # contour → 0 over the last 80ms of a note
BETA_START_TAPER_S = 0.04   # …and the first 40ms of the next
VOWEL_HEAD_S = 0.05         # unstretched vowel onset (formant transition/VOT)
ONSET_GLIDE_S = 0.06        # natural-pitch → plateau glide


def midi_to_hz(m):
    return 440.0 * (2.0 ** ((np.asarray(m, dtype=np.float64) - 69.0) / 12.0))


def hz_to_midi(hz):
    return 69.0 + 12.0 * np.log2(hz / 440.0)


# ── analysis ───────────────────────────────────────────────────────────────

def analyze(x, fs, f0_floor, f0_ceil):
    f0_raw, t = pw.harvest(x, fs, f0_floor=f0_floor, f0_ceil=f0_ceil,
                           frame_period=FRAME_S * 1000.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    # de-kermit hard wall: stonemask can still jump the fence
    f0 = np.where((f0 > 0) & ((f0 < f0_floor * 0.9) | (f0 > f0_ceil * 1.15)), 0.0, f0)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=f0_floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=f0_floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    return f0, t, sp, ap, fft_size


def frame_features(x, fs, f0, sp):
    n = len(f0)
    hop = int(round(fs * FRAME_S))
    win = hop * 2
    rms = np.zeros(n)
    for i in range(n):
        a = max(0, i * hop - win // 2)
        b = min(len(x), i * hop + win // 2)
        if b > a:
            rms[i] = np.sqrt(np.mean(x[a:b] ** 2))
    nbins = sp.shape[1]
    nyq = fs / 2.0
    k1 = int(nbins * 1000.0 / nyq)
    k4 = int(nbins * 4000.0 / nyq)
    tot = sp.sum(axis=1) + 1e-12
    low_dom = sp[:, :k1].sum(axis=1) / tot
    hf_ratio = sp[:, k4:].sum(axis=1) / tot
    lsp = np.log(sp + 1e-12)
    d = np.diff(lsp, axis=0)
    flux = np.zeros(n)
    flux[1:] = np.maximum(d, 0).mean(axis=1)
    return rms, flux, low_dom, hf_ratio


def classify_frames(f0, rms, flux, low_dom, hf_ratio):
    n = len(f0)
    voiced = f0 > 0
    floor = max(1e-4, 0.05 * np.percentile(rms, 95))
    cls = np.array(["sil"] * n, dtype=object)
    speech = rms > floor
    flux_thr = np.percentile(flux[speech], 80) if speech.any() else np.inf
    for i in range(2, n):
        if flux[i] > flux_thr and rms[max(0, i - 7):max(1, i - 1)].mean() < 2.5 * floor:
            cls[i] = "plosive"
    for i in range(n):
        if cls[i] == "plosive" or not speech[i]:
            continue
        if not voiced[i]:
            cls[i] = "fric" if hf_ratio[i] > 0.10 else "plosive"
        else:
            cls[i] = "vowel" if low_dom[i] > 0.35 else "vcons"
    return cls, voiced, floor


def find_nuclei(w0, w1, n_slots, voiced, rms, low_dom):
    seg = slice(w0, w1)
    v = np.where(voiced[seg], rms[seg] * (0.25 + low_dom[seg]), 0.0)
    if v.max() <= 0:
        step = max(1, (w1 - w0) // max(1, n_slots))
        return [(w0 + k * step, min(w1, w0 + (k + 1) * step)) for k in range(n_slots)], False
    thr = 0.30 * v.max()
    regions = []
    i = 0
    while i < len(v):
        if v[i] > thr:
            j = i
            while j < len(v) and v[j] > thr:
                j += 1
            if (j - i) * FRAME_S >= 0.02:
                regions.append([w0 + i, w0 + j])
            i = j
        else:
            i += 1
    if not regions:
        regions = [[w0, w1]]
    merged = [regions[0]]
    for a, b in regions[1:]:
        if a - merged[-1][1] < 6:
            merged[-1][1] = b
        else:
            merged.append([a, b])
    regions = merged
    while len(regions) > n_slots:
        scores = [v[a - w0:b - w0].sum() for a, b in regions]
        regions.pop(int(np.argmin(scores)))
    while len(regions) < n_slots:
        lens = [b - a for a, b in regions]
        k = int(np.argmax(lens))
        a, b = regions[k]
        mid = (a + b) // 2
        regions[k:k + 1] = [[a, mid], [mid, b]]
    return [(int(a), int(b)) for a, b in regions], True


def trim_silence(a, b, rms, floor, from_left=True, from_right=True):
    while from_left and a < b - 1 and rms[a] < floor:
        a += 1
    while from_right and b > a + 1 and rms[b - 1] < floor:
        b -= 1
    return a, b


# ── guided segmentation (round 3, part A3) ─────────────────────────────────
# Expected phoneme classes from the choral score anchor the boundaries.

def _cluster_budget_s(phones, base=0.10, per=0.055, fric_bonus=0.08):
    if not phones:
        return 0.03
    budget = base + per * (len(phones) - 1)
    if any(p["cls"] in ("fricative", "affricate") for p in phones):
        budget += fric_bonus
    return min(budget, MAX_ONSET_S + 0.06)


def guide_onset(nuc_start, w0, expected, cls, flux, voiced, hf_ratio, rms, floor):
    """Onset cluster window [a, nuc_start) matching the expected consonants."""
    if not expected:                       # vowel-initial → essentially no onset
        a = nuc_start
        while a > w0 and a > nuc_start - 4 and voiced[a - 1] and cls[a - 1] != "vowel":
            a -= 1
        return a, nuc_start
    budget = int(_cluster_budget_s(expected) / FRAME_S)
    a = nuc_start
    lim = max(w0, nuc_start - budget)
    # walk back over consonant-ish frames
    while a > lim and cls[a - 1] in ("plosive", "fric", "vcons"):
        a -= 1
    first = expected[0]
    if first["cls"] in ("plosive", "affricate") and not first["voiced"]:
        # anchor on the LAST burst before the vowel; include ~25ms closure
        win_a = max(w0, nuc_start - budget - 6)
        bursts = [i for i in range(win_a, nuc_start)
                  if cls[i] == "plosive" and flux[i] > 0]
        if bursts:
            a = min(a, max(win_a, bursts[-1] - 5))
    elif first["cls"] == "fricative" and not first["voiced"]:
        # extend through the unvoiced high-band run (s-clusters are long)
        while a > lim and not voiced[a - 1] and hf_ratio[a - 1] > 0.06 and rms[a - 1] > floor * 0.5:
            a -= 1
    elif first["cls"] in ("nasal", "approximant") or first["voiced"]:
        # voiced sonorant onset: must stay voiced
        while a < nuc_start and not voiced[a]:
            a += 1
    return a, nuc_start


def guide_coda(nuc_end, w1, expected, cls, voiced, hf_ratio, rms, floor):
    if not expected:                       # open syllable → tiny release only
        return nuc_end, min(w1, nuc_end + int(0.04 / FRAME_S))
    budget = int(_cluster_budget_s(expected, base=0.12, per=0.06) / FRAME_S)
    b = nuc_end
    lim = min(w1, nuc_end + budget)
    while b < lim and cls[b] in ("plosive", "fric", "vcons"):
        b += 1
    last = expected[-1]
    if last["cls"] == "fricative":
        while b < lim and not voiced[b] and hf_ratio[b] > 0.06 and rms[b] > floor * 0.4:
            b += 1
    return nuc_end, b


# ── curve helpers ──────────────────────────────────────────────────────────

def smooth_runs(curve, mask, sigma_frames):
    out = curve.copy()
    r = int(max(1, round(3 * sigma_frames)))
    kernel = np.exp(-0.5 * (np.arange(-r, r + 1) / sigma_frames) ** 2)
    kernel /= kernel.sum()
    i = 0
    n = len(curve)
    while i < n:
        if not mask[i]:
            i += 1
            continue
        j = i
        while j < n and mask[j]:
            j += 1
        seg = curve[i:j]
        if len(seg) > 2:
            pad = np.pad(seg, (r, r), mode="edge")
            out[i:j] = np.convolve(pad, kernel, mode="valid")
        i = j
    return out


def monotone_map(a0, a1, n_out, rng, jitter=0.6):
    """Strictly monotone fractional source positions a0→a1 over n_out frames."""
    if n_out <= 1:
        return np.array([a0], dtype=float)[:n_out] if n_out else np.array([])
    base = np.linspace(a0, a1, n_out)
    if a1 <= a0 + 1 or n_out < 8:
        return base
    j = rng.standard_normal(max(2, n_out // 6))
    j = np.interp(np.linspace(0, 1, n_out), np.linspace(0, 1, len(j)), j) * jitter
    pos = base + j
    pos = np.maximum.accumulate(pos)                    # never runs backwards
    pos = a0 + (pos - pos[0]) * ((a1 - a0) / max(1e-9, pos[-1] - pos[0]))
    return np.clip(pos, a0, a1)


def db(g):
    return 10.0 ** (g / 20.0)


# ── main ───────────────────────────────────────────────────────────────────

def main():
    plan = json.loads(open(sys.argv[1]).read())
    x, fs = sf.read(plan["line_wav"], dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0_floor = float(plan.get("f0_floor", 60.0))
    f0_ceil = float(plan.get("f0_ceil", 300.0))
    tweaks = plan.get("tweaks", {}) or {}
    drift_scale = float(tweaks.get("drift_scale", 1.0))
    glide_scale = float(tweaks.get("glide_scale", 1.0))
    vib_depth_scale = float(tweaks.get("vib_depth_scale", 1.0))
    beta_scale = float(tweaks.get("beta_scale", 1.0))
    air_scale = float(tweaks.get("air_scale", 1.0))

    harmony = float(plan.get("harmony", 0.875))
    beta = max(0.0, min(1.0, (1.0 - harmony))) * beta_scale

    gp = None
    if plan.get("goalposts") and os.path.exists(plan["goalposts"]):
        gp = json.loads(open(plan["goalposts"]).read())
    bands = (gp or {}).get("bands", {})
    vib_hz = float(bands.get("vib_rate_hz", {}).get("p50", 5.5))
    vib_cents = min(25.0, float(bands.get("vib_depth_cents", {}).get("p50", 11.0))) * vib_depth_scale

    score = None
    if plan.get("score") and os.path.exists(plan["score"]):
        score = json.loads(open(plan["score"]).read())
    score_by_word = {}
    if score:
        for nnote in score["notes"]:
            score_by_word.setdefault(nnote["wordIndex"], []).append(nnote)

    f0, t, sp, ap, fft_size = analyze(x, fs, f0_floor, f0_ceil)
    n_src = len(f0)
    rms, flux, low_dom, hf_ratio = frame_features(x, fs, f0, sp)
    cls, voiced, floor = classify_frames(f0, rms, flux, low_dom, hf_ratio)

    vi = np.where(voiced)[0]
    if len(vi) < 5:
        sf.write(plan["out_wav"], np.zeros(16, dtype=np.float32), fs)
        print(json.dumps({"error": "line has almost no voiced frames"}))
        return 1
    log_src = np.interp(np.arange(n_src), vi, np.log(f0[vi]))

    words = plan["words"]
    rng = np.random.default_rng(int(plan.get("seed", 7)))

    # ── segment every word (guided by the choral score) ────────────────────
    segs = []
    sidecar_words = []
    for w in words:
        w0 = max(0, int(round(w["srcFromMs"] / 1000.0 / FRAME_S)))
        w1 = min(n_src, int(round(w["srcToMs"] / 1000.0 / FRAME_S)))
        if w1 <= w0 + 2:
            w1 = min(n_src, w0 + 8)
        w0t, w1t = trim_silence(w0, w1, rms, floor)
        exp_notes = score_by_word.get(w.get("wordIndex", -1), [])
        # distinct sung nuclei: melisma mid/end notes reuse the previous vowel
        nucleus_slot = []           # per slot → nucleus group index
        g = -1
        for k in range(len(w["slots"])):
            mel = exp_notes[k]["melisma"] if k < len(exp_notes) else None
            if mel in ("mid", "end"):
                nucleus_slot.append(max(0, g))
            else:
                g += 1
                nucleus_slot.append(g)
        n_groups = g + 1 if g >= 0 else len(w["slots"])
        nuclei, sung = find_nuclei(w0t, w1t, n_groups, voiced, rms, low_dom)
        if not sung and segs:
            pa = min(segs[-1]["nuclei"][-1][1] + 2, w0t)
            pb = min(n_src, w1t + int(0.10 / FRAME_S))
            if pb > pa + 4 and voiced[pa:pb].any():
                pa2, pb2 = trim_silence(pa, pb, rms, floor)
                nuclei, sung = find_nuclei(pa2, pb2, n_groups, voiced, rms, low_dom)
                w0t, w1t = pa2, pb2
                ps = segs[-1]
                ps["coda"] = (ps["coda"][0], min(ps["coda"][1], nuclei[0][0]))
                ps["w1"] = min(ps["w1"], nuclei[0][0])
        # guided onset / coda from the expected phoneme classes
        exp_onset = exp_notes[0]["phonemes"]["onset"] if exp_notes else None
        exp_coda = exp_notes[-1]["phonemes"]["coda"] if exp_notes else None
        if exp_onset is not None:
            onset = guide_onset(nuclei[0][0], w0t, exp_onset, cls, flux, voiced,
                                hf_ratio, rms, floor)
        else:
            onset = (w0t, nuclei[0][0])
        if (onset[1] - onset[0]) * FRAME_S > MAX_ONSET_S:
            onset = (onset[1] - int(MAX_ONSET_S / FRAME_S), onset[1])
        medials = [(nuclei[k][1], nuclei[k + 1][0]) for k in range(n_groups - 1)]
        if exp_coda is not None:
            coda = guide_coda(nuclei[-1][1], w1t, exp_coda, cls, voiced,
                              hf_ratio, rms, floor)
        else:
            coda = (nuclei[-1][1], w1t)
        if (coda[1] - coda[0]) * FRAME_S > MAX_CODA_S:
            coda = (coda[0], coda[0] + int(MAX_CODA_S / FRAME_S))
        segs.append({"w": w, "onset": onset, "nuclei": nuclei, "medials": medials,
                     "coda": coda, "sung": sung, "w0": w0t, "w1": w1t,
                     "nucleus_slot": nucleus_slot, "exp": exp_notes})
        sidecar_words.append({
            "word": w["w"],
            "expected": [{"syll": e["syllable"],
                          "onset": [p["ipa"] for p in e["phonemes"]["onset"]],
                          "nucleus": e["phonemes"]["nucleus"]["ipa"],
                          "coda": [p["ipa"] for p in e["phonemes"]["coda"]]}
                         for e in exp_notes],
            "onsetMs": [round(onset[0] * FRAME_S * 1000), round(onset[1] * FRAME_S * 1000)],
            "nucleiMs": [[round(a * FRAME_S * 1000), round(b * FRAME_S * 1000)] for a, b in nuclei],
            "codaMs": [round(coda[0] * FRAME_S * 1000), round(coda[1] * FRAME_S * 1000)],
            "classes": "".join({"sil": ".", "plosive": "P", "fric": "F",
                                "vcons": "C", "vowel": "V"}[c] for c in cls[w0t:w1t]),
        })

    with open(plan["phoneme_sidecar"], "w") as fh:
        json.dump({"framePeriodMs": FRAME_S * 1000, "guided": score is not None,
                   "words": sidecar_words}, fh, indent=1)

    # ── per-line octave transposition: minimize |f0 shift| from speech ─────
    det_by_word = []
    for s in segs:
        nuc_v = []
        for a, b in s["nuclei"]:
            nuc_v.extend(f0[a:b][f0[a:b] > 0].tolist())
        det_by_word.append(float(hz_to_midi(np.median(nuc_v))) if nuc_v else None)
    line_transpose = 0
    if plan.get("octave_opt", True):
        best = None
        for k in (-12, 0, 12):
            costs = []
            ok = True
            for s, det in zip(segs, det_by_word):
                mids = [sl["midi"] + k for sl in s["w"]["slots"]]
                if min(mids) < 38 or max(mids) > 67:     # keep inside a real voice
                    ok = False
                if det is not None:
                    costs.append(abs(float(np.mean(mids)) - det))
            if not ok or not costs:
                continue
            c = float(np.mean(costs))
            if best is None or c < best[1]:
                best = (k, c)
        if best:
            line_transpose = best[0]
    if line_transpose:
        for s in segs:
            for sl in s["w"]["slots"]:
                sl["midi"] += line_transpose

    # ── output timeline ────────────────────────────────────────────────────
    line_t0 = float(plan["line_t0"])
    line_t1 = float(plan["line_t1"])
    out_n = int(round((line_t1 - line_t0) / FRAME_S))
    src_pos = np.full(out_n, -1.0)
    natural = np.zeros(out_n, dtype=bool)
    target_midi = np.full(out_n, np.nan)
    in_word = np.zeros(out_n, dtype=bool)
    vib_gain = np.zeros(out_n)
    scoop = np.zeros(out_n)
    word_med = np.zeros(out_n)
    beta_taper = np.ones(out_n)
    onset_glide = np.zeros(out_n)       # log-f0 offsets (natural onset pitch)
    glide_pending = []                  # (out_frame, src_frame) — filled later
    hold_air = np.zeros(out_n)          # airy-sustain gain (angelic)
    seam = np.zeros(out_n, dtype=bool)  # composite seams → energy smoothing

    def of(t_abs):
        return int(round((t_abs - line_t0) / FRAME_S))

    def place(o_start, s_a, s_b, midi=None, med=None):
        L = s_b - s_a
        a = max(0, o_start)
        b = min(out_n, o_start + L)
        if b <= a:
            return
        idx = np.arange(a, b)
        src_pos[idx] = s_a + (idx - o_start)
        natural[idx] = True
        in_word[idx] = True
        if midi is not None:
            target_midi[idx] = midi
        if med is not None:
            word_med[idx] = med

    onset_len = [(s["onset"][1] - s["onset"][0]) * FRAME_S for s in segs]

    stats_words = []
    for wi, s in enumerate(segs):
        w = s["w"]
        slots = w["slots"]
        n_slots = len(slots)
        nuc_v = []
        for a, b in s["nuclei"]:
            nuc_v.extend(f0[a:b][f0[a:b] > 0].tolist())
        med_hz = float(np.median(nuc_v)) if nuc_v else float(np.exp(log_src[s["w0"]]))
        med_log = np.log(med_hz)
        detected = float(hz_to_midi(med_hz)) if nuc_v else None

        hard_end = float(w["hardEnd"])
        if wi + 1 < len(segs):
            hard_end = min(hard_end, segs[wi + 1]["w"]["slots"][0]["t"] - onset_len[wi + 1] - 0.01)
        hard_end = max(hard_end, slots[-1]["t"] + 0.10)

        oa, ob = s["onset"]
        place(of(slots[0]["t"]) - (ob - oa), oa, ob, midi=slots[0]["midi"], med=med_log)

        coda_len = (s["coda"][1] - s["coda"][0]) * FRAME_S
        ngroups = len(s["nuclei"])
        for k in range(n_slots):
            grp = s["nucleus_slot"][k] if k < len(s["nucleus_slot"]) else min(k, ngroups - 1)
            na, nb = s["nuclei"][min(grp, ngroups - 1)]
            # melisma: successive slots share the vowel — split its frames
            melmates = [q for q in range(n_slots)
                        if (s["nucleus_slot"][q] if q < len(s["nucleus_slot"]) else q) == grp]
            if len(melmates) > 1:
                pos_in = melmates.index(k)
                span = nb - na
                na = na + (span * pos_in) // len(melmates)
                nb = s["nuclei"][min(grp, ngroups - 1)][0] + (span * (pos_in + 1)) // len(melmates)
            v_start = slots[k]["t"]
            if k + 1 < n_slots:
                same_group = (s["nucleus_slot"][k + 1] if k + 1 < len(s["nucleus_slot"]) else -1) == grp
                if same_group:
                    med_len = 0.0
                else:
                    med_idx = min(grp, len(s["medials"]) - 1)
                    med_len = ((s["medials"][med_idx][1] - s["medials"][med_idx][0]) * FRAME_S
                               if s["medials"] else 0.0)
                    med_len = min(med_len, max(0.0, slots[k + 1]["t"] - v_start - 0.03))
                v_end = slots[k + 1]["t"] - med_len
            else:
                v_end = hard_end - coda_len
            v_end = max(v_end, v_start + 0.03)
            o_a, o_b = of(v_start), of(v_end)
            o_a = max(0, o_a)
            o_b = min(out_n, max(o_b, o_a + 2))
            n_out = o_b - o_a
            n_nuc = nb - na
            # C3: keep the first ~50ms (formant transition / VOT) unstretched,
            # a shorter natural tail, stretch only the steady middle —
            # STRICTLY monotone (interpolate, never tile).
            head = min(int(VOWEL_HEAD_S / FRAME_S), max(1, n_nuc // 2))
            tail = min(6, max(0, n_nuc // 4))
            if n_out <= n_nuc:
                pos = np.linspace(na, nb - 1, n_out)
            else:
                pos = np.empty(n_out)
                pos[:head] = na + np.arange(head)
                if tail > 0:
                    pos[n_out - tail:] = nb - tail + np.arange(tail)
                mid_out = n_out - head - tail
                mid_a, mid_b = na + head, max(na + head, nb - tail - 1)
                pos[head:head + mid_out] = monotone_map(mid_a, mid_b, mid_out, rng)
            idx = np.arange(o_a, o_b)
            src_pos[idx] = pos[:len(idx)]
            in_word[idx] = True
            target_midi[idx] = slots[k]["midi"]
            word_med[idx] = med_log
            seam[o_a:min(out_n, o_a + 2)] = True
            # C4: contour retention tapers out at note edges
            tt = (idx - o_a) * FRAME_S
            te = (o_b - 1 - idx) * FRAME_S
            beta_taper[idx] = np.clip(tt / BETA_START_TAPER_S, 0, 1) * \
                np.clip(te / BETA_END_TAPER_S, 0, 1)
            # C3: glide from the NATURAL onset pitch into the plateau
            first_of_group = len(melmates) == 1 or melmates.index(k) == 0
            if first_of_group:
                glide_pending.append((o_a, na))
            # vibrato + airy sustain on true holds (angelic)
            hold = v_end - v_start
            if hold >= VIB_HOLD_S:
                vib_gain[idx] = np.clip((tt - 0.12) / VIB_ONSET_S, 0, 1)
            if hold >= 0.35:
                hold_air[idx] = np.clip((tt - 0.10) / 0.35, 0, 1) * air_scale
            if k == 0 and w.get("phraseStart"):
                scoop[idx] = -SCOOP_ST * np.clip(1.0 - tt / SCOOP_S, 0, 1)
            if k + 1 < n_slots and v_end < slots[k + 1]["t"]:
                med_idx = min(grp, len(s["medials"]) - 1)
                if s["medials"]:
                    ma, mb = s["medials"][med_idx]
                    mlen = of(slots[k + 1]["t"]) - o_b
                    if mb - ma > 0 and mlen > 0:
                        place(o_b, mb - min(mb - ma, mlen), mb,
                              midi=slots[k + 1]["midi"], med=med_log)

        ca, cb = s["coda"]
        if cb > ca:
            place(of(hard_end) - (cb - ca), ca, cb, midi=slots[-1]["midi"], med=med_log)
            seam[max(0, of(hard_end) - (cb - ca)):min(out_n, of(hard_end) - (cb - ca) + 2)] = True

        if wi + 1 < len(segs) and segs[wi + 1]["w"].get("phraseStart"):
            ga, gb = s["w1"], segs[wi + 1]["w0"]
            ga, gb = trim_silence(ga, gb, rms, floor * 0.6)
            if gb > ga + 4 and rms[ga:gb].mean() > floor * 0.6:
                blen = min(gb - ga, int(MAX_BREATH_S / FRAME_S))
                nxt_on = of(segs[wi + 1]["w"]["slots"][0]["t"]) - \
                    int(onset_len[wi + 1] / FRAME_S)
                ba = nxt_on - blen - 4
                if ba > of(hard_end) + 2:
                    place(ba, gb - blen, gb)
        stats_words.append({
            "word": w["w"], "targets": [sl["midi"] for sl in slots],
            "detected_midi": None if detected is None else round(detected, 2),
            "shift_st": None if detected is None else
                round(float(np.mean([sl["midi"] for sl in slots])) - detected, 2),
            "onset_ms": round(onset_len[wi] * 1000),
            "coda_ms": round(coda_len * 1000),
            "nuclei": len(s["nuclei"]), "sung": s["sung"],
        })

    # ── continuous target-f0 curve ─────────────────────────────────────────
    mapped = src_pos >= 0
    src_i = np.clip(np.round(src_pos).astype(int), 0, n_src - 1)
    voiced_out = np.zeros(out_n, dtype=bool)
    voiced_out[mapped] = voiced[src_i[mapped]]

    tm = target_midi.copy()
    have = ~np.isnan(tm)
    if have.any():
        ii = np.where(have)[0]
        tm = np.interp(np.arange(out_n), ii, tm[ii])
    step_log = np.log(midi_to_hz(tm))            # unsmoothed plateau steps
    target_log = np.log(midi_to_hz(tm + scoop))
    target_log = smooth_runs(target_log, in_word, GLIDE_SIGMA_S * glide_scale / FRAME_S)

    # C4: inter-note glides clamped between the two plateaus + monotonic —
    # no dip before a transition, by construction.
    R = int(0.10 / FRAME_S)
    steps = np.where(np.abs(np.diff(step_log)) > 1e-9)[0]
    for c in steps:
        if not (in_word[c] and c + 1 < out_n and in_word[c + 1]):
            continue
        A, B = step_log[c], step_log[c + 1]
        a = max(0, c - R)
        b = min(out_n, c + 1 + R)
        while a > 0 and in_word[a - 1] and abs(step_log[a] - A) < 1e-9 and c - a < R:
            a -= 1
        seg = target_log[a:b]
        lo, hi = (A, B) if A < B else (B, A)
        seg = np.clip(seg, lo, hi)
        if B >= A:
            seg = np.maximum.accumulate(seg)
        else:
            seg = np.minimum.accumulate(seg)
        target_log[a:b] = seg

    tt = np.arange(out_n) * FRAME_S
    ph = rng.uniform(0, 2 * np.pi, 2)
    drift = (DRIFT_CENTS * drift_scale / 1200.0) * np.log(2) * \
        (0.6 * np.sin(2 * np.pi * 0.23 * tt + ph[0]) +
         0.4 * np.sin(2 * np.pi * 0.61 * tt + ph[1]))
    vib = (vib_cents / 1200.0) * np.log(2) * vib_gain * np.sin(2 * np.pi * vib_hz * tt)

    resid = np.zeros(out_n)
    fm = mapped & (word_med != 0)
    frac = src_pos - np.floor(src_pos)
    i0 = np.clip(np.floor(src_pos).astype(int), 0, n_src - 1)
    i1 = np.clip(i0 + 1, 0, n_src - 1)
    resid[fm] = ((1 - frac[fm]) * log_src[i0[fm]] + frac[fm] * log_src[i1[fm]]) - word_med[fm]
    resid = resid - np.log(2) * np.round(resid / np.log(2))
    resid = smooth_runs(resid, fm, 2.0)

    beta_arr = beta * beta_taper

    # C3: natural onset pitch → plateau glide (40–80ms), from the source's own
    # folded deviation at each vowel onset, weighted so t=0 is fully natural.
    g_frames = int(ONSET_GLIDE_S * glide_scale / FRAME_S)
    for (o_a, na) in glide_pending:
        if o_a >= out_n:
            continue
        off = resid[min(o_a, out_n - 1)]
        off = float(np.clip(off, -np.log(2) * 0.25, np.log(2) * 0.25))  # ±3 st
        for d in range(g_frames):
            if o_a + d >= out_n:
                break
            wgt = 1.0 - d / g_frames
            onset_glide[o_a + d] += off * (1.0 - beta_arr[o_a + d]) * wgt

    f0_out = np.exp(target_log + beta_arr * resid + onset_glide + drift + vib)
    f0_out[~voiced_out] = 0.0

    # ── sp/ap streams (formants untouched by any f0 move) ──────────────────
    sp_out = np.full((out_n, sp.shape[1]), 1e-12)
    ap_out = np.ones((out_n, ap.shape[1]))
    w0f = (1 - frac[mapped])[:, None]
    w1f = frac[mapped][:, None]
    sp_out[mapped] = w0f * sp[i0[mapped]] + w1f * sp[i1[mapped]]
    ap_out[mapped] = np.clip(w0f * ap[i0[mapped]] + w1f * ap[i1[mapped]], 0.0, 1.0)

    # angelic air: raise HF aperiodicity on sustains (breath, not buzz)
    if hold_air.any():
        nbins = ap_out.shape[1]
        k3 = int(nbins * 3000.0 / (fs / 2.0))
        airy = np.where(hold_air > 0)[0]
        gain = (0.30 * hold_air[airy])[:, None]
        ap_out[airy, k3:] = np.clip(
            ap_out[airy, k3:] + (1.0 - ap_out[airy, k3:]) * gain, 0.0, 1.0)

    y = pw.synthesize(np.ascontiguousarray(f0_out),
                      np.ascontiguousarray(sp_out),
                      np.ascontiguousarray(ap_out), fs,
                      frame_period=FRAME_S * 1000.0)

    # ── unvoiced natural runs: composite ORIGINAL samples back in ──────────
    hop = int(round(fs * FRAME_S))
    comp = natural & mapped & ~voiced_out
    ramp = int(0.008 * fs)
    i = 0
    while i < out_n:
        if not comp[i]:
            i += 1
            continue
        j = i
        while j < out_n and comp[j] and (j == i or abs(src_pos[j] - src_pos[j - 1] - 1) < 0.5):
            j += 1
        if (j - i) >= 2:
            oa, ob = i * hop, min(len(y), j * hop)
            sa = int(round(src_pos[i])) * hop
            sb = sa + (ob - oa)
            if sb <= len(x) and ob <= len(y):
                seg = x[sa:sb].copy()
                L = ob - oa
                r = min(ramp, L // 2)
                if r > 1:
                    fade = 0.5 - 0.5 * np.cos(np.pi * np.arange(r) / r)
                    mixin = np.ones(L)
                    mixin[:r] = fade
                    mixin[L - r:] = fade[::-1]
                    y[oa:ob] = mixin * seg + (1 - mixin) * y[oa:ob]
                else:
                    y[oa:ob] = seg
            seam[max(0, i - 1):min(out_n, i + 2)] = True
            seam[max(0, j - 1):min(out_n, j + 2)] = True
        i = j

    # C5: smooth energy across assembly seams (never touch real attacks hard)
    n_env = len(y) // hop
    env = np.array([np.sqrt(np.mean(y[i * hop:(i + 1) * hop] ** 2)) for i in range(n_env)])
    smoothed = np.convolve(np.pad(env, 3, mode="edge"), np.ones(7) / 7, mode="valid")
    gain_f = np.ones(n_env)
    for i in range(n_env):
        near = seam[max(0, i - 5):i + 5].any() if i < out_n else False
        if near and env[i] > 1e-5:
            gain_f[i] = np.clip(smoothed[i] / env[i], 0.8, 1.25)
    gain_s = np.interp(np.arange(len(y)) / hop, np.arange(n_env), gain_f) \
        if n_env > 1 else np.ones(len(y))
    y = y * gain_s

    ef = int(0.010 * fs)
    if len(y) > 2 * ef:
        y[:ef] *= np.linspace(0, 1, ef)
        y[-ef:] *= np.linspace(1, 0, ef)

    y_lead = y.copy()

    # ── C6: quiet self-choir under the lead ────────────────────────────────
    if plan.get("choir", True):
        layers = [
            {"ratio": 1.0, "cents": +9.0, "ms": 12, "db": -10.0},
            {"ratio": 1.0, "cents": -12.0, "ms": 19, "db": -10.5},
            {"ratio": 2.0, "cents": -8.0, "ms": 15, "db": -11.0},
            {"ratio": 1.5, "cents": +10.0, "ms": 22, "db": -13.0},
        ]
        ap_choir = ap_out.copy()
        nbins = ap_choir.shape[1]
        k3 = int(nbins * 3000.0 / (fs / 2.0))
        ap_choir[:, k3:] = np.clip(ap_choir[:, k3:] + (1 - ap_choir[:, k3:]) * 0.2, 0, 1)
        for lay in layers:
            f0_c = f0_out * lay["ratio"] * (2.0 ** (lay["cents"] / 1200.0))
            yc = pw.synthesize(np.ascontiguousarray(f0_c),
                               np.ascontiguousarray(sp_out),
                               np.ascontiguousarray(ap_choir), fs,
                               frame_period=FRAME_S * 1000.0)
            off = int(lay["ms"] * fs / 1000.0)
            g = db(lay["db"])
            L = min(len(y), len(yc) - 0) - off
            if L > 0:
                y[off:off + L] += g * yc[:L]

    sf.write(plan["out_wav"], y.astype(np.float32), fs)
    if plan.get("lead_wav"):
        sf.write(plan["lead_wav"], y_lead.astype(np.float32), fs)

    # ── QA: percentile conformance vs the goalposts + click scan ───────────
    conf = None
    if bands:
        f0r_raw, tr = pw.harvest(y_lead, fs, f0_floor=55.0, f0_ceil=700.0,
                                 frame_period=FRAME_S * 1000.0)
        f0r = pw.stonemask(y_lead, f0r_raw, tr, fs)
        rms_r, hf_r = frame_rms_hf(y_lead, fs, len(f0r))
        fl = 0.05 * np.percentile(rms_r, 95)
        f0r = np.where(rms_r > fl, f0r, 0.0)
        notes_r = segment_notes(f0r, min_dur_s=0.12)
        feats_r = [note_features(f0r, rms_r, hf_r, a, b) for a, b in notes_r]
        conf = conformance(feats_r, bands)
        conf["_notes_measured"] = len(feats_r)
    clicks = click_scan(y.astype(np.float64), fs)

    j = np.abs(np.diff(np.log(np.where(f0_out > 0, f0_out, np.nan)))) * 1200 / np.log(2)
    j = j[~np.isnan(j)]
    print(json.dumps({
        "words": stats_words,
        "line_transpose": line_transpose,
        "beta": round(beta, 4), "harmony": harmony,
        "f0_jump_max_cents": round(float(j.max()), 1) if len(j) else 0,
        "f0_jump_p95_cents": round(float(np.percentile(j, 95)), 1) if len(j) else 0,
        "conformance": conf,
        "clicks": clicks,
        "out_frames": out_n,
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
