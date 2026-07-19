#!/usr/bin/env python3
"""
sing_line_world.py — phoneme-aware, LINE-CONTINUOUS singing for the Menu Band
jingles. Successor to sing_word_world.py (which pitched one word at a time and
left the joins to audio-domain concatenation — the choppiness jeffrey heard).

jeffrey's spec, verbatim intent: "map the plosives / consonants and vowels —
align and lengthen — master." So per lyric line:

  1 · ONE WORLD analysis of the whole TTS take (harvest → stonemask →
      cheaptrick → d4c, 5ms frames).
  2 · Phoneme segmentation (heuristic stack — no aligner in pop/.venv):
      voiced mask + energy envelope + spectral-flux burst detection for
      plosives, low/high-band balance for fricatives vs sonorants, and a
      vowelness score (voiced · energy · low-band dominance) whose peaks are
      the vowel NUCLEI. Cached as a *.phonemes.json sidecar per line.
  3 · The note lives on the vowel. Consonant clusters keep their NATURAL
      duration: onsets are pickups ending where the vowel starts ON the note
      time; codas attach at the vowel's end, stealing from the note tail.
      Only the vowel nucleus is lengthened — in the WORLD frame domain
      (fractional sp/ap frame interpolation, transitions kept at natural
      rate, slight smooth jitter so held frames don't buzz).
  4 · ONE continuous synthesis per line: a single target-f0 curve — plateaus
      on vowels, ~40-80ms log-space portamento through voiced joins, f0=0 on
      unvoiced frames, onset scoop at phrase starts, ±12-cent slow drift,
      vibrato only on holds ≥ 0.7s — and jeffrey's own micro-contour kept at
      --beta. Unvoiced consonant runs are composited back from the ORIGINAL
      take (they were never warped, so the samples line up 1:1).

Driven by sing-jingle.mjs with a JSON plan:

  sing_line_world.py plan.json

plan.json = {
  "line_wav": …48k mono wav of the TTS take…,
  "out_wav": …, "phoneme_sidecar": …,
  "line_t0": …, "line_t1": …,            # absolute reel-clock span to render
  "beta": 0.25, "seed": 7,
  "words": [ { "w": "menu", "srcFromMs": …, "srcToMs": …,
               "slots": [{"t":…,"dur":…,"midi":…},…],   # absolute times
               "hardEnd": …, "phraseStart": true }, … ]
}

Prints one JSON line of stats (per-word detected midi/shift, f0-continuity
metrics) for the caller's report.
"""
import json
import sys

import numpy as np
import soundfile as sf
import pyworld as pw

FRAME_S = 0.005          # WORLD frame period (5ms)
F0_FLOOR = 65.0
F0_CEIL = 500.0
MAX_ONSET_S = 0.22       # consonant pickup cap
MAX_CODA_S = 0.26
MAX_BREATH_S = 0.45
GLIDE_SIGMA_S = 0.022    # gaussian smoothing of the target curve → ~40-80ms glides
SCOOP_S = 0.06           # phrase-onset scoop length
SCOOP_ST = 1.0           # …starting a semitone below
DRIFT_CENTS = 12.0       # slow detune drift depth
VIB_HOLD_S = 0.7
VIB_HZ = 5.0
VIB_CENTS = 30.0
VIB_ONSET_S = 0.30


def midi_to_hz(m):
    return 440.0 * (2.0 ** ((np.asarray(m, dtype=np.float64) - 69.0) / 12.0))


def hz_to_midi(hz):
    return 69.0 + 12.0 * np.log2(hz / 440.0)


# ── analysis ───────────────────────────────────────────────────────────────

def analyze(x, fs):
    f0_raw, t = pw.harvest(x, fs, f0_floor=F0_FLOOR, f0_ceil=F0_CEIL,
                           frame_period=FRAME_S * 1000.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=F0_FLOOR)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=F0_FLOOR)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    return f0, t, sp, ap, fft_size


def frame_features(x, fs, f0, sp):
    """Per-frame: rms energy, spectral flux, low-band dominance, hf ratio."""
    n = len(f0)
    hop = int(round(fs * FRAME_S))
    win = hop * 2
    rms = np.zeros(n)
    for i in range(n):
        a = max(0, i * hop - win // 2)
        b = min(len(x), i * hop + win // 2)
        if b > a:
            rms[i] = np.sqrt(np.mean(x[a:b] ** 2))
    # band edges on the sp bins
    nbins = sp.shape[1]
    nyq = fs / 2.0
    k1 = int(nbins * 1000.0 / nyq)     # < 1 kHz  — vowel/sonorant power
    k4 = int(nbins * 4000.0 / nyq)     # > 4 kHz  — sibilance / bursts
    tot = sp.sum(axis=1) + 1e-12
    low_dom = sp[:, :k1].sum(axis=1) / tot
    hf_ratio = sp[:, k4:].sum(axis=1) / tot
    # spectral flux on log-sp (positive changes only)
    lsp = np.log(sp + 1e-12)
    d = np.diff(lsp, axis=0)
    flux = np.zeros(n)
    flux[1:] = np.maximum(d, 0).mean(axis=1)
    return rms, flux, low_dom, hf_ratio


def classify_frames(f0, rms, flux, low_dom, hf_ratio):
    """Coarse per-frame phone class: sil / plosive / fric / vcons / vowel."""
    n = len(f0)
    voiced = f0 > 0
    floor = max(1e-4, 0.05 * np.percentile(rms, 95))
    cls = np.array(["sil"] * n, dtype=object)
    speech = rms > floor
    # plosive bursts: strong flux spike with a quiet 30ms run just before
    flux_thr = np.percentile(flux[speech], 80) if speech.any() else np.inf
    for i in range(2, n):
        if flux[i] > flux_thr and rms[max(0, i - 7):max(1, i - 1)].mean() < 2.5 * floor:
            cls[i] = "plosive"
    for i in range(n):
        if cls[i] == "plosive":
            continue
        if not speech[i]:
            continue
        if not voiced[i]:
            cls[i] = "fric" if hf_ratio[i] > 0.10 else "plosive"
        else:
            cls[i] = "vowel" if low_dom[i] > 0.35 else "vcons"
    return cls, voiced, floor


def find_nuclei(w0, w1, n_slots, voiced, rms, low_dom):
    """Vowel nuclei inside word frames [w0,w1) — one per note slot."""
    seg = slice(w0, w1)
    v = np.where(voiced[seg], rms[seg] * (0.25 + low_dom[seg]), 0.0)
    if v.max() <= 0:
        # completely unvoiced take — spread slots evenly, caller flags it
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
    # merge regions separated by < 30ms (same vowel split by a flicker)
    merged = [regions[0]]
    for a, b in regions[1:]:
        if a - merged[-1][1] < 6:
            merged[-1][1] = b
        else:
            merged.append([a, b])
    regions = merged
    # match count to slots: too many → keep strongest n in time order;
    # too few → split the longest until we have enough
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


# ── the line builder ───────────────────────────────────────────────────────

def smooth_runs(curve, mask, sigma_frames):
    """Gaussian-smooth `curve` independently inside each contiguous True run."""
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


def main():
    plan = json.loads(open(sys.argv[1]).read())
    x, fs = sf.read(plan["line_wav"], dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0, t, sp, ap, fft_size = analyze(x, fs)
    n_src = len(f0)
    rms, flux, low_dom, hf_ratio = frame_features(x, fs, f0, sp)
    cls, voiced, floor = classify_frames(f0, rms, flux, low_dom, hf_ratio)

    # interpolated log-f0 through unvoiced gaps (for the residual contour)
    vi = np.where(voiced)[0]
    if len(vi) < 5:
        sf.write(plan["out_wav"], np.zeros(16, dtype=np.float32), fs)
        print(json.dumps({"error": "line has almost no voiced frames"}))
        return 1
    log_src = np.interp(np.arange(n_src), vi, np.log(f0[vi]))

    words = plan["words"]
    beta = float(plan.get("beta", 0.25))
    rng = np.random.default_rng(int(plan.get("seed", 7)))

    # ── segment every word: onset cluster / nuclei / medials / coda ────────
    segs = []          # per word dict
    sidecar_words = []
    for w in words:
        w0 = max(0, int(round(w["srcFromMs"] / 1000.0 / FRAME_S)))
        w1 = min(n_src, int(round(w["srcToMs"] / 1000.0 / FRAME_S)))
        if w1 <= w0 + 2:
            w1 = min(n_src, w0 + 8)
        w0t, w1t = trim_silence(w0, w1, rms, floor)
        n_slots = len(w["slots"])
        nuclei, sung = find_nuclei(w0t, w1t, n_slots, voiced, rms, low_dom)
        if not sung and segs:
            # whisper dropped a word and alignment handed this one a window
            # of pure consonants — the vowel usually sits just OUTSIDE, in
            # the previous word's overwide window or the following silence.
            # Re-search from past the previous word's last nucleus.
            pa = min(segs[-1]["nuclei"][-1][1] + 2, w0t)
            pb = min(n_src, w1t + int(0.10 / FRAME_S))
            if pb > pa + 4 and voiced[pa:pb].any():
                pa2, pb2 = trim_silence(pa, pb, rms, floor)
                nuclei, sung = find_nuclei(pa2, pb2, n_slots, voiced, rms, low_dom)
                w0t, w1t = pa2, pb2
                # the previous word must not keep sounding into the stolen
                # region — pull its coda back before our material starts
                ps = segs[-1]
                ps["coda"] = (ps["coda"][0], min(ps["coda"][1], nuclei[0][0]))
                ps["w1"] = min(ps["w1"], nuclei[0][0])
        onset = (w0t, nuclei[0][0])
        if (onset[1] - onset[0]) * FRAME_S > MAX_ONSET_S:
            onset = (onset[1] - int(MAX_ONSET_S / FRAME_S), onset[1])
        medials = [(nuclei[k][1], nuclei[k + 1][0]) for k in range(n_slots - 1)]
        coda = (nuclei[-1][1], w1t)
        if (coda[1] - coda[0]) * FRAME_S > MAX_CODA_S:
            coda = (coda[0], coda[0] + int(MAX_CODA_S / FRAME_S))
        segs.append({"w": w, "onset": onset, "nuclei": nuclei,
                     "medials": medials, "coda": coda, "sung": sung,
                     "w0": w0t, "w1": w1t})
        sidecar_words.append({
            "word": w["w"],
            "onsetMs": [round(onset[0] * FRAME_S * 1000), round(onset[1] * FRAME_S * 1000)],
            "nucleiMs": [[round(a * FRAME_S * 1000), round(b * FRAME_S * 1000)] for a, b in nuclei],
            "codaMs": [round(coda[0] * FRAME_S * 1000), round(coda[1] * FRAME_S * 1000)],
            "classes": "".join({"sil": ".", "plosive": "P", "fric": "F",
                                "vcons": "C", "vowel": "V"}[c] for c in cls[w0t:w1t]),
        })

    with open(plan["phoneme_sidecar"], "w") as fh:
        json.dump({"framePeriodMs": FRAME_S * 1000, "words": sidecar_words}, fh, indent=1)

    # ── output timeline ────────────────────────────────────────────────────
    line_t0 = float(plan["line_t0"])
    line_t1 = float(plan["line_t1"])
    out_n = int(round((line_t1 - line_t0) / FRAME_S))
    src_pos = np.full(out_n, -1.0)          # fractional source frame per out frame
    natural = np.zeros(out_n, dtype=bool)   # 1:1-mapped (consonants, breaths)
    target_midi = np.full(out_n, np.nan)    # note plateaus (pre-glide)
    in_word = np.zeros(out_n, dtype=bool)
    vib_gain = np.zeros(out_n)
    scoop = np.zeros(out_n)                 # semitone offsets (phrase scoops)
    word_med = np.zeros(out_n)              # per-frame word median log-f0 (residual ref)

    def of(t_abs):                          # absolute seconds → out frame index
        return int(round((t_abs - line_t0) / FRAME_S))

    def place(o_start, s_a, s_b, midi=None, med=None):
        """1:1 placement of source frames [s_a,s_b) at out frame o_start."""
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

    # pre-compute each word's onset length (for the previous word's hard end)
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

        # where must this word stop? (next word's pickup steals the tail)
        hard_end = float(w["hardEnd"])
        if wi + 1 < len(segs):
            hard_end = min(hard_end, segs[wi + 1]["w"]["slots"][0]["t"] - onset_len[wi + 1] - 0.01)
        hard_end = max(hard_end, slots[-1]["t"] + 0.10)

        # onset cluster — pickup ending ON the first note time
        oa, ob = s["onset"]
        place(of(slots[0]["t"]) - (ob - oa), oa, ob, midi=slots[0]["midi"], med=med_log)

        coda_len = (s["coda"][1] - s["coda"][0]) * FRAME_S
        for k in range(n_slots):
            na, nb = s["nuclei"][k]
            v_start = slots[k]["t"]
            if k + 1 < n_slots:
                med_len = (s["medials"][k][1] - s["medials"][k][0]) * FRAME_S
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
            # vowel mapping: transitions natural, middle stretched w/ jitter
            head = min(6, n_nuc // 3)
            tail = min(6, n_nuc // 3)
            if n_out <= n_nuc:
                pos = na + np.linspace(0, n_nuc - 1, n_out)
            else:
                pos = np.empty(n_out)
                pos[:head] = na + np.arange(head)
                pos[n_out - tail:] = nb - tail + np.arange(tail)
                mid_out = n_out - head - tail
                mid_a, mid_b = na + head, nb - tail - 1
                base = np.linspace(mid_a, max(mid_a, mid_b), mid_out)
                jit = rng.standard_normal(max(2, mid_out // 4))
                jit = np.interp(np.linspace(0, 1, mid_out),
                                np.linspace(0, 1, len(jit)), jit) * 1.2
                pos[head:head + mid_out] = np.clip(base + jit, mid_a, max(mid_a, mid_b))
            idx = np.arange(o_a, o_b)
            src_pos[idx] = pos[:len(idx)]
            in_word[idx] = True
            target_midi[idx] = slots[k]["midi"]
            word_med[idx] = med_log
            # vibrato only on true holds
            hold = v_end - v_start
            if hold >= VIB_HOLD_S:
                tt = (idx - o_a) * FRAME_S
                vib_gain[idx] = np.clip((tt - 0.15) / VIB_ONSET_S, 0, 1)
            # phrase-onset scoop on the first vowel
            if k == 0 and w.get("phraseStart"):
                tt = (idx - o_a) * FRAME_S
                scoop[idx] = -SCOOP_ST * np.clip(1.0 - tt / SCOOP_S, 0, 1)
            # medial consonant cluster — pickup into the NEXT note
            if k + 1 < n_slots:
                ma, mb = s["medials"][k]
                mlen = of(slots[k + 1]["t"]) - o_b
                if mb - ma > 0 and mlen > 0:
                    place(o_b, mb - min(mb - ma, mlen), mb,
                          midi=slots[k + 1]["midi"], med=med_log)

        # coda at the vowel's end
        ca, cb = s["coda"]
        if cb > ca:
            place(of(hard_end) - (cb - ca), ca, cb, midi=slots[-1]["midi"], med=med_log)

        # breath in the source gap before the NEXT phrase start → natural
        if wi + 1 < len(segs) and segs[wi + 1]["w"].get("phraseStart"):
            ga, gb = s["w1"], segs[wi + 1]["w0"]
            ga, gb = trim_silence(ga, gb, rms, floor * 0.6)
            if gb > ga + 4 and rms[ga:gb].mean() > floor * 0.6:
                blen = min(gb - ga, int(MAX_BREATH_S / FRAME_S))
                nxt_on = of(segs[wi + 1]["w"]["slots"][0]["t"]) - \
                    int(onset_len[wi + 1] / FRAME_S)
                ba = nxt_on - blen - 4
                if ba > of(hard_end) + 2:
                    place(ba, gb - blen, gb)   # unpitched — no midi
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

    # fill target through voiced consonants that carry no note of their own
    tm = target_midi.copy()
    have = ~np.isnan(tm)
    if have.any():
        ii = np.where(have)[0]
        tm = np.interp(np.arange(out_n), ii, tm[ii])
    tm = tm + scoop
    target_log = np.log(midi_to_hz(tm))
    # portamento: smooth the target INSIDE contiguous in-word runs only
    target_log = smooth_runs(target_log, in_word, GLIDE_SIGMA_S / FRAME_S)

    # slow detune drift so plateaus never sound quantized
    tt = np.arange(out_n) * FRAME_S
    ph = rng.uniform(0, 2 * np.pi, 2)
    drift = (DRIFT_CENTS / 1200.0) * np.log(2) * \
        (0.6 * np.sin(2 * np.pi * 0.23 * tt + ph[0]) +
         0.4 * np.sin(2 * np.pi * 0.61 * tt + ph[1]))
    vib = (VIB_CENTS / 1200.0) * np.log(2) * vib_gain * np.sin(2 * np.pi * VIB_HZ * tt)

    resid = np.zeros(out_n)
    fm = mapped & (word_med != 0)
    frac = src_pos - np.floor(src_pos)
    i0 = np.clip(np.floor(src_pos).astype(int), 0, n_src - 1)
    i1 = np.clip(i0 + 1, 0, n_src - 1)
    resid[fm] = ((1 - frac[fm]) * log_src[i0[fm]] + frac[fm] * log_src[i1[fm]]) - word_med[fm]
    # octave-fold: vocal fry and harvest octave errors put ~1200-cent steps in
    # the residual which beta would scale into audible lurches — fold them out,
    # keep only the sub-octave micro-contour, then smooth it
    resid = resid - np.log(2) * np.round(resid / np.log(2))
    resid = smooth_runs(resid, fm, 2.0)

    f0_out = np.exp(target_log + beta * resid + drift + vib)
    f0_out[~voiced_out] = 0.0

    # ── assemble sp/ap streams and synthesize ONCE ─────────────────────────
    sp_out = np.full((out_n, sp.shape[1]), 1e-12)
    ap_out = np.ones((out_n, ap.shape[1]))
    w0f = (1 - frac[mapped])[:, None]
    w1f = frac[mapped][:, None]
    sp_out[mapped] = w0f * sp[i0[mapped]] + w1f * sp[i1[mapped]]
    ap_out[mapped] = np.clip(w0f * ap[i0[mapped]] + w1f * ap[i1[mapped]], 0.0, 1.0)

    y = pw.synthesize(np.ascontiguousarray(f0_out),
                      np.ascontiguousarray(sp_out),
                      np.ascontiguousarray(ap_out), fs,
                      frame_period=FRAME_S * 1000.0)

    # ── unvoiced natural runs: composite the ORIGINAL samples back in ──────
    hop = int(round(fs * FRAME_S))
    comp = natural & mapped & ~voiced_out
    ramp = int(0.005 * fs)
    i = 0
    while i < out_n:
        if not comp[i]:
            i += 1
            continue
        j = i
        while j < out_n and comp[j] and (j == i or abs(src_pos[j] - src_pos[j - 1] - 1) < 0.5):
            j += 1
        if (j - i) >= 2:   # ≥10ms
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
        i = j
    # line-edge fades
    ef = int(0.010 * fs)
    if len(y) > 2 * ef:
        y[:ef] *= np.linspace(0, 1, ef)
        y[-ef:] *= np.linspace(1, 0, ef)

    sf.write(plan["out_wav"], y.astype(np.float32), fs)

    # f0-continuity metric: max & p95 cents jump between consecutive voiced
    # frames inside words (the smoothness evidence)
    j = np.abs(np.diff(np.log(np.where(f0_out > 0, f0_out, np.nan)))) * 1200 / np.log(2)
    j = j[~np.isnan(j)]
    print(json.dumps({
        "words": stats_words,
        "f0_jump_max_cents": round(float(j.max()), 1) if len(j) else 0,
        "f0_jump_p95_cents": round(float(np.percentile(j, 95)), 1) if len(j) else 0,
        "out_frames": out_n,
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
