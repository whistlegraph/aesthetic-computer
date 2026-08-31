#!/usr/bin/env python3
"""aesthetivox.py — every xpld vocal goes through the WORLD chain.

The house rule (cult -> loner -> flwe -> here): no lead vocal ships raw —
every line becomes a SUNG NOTE, not a chopped speech hit. This is the
xpld adaptation of flwe's aesthetivox (loner's halo3.py v4pid regulation
engine), driven by analysis/melody-chart.json (bin/notes.py's score —
the derived 1 2 3 5 6 b7 mixolydian set in the take's own 188.3 Hz /
F#3+31c frame, 130.8 BPM).

The chain, per charted phrase of the primary take:

  THE ANALYSIS   WORLD (harvest -> stonemask -> cheaptrick -> d4c) at
                 5 ms frames, with the floor FITTED to the phrase (a wide
                 probe finds where the voice lives, then a snug bracket —
                 halo3's lesson: a fixed floor under a low phrase reads
                 the second harmonic and everything downstream is wrong).
  THE BOUNDARIES whisper times a word where the transcript hands over,
                 not where the singing changes; each word start is pulled
                 to the nearest acoustic event (pitch step or energy
                 valley, +/-250 ms, never reordering, never under 80 ms).
  THE TRIM       a word's source span ends where its audio ends — trailing
                 decay is dropped, not stretched (quiet AND dull = silence;
                 a fricative is dim but BRIGHT and survives the gate).
  THE ATTACK     every word keeps 30 ms of runway, borrowed only from
                 silence, so onsets are never shaved.
  THE REGULATION plosives vs vowels: consonants (unvoiced frames) ride
                 1:1 and are NEVER stretched; the voiced nucleus carries
                 the note — stretched to fill the word's beat slot and
                 pulled to the chart's target at SNAP 0.92, correction
                 smoothed 45 ms, snap fading out where the pitch is
                 genuinely sliding (a slide is not out of tune).
  VOWEL-ON-THE-BEAT  each word's voiced onset lands on its grid slot and
                 the consonant runway plays just AHEAD of the beat, the
                 way a singer leans in.
  THE HOLD       a nucleus stretched past 1.8x flattens to its target
                 with vibrato fading in over 0.4 s (plus a read shimmer
                 past 2.2x so the envelope doesn't freeze).
  THE RELEASE    a phrase whose source hard-stops (< 150 ms of tail) gets
                 a synthesized WORLD release — a ping-pong read of the
                 last 120 ms, faded over 0.4 s.
  THE COMPOSITE  voiced regions are WORLD audio; the warped ORIGINAL is
                 composited back through the unvoiced regions (5 ms cosine
                 seams) so /s/ /t/ /k/ stay real — then unvoiced-bright
                 frames get +8 dB back (the sibilant restore).

The primary's room is NOISY (HF floor 0.054 vs 0.003 in the July takes)
but its silences still read: ~19% of 5 ms frames sit under the -36 dB
gate, so the trim and the attack-runway machinery hold as-is. The render
NEVER dubs — the flagged alternates sing their phrases in another
register (see notes.py) — but the sample bank carves them.

Renders:
  out/xpld-aesthetivox.wav/.mp3       the sung-note vocal on the click grid
  out/xpld-aesthetivox-halo.wav/.mp3  + octave halo (vowels-only, dark,
                                      +/-6c pair) and low self-backup at
                                      -2 and -4 scale degrees, low gain
  vox/NN-slug.wav                     the phrase bank (sung renders, dry;
                                      lead-in noted so beat 0 is placeable)
  vox/words/NN-MM-word.wav            raw per-word carves from the primary
                                      (post boundary-repair/trim/attack —
                                      natural onsets, nothing synthesized)
  vox/dubs/NN-slug--TAKE.wav          raw phrase carves from the flagged
                                      better takes (chart's dub blocks)
  vox/.manifest.json                  source take, time range, f0/note,
                                      word — for every file above
  analysis/aesthetivox.json           per-word receipts: runway ms,
                                      stretch ratio, holds, and QC —
                                      cents-from-chart re-measured off
                                      the rendered audio

  ../../.venv/bin/python3 bin/aesthetivox.py
"""
import json, os, re, subprocess
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
ANA = os.path.join(LANE, "analysis")
OUT = os.path.join(LANE, "out")
VOX = os.path.join(LANE, "vox")
os.makedirs(OUT, exist_ok=True)
os.makedirs(VOX, exist_ok=True)
os.makedirs(os.path.join(VOX, "words"), exist_ok=True)
os.makedirs(os.path.join(VOX, "dubs"), exist_ok=True)

CHART = json.load(open(os.path.join(ANA, "melody-chart.json")))
PRIMARY = CHART["primary"]
SRC = os.path.join(LANE, "source", CHART["source"])

TONIC = CHART["tonic_hz"]            # 188.3 — F#3 +31c, his own frame
SCALE = np.array(CHART["scale_semitones"])   # [0,2,4,7,9,10]
BPM = CHART["bpm"]
SPB = 60.0 / BPM
COUNT_IN = 8                          # beats, matches xpld-clickvox

FRAME_MS = 5.0
FRAME_S = FRAME_MS / 1000.0
SNAP = 0.92                           # THE REGULATION (halo3's number)
SMOOTH_MS = 45.0
GLIDE_ST_S = 18.0                     # faster than this = a slide, let it be
FORMANT_DB = 1.6                      # singer's formant, ballad-gentle
AIR_DB = 2.5
BREATH = 0.14
SIB_DB = 8.0                          # the sibilant restore
SIB_HI_HZ = 3000.0
SIB_SHARE = 0.45
SIB_RAMP_S = 0.030
UNVOICED_W = 0.18                     # consonant share of any stretch
SILENT_W = 0.04
HOLD_RATIO = 1.8
SHIMMER_RATIO = 2.2
VIB_HZ, VIB_CENTS, VIB_RISE_S = 5.0, 25.0, 0.4
ATTACK_S = 0.030
TRIM_GATE_DB = -36.0
TRIM_HF_DB = -30.0
TRIM_MARGIN_S = 0.050
TRIM_MIN_S = 0.080
TRIM_QUIET_RUN_S = 0.120
TRIM_LEAK_S = 0.150
TRIM_KEEP = 0.35
SNAP_WIN_S = 0.250                    # boundary repair
SNAP_MED_S = 0.120
SNAP_STEP_ST = 0.50
SNAP_MIN_S = 0.080
SNAP_QUIET = 0.30
HALO_DARK_HZ = 5500.0
HALO_GAIN = 0.32
BACKUP_GAINS = {-2: 0.26, -4: 0.20}  # low self-backup, scale degrees down
PRE_S = 0.35                          # slice pre-roll (first consonant room)
POST_S = 0.60                        # slice post-roll (release room)


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


STEPS = np.concatenate([SCALE + 12 * o for o in range(-4, 5)]).astype(float)


def nearest_scale_cents(hz):
    """Cents from tonic of the nearest scale tone, per frame."""
    st = 12.0 * np.log2(np.maximum(hz, 1e-6) / TONIC)
    idx = np.argmin(np.abs(st[:, None] - STEPS[None, :]), axis=1)
    return STEPS[idx] * 100.0


def shelf(freqs, centre, width):
    return 1.0 / (1.0 + np.exp(-(freqs - centre) / width))


# ── analysis with a fitted floor (halo3's lesson) ─────────────────────
def analyze(x, fs):
    x = np.ascontiguousarray(x, dtype=np.float64)
    probe, _ = pw.harvest(x, fs, f0_floor=55.0, f0_ceil=900.0,
                          frame_period=FRAME_MS)
    vp = probe[probe > 0]
    med = float(np.median(vp)) if len(vp) else TONIC
    floor = max(55.0, min(140.0, med * 0.55))
    ceil = min(1000.0, max(600.0, med * 3.0))
    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=ceil,
                           frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    voiced = f0 > 0
    # rate of pitch change — where he is genuinely sliding the snap lets go
    if voiced.sum() >= 2:
        st = 12.0 * np.log2(np.maximum(f0, 1e-6) / TONIC)
        idx = np.arange(len(f0))
        st = np.interp(idx, idx[voiced], st[voiced])
        rate = np.abs(np.gradient(smooth(st, int(60.0 / FRAME_MS)),
                                  FRAME_S))
    else:
        rate = np.zeros_like(f0)
    return dict(x=x, fs=fs, f0=f0, sp=sp, ap=ap, voiced=voiced, rate=rate,
                floor=floor)


# ── boundary repair (halo3's snap_boundaries, slice-relative) ─────────
def snap_boundaries(a, words):
    x, fs, f0 = a["x"], a["fs"], a["f0"]
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    if m < 8 or len(words) < 2:
        return words, []
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    st = np.where(f0[:m] > 0,
                  12.0 * np.log2(np.maximum(f0[:m], 1e-6) / TONIC), np.nan)
    W = max(2, int(round(SNAP_MED_S / FRAME_S)))
    step = np.zeros(m)
    if m > 2 * W:
        win = np.lib.stride_tricks.sliding_window_view(st, W)
        med = np.nanmedian(win, axis=1)
        cnt = (~np.isnan(win)).sum(axis=1)
        lo, hi = med[:m - 2 * W + 1], med[W:m - W + 1]
        ok = (cnt[:m - 2 * W + 1] >= W // 2) & (cnt[W:m - W + 1] >= W // 2)
        d = np.abs(hi - lo)
        d[~ok | np.isnan(d)] = 0.0
        step[W:m - W + 1] = d
    mins = int(round(SNAP_MIN_S / FRAME_S))
    win_f = int(round(SNAP_WIN_S / FRAME_S))
    out = [dict(w) for w in words]
    log = []
    for i in range(1, len(out)):
        k0 = int(round(out[i]["start"] / FRAME_S))
        prev = int(round(out[i - 1]["start"] / FRAME_S))
        nxt = int(round(out[i + 1]["start"] / FRAME_S)) if i + 1 < len(out) else m
        lo = max(prev + mins, k0 - win_f, W)
        hi = min(nxt - mins, k0 + win_f, m - W)
        if hi <= lo:
            continue
        kk = lo + int(np.argmax(step[lo:hi]))
        if step[kk] < SNAP_STEP_ST:
            seg = rms[lo:hi]
            kv = lo + int(np.argmin(seg))
            kk = kv if seg.min() < SNAP_QUIET * (np.median(seg) or 1.0) else k0
        if kk == k0:
            continue
        ts = kk * FRAME_S
        out[i]["start"] = ts
        out[i - 1]["end"] = ts
        log.append(f"{out[i]['word']} {(kk - k0) * FRAME_S * 1000:+.0f}ms")
    return out, log


# ── the spread: un-cram whisper's fry clusters (an xpld lesson) ───────
# In the fry-sung lines ("This house that I…", "Hit a…", "It flickers…")
# whisper stamps several words into a 10 ms sliver; a 10 ms source
# stretched to a half-beat slot is a 23x frozen loop, not a word. After
# the boundary repair, any run of sub-60 ms words is redistributed over
# the room up to the next word's (repaired) start — and if there is no
# room, the next word's front half is annexed, at most 150 ms per
# crammed word.
CRAM_S = 0.060
CRAM_GIVE_S = 0.150


def spread_crammed(ws):
    log = []
    i = 0
    while i < len(ws):
        if ws[i]["end"] - ws[i]["start"] >= CRAM_S:
            i += 1
            continue
        k = i
        while k < len(ws) and ws[k]["end"] - ws[k]["start"] < CRAM_S:
            k += 1
        n_t = k - i
        start = ws[i]["start"]
        avail = ws[k]["start"] if k < len(ws) else ws[k - 1]["end"]
        if avail - start < 0.05 * n_t and k < len(ws):
            mid = ws[k]["start"] + (ws[k]["end"] - ws[k]["start"]) * 0.5
            avail = min(mid, start + CRAM_GIVE_S * n_t)
            ws[k]["start"] = avail
        bounds = np.linspace(start, avail, n_t + 1)
        for u in range(n_t):
            ws[i + u]["start"] = float(bounds[u])
            ws[i + u]["end"] = float(bounds[u + 1])
        log.append(" ".join(w["word"] for w in ws[i:k]) +
                   f" -> {1000 * (avail - start) / n_t:.0f}ms each")
        i = k
    return ws, log


# ── the energy trim ───────────────────────────────────────────────────
def energy_end(x, fs, f0i, f1i, peak):
    n = int(round(fs * FRAME_S))
    seg = x[f0i * n:f1i * n]
    if len(seg) < n:
        return f1i
    m = len(seg) // n
    rms = np.sqrt((seg[:m * n].reshape(m, n) ** 2).mean(axis=1))
    d = np.diff(np.concatenate([seg[:m * n], seg[-1:]]))
    hf = np.sqrt((d[:m * n].reshape(m, n) ** 2).mean(axis=1))
    quiet = (rms <= peak * 10.0 ** (TRIM_GATE_DB / 20.0)) & \
            (hf <= (np.max(hf) or 1.0) * 10.0 ** (TRIM_HF_DB / 20.0))
    on = np.nonzero(~quiet)[0]
    if not len(on):
        return f0i + m
    run = int(round(TRIM_QUIET_RUN_S / FRAME_S))
    leak = int(round(TRIM_LEAK_S / FRAME_S))
    runs, k = [], int(on[0])
    while k < m:
        if quiet[k]:
            j = k
            while j < m and quiet[j]:
                j += 1
            if j - k >= run:
                runs.append((k, j))
            k = j
        else:
            k += 1
    for (a_, b_) in reversed(runs):
        if int((~quiet[b_:]).sum()) <= leak:
            return f0i + a_
    return f0i + int(on[-1]) + 1


def trim_units(x, fs, unit_src):
    peak = np.max(np.abs(x)) or 1.0
    out, log = [], []
    for u, (s0, s1) in enumerate(unit_src):
        if u == len(unit_src) - 1:
            out.append((s0, s1))
            continue
        e = energy_end(x, fs, s0, s1, peak) + int(round(TRIM_MARGIN_S / FRAME_S))
        e = max(s0 + int(round(TRIM_KEEP * (s1 - s0))), min(e, s1))
        if (s1 - e) * FRAME_S < TRIM_MIN_S:
            e = s1
        else:
            log.append(f"-{(s1 - e) * FRAME_S * 1000:.0f}ms")
        out.append((s0, e))
    return out, log


def keep_attacks(unit_src, x, fs):
    pre = int(round(ATTACK_S / FRAME_S))
    n = int(round(fs * FRAME_S))
    m = len(x) // n
    e = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    quiet = e <= (np.max(np.abs(x)) or 1.0) * 10.0 ** (TRIM_GATE_DB / 20.0)
    out = []
    for u, (s0, s1) in enumerate(unit_src):
        floor = 0 if u == 0 else unit_src[u - 1][1]
        lo = max(floor, s0 - pre)
        k = s0
        while k > lo and k - 1 < len(quiet) and quiet[k - 1]:
            k -= 1
        out.append((k, s1))
    return out


# ── the warp: vowel-on-the-beat ───────────────────────────────────────
def build_warp(a, unit_src, beats, dursb, tail_cap_s=0.40, tail_end=None):
    F = len(a["f0"])
    x, fs = a["x"], a["fs"]
    w = np.where(a["voiced"], 1.0, UNVOICED_W)
    spf = int(round(fs * FRAME_S))
    nf = min(F, len(x) // spf)
    if nf > 0:
        e = np.sqrt((x[:nf * spf].reshape(nf, spf) ** 2).mean(axis=1))
        quiet = e <= (np.max(np.abs(x)) or 1.0) * 10.0 ** (TRIM_GATE_DB / 20.0)
        w[:nf][quiet] = SILENT_W
    ants, voiced_at = [], []
    for (s0, s1) in unit_src:
        v0 = s0
        lim = min(s0 + int(0.30 / FRAME_S), s1 - 1, F - 1)
        while v0 < lim and not a["voiced"][v0]:
            v0 += 1
        if not a["voiced"][min(v0, F - 1)]:
            v0 = s0
        voiced_at.append(v0)
        ants.append(max(0, v0 - s0))
    Z = ants[0]                                   # phrase lead-in, 1:1
    idx = list(range(unit_src[0][0], unit_src[0][0] + Z))
    T = [Z + int(round(b * SPB / FRAME_S)) for b in beats]
    Tend = [Z + int(round((b + d) * SPB / FRAME_S))
            for b, d in zip(beats, dursb)]
    holds, ratios = [], []
    for u, (s0, s1) in enumerate(unit_src):
        s0, s1 = max(0, min(s0, F - 1)), max(1, min(s1, F))
        v0 = min(voiced_at[u], s1 - 1)
        t0 = T[u]
        if u + 1 < len(unit_src):
            nxt_a = min(ants[u + 1], max(0, (T[u + 1] - t0) - 2))
            body_end = T[u + 1] - nxt_a
        else:
            nxt_a = 0
            body_end = Tend[u]
        out_n = max(1, body_end - t0)
        src_n = max(1, s1 - v0)
        ratio = out_n / src_n
        ratios.append(ratio)
        seg_w = w[v0:s1].copy() if s1 > v0 else np.ones(1)
        cum = np.concatenate([[0.0], np.cumsum(seg_w)])
        cum /= cum[-1]
        pos = np.interp((np.arange(out_n) + 0.5) / out_n, cum,
                        np.arange(len(cum), dtype=float)) - 0.5
        pos = np.clip(pos, 0, src_n - 1)
        if ratio > SHIMMER_RATIO:                 # unfreeze the envelope
            tsec = np.arange(out_n) * FRAME_S
            pos = np.clip(pos + 2.2 * np.sin(2 * np.pi * 0.85 * tsec),
                          0, src_n - 1)
        if ratio > HOLD_RATIO:
            holds.append((len(idx), len(idx) + out_n, u))
        idx.extend((v0 + np.round(pos).astype(int)).tolist())
        if u + 1 < len(unit_src):                 # next consonant, 1:1
            ns0 = unit_src[u + 1][0]
            idx.extend(range(ns0, ns0 + nxt_a))
    # The tail plays 1:1 after the last word — but never past the next
    # phrase's start in the take (halo3's lesson: without the cap the tail
    # plays the next utterance too, and the render doubles it).
    tail0 = unit_src[-1][1]
    Fend = min(F, tail0 + int(tail_cap_s / FRAME_S))
    if tail_end is not None:
        Fend = min(Fend, max(tail0, tail_end))
    idx += list(range(min(tail0, Fend), Fend))
    synth_rel = (Fend - tail0) * FRAME_S < 0.15
    if synth_rel:                                 # THE RELEASE
        rel_n = int(0.40 / FRAME_S)
        s0, s1 = unit_src[-1]
        lo = max(s0, s1 - int(0.12 / FRAME_S))
        span = max(2, s1 - lo)
        fade = (len(idx), len(idx) + rel_n)
        for k in range(rel_n):
            p = lo + (span - 1) - abs((k % (2 * span - 2)) - (span - 1))
            idx.append(int(np.clip(p, 0, F - 1)))
    else:
        fade = (len(idx) - max(1, Fend - tail0), len(idx))
    return np.array(idx, dtype=int), holds, fade, Z, ants, ratios, synth_rel, T


# ── the regulation: per-word melody lock on the source frame axis ─────
def regulate(a, unit_src, targets_st):
    f0, voiced, rate = a["f0"], a["voiced"], a["rate"]
    cents = np.where(voiced,
                     1200.0 * np.log2(np.maximum(f0, 1e-6) / TONIC), 0.0)
    tgt = np.full(len(f0), np.nan)
    for (s0, s1), st in zip(unit_src, targets_st):
        if st is not None:
            tgt[s0:s1] = st * 100.0
    # untargeted frames (tail, chartless words): nearest scale tone
    free = np.isnan(tgt) & voiced
    if free.any():
        tgt[free] = nearest_scale_cents(f0[free])
    corr = np.zeros(len(f0))
    m = voiced & ~np.isnan(tgt)
    corr[m] = (tgt[m] - cents[m]) * SNAP
    corr *= np.clip(1.0 - (rate - GLIDE_ST_S) / GLIDE_ST_S, 0.0, 1.0)
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    return np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)


def vuv_mask(voiced, fs, n):
    spf = int(round(fs * FRAME_S))
    mask = np.repeat(voiced.astype(np.float64), spf)
    mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
    ramp = int(0.005 * fs)
    edges = np.diff(mask.astype(np.int8))
    for i in np.where(edges == 1)[0]:
        k = np.arange(min(ramp, n - i - 1))
        mask[i + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    for i in np.where(edges == -1)[0]:
        k = np.arange(min(ramp, i + 1))
        mask[i - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    return mask


def synth_from(a, idx, f0_o, *, dark=None, breath_x=1.0, vowels_only=False,
               fade=None):
    fs, x = a["fs"], a["x"]
    sp_o = np.ascontiguousarray(a["sp"][idx])
    ap_o = np.ascontiguousarray(a["ap"][idx])
    voiced_o = a["voiced"][idx]
    freqs = np.linspace(0.0, fs / 2.0, sp_o.shape[1])
    sp_o = sp_o * (10.0 ** ((FORMANT_DB *
        np.exp(-((freqs - 2800.0) / 450.0) ** 2)) / 10.0))[None, :]
    sp_o = sp_o * (10.0 ** (AIR_DB * shelf(freqs, 8000.0, 900.0) / 10.0))[None, :]
    if dark:
        sp_o = sp_o * (1.0 / (1.0 + (freqs / dark) ** 2))[None, :]
    depth = np.zeros(len(idx)); acc = 0.0
    for i, v in enumerate(voiced_o):
        acc = acc + FRAME_S if v else 0.0
        depth[i] = acc
    bw = np.clip((depth - 0.15) / 0.25, 0.0, 1.0)
    ap_o = np.minimum(1.0, ap_o + (BREATH * breath_x) * bw[:, None]
                      * shelf(freqs, 8000.0, 800.0)[None, :])
    f0_o = np.where(voiced_o, f0_o, 0.0)
    vi = np.where(voiced_o)[0]
    f0s = (np.exp(np.interp(np.arange(len(f0_o)), vi,
                            np.log(np.maximum(f0_o[vi], 1e-6))))
           if vi.size >= 2 else np.maximum(f0_o, 1e-6))
    y = pw.synthesize(f0s, sp_o, ap_o, fs, frame_period=FRAME_MS)
    n = len(y)
    mask = vuv_mask(voiced_o, fs, n)
    if vowels_only:
        out = mask * y
    else:
        spf = int(fs * FRAME_S)
        nf = min(len(idx), (n + spf - 1) // spf)
        pos = (np.asarray(idx[:nf], dtype=np.int64)[:, None] * spf
               + np.arange(spf, dtype=np.int64)[None, :]).ravel()
        np.clip(pos, 0, len(x) - 1, out=pos)
        xw = np.zeros(n)
        take = min(n, pos.size)
        xw[:take] = x[pos[:take]]
        out = mask * y + (1 - mask) * xw
        band = freqs > SIB_HI_HZ                  # the sibilant restore
        share = sp_o[:, band].sum(1) / (sp_o.sum(1) + 1e-12)
        fric = (~voiced_o) & (share > SIB_SHARE)
        if fric.any():
            gf = np.where(fric, 10.0 ** (SIB_DB / 20.0), 1.0)
            gf = smooth(gf, max(1, int(SIB_RAMP_S / FRAME_S)))
            g = np.repeat(gf, spf)
            out *= g[:n] if len(g) >= n else np.pad(
                g, (0, n - len(g)), constant_values=1.0)
    if fade is not None:
        spf = int(fs * FRAME_S)
        a0, a1 = fade[0] * spf, min(fade[1] * spf, n)
        if a0 < n and a1 > a0:
            k = np.arange(a1 - a0)
            out[a0:a1] *= 0.5 + 0.5 * np.cos(np.pi * k / max(1, a1 - a0))
            out[a1:] = 0.0
    tip = int(0.004 * fs)
    if len(out) > 2 * tip:
        wtip = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
        out[:tip] *= wtip
        out[-tip:] *= wtip[::-1]
    return out


def diatonic_shift_cents(f0_o, degrees):
    """Move each frame `degrees` scale steps in the lane's own scale."""
    st = 12.0 * np.log2(np.maximum(f0_o, 1e-6) / TONIC)
    idx = np.argmin(np.abs(st[:, None] - STEPS[None, :]), axis=1)
    tgt = np.clip(idx + degrees, 0, len(STEPS) - 1)
    return (STEPS[tgt] - STEPS[idx]) * 100.0


def edge_fade(y, fs, s=0.010):
    tip = max(2, int(s * fs))
    if len(y) > 2 * tip:
        w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
        y = y.copy()
        y[:tip] *= w
        y[-tip:] *= w[::-1]
    return y


# ── main ──────────────────────────────────────────────────────────────
x_all, FS = sf.read(SRC, dtype="float64")
if x_all.ndim > 1:
    x_all = x_all.mean(axis=1)

BEAT = SPB
last = CHART["phrases"][-1]
total_beats = COUNT_IN + int(np.ceil((last["beat"] + last["beats_total"]) / 4) * 4) + 4
dur = total_beats * BEAT + 1.0
mix = np.zeros(int(dur * FS))
voc = np.zeros(int(dur * FS))         # vocal-only, for the QC re-measure
halo_extra = np.zeros(int(dur * FS))

# the click grid (same voice as xpld-clickvox)
def kick():
    n = int(0.065 * FS)
    t = np.arange(n) / FS
    f = 150 * np.exp(np.log(52 / 150) * t / t[-1])
    return np.sin(2 * np.pi * np.cumsum(f) / FS) * np.exp(-t * 40) * 0.9

def tick():
    n = int(0.012 * FS)
    t = np.arange(n) / FS
    return np.sin(2 * np.pi * 1800 * t) * np.exp(-t * 350) * 0.35

k_, tk_ = kick(), tick()
for b in range(total_beats):
    s = int(b * BEAT * FS)
    g = k_ if b % 4 == 0 else tk_
    mix[s:s + len(g)] += g

receipt = {"source": os.path.basename(SRC), "tonic_hz": TONIC, "bpm": BPM,
           "snap": SNAP, "scale_semitones": SCALE.tolist(), "phrases": []}
manifest = {"primary": PRIMARY, "tonic_hz": TONIC, "bpm": BPM,
            "phrases": {}}
dub_audio = {}

for pi, ph in enumerate(CHART["phrases"]):
    words = ph["words"]
    t0 = words[0]["src"][0] - PRE_S
    t1 = words[-1]["src"][1] + POST_S
    a0, a1 = max(0, int(t0 * FS)), min(len(x_all), int(t1 * FS))
    xs = x_all[a0:a1]
    a = analyze(xs, FS)
    # slice-relative word dicts for the boundary repair
    ws = [{"word": w["word"], "start": w["src"][0] - t0,
           "end": w["src"][1] - t0} for w in words]
    ws, blog = snap_boundaries(a, ws)
    ws, clog = spread_crammed(ws)
    unit_src = [(int(round(w["start"] / FRAME_S)),
                 int(round(w["end"] / FRAME_S))) for w in ws]
    unit_src = [(s0, max(s0 + 2, s1)) for s0, s1 in unit_src]
    unit_src, tlog = trim_units(xs, FS, unit_src)
    unit_src = keep_attacks(unit_src, xs, FS)
    targets = [w.get("st_target") for w in words]
    f0c = regulate(a, unit_src, targets)
    beats = [w["beat"] for w in words]
    dursb = [w["dur_beats"] for w in words]
    tail_end = None
    if "tail_end_s" in ph:
        tail_end = int((ph["tail_end_s"] - t0 - 0.05) / FRAME_S)
    idx, holds, fade, Z, ants, ratios, synth_rel, T = \
        build_warp(a, unit_src, beats, dursb, tail_end=tail_end)
    f0_o = f0c[idx]
    # THE HOLD: flatten to the target with vibrato fading in
    for (h0, h1, u) in holds:
        st = targets[u]
        if st is None:
            continue
        tgt_hz = TONIC * 2.0 ** (st / 12.0)
        tsec = np.arange(h1 - h0) * FRAME_S
        vib = VIB_CENTS * np.clip(tsec / VIB_RISE_S, 0, 1) \
            * np.sin(2 * np.pi * VIB_HZ * tsec)
        seg = a["voiced"][idx[h0:h1]]
        f0_o[h0:h1] = np.where(seg, tgt_hz * 2.0 ** (vib / 1200.0),
                               f0_o[h0:h1])
    y = synth_from(a, idx, f0_o, fade=fade)
    # placement: phrase beat 0 lands on the grid; lead-in Z plays ahead
    lead_s = Z * FRAME_S
    pos = int(((COUNT_IN + ph["beat"]) * BEAT - lead_s) * FS)
    n = min(len(y), len(mix) - pos)
    mix[pos:pos + n] += y[:n] * 0.95
    voc[pos:pos + n] += y[:n] * 0.95
    # THE HALO (octave, vowels-only, dark) + low self-backup
    halo = synth_from(a, idx, f0_o * 2.0 ** (6 / 1200.0) * 2.0,
                      dark=HALO_DARK_HZ, breath_x=1.5, vowels_only=True,
                      fade=fade)
    halo2 = synth_from(a, idx, f0_o * 2.0 ** (-7 / 1200.0) * 2.0,
                       dark=HALO_DARK_HZ, breath_x=1.5, vowels_only=True,
                       fade=fade)
    hsum = np.zeros(max(len(halo), len(halo2)))
    hsum[:len(halo)] += halo * 0.5
    hsum[:len(halo2)] += halo2 * 0.5
    hn = min(len(hsum), len(halo_extra) - pos)
    halo_extra[pos:pos + hn] += hsum[:hn] * HALO_GAIN
    for deg, gain in BACKUP_GAINS.items():
        sh = diatonic_shift_cents(f0_o, deg)
        yb = synth_from(a, idx, f0_o * 2.0 ** (sh / 1200.0),
                        dark=HALO_DARK_HZ, breath_x=1.3, fade=fade)
        bn = min(len(yb), len(halo_extra) - pos)
        halo_extra[pos:pos + bn] += yb[:bn] * gain
    # ── the bank: rendered phrase + raw word carves ───────────────────
    slug = re.sub(r"[^a-z0-9]+", "-",
                  " ".join(w["word"] for w in words[:4]).lower()).strip("-")
    name = f"{pi:02d}-{slug}"
    peak = np.max(np.abs(y)) or 1.0
    sf.write(os.path.join(VOX, name + ".wav"),
             (y * 0.9 / peak).astype(np.float32), FS)
    word_rows = []
    for u, w in enumerate(words):
        s0, s1 = unit_src[u]
        seg = edge_fade(xs[s0 * int(FS * FRAME_S):s1 * int(FS * FRAME_S)], FS)
        wslug = re.sub(r"[^a-z0-9]+", "-", w["word"].lower()).strip("-") or "x"
        wfile = f"words/{pi:02d}-{u:02d}-{wslug}.wav"
        wpeak = np.max(np.abs(seg)) or 1.0
        sf.write(os.path.join(VOX, wfile),
                 (seg * 0.9 / wpeak).astype(np.float32), FS)
        row = {"word": w["word"], "file": wfile, "take": PRIMARY,
               "src": [round(t0 + s0 * FRAME_S, 3),
                       round(t0 + s1 * FRAME_S, 3)]}
        for key in ("note", "st_target", "hz_measured"):
            if key in w:
                row[key] = w[key]
        word_rows.append(row)
    entry = dict(
        phrase=ph["phrase"], take=PRIMARY, beat=ph["beat"],
        beats_total=ph["beats_total"],
        src=[round(t0 + unit_src[0][0] * FRAME_S, 3),
             round(t0 + unit_src[-1][1] * FRAME_S, 3)],
        lead_in_s=round(lead_s, 3), dur_s=round(len(y) / FS, 3),
        release_synthesized=bool(synth_rel),
        words=word_rows,
    )
    # ── the dub carve: the better take's phrase, raw ──────────────────
    if "dub" in ph:
        dub = ph["dub"]
        tid = dub["take"]
        if tid not in dub_audio:
            xd, fsd = sf.read(os.path.join(LANE, "source",
                                           f"xpld-{tid}.wav"), dtype="float64")
            dub_audio[tid] = xd.mean(axis=1) if xd.ndim > 1 else xd
        xd = dub_audio[tid]
        d0 = max(0.0, dub["t"][0] - 0.15)
        d1 = dub["t"][1] + 0.45
        if dub.get("tail_end_s"):
            d1 = min(d1, dub["tail_end_s"] - 0.05)
        seg = edge_fade(xd[int(d0 * FS):int(d1 * FS)], FS, s=0.020)
        dfile = f"dubs/{name}--{tid}.wav"
        dpeak = np.max(np.abs(seg)) or 1.0
        sf.write(os.path.join(VOX, dfile),
                 (seg * 0.9 / dpeak).astype(np.float32), FS)
        entry["dub"] = {
            "file": dfile, "take": tid, "src": [round(d0, 3), round(d1, 3)],
            "matched_text": dub["matched_text"],
            "words": dub["words"],
            "note": ("the alternate performance, raw — its own register/"
                     "contour (see hz_dub); the render does not use it"),
        }
    manifest["phrases"][name] = entry
    # ── QC: re-measure the rendered phrase against the chart ──────────
    # (per word, with the tracker BRACKETED around the target — an
    # unbracketed harvest octave-flips on fry lows and breathy highs and
    # then reports the flip, not the render)
    yq = np.ascontiguousarray(y, dtype=np.float64)
    spf_q = int(round(FS * FRAME_S))
    qc_words = []
    for u, w in enumerate(words):
        if w.get("st_target") is None:
            qc_words.append(None)
            continue
        q0 = T[u]
        q1 = T[u + 1] if u + 1 < len(words) else \
            Z + int(round((beats[u] + dursb[u]) * SPB / FRAME_S))
        lo = q0 + int(0.2 * (q1 - q0))
        hi = max(lo + 8, q0 + int(0.85 * (q1 - q0)))
        seg = yq[lo * spf_q:hi * spf_q]
        cents = None
        if len(seg) > spf_q * 6:
            tgt_hz = TONIC * 2.0 ** (w["st_target"] / 12.0)
            fq, _ = pw.harvest(seg, FS, f0_floor=max(45.0, tgt_hz * 0.55),
                               f0_ceil=min(900.0, tgt_hz * 1.9),
                               frame_period=FRAME_MS)
            fv = fq[fq > 0]
            if len(fv) >= 3:
                cents = int(round(1200.0 * np.log2(
                    float(np.median(fv)) / tgt_hz)))
        qc_words.append(cents)
    wr = []
    for u, w in enumerate(words):
        wr.append(dict(
            word=w["word"], note=w.get("note"), beat=w["beat"],
            dur_beats=w["dur_beats"],
            runway_ms=int(ants[u] * FRAME_MS),
            stretch=round(ratios[u], 2),
            hold=any(h[2] == u for h in holds),
            qc_cents=qc_words[u],
        ))
    receipt["phrases"].append(dict(
        phrase=ph["phrase"], beat=ph["beat"], floor_hz=round(a["floor"], 1),
        boundary_moves=blog, cram_spreads=clog, trims=tlog, words=wr,
        release_synthesized=bool(synth_rel),
    ))
    hstr = " ".join(f"{w['word']}({w['stretch']}x{'H' if w['hold'] else ''})"
                    for w in wr)
    print(f"phrase {pi:2d} beat {ph['beat']:3d} floor {a['floor']:5.1f} "
          f"lead {lead_s * 1000:3.0f}ms  {hstr}")

# QC summary: cents-from-chart across every measurable word
allqc = [w["qc_cents"] for p in receipt["phrases"] for w in p["words"]
         if w["qc_cents"] is not None]
receipt["qc"] = {
    "method": ("WORLD harvest over each rendered phrase; per word, median "
               "f0 over the middle of its beat slot vs the chart target"),
    "words_measured": len(allqc),
    "median_abs_cents": int(np.median(np.abs(allqc))) if allqc else None,
    "p90_abs_cents": int(np.percentile(np.abs(allqc), 90)) if allqc else None,
    "worst": sorted(
        [(w["word"], w["qc_cents"]) for p in receipt["phrases"]
         for w in p["words"] if w["qc_cents"] is not None],
        key=lambda r: -abs(r[1]))[:8],
}

peak = np.max(np.abs(mix)) or 1.0
mix *= 0.89 / peak
sf.write(os.path.join(OUT, "xpld-aesthetivox.wav"), mix.astype(np.float32), FS)
hmix = mix + halo_extra * (0.89 / peak)
hp = np.max(np.abs(hmix)) or 1.0
hmix *= 0.89 / hp
sf.write(os.path.join(OUT, "xpld-aesthetivox-halo.wav"),
         hmix.astype(np.float32), FS)
for stem in ("xpld-aesthetivox", "xpld-aesthetivox-halo"):
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error",
                    "-i", os.path.join(OUT, stem + ".wav"),
                    "-b:a", "192k", os.path.join(OUT, stem + ".mp3")],
                   check=True)
json.dump(receipt, open(os.path.join(ANA, "aesthetivox.json"), "w"), indent=1)
json.dump(manifest, open(os.path.join(VOX, ".manifest.json"), "w"), indent=1)
print(f"QC: {receipt['qc']['words_measured']} words, median "
      f"{receipt['qc']['median_abs_cents']}c, p90 "
      f"{receipt['qc']['p90_abs_cents']}c from chart")
print("wrote out/xpld-aesthetivox.wav+.mp3, out/xpld-aesthetivox-halo.wav+.mp3,"
      f" vox/ ({len(manifest['phrases'])} phrases), analysis/aesthetivox.json")
