# halo3.py — the v4 bank: Camille, regulated, on the beat.
#
# @jeffrey (2026-08-17, in order): "i want the lyrics starting right
# away" · "more musical notes" · "some words directly seem to just cut
# off" · "we need a smoother time stretching so we dont cut off her
# words" · "i wish our musical notes were the same as her voice" · "we
# could give her a backup vocal to match the words" · "i guess i want
# that world snapping for camille / that regulation" · "keep a pretty
# strict beat that the lyrics can now regulate around".
#
# So v4 moves the lane from bedroom ballad to dance floor, and this
# script does the vocal half: every charted phrase is WORLD-analyzed
# once and re-rendered onto a 122 BPM beat chart —
#
#   THE REGULATION   snap strength 0.92 (up from v2/v3's 0.70): her
#                    notes become NOTES, dead on the A#-minor grid in
#                    her own 237 Hz frame; the 45 ms correction smoothing
#                    still lets slides be slides.
#   THE WARP         per-word frame-axis time warp onto the chart:
#                    each word's onset lands on its beat slot, and the
#                    stretch is absorbed by VOICED frames (weight 1.0)
#                    while consonants ride near 1:1 (weight 0.18) — the
#                    smoother stretching; no word ends at a fade, the
#                    slice tail always plays 1:1 after the last word.
#   THE HOLD         a unit stretched past 1.8× flattens to its median
#                    grid tone with vibrato fading in over 0.4 s and a
#                    ±2.2-frame read shimmer (halo2's frozen-envelope
#                    trick) — "stone" and "pass" hold for whole bars.
#   THE HALO         the octave self-choir pair (f0 × 2, ±6/−7 ¢,
#                    vowels-only, darker) rendered FROM THE SAME WARP,
#                    so the halo locks to the chart sample-for-sample.
#   THE BACKUP       Camille sings backup for Camille: full-word
#                    renders (consonants composited, not vowels-only)
#                    at the diatonic 3rd and 5th BELOW, darker and
#                    breathier — a backing line that matches the words.
#
# Writes vox4/*.wav + vox4/.manifest.json + c/loner-chart.h — the
# generated header the C engine (c/lonerremix.c) reads the pluck-melody
# chart from, so the band's notes ARE her notes.
#
#   pop/.venv/bin/python pop/loner/bin/halo3.py

import json, os
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX4 = os.path.join(LANE, "vox4")
CDIR = os.path.join(LANE, "c")
os.makedirs(VOX4, exist_ok=True)
os.makedirs(CDIR, exist_ok=True)

TONIC = 237.0
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
FRAME_MS = 5.0
FRAME_S = FRAME_MS / 1000.0
FLOOR = 140.0
SNAP = 0.92                 # THE REGULATION (v3 was 0.70)
SMOOTH_MS = 45.0
FORMANT_DB = 1.6
AIR_DB = 2.5
BREATH = 0.14
HALO_DARK_HZ = 5500.0
HALO_BREATH_X = 1.5
UNVOICED_W = 0.18           # consonant share of any stretch
HOLD_RATIO = 1.8            # stretch beyond this → flat tone + vibrato

BPM = 122.0
SPB = 60.0 / BPM            # seconds per beat


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


def cents_to_grid(hz):
    cents = 1200.0 * np.log2(hz / TONIC)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([MINOR * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def diatonic_delta(hz, degrees):
    """Cents to add to move each frame `degrees` scale degrees (± ok)."""
    cents = 1200.0 * np.log2(hz / TONIC)
    steps = np.concatenate([MINOR + 12 * o for o in range(-3, 5)])
    idx = np.argmin(np.abs(cents[:, None] - steps[None, :] * 100.0), axis=1)
    tgt = np.clip(idx + degrees, 0, len(steps) - 1)
    return (steps[tgt] - steps[idx]) * 100.0


def shelf(freqs, centre, width):
    return 1.0 / (1.0 + np.exp(-(freqs - centre) / width))


def analyze(x, fs):
    f0_raw, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=FLOOR)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=FLOOR)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    return dict(x=x, fs=fs, f0c=f0c, sp=sp, ap=ap, voiced=voiced)


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


def dress(y, fs, tip_s=0.004):
    peak = np.max(np.abs(y)) or 1.0
    y = y * (0.90 / peak)
    tip = int(tip_s * fs)
    w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    y[:tip] *= w
    y[-tip:] *= w[::-1]
    return y.astype(np.float32)


# ── the chart — DERIVED from her own rhythm ───────────────────────────
# @jeffrey: "sometimes it feels like our words get faster and faster —
# i want a constant rate of utterance … a more regular rhythm, not
# change / realign it TOO much". So the chart is no longer hand-set:
# each phrase gets ONE uniform time-scale (her whole take onto its
# target beat count — the whole line lands on 48 beats, a 4% quicken)
# and then each word onset is nudged AT MOST a 16th note onto the
# 8th-note grid. Relative pacing is hers everywhere; nothing rushes,
# nothing crawls, and the regularity comes from the tiny nudges alone.
CHART = {
    # stretch: per word-unit index, a duration multiplier on top of the
    # uniform scale — @jeffrey: "'time to pass' sounds well durationed
    # but sitting and curled up could be longer". durs: EXACT beat
    # lengths, the word-by-word tuning knob ("sitting should take 4
    # beats") — wins over stretch and quantize.
    "w-whole-line":        { "slice": "f-whole-line",        "beats": 56.0,
                             # splits: original unit indices to cut at their
                             # internal fricative (myself → my·self), so the
                             # bar map can be: sitting = bar 1 · curled up =
                             # bar 2 · in my = bar 3 · self i = bar 4 — and
                             # "think" lands on the bar-5 downbeat. durs are
                             # POST-split indices.
                             "splits": [5],
                             # then: think = 2 · OF (the octave) = 4 ·
                             # a stone = 4
                             "durs": { 0: 4.0, 1: 1.5, 2: 2.0, 3: 0.5,
                                       4: 1.0, 5: 3.0, 6: 3.0, 7: 1.0,
                                       8: 2.0, 9: 4.0, 10: 0.5, 11: 3.5 } },
    "w-sitting-curled":    { "slice": "f-sitting-curled",    "beats": 11.0 },
    "w-i-think":           { "slice": "f-i-think",           "beats": 3.5 },
    "w-of-a-stone":        { "slice": "f-of-a-stone",        "beats": 8.0 },
    "w-just-waiting":      { "slice": "f-just-waiting",      "beats": 6.5 },
    "w-very-patiently":    { "slice": "f-very-patiently",    "beats": 8.5 },
    "w-for-time-to-pass":  { "slice": "f-for-time-to-pass",  "beats": 12.5 },
    "w-n-getting-curled":  { "slice": "n-getting-curled",    "beats": 10.5 },
    "w-n-stone-waiting":   { "slice": "n-stone-waiting",     "beats": 16.5 },
    "w-n-for-time-to-pass": { "slice": "n-for-time-to-pass", "beats": 6.0 },
}


def derive_units(words, beats_total, stretch=None, durs=None):
    """Uniform scale + per-word stretch/exact durs + 8th-note quantize."""
    on0 = words[0]["start"]
    span = words[-1]["end"] - on0
    k = beats_total * SPB / span
    units, acc = [], 0.0
    for i, w in enumerate(words):
        end = words[i + 1]["start"] if i + 1 < len(words) else w["end"]
        d = (end - w["start"]) * k / SPB
        if stretch and i in stretch:
            d *= stretch[i]
        dq = max(0.5, round(d * 2) / 2.0)       # every word a whole number of 8ths
        if durs and i in durs:
            dq = durs[i]                        # the exact override wins
        units.append((acc, dq))                 # onsets land on-grid by construction
        acc += dq
    return units

SLICES = json.load(open(os.path.join(LANE, "samples", ".manifest.json")))


def build_warp(a, unit_src, beats, dursb):
    """Frame index map with VOWEL-ON-THE-BEAT alignment.

    Each word's voiced onset (its vowel) lands exactly on its chart
    slot; the consonant runway plays 1:1 just AHEAD of the beat — the
    way a singer actually places a word. Stretch lives in the voiced
    body (weight 1.0 vs 0.18 for unvoiced); pre-roll and the slice tail
    ride 1:1. Returns (idx, holds, fade, Z) where Z is the output frame
    of the phrase's beat 0 (the lead-in the C engine subtracts).
    """
    F = len(a["f0c"])
    w = np.where(a["voiced"], 1.0, UNVOICED_W)
    ants = []                                   # consonant frames per unit
    for (s0, s1) in unit_src:
        v0 = s0
        lim = min(s0 + int(0.20 / FRAME_S), s1 - 1, F - 1)
        while v0 < lim and not a["voiced"][v0]:
            v0 += 1
        ants.append(v0 - s0 if a["voiced"][min(v0, F - 1)] else 0)
    Z = unit_src[0][0] + ants[0]
    T = [Z + int(round(b * SPB / FRAME_S)) for b in beats]
    Tend = [Z + int(round((b + d) * SPB / FRAME_S)) for b, d in zip(beats, dursb)]
    idx = list(range(0, unit_src[0][0] + ants[0]))  # pre + consonant, 1:1
    holds = []
    for u, (s0, s1) in enumerate(unit_src):
        s0, s1 = max(0, min(s0, F - 1)), max(1, min(s1, F))
        v0 = min(s0 + ants[u], s1 - 1)
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
        seg_w = w[v0:s1].copy() if s1 > v0 else np.ones(1)
        cum = np.concatenate([[0.0], np.cumsum(seg_w)])
        cum /= cum[-1]
        pos = np.interp((np.arange(out_n) + 0.5) / out_n, cum,
                        np.arange(len(cum), dtype=float)) - 0.5
        pos = np.clip(pos, 0, src_n - 1)
        if ratio > 2.2:                                       # read shimmer
            tsec = np.arange(out_n) * FRAME_S
            pos = np.clip(pos + 2.2 * np.sin(2 * np.pi * 0.85 * tsec), 0, src_n - 1)
        if ratio > HOLD_RATIO:
            holds.append((len(idx), len(idx) + out_n, v0, s1))
        idx.extend((v0 + np.round(pos).astype(int)).tolist())
        if u + 1 < len(unit_src):                 # next word's consonant, 1:1
            ns0 = unit_src[u + 1][0]
            idx.extend(range(ns0, ns0 + nxt_a))
    tail0 = unit_src[-1][1]
    idx += list(range(min(tail0, F), F))                      # tail, 1:1
    # THE RELEASE — when the source has no tail (she flows straight into
    # the next phrase in the take), synthesize one: a ping-pong read of
    # the final unit's last 120 ms, faded to nothing by the caller.
    fade = None
    if (F - tail0) * FRAME_S < 0.15:
        rel_n = int(0.40 / FRAME_S)
        s0, s1 = unit_src[-1]
        lo = max(s0, s1 - int(0.12 / FRAME_S))
        span = max(2, s1 - lo)
        fade = (len(idx), len(idx) + rel_n)
        for k in range(rel_n):
            p = lo + (span - 1) - abs((k % (2 * span - 2)) - (span - 1))
            idx.append(int(np.clip(p, 0, F - 1)))
    return np.array(idx, dtype=int), holds, fade, Z


def synth_from(a, idx, f0_o, *, dark=None, breath_x=1.0, vowels_only=False,
               air=True, formant=True, fade=None):
    fs, x = a["fs"], a["x"]
    sp_o = np.ascontiguousarray(a["sp"][idx])
    ap_o = np.ascontiguousarray(a["ap"][idx])
    voiced_o = a["voiced"][idx]
    freqs = np.linspace(0.0, fs / 2.0, sp_o.shape[1])
    if formant:
        sp_o = sp_o * (10.0 ** ((FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)) / 10.0))[None, :]
    if air:
        sp_o = sp_o * (10.0 ** (AIR_DB * shelf(freqs, 8000.0, 900.0) / 10.0))[None, :]
    if dark:
        sp_o = sp_o * (1.0 / (1.0 + (freqs / dark) ** 2))[None, :]
    # breath rides voiced run depth on the OUTPUT timeline
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
        # full words: rebuild a warped copy of the source for the consonants
        spf = int(fs * FRAME_S)
        xw = np.zeros(n)
        for j, sf_i in enumerate(idx):
            o0, o1 = j * spf, min((j + 1) * spf, n)
            if o0 >= n:
                break
            blk = x[sf_i * spf:sf_i * spf + (o1 - o0)]
            xw[o0:o0 + len(blk)] = blk
        out = mask * y + (1 - mask) * xw
    if fade is not None:
        spf = int(fs * FRAME_S)
        a0, a1 = fade[0] * spf, min(fade[1] * spf, n)
        if a0 < n:
            k = np.arange(a1 - a0)
            out[a0:a1] *= 0.5 + 0.5 * np.cos(np.pi * k / max(1, a1 - a0))
            out[a1:] = 0.0
    return dress(out, fs), fs


manifest = {}
chart_c = []   # (phrase, lead_in_s, beats_total, [(beat, dur, st)])

for name, ch in CHART.items():
    slice_name = ch["slice"]
    entry = SLICES[slice_name]
    src = os.path.join(LANE, "samples", f"{slice_name}.wav")
    x, fs = sf.read(src, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    a = analyze(x, fs)
    F = len(a["f0c"])
    t0_slice = entry["start"]
    words = list(entry["word_f0"])
    # sub-split units at their internal fricative (e.g. myself → my·self)
    for ui in sorted(ch.get("splits", []), reverse=True):
        w = words[ui]
        f0 = int(round((w["start"] - t0_slice) / FRAME_S))
        f1 = int(round((w["end"] - t0_slice) / FRAME_S))
        lo, run, split_f = f0 + max(3, (f1 - f0) // 4), 0, None
        for f in range(lo, min(f1, len(a["voiced"]))):
            if not a["voiced"][f]:
                run += 1
                if run >= 3:
                    split_f = f - run + 1
                    break
            else:
                run = 0
        if split_f is None:
            split_f = (f0 + f1) // 2
        ts = t0_slice + split_f * FRAME_S
        first = dict(w, end=ts, t=w["t"] + "·a")
        second = dict(w, start=ts, t=w["t"] + "·b")
        words[ui:ui + 1] = [first, second]
    ch["units"] = derive_units(words, ch["beats"], ch.get("stretch"), ch.get("durs"))

    # source frame bounds per unit (slice clock)
    unit_src = []
    for i, wd in enumerate(words):
        s0 = int(round((wd["start"] - t0_slice) / FRAME_S))
        s1 = int(round((wd["end"] - t0_slice) / FRAME_S))
        if i + 1 < len(words):
            s1 = int(round((words[i + 1]["start"] - t0_slice) / FRAME_S))
        unit_src.append((max(0, s0), min(F, max(s0 + 1, s1))))

    idx, holds, fade, Z = build_warp(a, unit_src,
                                     [b for (b, d) in ch["units"]],
                                     [d for (b, d) in ch["units"]])
    f0_o = a["f0c"][idx].copy()
    voiced_o = a["voiced"][idx]

    # THE HOLD — long stretches flatten to the unit's median grid tone
    for (o0, o1, s0, s1) in holds:
        seg = a["f0c"][s0:s1][a["voiced"][s0:s1]]
        if not len(seg):
            continue
        med = np.median(seg)
        st = np.round(12.0 * np.log2(med / TONIC))
        # keep to the A#-minor grid
        steps = np.concatenate([MINOR + 12 * o for o in range(-3, 5)])
        st = steps[np.argmin(np.abs(steps - st))]
        tgt = TONIC * 2.0 ** (st / 12.0)
        n = o1 - o0
        tsec = np.arange(n) * FRAME_S
        vib = 2.0 ** (0.15 * np.clip((tsec - 0.4) / 0.4, 0, 1)
                      * np.sin(2 * np.pi * 5.2 * tsec) / 12.0)
        blend = np.clip(tsec / 0.12, 0, 1)          # glide into the flat tone
        f0_o[o0:o1] = np.where(voiced_o[o0:o1],
                               f0_o[o0:o1] * (1 - blend) + tgt * vib * blend, 0.0)

    renders = {}
    out, _ = synth_from(a, idx, f0_o, fade=fade)
    sf.write(os.path.join(VOX4, f"{name}.wav"), out, fs)
    renders["lead"] = round(len(out) / fs, 3)

    for tag, cents in (("8ve-a", 1200 + 6), ("8ve-b", 1200 - 7)):
        out, _ = synth_from(a, idx, f0_o * 2.0 ** (cents / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                            vowels_only=True, air=False, fade=fade)
        sf.write(os.path.join(VOX4, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    # THE BACKUP — full words at the 3rd and 5th below, darker, ±det
    for tag, deg, det in (("low3", -2, 5.0), ("low5", -4, -6.0)):
        delta = np.zeros_like(f0_o)
        v = voiced_o & (f0_o > 0)
        if v.any():
            delta[v] = diatonic_delta(f0_o[v], deg)
        delta = smooth(delta, int(60.0 / FRAME_MS))
        out, _ = synth_from(a, idx, f0_o * 2.0 ** ((delta + det) / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X, air=False,
                            fade=fade)
        sf.write(os.path.join(VOX4, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    lead_in = Z * FRAME_S
    beats_total = ch["units"][-1][0] + ch["units"][-1][1]
    notes = []
    for wd, (beat, durb) in zip(words, ch["units"]):
        st = int(np.round(12.0 * np.log2(wd["f0_hz"] / TONIC))) if wd["f0_hz"] else 0
        notes.append((beat, durb, st, wd["t"].strip()))
    chart_c.append((name, lead_in, beats_total, notes))
    manifest[name] = dict(slice=slice_name, lead_in=round(lead_in, 3),
                          beats=beats_total, renders=renders,
                          words=entry["words"])
    print(f"  {name:22s} {renders['lead']:5.2f}s  lead·8ve×2·low3·low5  «{entry['words']}»")

json.dump(manifest, open(os.path.join(VOX4, ".manifest.json"), "w"), indent=1)

# ── the generated header — her melody, for the band ───────────────────
lines = [
    "// loner-chart.h — GENERATED by bin/halo3.py; do not edit.",
    "// The v4 beat chart: per phrase, its lead-in (consonant runway before",
    "// beat 0), length in beats, and per word unit its slot, length, and",
    "// semitone above TONIC (237 Hz) — measured from Camille's own take, so",
    "// any instrument reading this plays HER melody.",
    "#pragma once",
    "",
    f"#define CHART_BPM {BPM}",
    f"#define CHART_TONIC {TONIC}",
    "",
    "typedef struct { double beat, dur; int st; } ChartNote;",
    "typedef struct { const char *name; double leadIn; double beats;",
    "                 int n; const ChartNote *notes; } ChartPhrase;",
    "",
]
for name, lead_in, beats_total, notes in chart_c:
    ident = name.replace("-", "_")
    lines.append(f"static const ChartNote {ident}_notes[] = {{")
    for (beat, durb, st, _t) in notes:
        lines.append(f"    {{ {beat:.2f}, {durb:.2f}, {st} }},")
    lines.append("};")
lines.append("")
lines.append("static const ChartPhrase CHART[] = {")
for name, lead_in, beats_total, notes in chart_c:
    ident = name.replace("-", "_")
    lines.append(f'    {{ "{name}", {lead_in:.3f}, {beats_total:.2f}, '
                 f"{len(notes)}, {ident}_notes }},")
lines.append("};")
lines.append(f"#define CHART_N {len(chart_c)}")
lines.append("")
open(os.path.join(CDIR, "loner-chart.h"), "w").write("\n".join(lines))

# the labeled chart, for tooling (the timeline video reads this)
json.dump({name: dict(leadIn=round(li, 3), beats=bt,
                      notes=[dict(beat=b, dur=d, st=s, t=t) for (b, d, s, t) in ns])
           for (name, li, bt, ns) in chart_c},
          open(os.path.join(VOX4, ".chart.json"), "w"), indent=1)
print(f"WROTE {VOX4}/.manifest.json + .chart.json + {CDIR}/loner-chart.h ({len(chart_c)} phrases)")
