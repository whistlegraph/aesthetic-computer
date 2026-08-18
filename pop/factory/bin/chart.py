# chart.py — the v3 bank: the chant, regulated, on the factory's own grid.
#
# The lane's v1 stamped raw slices on principle and v2 pressed every vocal
# through the aesthetivox. Neither of them put the words on a BEAT — each
# line was stamped as one die-block at the take's own internal word
# offsets, in seconds. v3 is the regulation: every word of the unbroken
# take is WORLD-analyzed once and re-rendered onto a 100 BPM beat chart,
# which is the move the title has been asking for since v1. A cookie
# cutter does not approximate.
#
#   THE REGULATION  snap 0.90 onto D natural minor in HER OWN frame, with
#                   v2's warble fixes carried in: the f0 track is
#                   de-spiked (octave-class tracking errors folded, not
#                   dropped) BEFORE anything reads it, the target tone
#                   comes off a 35 ms median contour rather than the raw
#                   per-frame pitch, and the correction is smoothed 45 ms.
#                   These chant hits glide 1–3 semitones per 5 ms frame;
#                   a naive per-frame target flaps between adjacent scale
#                   tones and writes square-wave FM into WORLD's f0. That
#                   was the "bad glitchy aesthetivox" of v2's first press
#                   and it is not being re-learned.
#   THE WARP        per-word frame-axis time warp onto the chart: each
#                   word's vowel lands on its beat slot, the consonant
#                   runway plays 1:1 just ahead of it, stretch is absorbed
#                   by VOICED frames (weight 1.0) while consonants ride
#                   near 1:1 (0.18) and silence inside a word hardly moves
#                   at all (0.04).
#   THE HOLD        a unit stretched past 1.8× flattens to its median grid
#                   tone with vibrato fading in — right for a sustained
#                   vowel ("in"), wrong for anything carrying syllables,
#                   which is what `nohold` is for.
#   THE FLIP        the whistlegraph's own melody, kept: she flips an
#                   octave inside a word all the time (factory ends C#4,
#                   "from" drops D4→Bb2), and the words where each
#                   SYLLABLE owns a flip are split at their event boundary
#                   so each gets its own note, slot and beat.
#   THE HALO        the octave self-choir pair rendered FROM THE SAME
#                   WARP, so it locks sample-for-sample.
#   THE BACKUP      full-word renders at the diatonic 3rd and 5th below.
#
# Writes vox3/*.wav + vox3/.manifest.json + vox3/.chart.json +
# c/factory-chart.h — the generated header the C engine reads its melody
# from, so the machine's notes ARE the chant's notes.
#
#   pop/.venv/bin/python pop/factory/bin/chart.py
#
# ── HER D, and why it is not 147.0 ────────────────────────────────────
# harvest.json records chant_root_hz 147.0 and the README rounds it to
# equal-tempered D3 (146.83). Both are medians over EVERY voiced frame of
# the take — glides, octave-tracking errors and the octave-up words all
# included. Converging a ±60 ¢ window onto the SUSTAINED frames only
# (pitch flat within 0.45 st across 90 ms) puts her D at 148.73 Hz, and
# with that as the frame her stable pitch classes come out D 38% · Bb 22%
# · C 17% · F 9% — the D-minor spine the README describes, at 25 ¢ median
# deviation from the grid. So the tonic here is HER D, not the piano's,
# exactly as the loner lane used Camille's 237 Hz rather than A#3.

import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import audit                      # ONE event detector, shared with the audit

LANE = os.path.dirname(HERE)
VOX3 = os.path.join(LANE, "vox3")
CDIR = os.path.join(LANE, "c")
os.makedirs(VOX3, exist_ok=True)
os.makedirs(CDIR, exist_ok=True)

TONIC = audit.TONIC         # 148.73 — her D
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])       # D natural minor
FRAME_MS = 5.0
FRAME_S = FRAME_MS / 1000.0
FLOOR = 90.0                # her bird sits on Bb2 (115 Hz); 140 would miss it
SNAP = 0.90                 # THE REGULATION
TARGET_MED_MS = 35.0        # v2's fix: the target tone comes off a median
SMOOTH_MS = 45.0            # …and the correction is smoothed
CORR_CLAMP_C = 250.0        # v2's clamp: never bend a frame more than this
FORMANT_DB = 1.6
AIR_DB = 2.5
BREATH = 0.14
HALO_DARK_HZ = 5500.0
HALO_BREATH_X = 1.5
UNVOICED_W = 0.18
SILENT_W = 0.04
PEAK_LEAD_MAX_S = 0.09
HOLD_RATIO = 1.8

BPM = 100.0                 # the chant's own tempo (median syllable IOI
SPB = 60.0 / BPM            # 0.299 s = an eighth at 100.3)


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


def median_filt(x, frames):
    if frames <= 1:
        return x
    pad = frames // 2
    p = np.pad(x, (pad, pad), mode="edge")
    return np.median(np.lib.stride_tricks.sliding_window_view(p, frames), axis=1)[:len(x)]


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


CACHE = os.path.join(VOX3, ".cache")


def analyze_cached(path, x, fs):
    """WORLD analysis depends only on the audio and the analysis constants
    — never on the chart. Tuning a bar re-ran it every time. Cached on
    disk, keyed by mtime and parameters, so an edit pays for synthesis
    only."""
    key = (f"{os.path.basename(path)}-{int(os.path.getmtime(path))}-{FRAME_MS}"
           f"-{FLOOR}-{SNAP}-{SMOOTH_MS}-{TARGET_MED_MS}-{TONIC}")
    dest = os.path.join(CACHE, key + ".npz")
    if os.path.exists(dest):
        z = np.load(dest)
        return dict(x=x, fs=fs, f0=z["f0"], f0c=z["f0c"],
                    sp=z["sp"].astype(np.float64), ap=z["ap"].astype(np.float64),
                    voiced=z["voiced"], folds=int(z["folds"]))
    a = analyze(x, fs)
    os.makedirs(CACHE, exist_ok=True)
    np.savez(dest, f0=a["f0"], f0c=a["f0c"], voiced=a["voiced"],
             folds=a["folds"],
             sp=a["sp"].astype(np.float32), ap=a["ap"].astype(np.float32))
    return a


def analyze(x, fs):
    f0_raw, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=FLOOR)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=FLOOR)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    # DE-SPIKE FIRST. v2's post-mortem: harvest at this floor drops 1–3
    # octave-class tracking errors into every word, and a correction
    # computed against a spike is a correction that fights the voice.
    f0, folds = audit.despike(f0)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        # THE TARGET COMES OFF A MEDIAN, not the raw frame. These hits
        # glide; a per-frame nearest-tone target flaps between adjacent
        # scale tones dozens of times a word, which is FM, not tuning.
        smoothed = f0.copy()
        smoothed[voiced] = median_filt(f0[voiced], int(TARGET_MED_MS / FRAME_MS) | 1)
        corr[voiced] = -cents_to_grid(smoothed[voiced]) * SNAP
        corr = np.clip(corr, -CORR_CLAMP_C, CORR_CLAMP_C)
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    return dict(x=x, fs=fs, f0=f0, f0c=f0c, sp=sp, ap=ap, voiced=voiced,
                folds=folds)


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


# ── the chart — DERIVED from the take, PINNED to the audio ────────────
#
# Every `times` value below is a boundary read off bin/audit.py's event
# table, not off a transcript. The receipts, in full:
#
#   whisper.cpp (harvest.json)  gave ' Spin'@13.85 'ning'@14.45 — one word
#                               split at its /n/ — and cut samples/line3
#                               and samples/spinning at 13.85, which is
#                               0.66 s AFTER she starts singing it. There
#                               is silence in the take from 12.37 to 13.19;
#                               "spinning" begins at 13.19. Those two
#                               slices are missing their first syllable.
#   whisper-1 (samples/.align)  heard 20 of the 21 units right and got
#                               "factory" as TWO words, "to read" — the
#                               initial /f/ is 35 ms and it could not
#                               segment it. It also gave "we're" 0.68 s,
#                               which swallows "in" whole (audit: SLICES
#                               event 23, 0.42 s in / 0.17 s left outside),
#                               and ended "bird" 0.55 s before she stops
#                               singing it.
#   pinned (below)              all 20 starts, from the event boundaries.
#
# So: `merge` puts "to"+"read" back together as one word; `times` pins
# every start; `sylls` cuts the four words whose syllables each own a
# whistle flip so each gets its own note.
CHART = {
    "f-whole-poem": {
        "slice": "chant-full",
        "beats": 31.5,
        # whisper-1 split "factory" into two words it could pronounce.
        "merge": {0: (2, "factory")},          # index: (how many, label)
        # THE PINS — source seconds, from bin/audit.py's events. Indices
        # are post-merge, pre-split.
        "times": {0: 0.000,   # factory   ev0  (C#3 · D3 · C#4)
                  1: 0.850,   # cookie    ev3  (D3 · C4)
                  2: 1.530,   # cutter    ev5  (Bb3 · C#4)
                  3: 2.168,   # personalities ev7–ev10 (D3 drone · F#3 · F3 · E4)
                  4: 4.000,   # we        ev11 (C3)   — whisper-1 said 3.70, inside "-ties"
                  5: 4.550,   # must      ev12 (A3 · C3)
                  6: 5.310,   # break     ev14 (D3)
                  7: 5.885,   # free      ev15 (D3)
                  8: 6.360,   # from      ev16 (D4 · Bb2 — the biggest drop in the poem)
                  9: 6.965,   # the       ev18 (C4)
                  10: 7.348,  # states    ev19 (A3)
                  11: 7.903,  # that      ev21 (A3)
                  12: 8.268,  # we're     ev22 (G3)
                  13: 8.595,  # in        ev23 (F3)  — whisper-1 said 9.02, 0.43 s late
                  14: 9.890,  # spinning  ev24 (E3 · G3) — whisper.cpp said 13.85 take-clock, 0.66 s late
                  15: 10.915,  # away      ev26 (D3)
                  16: 12.015,  # i         ev27 (A3 · C3)
                  17: 13.015,  # hear      ev29 (D4)
                  18: 13.900,  # a         ev30 (Bb3 · Bb2)
                  19: 14.690},  # bird     ev32 (A3 · Bb2, held)
        # THE LAST WORD'S END, pinned too. Only starts come from `times`,
        # because every other word's end IS its neighbour's start — but
        # the final word has no neighbour, so it keeps whatever end the
        # transcriber gave it, and whisper-1 ends "bird" at 15.26 while
        # she is still singing it until 15.805. bin/audit.py caught this
        # as `bird: SLICES event 33 (0.39s in, 0.55s left outside)` — half
        # a second of the poem's last word, cut off, on the one word the
        # whole record is walking toward.
        "ends": {19: 15.805},
        # THE FLIPS. Split only where the word has MULTIPLE SYLLABLES and
        # each syllable owns its own event — the whistlegraph gesture. NOT
        # "from" or "a" or "bird", which flip an octave inside ONE syllable
        # and would be broken in half by a split.
        # The cut goes at the START of the closure, not its middle: the /k/
        # of "cookie" belongs to the SECOND syllable (koo-kie), so putting
        # the boundary at 1.270 gave "cook" 75 ms of silence to stretch and
        # squashed its vowel to 0.67×. At 1.195 the closure rides in front
        # of "ie" as its consonant runway, which is where a singer puts it.
        "sylls": {0: [(None, "fac"), (0.295, "to"), (0.665, "ry")],
                  1: [(None, "cook"), (1.195, "ie")],
                  2: [(None, "cut"), (1.835, "ter")],
                  14: [(None, "spin"), (10.495, "ning")]},
        # THE BAR MAP, post-split indices. The whole point of the lane is
        # that a die stamps the same shape every time, so the poem gets a
        # machine grid: her syllables ARE eighths at 100 (median IOI
        # 0.299 s), so line 1's seven syllables take seven eighths and the
        # drone gets the rest of bar 0.
        #   bar 0   fac·to·ry cook·ie cut·ter  ·  personalities (3.5)
        #   bar 1   personalities holding to 6.5, then her breath
        #   bar 2   we(8) must(9) break(10.5) free(11.5)
        #   bar 3   from(12.5) the(13.5) states(14) that(15) we're(15.5)
        #   bar 4   IN — the line lands held on the downbeat, then the gap
        #   bar 5   spin(20) ning(21) away(22)
        #   bar 6   i(24) hear(26)
        #   bar 7   a(27.5) bird(29 → 31.5)
        "durs": {0: 0.5, 1: 0.5, 2: 0.5, 3: 0.5, 4: 0.5, 5: 0.5, 6: 0.5,
                 7: 3.0,
                 8: 1.0, 9: 1.5, 10: 1.0, 11: 1.0, 12: 1.0, 13: 0.5,
                 14: 1.0, 15: 0.5, 16: 0.5, 17: 2.0,
                 18: 1.0, 19: 1.0, 20: 2.0, 21: 2.0, 22: 1.5, 23: 1.5,
                 24: 2.5},
        # rest AFTER the given unit, in beats — her own air. She breathes
        # 0.4 s after "personalities" and 0.83 s after "in"; the chart
        # rounds both up to land the next line on a bar line.
        "gaps": {7: 1.5, 17: 2.0},
        # words whose syllables carry a melody must not be flattened to
        # one tone by THE HOLD. "personalities" is five syllables in one
        # unit (per·son·al are all on the same D3 drone, so there is no
        # event to split them at) and "away" is two.
        "nohold": (7, 20),
        "lead": 0.0,          # she opens ON the /f/; there is no pickup to build
        "end": 15.82,         # the tail plays 1:1 after the last word — and
                              # her bird's audio ends at 15.805. Without the
                              # cap the slice's last 74 ms of room would sing.
    },
}


def derive_units(words, beats_total, stretch=None, durs=None, gaps=None):
    """Uniform scale + per-word stretch/exact durs + 8th-note quantize."""
    on0 = words[0]["start"]
    span = words[-1]["end"] - on0
    k = beats_total * SPB / span if span > 0 else 1.0
    units, acc = [], 0.0
    for i, w in enumerate(words):
        end = words[i + 1]["start"] if i + 1 < len(words) else w["end"]
        d = (end - w["start"]) * k / SPB
        if stretch and i in stretch:
            d *= stretch[i]
        dq = max(0.5, round(d * 2) / 2.0)
        if durs and i in durs:
            dq = durs[i]
        units.append((acc, dq))
        acc += dq + (gaps.get(i, 0.0) if gaps else 0.0)
    return units


def harvest_slices():
    """basename → the lane's own whisper.cpp receipt."""
    h = json.load(open(os.path.join(LANE, "harvest.json")))
    out = {}
    for take, t in h["takes"].items():
        for s in t.get("samples", []):
            out.setdefault(os.path.basename(s["file"])[:-4], dict(s, take=take))
    return out


SLICES = harvest_slices()

ALIGN_PATH = os.path.join(LANE, "samples", ".align.json")
ALIGN = json.load(open(ALIGN_PATH)) if os.path.exists(ALIGN_PATH) else {}

# ── boundary repair — for anything NOT pinned ─────────────────────────
# Pinned boundaries are already sitting on the acoustic event; this only
# runs where the chart is trusting a transcriber, and it pulls the
# boundary to the nearest real acoustic event — a sustained pitch step,
# or, for words opening on an unvoiced consonant, an energy valley.
SNAP_WIN_S = 0.250
SNAP_MED_S = 0.120
SNAP_STEP_ST = 0.50
SNAP_MIN_S = 0.080
SNAP_QUIET = 0.30


def snap_boundaries(a, words, t0, pinned=()):
    """Pull each unpinned word start onto the acoustic event nearest it."""
    x, fs, f0 = a["x"], a["fs"], a["f0"]
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    if m < 8 or len(words) < 2:
        return words, []
    if all(i in pinned for i in range(1, len(words))):
        return words, []          # nothing to guess at; the audio already said
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    st = np.where(f0[:m] > 0, 12.0 * np.log2(np.maximum(f0[:m], 1e-6) / TONIC), np.nan)
    W = max(2, int(round(SNAP_MED_S / FRAME_S)))
    step = np.zeros(m)
    if m > 2 * W:
        win = np.lib.stride_tricks.sliding_window_view(st, W)
        med = np.nanmedian(win, axis=1)
        cnt = (~np.isnan(win)).sum(axis=1)
        lo, hi = med[:m - 2 * W + 1], med[W:m - W + 1]
        okl, okh = cnt[:m - 2 * W + 1] >= W // 2, cnt[W:m - W + 1] >= W // 2
        d = np.abs(hi - lo)
        d[~(okl & okh) | np.isnan(d)] = 0.0
        step[W:m - W + 1] = d
    mins = int(round(SNAP_MIN_S / FRAME_S))
    win = int(round(SNAP_WIN_S / FRAME_S))
    out = [dict(w) for w in words]
    log = []
    for i in range(1, len(out)):
        if i in pinned:
            continue
        k0 = int(round((out[i]["start"] - t0) / FRAME_S))
        prev = int(round((out[i - 1]["start"] - t0) / FRAME_S))
        nxt = (int(round((out[i + 1]["start"] - t0) / FRAME_S))
               if i + 1 < len(out) else m)
        lo = max(prev + mins, k0 - win, W)
        hi = min(nxt - mins, k0 + win, m - W)
        if hi <= lo:
            continue
        kk = lo + int(np.argmax(step[lo:hi]))
        if step[kk] < SNAP_STEP_ST:
            seg = rms[lo:hi]
            kv = lo + int(np.argmin(seg))
            kk = kv if seg.min() < SNAP_QUIET * (np.median(seg) or 1.0) else k0
        if kk == k0:
            continue
        ts = t0 + kk * FRAME_S
        out[i]["start"] = ts
        out[i - 1]["end"] = ts
        log.append(f"{out[i]['t']} {(kk - k0) * FRAME_S * 1000:+.0f}ms")
    return out, log


# ── the energy trim — only SUNG frames stretch ────────────────────────
TRIM_GATE_DB = -36.0
TRIM_MARGIN_S = 0.050
TRIM_MIN_S = 0.080
TRIM_QUIET_RUN_S = 0.120
TRIM_LEAK_S = 0.150
ATTACK_S = 0.030
TRIM_KEEP = 0.35


def keep_attacks(unit_src, rest_src):
    """Pull each unit's start back 30 ms so its ATTACK survives. A word's
    onset is the loudest, most fragile thing in it, and a boundary aimed
    at where the NOTE changes lands a frame or two after where the SOUND
    starts.

    THE FLOOR IS THE WHOLE SAFETY ARGUMENT. This is one of the two ways
    the loner lane shipped a contaminated boundary: a pre-roll that walks
    back into the previous word's VOICE rather than the silence in front
    of this one. It cannot happen here, structurally — the walk-back stops
    at `unit_src[u-1][1]`, which is the previous unit's end AFTER the
    energy trim, and the trim only ever moves an end back to where that
    word's audio actually stopped. So the ground between the floor and s0
    is silence by construction, and widening ATTACK_S does not reach past
    it (verified: at 220 ms the audit still reports clean, because the
    clamp, not the constant, is what bounds the walk).

    Structure is not evidence, though, so bin/audit.py also polices it
    from the other side and will flag any unit whose span holds a sliver
    of a neighbour's event — see BORROW_S over there.
    """
    pre = int(round(ATTACK_S / FRAME_S))
    out = []
    for u, (s0, s1) in enumerate(unit_src):
        floor = 0 if u == 0 else unit_src[u - 1][1]
        out.append((max(floor, s0 - pre), s1))
    return out


def energy_end(x, fs, f0, f1, peak):
    """Where this word's own audio stops — the start of the first long
    silence with nothing after it but the next word's leak. NOT the last
    loud frame (the next attack leaks across the boundary and reads as
    'the word runs to the end'), and NOT the FIRST quiet run either: a
    long word holds a pause INSIDE it, and cutting there throws its last
    syllable away."""
    n = int(round(fs * FRAME_S))
    seg = x[f0 * n:f1 * n]
    if len(seg) < n:
        return f1
    m = len(seg) // n
    rms = np.sqrt((seg[:m * n].reshape(m, n) ** 2).mean(axis=1))
    quiet = rms <= peak * 10.0 ** (TRIM_GATE_DB / 20.0)
    on = np.nonzero(~quiet)[0]
    if not len(on):
        return f0 + m
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
            return f0 + a_
    return f0 + int(on[-1]) + 1


def trim_units(x, fs, unit_src, names=None):
    """Pull each unit's end back to its real audio end. Last unit keeps
    its span (the tail/release machinery owns it)."""
    peak = np.max(np.abs(x)) or 1.0
    out, log = [], []
    for u, (s0, s1) in enumerate(unit_src):
        if u == len(unit_src) - 1:
            out.append((s0, s1))
            continue
        e = energy_end(x, fs, s0, s1, peak) + int(round(TRIM_MARGIN_S / FRAME_S))
        e = max(s0 + int(round(TRIM_KEEP * (s1 - s0))), min(e, s1))
        cut = (s1 - e) * FRAME_S
        if cut < TRIM_MIN_S:
            e = s1
        elif names:
            log.append(f"{names[u]} −{cut * 1000:.0f}ms")
        out.append((s0, e))
    return out, log, [(b, c) for (a_, b), (_, c) in zip(out, unit_src)]


def build_warp(a, unit_src, beats, dursb, gapsb=None, rest_src=None, lead_b=0.0,
               nohold=(), tail_end=None):
    """Frame index map with PEAK-ON-THE-BEAT alignment.

    A sung syllable swells, and the beat is felt at the loudest moment, so
    a word whose peak is 60 ms into the vowel reads as 60 ms late even
    though its onset is exact. The anticipation runs to the PEAK, capped
    at 90 ms so a late-peaking word cannot drag its start into the word
    before it; everything ahead of the peak plays 1:1, the way a singer
    leans in. Returns (idx, holds, fade, Z, ants, rise, rests).
    """
    F = len(a["f0c"])
    w = np.where(a["voiced"], 1.0, UNVOICED_W)
    xs, fsr = a["x"], a["fs"]
    spf = int(round(fsr * FRAME_S))
    nf = min(F, len(xs) // spf)
    if nf > 0:
        e = np.sqrt((xs[:nf * spf].reshape(nf, spf) ** 2).mean(axis=1))
        quiet = e <= (np.max(np.abs(xs)) or 1.0) * 10.0 ** (TRIM_GATE_DB / 20.0)
        w[:nf][quiet] = SILENT_W
    nfr = nf
    fen = np.sqrt((xs[:nfr * spf].reshape(nfr, spf) ** 2).mean(axis=1))
    lead_cap = int(round(PEAK_LEAD_MAX_S / FRAME_S))
    ants, voiced_at = [], []
    for (s0, s1) in unit_src:
        v0 = s0
        lim = min(s0 + int(0.20 / FRAME_S), s1 - 1, F - 1)
        while v0 < lim and not a["voiced"][v0]:
            v0 += 1
        if not a["voiced"][min(v0, F - 1)]:
            v0 = s0
        voiced_at.append(v0)
        hi = min(s1, nfr, v0 + int(0.35 / FRAME_S))
        if hi > v0:
            pk = v0 + int(np.argmax(fen[v0:hi]))
            v0 = min(pk, v0 + lead_cap)
        ants.append(max(0, v0 - s0))
    # THE PICKUP is the UNVOICED prefix only — the fricative before her
    # first voiced frame. The peak lead that follows plays 1:1.
    u0 = unit_src[0][0]
    von = max(u0, min(voiced_at[0], u0 + ants[0]))
    unv = list(range(0, von))
    lead_tail = list(range(von, u0 + ants[0]))
    rise = None
    if lead_b > 0.0 and len(unv):
        want = max(1, int(round(lead_b * SPB / FRAME_S)))
        pos = np.linspace(0, len(unv) - 1, want)
        src_n = len(unv)
        unv = [unv[int(round(p))] for p in pos]
        rise = (0, want, src_n)
    pre = unv + lead_tail
    Z = len(pre)
    T = [Z + int(round(b * SPB / FRAME_S)) for b in beats]
    Tend = [Z + int(round((b + d) * SPB / FRAME_S)) for b, d in zip(beats, dursb)]
    idx = list(pre)
    holds, rests, ratios = [], [], []
    for u, (s0, s1) in enumerate(unit_src):
        s0, s1 = max(0, min(s0, F - 1)), max(1, min(s1, F))
        v0 = min(s0 + ants[u], s1 - 1)
        t0 = T[u]
        gap_fr = (int(round(gapsb[u] * SPB / FRAME_S))
                  if gapsb and u < len(gapsb) else 0)
        if u + 1 < len(unit_src):
            nxt_a = min(ants[u + 1], max(0, (T[u + 1] - t0) - 2))
            body_end = T[u + 1] - nxt_a - gap_fr
        else:
            nxt_a = 0
            body_end = Tend[u]
        out_n = max(1, body_end - t0)
        src_n = max(1, s1 - v0)
        seg_w = w[v0:s1].copy() if s1 > v0 else np.ones(1)
        cum = np.concatenate([[0.0], np.cumsum(seg_w)])
        cum /= cum[-1]
        pos = np.interp((np.arange(out_n) + 0.5) / out_n, cum,
                        np.arange(len(cum), dtype=float)) - 0.5
        pos = np.clip(pos, 0, src_n - 1)
        # THE STRETCH IS THE VOWEL'S STRETCH, not the block's. out_n/src_n
        # is the number the chart is written in, but it is not the number
        # the ear hears: silence inside a unit carries weight 0.04, so a
        # word ending in half a second of room ("in") reads as 0.99× on
        # raw frames while its 0.59 s of singing is actually being spread
        # across 1.2 s. Count the output frames that land on VOICED source
        # and divide by the voiced source frames — that is what got
        # stretched, and it is what THE HOLD must be decided on.
        srcv = (v0 + np.round(pos).astype(int))
        vout = int(a["voiced"][np.clip(srcv, 0, F - 1)].sum())
        vsrc = max(1, int(a["voiced"][v0:s1].sum()))
        ratio = vout / vsrc if vout else out_n / src_n
        ratios.append(ratio)
        if ratio > 2.2:
            tsec = np.arange(out_n) * FRAME_S
            pos = np.clip(pos + 2.2 * np.sin(2 * np.pi * 0.85 * tsec), 0, src_n - 1)
        if ratio > HOLD_RATIO and u not in nohold:
            holds.append((len(idx), len(idx) + out_n, v0, s1))
        idx.extend((v0 + np.round(pos).astype(int)).tolist())
        if gap_fr > 0:
            rests.append((len(idx), len(idx) + gap_fr))
            ra, rb = (rest_src[u] if rest_src else (s1, s1))
            span = max(1, rb - ra)
            for k in range(gap_fr):
                idx.append(int(np.clip(
                    ra + (span - 1) - abs((k % max(1, 2 * span - 2)) - (span - 1)),
                    0, F - 1)))
        if u + 1 < len(unit_src):
            ns0 = unit_src[u + 1][0]
            idx.extend(range(ns0, ns0 + nxt_a))
    Fend = min(F, tail_end) if tail_end else F
    tail0 = unit_src[-1][1]
    idx += list(range(min(tail0, Fend), Fend))
    fade = None
    if (Fend - tail0) * FRAME_S < 0.15:
        rel_n = int(0.40 / FRAME_S)
        s0, s1 = unit_src[-1]
        lo = max(s0, s1 - int(0.12 / FRAME_S))
        span = max(2, s1 - lo)
        fade = (len(idx), len(idx) + rel_n)
        for k in range(rel_n):
            p = lo + (span - 1) - abs((k % (2 * span - 2)) - (span - 1))
            idx.append(int(np.clip(p, 0, F - 1)))
    return np.array(idx, dtype=int), holds, fade, Z, ants, rise, rests, ratios


def synth_from(a, idx, f0_o, *, dark=None, breath_x=1.0, vowels_only=False,
               air=True, formant=True, fade=None, rise=None, rests=()):
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
    depth = np.zeros(len(idx))
    acc = 0.0
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
    for (q0, q1) in rests:
        spf = int(fs * FRAME_S)
        p0, p1 = q0 * spf, min(q1 * spf, n)
        if p1 > p0:
            k = np.linspace(0.0, 1.0, p1 - p0)
            out[p0:p1] *= 0.15 + 0.85 * np.clip(1.0 - k / 0.35, 0.0, 1.0)
    if rise is not None:
        # THE PICKUP swells from nothing. It must be spectrally-shaped
        # NOISE: a fricative is noise, and the warp stretches by repeating
        # frame indices, which makes the unvoiced path paste the same 5 ms
        # of waveform over and over — a periodic buzz. Colour white noise
        # with the real sibilant's spectrum instead: aperiodic by
        # construction, any length, still her /f/.
        spf = int(fs * FRAME_S)
        r0, r1, src_n = rise
        a0, a1 = r0 * spf, min(r1 * spf, n)
        sib = x[:max(int(0.045 * fs), src_n * spf)]
        if a1 > a0:
            N = a1 - a0
            if vowels_only or len(sib) < 256:
                out[a0:a1] = 0.0
            else:
                wnd = np.hanning(256)
                mags = [np.abs(np.fft.rfft(sib[k:k + 256] * wnd))
                        for k in range(0, len(sib) - 256, 128)]
                env = (np.mean(mags, axis=0) if mags
                       else np.abs(np.fft.rfft(sib[:256] * wnd)))
                rng = np.random.default_rng(7)
                NF = np.fft.rfft(rng.standard_normal(N))
                shape = np.interp(np.linspace(0, 1, len(NF)),
                                  np.linspace(0, 1, len(env)), env)
                g = np.fft.irfft(NF * shape, n=N)
                rms = np.sqrt((sib ** 2).mean()) or 1.0
                g *= rms / (np.sqrt((g ** 2).mean()) or 1.0)
                k = np.arange(N)
                out[a0:a1] = g * (0.5 - 0.5 * np.cos(np.pi * k / max(1, N))) ** 2.0
    if fade is not None:
        spf = int(fs * FRAME_S)
        a0, a1 = fade[0] * spf, min(fade[1] * spf, n)
        if a0 < n:
            k = np.arange(a1 - a0)
            out[a0:a1] *= 0.5 + 0.5 * np.cos(np.pi * k / max(1, a1 - a0))
            out[a1:] = 0.0
    return dress(out, fs), fs


def _seed(path):
    try:
        return json.load(open(path))
    except Exception:
        return {}


_old_chart = _seed(os.path.join(VOX3, ".chart.json"))
manifest = _seed(os.path.join(VOX3, ".manifest.json"))
chart_c = []
VOICING = {}

ONLY = {p for p in os.environ.get("PHRASES", "").split(",") if p}
LEAD_ONLY = os.environ.get("LEAD_ONLY") is not None
ALL_FLAGS = []

for name, ch in CHART.items():
    if ONLY and name not in ONLY:
        continue
    slice_name = ch["slice"]
    entry = SLICES[slice_name]
    src = os.path.join(LANE, "samples", f"{slice_name}.wav")
    x, fs = sf.read(src, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    a = analyze_cached(src, x, fs)
    F = len(a["f0c"])
    t0_slice = 0.0                # every time in this chart is slice-relative

    # ── PROVENANCE. @jeffrey: keep the whisper path explicit and legible.
    # Every word records where its boundary came from, so it is always
    # clear which numbers are trusted and why.
    aligned = slice_name in ALIGN
    if aligned:
        words = [dict(t=w["t"], start=w["start"], end=w["end"],
                      f0_hz=w["f0_hz"], note=w["note"], src="whisper-1")
                 for w in ALIGN[slice_name]["words"]]
    else:
        wf = entry.get("word_f0") or []
        words = [dict(w, src="whisper.cpp") for w in wf]

    # MERGE — whisper-1 splits a word it cannot pronounce. On this take
    # "factory" came back as "to"+"read"; the audio has one word there.
    for wi in sorted((ch.get("merge") or {}), reverse=True):
        n_, label = ch["merge"][wi]
        grp = words[wi:wi + n_]
        words[wi:wi + n_] = [dict(grp[0], t=label, end=grp[-1]["end"],
                                  src="whisper-1+merged")]

    # THE PINS
    pinned = set()
    for wi, ts in (ch.get("times") or {}).items():
        if 0 <= wi < len(words):
            drift = ts - words[wi]["start"]
            words[wi]["start"] = t0_slice + ts
            words[wi]["src"] = f"pinned (event; whisper-1 was {drift*-1000:+.0f} ms)"
            pinned.add(wi)
            if wi:
                words[wi - 1]["end"] = t0_slice + ts
    for wi, ts in (ch.get("ends") or {}).items():
        if 0 <= wi < len(words):
            drift = ts - words[wi]["end"]
            words[wi]["end"] = t0_slice + ts
            words[wi]["src"] += f" · end pinned ({drift*-1000:+.0f} ms)"

    # SYLLABLE SPLITS at explicit source times, applied AFTER the pins so
    # those indices still mean what they say.
    for wi in sorted((ch.get("sylls") or {}), reverse=True):
        base = words[wi]
        cuts = ch["sylls"][wi]
        head = next((c[1] for c in cuts if c[0] is None), base["t"])
        cuts = [c for c in cuts if c[0] is not None]
        edges = [base["start"]] + [t0_slice + c[0] for c in cuts] + [base["end"]]
        labels = [head] + [c[1] for c in cuts]
        words[wi:wi + 1] = [dict(base, start=edges[k], end=edges[k + 1],
                                 t=labels[k], src=base["src"] + " · split")
                            for k in range(len(labels))]
    pin_idx = set()                      # re-index the pins after splitting
    for i, wd in enumerate(words):
        if wd["src"].startswith("pinned"):
            pin_idx.add(i)

    words, snaps = snap_boundaries(a, words, t0_slice, pin_idx)

    # sub-split units at their internal fricative (unused here; kept for
    # any phrase that needs myself → my·self)
    for ui in sorted(ch.get("splits", []), reverse=True):
        wd = words[ui]
        f0i = int(round((wd["start"] - t0_slice) / FRAME_S))
        f1i = int(round((wd["end"] - t0_slice) / FRAME_S))
        lo, run, split_f = f0i + max(3, (f1i - f0i) // 4), 0, None
        for f in range(lo, min(f1i, len(a["voiced"]))):
            if not a["voiced"][f]:
                run += 1
                if run >= 3:
                    split_f = f - run + 1
                    break
            else:
                run = 0
        if split_f is None:
            split_f = (f0i + f1i) // 2
        ts = t0_slice + split_f * FRAME_S
        words[ui:ui + 1] = [dict(wd, end=ts, t=wd["t"] + "·a"),
                            dict(wd, start=ts, t=wd["t"] + "·b")]

    # RE-MEASURE THE NOTE, over the unit's LONGEST EVENT.
    #
    # Two corrections in one. First: the aligner's f0 described the span
    # the ALIGNER thought the word had, so once a pin moves a boundary or
    # a split cuts a word in two, that number describes the wrong audio.
    # Second, and less obvious: a median over the whole unit span is not
    # the note either. "ie" is 150 ms of C4 sitting behind a 145 ms /k/
    # closure, and the median across its block came out 154 Hz — D#3, a
    # note she never sings there, off by nine semitones, and the pluck
    # would have doubled it. The note of a unit is the note of the event
    # it RESTS on: the longest one inside it. Doing it this way also puts
    # the lane's own documented facts back on the page — "bird" measures
    # Bb2, exactly as the README says it should.
    src_ev = audit.find_events(x, fs, audit.despike(a["f0"])[0])
    for wd in words:
        f_a = int(round((wd["start"] - t0_slice) / FRAME_S))
        f_b = int(round((wd["end"] - t0_slice) / FRAME_S))
        inside = [(e1 - e0, e0, e1) for (e0, e1) in src_ev
                  if min(f_b, e1) - max(f_a, e0) > (e1 - e0) * 0.5]
        if inside:
            _, e0, e1 = max(inside)
            seg = a["f0"][e0:e1]
        else:
            seg = a["f0"][max(0, f_a):max(1, min(f_b, len(a["f0"])))]
        seg = seg[seg > 0]
        if len(seg):
            wd["f0_hz"] = float(np.median(seg))

    ch["units"] = derive_units(words, ch["beats"], ch.get("stretch"),
                               ch.get("durs"), ch.get("gaps"))

    unit_src = []
    for i, wd in enumerate(words):
        s0 = int(round((wd["start"] - t0_slice) / FRAME_S))
        s1 = int(round((wd["end"] - t0_slice) / FRAME_S))
        if i + 1 < len(words):
            s1 = int(round((words[i + 1]["start"] - t0_slice) / FRAME_S))
        unit_src.append((max(0, s0), min(F, max(s0 + 1, s1))))

    unit_src, trims, rest_src = trim_units(x, fs, unit_src, [w["t"] for w in words])
    unit_src = keep_attacks(unit_src, rest_src)
    if ch.get("end"):
        cap = int(round((ch["end"] - t0_slice) / FRAME_S))
        a0_, b0_ = unit_src[-1]
        unit_src[-1] = (a0_, min(b0_, max(a0_ + 1, cap)))

    # ── THE AUDIT, run on the spans the warp is about to read ────────
    # @jeffrey: "a bad boundary should announce itself instead of waiting
    # to be heard." These are the FINAL source spans — post-pin, post-
    # split, post-trim — so what the audit sees is exactly what gets sung.
    ev, rows, flags = audit.report(slice_name, unit_src,
                                   [w["t"] for w in words], verbose=False)
    ALL_FLAGS += [f"{name}: {f}" for f in flags]

    gapsb = [ch.get("gaps", {}).get(i, 0.0) for i in range(len(ch["units"]))]
    idx, holds, fade, Z, ants, rise, rests, ratios = build_warp(
        a, unit_src, [b for (b, d) in ch["units"]],
        [d for (b, d) in ch["units"]], gapsb, rest_src, ch.get("lead", 0.0),
        set(ch.get("nohold", ())),
        int(round((ch["end"] - t0_slice) / FRAME_S)) if ch.get("end") else None)
    f0_o = a["f0c"][idx].copy()
    voiced_o = a["voiced"][idx]

    for (o0, o1, s0, s1) in holds:
        seg = a["f0c"][s0:s1][a["voiced"][s0:s1]]
        if not len(seg):
            continue
        med = np.median(seg)
        st = np.round(12.0 * np.log2(med / TONIC))
        steps = np.concatenate([MINOR + 12 * o for o in range(-3, 5)])
        st = steps[np.argmin(np.abs(steps - st))]
        tgt = TONIC * 2.0 ** (st / 12.0)
        n = o1 - o0
        tsec = np.arange(n) * FRAME_S
        vib = 2.0 ** (0.15 * np.clip((tsec - 0.4) / 0.4, 0, 1)
                      * np.sin(2 * np.pi * 5.2 * tsec) / 12.0)
        blend = np.clip(tsec / 0.12, 0, 1)
        f0_o[o0:o1] = np.where(voiced_o[o0:o1],
                               f0_o[o0:o1] * (1 - blend) + tgt * vib * blend, 0.0)

    for ui, semis in (ch.get("shift") or {}).items():
        if not (0 <= ui < len(ch["units"])):
            continue
        bt, du = ch["units"][ui]
        o0 = Z + int(round(bt * SPB / FRAME_S))
        o1 = Z + int(round((bt + du) * SPB / FRAME_S))
        o0, o1 = max(0, o0), min(len(f0_o), o1)
        if o1 > o0:
            f0_o[o0:o1] *= 2.0 ** (semis / 12.0)

    renders = {}
    out, _ = synth_from(a, idx, f0_o, fade=fade, rise=rise, rests=rests)
    sf.write(os.path.join(VOX3, f"{name}.wav"), out, fs)
    renders["lead"] = round(len(out) / fs, 3)

    for tag, cents in () if LEAD_ONLY else (("8ve-a", 1200 + 6), ("8ve-b", 1200 - 7)):
        out, _ = synth_from(a, idx, f0_o * 2.0 ** (cents / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                            vowels_only=True, air=False, fade=fade, rise=rise,
                            rests=rests)
        sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    for tag, deg, det in () if LEAD_ONLY else (("low3", -2, 5.0), ("low5", -4, -6.0)):
        delta = np.zeros_like(f0_o)
        v = voiced_o & (f0_o > 0)
        if v.any():
            delta[v] = diatonic_delta(f0_o[v], deg)
        delta = smooth(delta, int(60.0 / FRAME_MS))
        out, _ = synth_from(a, idx, f0_o * 2.0 ** ((delta + det) / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X, air=False,
                            fade=fade, rise=rise, rests=rests)
        sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    lead_in = Z * FRAME_S
    beats_total = ch["units"][-1][0] + ch["units"][-1][1]

    def to_beat(frame):
        return round((frame * FRAME_S - lead_in) / SPB, 4)

    voiced_runs, k = [], 0
    while k < len(voiced_o):
        if voiced_o[k]:
            j = k
            while j < len(voiced_o) and voiced_o[j]:
                j += 1
            if (j - k) * FRAME_S >= 0.020:
                voiced_runs.append([to_beat(k), to_beat(j)])
            k = j
        else:
            k += 1

    # ON THE GRID, in cents. The chart's semitone is the note the
    # REGULATED vocal sings, not the note she sang raw — otherwise the
    # pluck doubling her would play C# against a voice snapped to C. She
    # touches C# three times and F# once in this take, and rounding those
    # to the nearest semitone is not the same as snapping them to the
    # scale: "fac" measures 138.6 Hz, which rounds to −1 (C#3) but is
    # 79 ¢ from C3 and 114 ¢ from D3, so the snap sends it to C.
    steps_all = np.concatenate([MINOR + 12 * o for o in range(-3, 5)])

    def on_grid(hz):
        c = 1200.0 * np.log2(hz / TONIC)
        return int(steps_all[np.argmin(np.abs(steps_all * 100.0 - c))])

    notes, wordrec = [], []
    for u, (wd, (beat, durb)) in enumerate(zip(words, ch["units"])):
        st = on_grid(wd["f0_hz"]) if wd["f0_hz"] else 0
        st += int((ch.get("shift") or {}).get(u, 0))
        notes.append((beat, durb, st, wd["t"].strip(),
                      round(ants[u] * FRAME_S / SPB, 4) if u < len(ants) else 0.0))
        r = rows[u] if u < len(rows) else {}
        wordrec.append(dict(t=wd["t"].strip(), src=wd["src"],
                            src_span=[round(unit_src[u][0] * FRAME_S, 3),
                                      round(unit_src[u][1] * FRAME_S, 3)],
                            start=round(wd["start"], 3),
                            beat=beat, dur=durb, st=st,
                            f0_hz=round(wd["f0_hz"], 1) if wd["f0_hz"] else 0,
                            stretch=round(ratios[u], 3) if u < len(ratios) else 0,
                            hold=any(o0 <= Z + int(round(beat * SPB / FRAME_S)) < o1
                                     for (o0, o1, _s, _e) in holds),
                            events=r.get("events", []), sung=r.get("sung"),
                            peak_ms=round((r.get("peak_s") or 0) * 1000)))
    chart_c.append((name, lead_in, beats_total, notes))
    VOICING[name] = voiced_runs
    manifest[name] = dict(slice=slice_name, lead_in=round(lead_in, 3),
                          beats=beats_total, bpm=BPM, tonic=TONIC,
                          renders=renders, snaps=snaps, trims=trims,
                          octave_folds=int(a.get("folds", 0)),
                          words=wordrec, flags=flags)
    print(f"  {name:16s} {renders['lead']:5.2f}s  {len(words)} units  "
          f"«{entry['words']}»  ({int(a.get('folds', 0))} octave folds)")
    print("    " + " · ".join(
        f"{w['t']}@{w['beat']:g}({w['stretch']:.2f}×{'H' if w['hold'] else ''})"
        for w in wordrec))
    if trims:
        print(f"    trimmed: {' · '.join(trims)}")
    if snaps:
        print(f"    boundaries snapped (unpinned only): {' · '.join(snaps)}")

_new_chart = dict(_old_chart)
_new_chart.update({name: dict(leadIn=round(li, 3), beats=bt, voiced=VOICING[name],
                              notes=[dict(beat=b, dur=d, st=s, t=t, lead=ld)
                                     for (b, d, s, t, ld) in ns])
                   for (name, li, bt, ns) in chart_c})

json.dump(manifest, open(os.path.join(VOX3, ".manifest.json"), "w"), indent=1)

# ── the generated header — her melody, for the machine ────────────────
lines = [
    "// factory-chart.h — GENERATED by bin/chart.py; do not edit.",
    "// The v3 beat chart: per phrase, its lead-in (consonant runway before",
    "// beat 0), length in beats, and per word unit its slot, length, and",
    "// semitone above TONIC (148.73 Hz — HER D, measured off the take's",
    "// sustained frames) — so any instrument reading this plays the",
    "// chant's own melody, flips and all.",
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
_all = [(nm, _new_chart[nm]["leadIn"], _new_chart[nm]["beats"],
         [(q["beat"], q["dur"], q["st"], q["t"], q.get("lead", 0.0))
          for q in _new_chart[nm]["notes"]]) for nm in _new_chart]
for name, lead_in, beats_total, notes in _all:
    ident = name.replace("-", "_")
    lines.append(f"static const ChartNote {ident}_notes[] = {{")
    for (beat, durb, st, _t, _lead) in notes:
        lines.append(f"    {{ {beat:.2f}, {durb:.2f}, {st} }},")
    lines.append("};")
lines.append("")
lines.append("static const ChartPhrase CHART[] = {")
for name, lead_in, beats_total, notes in _all:
    ident = name.replace("-", "_")
    lines.append(f'    {{ "{name}", {lead_in:.3f}, {beats_total:.2f}, '
                 f"{len(notes)}, {ident}_notes }},")
lines.append("};")
lines.append(f"#define CHART_N {len(_all)}")
lines.append("")
open(os.path.join(CDIR, "factory-chart.h"), "w").write("\n".join(lines))

json.dump(_new_chart, open(os.path.join(VOX3, ".chart.json"), "w"), indent=1)

if ALL_FLAGS:
    print("── AUDIT FLAGS (bin/audit.py, on the final source spans) ──")
    for f in ALL_FLAGS:
        print(f"  ⚠ {f}")
else:
    print("── AUDIT: ✓ every unit holds whole events; no contamination ──")
print(f"WROTE {VOX3}/.manifest.json + .chart.json + {CDIR}/factory-chart.h "
      f"({len(chart_c)} phrases)")
