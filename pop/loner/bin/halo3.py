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
    return dict(x=x, fs=fs, f0=f0, f0c=f0c, sp=sp, ap=ap, voiced=voiced)


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
                             # bar map can be one word per bar. Index 4 is
                             # "myself" under the OpenAI alignment, where
                             # "curled" is ONE word — under whisper.cpp's
                             # sub-word tokens it was cur+led and this was 5.
                             # durs are POST-split indices.
                             "splits": [0, 4],
                             # The bar map, one phrase per bar (@jeffrey:
                             # "curled up should be bar 1 — not bar 1 and
                             # half of bar 2 · and in my should be bar
                             # 2"): sitting = bar 0 · CURLED UP = bar 1,
                             # exactly (2.5 + 1.5) · IN MY = bar 2,
                             # exactly (2 + 2) · self i = bar 3, so think
                             # still lands on the bar-4 downbeat · OF
                             # (the octave) holds its 4 — at 1.01× it is
                             # her real held octave, and compressing it
                             # would gut the one loud feeling in the
                             # lyric · then "a should be at least two
                             # beats, its too short now": a goes 0.5 → 2,
                             # paid for by stone, which drops 3.5 → 2 and
                             # in doing so stops being a 2× synthetic
                             # hold and becomes her voice at 1.16×.
                             # of·a·stone still spans beats 18–26, so
                             # nothing downstream moves.
                             # curled 3 · up 2 · stone longer · and the
                             # gaps she actually takes, measured off the
                             # take: she is silent 13% of this line, and
                             # her three big breaths are after up (0.79
                             # beats), after stone (1.31 — the longest
                             # in the song, exactly where @jeffrey asked
                             # for "a bar break between end of stone and
                             # just") and after to. The chart used to
                             # have no rests at all, so every one of
                             # those breaths was being stretched into
                             # the note before it.
                             "lead": 1.0,        # the /s/ of sitting, as a pickup
                             # curled 2 (one shorter than 3, and 0.94× —
                             # essentially her own speed): curled + up now
                             # fill bar 1 exactly, 4→8. The beat goes into
                             # her breath rather than downstream, so every
                             # anchor after it holds.
                             # …and the back half, which had been left to
                             # the auto scale: it gave two-syllable VERY
                             # 4.5 beats and three-syllable PATIENTLY
                             # only 2.5, so patiently ran at 1.02× while
                             # everything around it was stretched 1.3–1.5×
                             # — @jeffrey: "patiently is weird · its too
                             # squished in time". Swapped to very 3 (1.02×)
                             # and patiently 4 (1.64×); the pair still
                             # totals 7 beats, so "for" is untouched.
                             # SITTING is split too — @jeffrey: "any way we
                             # can snap sitting so the 'ttin' is beat
                             # aligned?" It is two syllables sung across
                             # four beats, so the split at its internal /t/
                             # lets sit and ting take two beats each and
                             # the second syllable land square on beat 2.
                             # Then curled 2.5 + up 1.5 fill bar 1 exactly
                             # and IN owns bar 2 outright — one phrase per
                             # bar the whole way down: sit·ting 0 · curled
                             # up 1 · in 2 · my·self 3 · i·think 4 · of 5 ·
                             # a 6 · stone 7.
                             # curled 3 · up 3. Everything downstream
                             # falls onto bars by itself: my·self is bar 3,
                             # i·think is bar 4, and "think" ends exactly
                             # on 20 — so OF starts the instant think ends
                             # ("of should start sooner, right after
                             # 'think' ends") with no rest needed at all.
                             "durs": { 0: 2.0, 1: 2.0, 2: 2.5, 3: 1.5,
                                       4: 1.5, 5: 2.0, 6: 2.0,
                                       # think 1 (1.17×, her own speed — at 2
                                       # it was a 2.3× held tone), so OF keeps
                                       # its 4 beats at 1.01×, her real octave,
                                       # "a" keeps 2, and STONE starts on bar 6
                                       # beat 2 as asked.
                                       # my on bar 2 beat 4; "i" takes the
                                       # freed beat (1.78×, still her voice)
                                       # so "of" keeps beat 19 and stone its
                                       # bar 6 beat 2.
                                       7: 3.0,
                                       8: 2.0, 9: 4.0, 10: 2.0, 11: 2.0,
                                       # very longer, patiently slower
                                       14: 4.0, 15: 5.0 },
                             # words whose syllables carry a melody must not
                             # be flattened to one tone by THE HOLD
                             "nohold": [15],   # patiently
                             # source seconds, read off her onsets, for the
                             # words whisper-1 mistimed. PRE-split indices.
                             "times": { 11: 12.70,   # waiting
                                        12: 14.40,   # very
                                        14: 17.66,   # for
                                        15: 18.54,   # time
                                        16: 19.48,   # to
                                        17: 21.20 },  # pass
                             # rest AFTER the given unit, in beats
                             # "the in should start sooner — at start of
                             # bar 2": the rest after up goes entirely, so
                             # IN MY is bar 2 exactly (8–12) and self·rest
                             # is bar 3, i·think bar 4. The freed beats go
                             # to the self/i rest and to think, so "of"
                             # still holds bar 5 and nothing after moves.
                             # "i should come after self ends": self's
                             # audio really stops mid-slot, so i follows
                             # it directly and "i think" runs contiguous.
                             # The bar break after stone was leaving 4.25
                             # beats of audible silence — her own gap there
                             # is 1.31, the longest in the song but nothing
                             # like that. stone 4 → 3 (and at 1.7× it drops
                             # under the hold line, so it plays as her voice
                             # instead of a 2.3× synthesized tone) with a
                             # 1-beat rest, which lands "just" square on the
                             # bar 8 downbeat.
                             "gaps": { 3: 0.5,   # "in" leans in behind the downbeat
                                       11: 1.0 } },
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


def derive_units(words, beats_total, stretch=None, durs=None, gaps=None):
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
        acc += dq + (gaps.get(i, 0.0) if gaps else 0.0)
    return units

SLICES = json.load(open(os.path.join(LANE, "samples", ".manifest.json")))

# ── the alignment — OpenAI's words, not whisper.cpp's sub-word tokens ──
# The lane's original receipts came from whisper.cpp ggml-small at -ml 1,
# which returns TOKENS: it cut "curled" into "cur" + "led", and every
# label after slid by a syllable, so the span we were calling `led` is
# where she sings **up**, and `stone` started 1.3 s early inside the held
# octave of "of a". bin/align.py re-aligns each slice through OpenAI
# whisper-1 with word timestamps; anything it can't align cleanly stays
# on the old receipt. Times there are slice-relative, so they come back
# onto the manifest's clock by adding the slice start.
ALIGN_PATH = os.path.join(LANE, "samples", ".align.json")
ALIGN = json.load(open(ALIGN_PATH)) if os.path.exists(ALIGN_PATH) else {}

# ── boundary repair — snap whisper's word times to the real note ──────
# @jeffrey, watching the study: "has word boundary wrong · led has up
# within it · that's definitely causing bugs · also of a stone, the
# 'stone' seems to include of a too". He was right, and it sat upstream
# of every dur we'd tuned. Whisper times a word where the TRANSCRIPT
# hands over, not where the singing changes: it put led 210 ms late (so
# led's slot opened inside cur's note and replayed cur's pitch for half
# a beat) and "of" 300 ms early (the octave starts before its own slot,
# spilling the leap into its neighbours). Every boundary is now pulled
# to the nearest real acoustic event — a sustained pitch step, or, for
# words that open on an unvoiced consonant like st-one, an energy
# valley — inside a ±250 ms search that can never reorder words or
# starve one below 80 ms.
SNAP_WIN_S = 0.250          # how far a boundary may travel
SNAP_MED_S = 0.120          # median window each side; 80 ms missed led's step
SNAP_STEP_ST = 0.50         # a pitch change this big IS the boundary
SNAP_MIN_S = 0.080          # no word may be shrunk below this
SNAP_QUIET = 0.30           # energy valley must be this share of local median


def snap_boundaries(a, words, t0):
    """Pull each word start onto the acoustic event nearest it."""
    x, fs, f0 = a["x"], a["fs"], a["f0"]
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    if m < 8 or len(words) < 2:
        return words, []
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    st = np.where(f0[:m] > 0, 12.0 * np.log2(np.maximum(f0[:m], 1e-6) / TONIC), np.nan)
    W = max(2, int(round(SNAP_MED_S / FRAME_S)))
    step = np.zeros(m)
    for k in range(W, m - W):
        b_, a_ = st[k - W:k], st[k:k + W]
        b_, a_ = b_[~np.isnan(b_)], a_[~np.isnan(a_)]
        if len(b_) >= W // 2 and len(a_) >= W // 2:
            step[k] = abs(np.median(a_) - np.median(b_))
    mins = int(round(SNAP_MIN_S / FRAME_S))
    win = int(round(SNAP_WIN_S / FRAME_S))
    out = [dict(w) for w in words]
    log = []
    for i in range(1, len(out)):
        k0 = int(round((out[i]["start"] - t0) / FRAME_S))
        prev = int(round((out[i - 1]["start"] - t0) / FRAME_S))
        nxt = (int(round((out[i + 1]["start"] - t0) / FRAME_S))
               if i + 1 < len(out) else m)
        lo = max(prev + mins, k0 - win, W)
        hi = min(nxt - mins, k0 + win, m - W)
        if hi <= lo:
            continue
        kk = lo + int(np.argmax(step[lo:hi]))
        if step[kk] < SNAP_STEP_ST:                 # no note change to grab —
            seg = rms[lo:hi]                        # try a consonant closure
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
# @jeffrey, reading the waveforms drawn into the timeline: "check the
# length of the actual waveforms in the utterances, not just ur trim".
# Whisper's word boundaries are handoffs, not note ends: it gave "led"
# 0.99 s when she stops singing after ~0.5 and the rest is decay. Those
# dead frames were stretching across the slot with the note, so a word
# could fill 78% of its block and then sit there. Each unit's source
# span now ends where its audio actually ends (+ a release margin), and
# the silence is DROPPED rather than warped — the vowel takes the slot.
TRIM_GATE_DB = -36.0        # of the take's peak; keeps quiet fricatives
TRIM_MARGIN_S = 0.050       # let the release start before we cut
TRIM_MIN_S = 0.080          # below this, not worth the surgery
TRIM_QUIET_RUN_S = 0.120    # silence this long may end the word
TRIM_LEAK_S = 0.150         # audio after it that is just the next word bleeding in
ATTACK_S = 0.030            # pre-roll kept ahead of every word's onset


def keep_attacks(unit_src, rest_src):
    """Pull each unit's start back a little so its ATTACK survives.

    @jeffrey on "in": "just make sure our start of the word in is not
    like too early, or we have like a nice start of it — feels like it
    may cut off a bit on the beginning". He was right: her /ɪ/ hits
    20 ms before the boundary, so the unit opened a hair late and the
    transient was gone. A word's onset is the loudest, most fragile
    thing in it; the boundary repair aims at where the NOTE changes,
    which is a frame or two after where the SOUND starts. Every unit now
    keeps 30 ms of runway, bounded by where the previous word's audio
    actually ended so nothing overlaps sung material.
    """
    pre = int(round(ATTACK_S / FRAME_S))
    out = []
    for u, (s0, s1) in enumerate(unit_src):
        floor = 0 if u == 0 else unit_src[u - 1][1]
        out.append((max(floor, s0 - pre), s1))
    return out
TRIM_KEEP = 0.35            # never leave a unit shorter than this share


def energy_end(x, fs, f0, f1, peak):
    """Where this word's own audio stops — the start of the first long
    silence after it, NOT the last loud frame in the span. The next
    word's attack routinely leaks across the boundary (whisper hands
    over a hair late), and a last-loud-frame search reads that leak as
    'the word runs to the end' and trims nothing. Requiring a sustained
    quiet run also protects a stop closure inside a word (the /t/ in
    patiently is ~60 ms) from being mistaken for the end."""
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
    runs, k = [], int(on[0])                    # never cut before she starts
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
    # Cut only at TRAILING silence. A long word can hold a pause INSIDE
    # it — "patiently" has 200 ms between "patient" and "ly" — and
    # cutting at the first quiet run threw that last syllable away. So
    # walk the runs from the back and take the first one with nothing
    # after it but the next word's leak.
    for (a_, b_) in reversed(runs):
        if int((~quiet[b_:]).sum()) <= leak:
            return f0 + a_
    return f0 + int(on[-1]) + 1


def trim_units(x, fs, unit_src, names=None):
    """Pull each unit's end back to its real audio end. Last unit keeps
    its span (the tail/release machinery owns it). Returns (spans, log)."""
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
               nohold=()):
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
    pre = list(range(0, unit_src[0][0] + ants[0]))   # pre + consonant, 1:1
    rise = None
    if lead_b > 0.0 and len(pre):
        want = max(1, int(round(lead_b * SPB / FRAME_S)))
        src_n = len(pre)
        pos = np.linspace(0, src_n - 1, want)
        pre = [pre[int(round(p))] for p in pos]
        rise = (0, want, src_n)     # synth_from rebuilds this range as noise
    Z = len(pre)
    T = [Z + int(round(b * SPB / FRAME_S)) for b in beats]
    Tend = [Z + int(round((b + d) * SPB / FRAME_S)) for b, d in zip(beats, dursb)]
    idx = list(pre)
    holds, rests = [], []
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
        # THE HOLD flattens a long stretch to one grid tone, which is
        # right for a sustained vowel and WRONG for a word with syllables
        # in it — @jeffrey: "the notes in 'patiently' seem wrong, it sort
        # of goes down in pitch each syllable in the proper pronunciation
        # but not in the current track". Flattening had erased her
        # descending contour. `nohold` keeps the contour and just stretches
        # it.
        if ratio > HOLD_RATIO and u not in nohold:
            holds.append((len(idx), len(idx) + out_n, v0, s1))
        idx.extend((v0 + np.round(pos).astype(int)).tolist())
        if gap_fr > 0:                            # THE REST — her own air
            rests.append((len(idx), len(idx) + gap_fr))
            ra, rb = (rest_src[u] if rest_src else (s1, s1))
            span = max(1, rb - ra)
            for k in range(gap_fr):
                idx.append(int(np.clip(
                    ra + (span - 1) - abs((k % max(1, 2 * span - 2)) - (span - 1)),
                    0, F - 1)))
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
    return np.array(idx, dtype=int), holds, fade, Z, ants, rise, rests


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
    for (q0, q1) in rests:
        # a long rest is her room tone ping-ponged; without this it
        # pulses. Let the breath sound, then settle to almost nothing.
        spf = int(fs * FRAME_S)
        p0, p1 = q0 * spf, min(q1 * spf, n)
        if p1 > p0:
            k = np.linspace(0.0, 1.0, p1 - p0)
            out[p0:p1] *= 0.15 + 0.85 * np.clip(1.0 - k / 0.35, 0.0, 1.0)
    if rise is not None:
        # THE PICKUP swells from nothing — the slice starts mid-sibilant,
        # so without this "sitting" arrives at full volume with no attack.
        # It must be GRANULAR: a fricative is noise, and the warp stretches
        # by repeating frame indices, which makes the unvoiced path paste
        # the same 5 ms of waveform over and over — a 200 Hz buzz, the
        # glitch. Drawing overlapped Hann grains from random offsets in her
        # real /s/ stretches noise the only way noise can be stretched.
        spf = int(fs * FRAME_S)
        r0, r1, src_n = rise
        a0, a1 = r0 * spf, min(r1 * spf, n)
        # Her /s/ is only the first ~45 ms of the slice, and WORLD calls
        # frame 0 voiced, so there is almost no "consonant runway" to
        # borrow — grains drawn from 5 ms are all the same grain, which
        # buzzed at 100 Hz. Instead take the SPECTRUM of her real
        # sibilant and colour white noise with it: aperiodic by
        # construction, any length, and still her /s/.
        sib = x[:max(int(0.045 * fs), src_n * spf)]
        if a1 > a0:
            N = a1 - a0
            if vowels_only or len(sib) < 256:
                out[a0:a1] = 0.0
            else:
                w = np.hanning(256)
                mags = [np.abs(np.fft.rfft(sib[k:k + 256] * w))
                        for k in range(0, len(sib) - 256, 128)]
                env = (np.mean(mags, axis=0) if mags
                       else np.abs(np.fft.rfft(sib[:256] * w)))
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


manifest = {}
chart_c = []   # (phrase, lead_in_s, beats_total, [(beat, dur, st, t, lead)])
VOICING = {}   # per phrase, voiced runs in beats — vowels vs consonants

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
    # MEASURED OVERRIDES — @jeffrey: "ly in patiently is stuck in for ·
    # like the lyrics are badly mapped". He was right, and the boundary
    # snap could not save it: whisper-1's word times drift at the END of
    # this take, by 0.6 s on "time", 1.0 s on "to" and 1.4 s on "pass" —
    # far outside the ±250 ms the snap is allowed to travel. A word
    # labelled a second late means the word BEFORE it swallows it whole,
    # which is the extra syllable heard inside "for". These are read off
    # her own onsets in the source and pinned.
    aligned = slice_name in ALIGN
    if aligned:
        words = [dict(t=w["t"], start=t0_slice + w["start"],
                      end=t0_slice + w["end"], f0_hz=w["f0_hz"],
                      note=w["note"]) for w in ALIGN[slice_name]["words"]]
    else:
        words = list(entry["word_f0"])
    for i, ts in (ch.get("times") or {}).items():
        if 0 <= i < len(words):
            words[i]["start"] = t0_slice + ts
            if i:
                words[i - 1]["end"] = t0_slice + ts
    words, snaps = snap_boundaries(a, words, t0_slice)
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
    ch["units"] = derive_units(words, ch["beats"], ch.get("stretch"),
                               ch.get("durs"), ch.get("gaps"))

    # source frame bounds per unit (slice clock)
    unit_src = []
    for i, wd in enumerate(words):
        s0 = int(round((wd["start"] - t0_slice) / FRAME_S))
        s1 = int(round((wd["end"] - t0_slice) / FRAME_S))
        if i + 1 < len(words):
            s1 = int(round((words[i + 1]["start"] - t0_slice) / FRAME_S))
        unit_src.append((max(0, s0), min(F, max(s0 + 1, s1))))

    unit_src, trims, rest_src = trim_units(x, fs, unit_src, [w["t"] for w in words])
    unit_src = keep_attacks(unit_src, rest_src)

    gapsb = [ch.get("gaps", {}).get(i, 0.0) for i in range(len(ch["units"]))]
    idx, holds, fade, Z, ants, rise, rests = build_warp(
        a, unit_src, [b for (b, d) in ch["units"]],
        [d for (b, d) in ch["units"]], gapsb, rest_src, ch.get("lead", 0.0),
        set(ch.get("nohold", ())))
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
    out, _ = synth_from(a, idx, f0_o, fade=fade, rise=rise, rests=rests)
    sf.write(os.path.join(VOX4, f"{name}.wav"), out, fs)
    renders["lead"] = round(len(out) / fs, 3)

    for tag, cents in (("8ve-a", 1200 + 6), ("8ve-b", 1200 - 7)):
        out, _ = synth_from(a, idx, f0_o * 2.0 ** (cents / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                            vowels_only=True, air=False, fade=fade, rise=rise, rests=rests)
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
                            fade=fade, rise=rise, rests=rests)
        sf.write(os.path.join(VOX4, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    lead_in = Z * FRAME_S
    beats_total = ch["units"][-1][0] + ch["units"][-1][1]

    # WHERE THE VOWEL STARTS — @jeffrey: "make sure for each sample we
    # know when the vowel / consonant / voicing starts". On the OUTPUT
    # timeline (the render the study plays), in beats from beat 0, so
    # anything reading the chart can draw or cue against it. Voiced runs
    # are the sung vowels; the gaps between them are consonants and
    # breaths. Per word, `lead` is how far AHEAD of its beat the
    # consonant runway starts — build_warp puts the vowel ON the beat
    # and runs the consonant 1:1 before it, the way a singer leans in.
    def to_beat(frame):
        return round((frame * FRAME_S - lead_in) / SPB, 4)

    voiced_runs, k = [], 0
    while k < len(voiced_o):
        if voiced_o[k]:
            j = k
            while j < len(voiced_o) and voiced_o[j]:
                j += 1
            if (j - k) * FRAME_S >= 0.020:      # ignore single-frame flecks
                voiced_runs.append([to_beat(k), to_beat(j)])
            k = j
        else:
            k += 1

    notes = []
    for u, (wd, (beat, durb)) in enumerate(zip(words, ch["units"])):
        st = int(np.round(12.0 * np.log2(wd["f0_hz"] / TONIC))) if wd["f0_hz"] else 0
        notes.append((beat, durb, st, wd["t"].strip(),
                      round(ants[u] * FRAME_S / SPB, 4) if u < len(ants) else 0.0))
    chart_c.append((name, lead_in, beats_total, notes))
    VOICING[name] = voiced_runs
    manifest[name] = dict(slice=slice_name, lead_in=round(lead_in, 3),
                          beats=beats_total, renders=renders,
                          snaps=snaps, trims=trims, words=entry["words"])
    print(f"  {name:22s} {renders['lead']:5.2f}s  lead·8ve×2·low3·low5  «{entry['words']}»")
    if snaps:
        print(f"    boundaries: {' · '.join(snaps)}")
    if trims:
        print(f"    trimmed: {' · '.join(trims)}")

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
    for (beat, durb, st, _t, _lead) in notes:
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
json.dump({name: dict(leadIn=round(li, 3), beats=bt, voiced=VOICING[name],
                      notes=[dict(beat=b, dur=d, st=s, t=t, lead=ld)
                             for (b, d, s, t, ld) in ns])
           for (name, li, bt, ns) in chart_c},
          open(os.path.join(VOX4, ".chart.json"), "w"), indent=1)
print(f"WROTE {VOX4}/.manifest.json + .chart.json + {CDIR}/loner-chart.h ({len(chart_c)} phrases)")
