#!/usr/bin/env python3
"""
sing_line_world.py — spinging's line-continuous singing engine (round 6).

What round 6 adds — REGISTER + the diction starve fixes:

  R6·1 REGISTER — plan.register (semitones) lifts every target AFTER the
       per-line minimal-|shift| octave fit: the fit still finds where the
       spoken take naturally sits, then the whole line rides up (jeffrey:
       "I could be higher octave?"). The formant envelope is untouched (no
       kermit by construction) and the goalpost conformance becomes
       register-aware (vocal_shapes.conformance widens/shifts the f0-linked
       bands; duration/energy/click gates unchanged). The caller may retry
       a line at lower registers if it sounds strained (fallback ladder).
  R6·2 FINAL UNSTRESSED SYLLABLES STOP STARVING ("diminished" → "deman"):
       a word-final unstressed syllable's vowel gets a minimum-duration
       floor (borrowed from the preceding stressed vowel by anticipating
       the note), and word-final coda CLUSTERS (≥2 phones) may articulate
       into the phrase gap (extension cap 0.05 → 0.18 s) so /ʃt/-style
       codas render at full value instead of vanishing.
  R6·3 PHRASE-BOUNDARY SILENCE ("keys. Sus" → "kisses"): a phrase-initial
       fricative onset now gets a real ~100 ms silence carved before it
       (near-zero floor, not the 22 ms glottal dip) so the /s/ can't weld
       backward onto the previous phrase's coda.
  R6·4 PHRASE-MEDIAL ONSET PROMINENCE ("control" → "Troll"): raw plosive
       composites inside a phrase-medial word's onset get an extra boost on
       top of RAW_BOOST — the /k/ burst stays legible against the already-
       leveled vowels around it. (Consonant-span bed ducking already covers
       these frames via cons_mask.)
  R6·5 (round 6.5 — the finisher pass; R6·2-4 shipped but the QA proved
       them insufficient on the two flagged chord lines):
       · FRICATIVE CODAS MUST ACTUALLY FRICATE — guide_coda gains two
         rescues, both bounded by the next word's source start: run a
         captured fricative out past the trimmed window edge ("keys"' /z/
         lived past w1 → 25 ms of a 160 ms fricative), and seek forward
         across a closure gap the contiguous walk can't cross
         ("diminished"'s /ʃt/ sat 120 ms after the nucleus → the coda was
         a 20 ms click and whisper heard "deman").
       · PHRASE-INITIAL UNVOICED FRICATIVES STOP REACHING BACK — the
         backward onset walk no longer crosses voiced frames (and never
         the previous word's captured coda): sus's /s/ walk was swallowing
         keys' /z/ tail, singing it on the WRONG side of the R6·3 phrase
         gap ("keys. Sus" → "kisses" with the gap in place).
       · onset prominence up (ONSET_RAW_EXTRA 1.25 → 1.6) and word-initial
         plosives mid-phrase get a longer glottal set-up gap (22 → 32 ms)
         so "control"'s /k/ reads against the leveled vowels; the caller
         widens the bed's consonant duck spans to match.

Round 5 (kept) — CONSONANT TIME-STRETCHING, the way trained choirs
handle diction (the whisper round-trip gate was failing on swallowed
consonants, not on the singing):

  R5·1 STRETCHED CONSONANTS — every onset/coda window is partitioned into
       homogeneous runs ('pitch' = voiced frames, 'noise' = unvoiced
       fricative-ish frames, 'raw' = plosive closure/burst). Sustainable
       runs stretch (~2.0× noise via WORLD's noise synthesis, ~1.7× voiced
       through the pitch path); plosive runs stay 1:1 raw composites with
       their full-strength burst. Affricates stretch only their fricative
       frames (the runs partition does this for free).
  R5·2 PLOSIVE GAPS — plosive/affricate-initial words get a ~22 ms
       pre-plosive dip (the choir's glottal set-up), carved into the final
       render (lead AND choir) and excluded from the continuity gate.
  R5·3 VOWELS STAY ON THE BEAT — all onsets (voiced ones included, revising
       R4·2's on-the-beat sonorants) now END at the note's beat so the vowel
       lands ON it; the stretch is stolen from the preceding vowel's tail
       and bridge sustains (reserve = stretched onset + gap). Word-final
       codas get full value, borrowing up to 50 ms past hardEnd at phrase
       ends.
  R5·4 CONSONANT PROMINENCE — stretched consonant frames sit +3 dB proud in
       the spectral envelope; raw composites lift 1.4→1.5×. The self-choir
       is gated to VOWELS ONLY (choirs unify on vowels — the lead carries
       diction); stats expose consonant_spans so the caller can duck the
       bed under them too.
       QA: per-word onset_out/coda_out/gap ms + cons_stretch_scale in stats;
       tweak knob cons_stretch_scale for clarity re-renders.

Round 4 (kept), per jeffrey's review of round 3:

  R4·1 LEGATO BRIDGING — melody rests between words INSIDE a phrase no longer
       render as vocal silence: the previous word's vowel sustains through
       gaps (phrase grouping comes from the notation sidecar — punctuation +
       melody rests ≥ 0.4 s break phrases), f0 glides toward the next note,
       energy dips shallowly (never to zero), and the coda consonant lands
       just before the next word's onset. Breaths only at phrase boundaries.
       QA: per-phrase voicing-continuity % in the stats JSON (gate ≥ 95 %).

  R4·2 VOICED CONSONANTS THROUGH THE PITCH PATH — notes whose expected onset
       is fully voiced (nasals, liquids, glides, voiced fricatives) begin ON
       the note with the consonant: onset frames are forced through WORLD
       synthesis (never raw-composited), beta-taper 0 through the consonant,
       f0 continuous from consonant into vowel (zero frame gap). Nasal
       onsets are also actually CAPTURED now (murmur frames classify as
       "vowel"; the guided walk-back follows the RMS dip instead). Unvoiced
       consonants still composite raw ahead of the beat (p-center).
       QA: voiced_onset_jump_max_cents in the stats JSON.

  R4·3 ALIGNMENT SANITY — adjacent words can no longer sing the SAME source
       audio (nucleus search windows are clamped past the previous word's
       last nucleus), and per-nucleus voiced fractions are reported so a
       mis-split ("synthesizer") is measurable, not just audible.

Round 3 (kept):

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
MAX_VOICED_ONSET_S = 0.12   # raw m/n/l onset capture cap (stretched on output)
MAX_CODA_S = 0.26
# R5 — consonant time-stretching (choir diction)
NOISE_STRETCH = 2.0         # unvoiced sustainable fricatives via WORLD noise
VOICED_STRETCH = 1.7        # nasal/liquid/voiced-fricative runs, pitch path
STRETCH_HARD_CAP = 2.5      # no run ever stretches beyond this
MAX_ONSET_OUT_S = 0.34      # stretched onset ceiling
MAX_CODA_OUT_S = 0.42       # stretched coda ceiling
PLOSIVE_GAP_S = 0.022       # pre-plosive silence (the choir's glottal set-up)
PLOSIVE_GAP_MEDIAL_S = 0.032  # R6·5 word-initial plosives mid-phrase set up longer
GAP_FLOOR_AMP = 0.06        # the gap dips to this, never digital zero
PHRASE_FRIC_GAP_S = 0.10    # R6·3 real silence before a phrase-initial fricative
PHRASE_FRIC_GAP_AMP = 0.02  # …and it dips near-zero (a true phrase breath)
CONS_GAIN_DB = 3.0          # consonant prominence on the WORLD path
RAW_BOOST = 1.5             # raw composite boost (plosive bursts; R4 was 1.4)
ONSET_RAW_EXTRA = 1.6       # R6·5 extra boost on phrase-medial onset bursts
                            # (1.25 in R6·4 — "control"'s /k/ still drowned)
CODA_FRIC_SEEK_S = 0.35     # R6·5 how far past the nucleus a fricative coda
                            # may be sought (closure gaps break the walk)
CODA_EXTEND_S = 0.05        # word-final codas may run past hardEnd at phrase ends
CODA_CLUSTER_EXTEND_S = 0.18  # R6·2 …coda CLUSTERS may run this far into the gap
FINAL_UNSTRESSED_VOWEL_S = 0.14  # R6·2 word-final unstressed vowel floor
MAX_BREATH_S = 0.45
BRIDGE_MAX_S = 0.45         # intra-phrase gaps up to this sustain legato
BRIDGE_DIP_AMP = 0.72       # shallow energy dip at a bridged word boundary
GLIDE_SIGMA_S = 0.020
SCOOP_S = 0.07
SCOOP_ST = 0.8
DRIFT_CENTS = 8.0
VIB_HOLD_S = 0.45   # R5: stretched codas shortened vowels — a 0.5 s hold
                    # still wants vibrato (the QA analyzer measures it)
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
    # R4: LOW threshold — the voicing gate already splits nuclei at unvoiced
    # consonants; an energy gate at 0.30·max dropped quiet schwas ("synthe-
    # sizer"'s ə and ɚ vanished, so its 4 slots force-split 2 regions and the
    # whole word sang on the wrong vowels).
    thr = 0.12 * v.max()
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
        # R4: a region hugging the window's RIGHT edge is usually the next
        # word's vowel bleeding through the padding (whisper stamps lag late)
        # — score it down so the word keeps its own, central vowel ("app"
        # was singing the front of "store"'s ɔɹ).
        scores = [v[a - w0:b - w0].sum() * (0.4 if b >= w1 - 2 else 1.0)
                  for a, b in regions]
        regions.pop(int(np.argmin(scores)))
    while len(regions) < n_slots:
        lens = [b - a for a, b in regions]
        k = int(np.argmax(lens))
        a, b = regions[k]
        mid = (a + b) // 2
        regions[k:k + 1] = [[a, mid], [mid, b]]
    # R4: widen each nucleus to its natural vowel extent — a held note should
    # stretch steady vowel, not a 20 ms sliver of formant transition. The
    # threshold is the REGION's own peak (0.40·peak): a schwa widens within
    # its quiet extent, while the nasal/stop shoulder next door (well below
    # the vowel's level) stops the walk instead of being swallowed. Capped at
    # 80 ms a side so neighbours keep their consonant room.
    grow = 16  # frames = 80 ms
    for k, (a, b) in enumerate(regions):
        vpk = v[a - w0:b - w0].max() if b > a else 0.0
        if vpk <= 0:
            continue
        thr_r = 0.40 * vpk
        floor_a = max(regions[k - 1][1] if k > 0 else w0, a - grow)
        ceil_b = min(regions[k + 1][0] if k + 1 < len(regions) else w1, b + grow)
        while a - 1 >= floor_a and v[a - 1 - w0] > thr_r:
            a -= 1
        while b < ceil_b and v[b - w0] > thr_r:
            b += 1
        regions[k] = [a, b]
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


def _cons_ish(i, cls, voiced, hf_ratio):
    """Consonant-ish frame: consonant class, or quiet sibilance the high
    band vouches for (quiet sibilants classify 'sil')."""
    return (cls[i] in ("plosive", "fric", "vcons")
            or (cls[i] == "sil" and hf_ratio[i] > 0.08))


def guide_onset(nuc_start, w0, w0_ext, expected, cls, flux, voiced, hf_ratio,
                rms, floor, prev_coda_end=0, phrase_start=False):
    """Onset cluster window [a, b) matching the expected consonants.

    R5: b can land BEFORE nuc_start — the found nucleus often under-runs the
    real vowel start, stranding the onset cluster behind a stretch of vowel
    frames ("store"'s /st/ sat 110 ms before its found nucleus). When no
    consonant material touches nuc_start we SEEK back across those frames
    first; the caller extends the nucleus left to b.

    Two floors: the SEEK (which crosses vowel frames) never passes w0 — the
    adjacent-word clamp — while the consonant walks may reach w0_ext (~100 ms
    lower): consonants straddle whisper's boundaries, vowels must not.

    R6·5: a third floor — the previous word's CAPTURED CODA (prev_coda_end).
    Now that codas may run past their trimmed window (the fricative
    rescues), the next onset must not re-sing those frames."""
    if prev_coda_end:
        cap = min(prev_coda_end, max(w0, nuc_start - 2))
        w0 = max(w0, cap)
        w0_ext = max(w0_ext, cap)
    if not expected:                       # vowel-initial → essentially no onset
        a = nuc_start
        while a > w0 and a > nuc_start - 4 and voiced[a - 1] and cls[a - 1] != "vowel":
            a -= 1
        return a, nuc_start
    budget = int(_cluster_budget_s(expected) / FRAME_S)
    seek = nuc_start
    if nuc_start > w0 and not _cons_ish(nuc_start - 1, cls, voiced, hf_ratio):
        lim_seek = max(w0, nuc_start - int(0.16 / FRAME_S))
        i = nuc_start
        while i > lim_seek and not _cons_ish(i - 1, cls, voiced, hf_ratio):
            i -= 1
        if i > lim_seek:
            seek = i
    a = seek
    lim = max(w0_ext, seek - budget)
    first = expected[0]
    # walk back over consonant-ish frames
    while a > lim and _cons_ish(a - 1, cls, voiced, hf_ratio):
        a -= 1
    if first["cls"] in ("plosive", "affricate"):
        # anchor on the LAST burst before the vowel; include ~25ms closure
        # (voiced plosives too — R5: they used to fall into the murmur walk
        # below and swallow the previous vowel's tail)
        win_a = max(w0_ext, seek - budget - 6)
        bursts = [i for i in range(win_a, seek)
                  if cls[i] == "plosive" and flux[i] > 0]
        if bursts:
            a = min(a, max(win_a, bursts[-1] - 5))
    elif first["cls"] == "fricative" and not first["voiced"]:
        # extend through the unvoiced high-band run (s-clusters are long);
        # an rms floor here starved quiet line-end sibilants ("store" lost
        # its whole /st/) — the high-band ratio is the real witness
        while a > lim and not voiced[a - 1] and hf_ratio[a - 1] > 0.08:
            a -= 1
        # R6·5: non-sibilant fricatives (f θ) barely tickle the high band
        # ("four"'s /f/ captured 15 ms) — accept quiet unvoiced frames with
        # real energy just above the floor
        while a > lim and not voiced[a - 1] and hf_ratio[a - 1] > 0.03 \
                and rms[a - 1] > floor * 0.5:
            a -= 1
    elif first["cls"] in ("nasal", "approximant") or first["voiced"]:
        # Voiced sonorant onset. Nasal murmur is low-frequency dominant, so
        # classify_frames calls it "vowel" and the consonant-class walk above
        # never captures it (round 3's zero-length "mac" onset). Walk back
        # over VOICED frames while the RMS sits below the nucleus level —
        # the murmur/liquid is the quiet voiced shoulder before the vowel.
        ref = rms[seek:seek + 20].max() if seek + 1 < len(rms) else floor
        while a > lim and voiced[a - 1] and rms[a - 1] > floor * 0.5 \
                and rms[a - 1] < 0.85 * max(ref, floor):
            a -= 1
        while a < seek and not voiced[a]:
            a += 1
    # R6·5: a phrase-initial UNVOICED fricative must not sing the previous
    # phrase's voiced coda — the /s/ walk was swallowing keys' /z/ tail and
    # rendering it on the WRONG side of the R6·3 phrase gap ("keys. Sus" →
    # "kisses" with the gap in place). Trim the captured window to its LAST
    # unvoiced run: everything voiced before that run is the previous word's
    # material (trailing voice-onset transition frames stay).
    if phrase_start and first["cls"] == "fricative" and not first["voiced"] \
            and a < seek and voiced[a:seek].any():
        j = seek
        while j > a and voiced[j - 1]:
            j -= 1
        i2 = j
        while i2 > a and not voiced[i2 - 1]:
            i2 -= 1
        if j > i2:
            a = i2
        # …but keep enough fricative to HEAR: voicing overcalls on a strong
        # sibilant (the harvest's error, not the source's) can class the /s/
        # itself "vowel" and shave the run to nothing ("see"'s /s/ fell to
        # 10 ms). Re-extend while the HIGH BAND witnesses sibilance (or the
        # frame is honestly consonantal); w0 is already floored at the
        # previous word's captured coda, so nothing gets re-sung.
        min_f = int(0.07 / FRAME_S)
        while seek - a < min_f and a > w0 \
                and (cls[a - 1] != "vowel" or hf_ratio[a - 1] > 0.10):
            a -= 1
    return a, seek


def guide_coda(nuc_start, nuc_end, w1, expected, cls, voiced, hf_ratio, rms,
               floor, w1_hard=None):
    """Coda cluster window [a, b) matching the expected consonants.

    R5: a can land BEFORE nuc_end — the found nucleus often OVER-runs the
    vowel and swallows the coda ("band"'s æ ate its /nd/ → "Ben"); we walk
    BACK to the true vowel end (never deeper than the nucleus core) and the
    caller trims the nucleus to a. The mirror case (nucleus under-run, coda
    stranded past leftover vowel frames) seeks FORWARD instead.

    R6·5: w1_hard (the next word's source start, or the line end) lets a
    FRICATIVE coda run past the trimmed window edge and be sought across a
    closure gap — see the rescues at the bottom."""
    if not expected:                       # open syllable → tiny release only
        return nuc_end, min(w1, nuc_end + int(0.04 / FRAME_S))
    budget = int(_cluster_budget_s(expected, base=0.12, per=0.06) / FRAME_S)
    first = expected[0]
    last = expected[-1]
    slack = int(0.14 / FRAME_S)
    ref = max(rms[max(0, nuc_end - 30):nuc_end + 1].max() if nuc_end > 0 else floor,
              floor)
    sonorant = first["cls"] in ("nasal", "approximant")

    def coda_ish(i):
        if _cons_ish(i, cls, voiced, hf_ratio):
            return True
        # murmur frames often classify "vowel" (low-frequency dominant):
        # accept the quiet voiced shoulder for sonorant-led codas
        return sonorant and voiced[i] and rms[i] < 0.85 * ref

    start = nuc_end
    floor_b = max(nuc_start + 12,
                  nuc_end - min(budget + slack, int(0.20 / FRAME_S)))
    while start > floor_b and coda_ish(start - 1):
        start -= 1
    if start == nuc_end:
        # under-run: seek forward past leftover vowel frames
        i = nuc_end
        lim_seek = min(w1, nuc_end + budget + slack)
        while i < lim_seek and not coda_ish(i):
            i += 1
        if i < lim_seek:
            start = i
    b = max(start, nuc_end)
    lim = min(w1, b + budget)
    while b < lim and coda_ish(b):
        b += 1
    if last["cls"] == "fricative":
        while b < lim and not voiced[b] and hf_ratio[b] > 0.08:
            b += 1
    # R6·5: FRICATIVE CODAS MUST ACTUALLY FRICATE. Two rescues, both bounded
    # by w1_hard (the next word's own source audio is never stolen):
    #  · the contiguous walk stopped at the trimmed window edge mid-run
    #    ("keys"' /z/ lived past w1 → 25 ms of a 160 ms fricative) — run
    #    the captured fricative out past the edge;
    #  · the fricative sits past a closure gap the walk can't cross
    #    ("diminished"'s /ʃt/ 120 ms after the nucleus → a 20 ms click) —
    #    seek forward across the junk, then capture through the run and its
    #    trailing burst. The intervening closure maps 1:1 raw (it is the
    #    source's own articulation silence).
    fric_exp = [p for p in expected if p["cls"] in ("fricative", "affricate")]
    if fric_exp:
        v_ok = bool(fric_exp[0]["voiced"])
        affr = any(p["cls"] == "affricate" for p in fric_exp)

        def fric_here(i):
            if v_ok:
                # a lenited /z/ murmurs more than it hisses — the voiced
                # consonant class is the witness, not the high band
                return voiced[i] and cls[i] == "vcons"
            if not voiced[i] and hf_ratio[i] > 0.07:
                return True
            # R6·5: an affricate's STOP half classifies "plosive" (its hf
            # sits under the fricative bar) — "aitch"'s /tʃ/ was invisible
            # to this seek and its coda fell to a 5 ms click
            return affr and not voiced[i] and cls[i] == "plosive" \
                and rms[i] > floor * 0.3

        far0 = min(len(cls), nuc_end + int(CODA_FRIC_SEEK_S / FRAME_S))
        if any(fric_here(i) for i in range(start, b)):
            # run a captured fricative out past the window edge while it
            # DECAYS — the padded source windows overlap, so the next word's
            # start is no bound; the rms rise on the far side of the energy
            # valley is the next word's own onset and stops the walk
            while b < far0 and fric_here(b) and rms[b] <= rms[b - 1] * 1.2:
                b += 1
        # a ONE-FLICKER capture must not satisfy the rescue — a single
        # stray fric frame inside the walk was short-circuiting the seek
        # while the real /tʃ/ sat past the closure gap ("aitch" again)
        fric_n = sum(1 for i in range(start, b) if fric_here(i))
        if fric_n < int(0.04 / FRAME_S) and \
                (far := min(far0, w1 if w1_hard is None else max(w1, w1_hard))) > b:
            i = b
            while i < far and not fric_here(i):
                i += 1
            if i < far:
                j = i
                while j < far and (fric_here(j) or cls[j] in ("plosive", "fric")
                                   or (cls[j] == "sil" and hf_ratio[j] > 0.05)):
                    j += 1
                b = j
    return start, b


# ── R5: consonant diction plans (choir-style time-stretching) ──────────────

def cons_runs(a, b, cls, voiced, hf_ratio):
    """Partition a consonant window into homogeneous render runs:
    'pitch' (voiced → WORLD pitch path), 'noise' (unvoiced fricative-ish →
    WORLD noise synthesis), 'raw' (plosive closure/burst → 1:1 composite)."""
    runs = []
    for i in range(a, b):
        if voiced[i]:
            m = "pitch"
        elif cls[i] == "fric" or (cls[i] == "sil" and hf_ratio[i] > 0.08):
            m = "noise"
        else:
            m = "raw"
        if runs and runs[-1][2] == m:
            runs[-1][1] = i + 1
        else:
            runs.append([i, i + 1, m])
    return runs


SIBILANTS = ("s", "z", "ʃ", "ʒ", "tʃ", "dʒ", "t͡ʃ", "d͡ʒ")


def diction_plan(a, b, expected, cls, voiced, hf_ratio, scale, cap_s,
                 force_pitch_all=False, noise_f=None):
    """[[src_a, src_b, out_frames, mode] …] for a consonant window.

    Sustainable runs stretch like a choir lengthens diction (fricatives/
    nasals/liquids ~1.5–2.5×); plosive runs keep raw length so bursts stay
    bursts; the whole plan shrinks back toward raw length if it outgrows
    cap_s (the room actually available before/after the vowel)."""
    if b <= a:
        return [], 0
    exp = expected or []
    if force_pitch_all:
        # R4 semantics preserved: an all-sonorant cluster stays on the pitch
        # path even where harvest missed the murmur
        runs = [[a, b, "pitch"]]
    else:
        runs = cons_runs(a, b, cls, voiced, hf_ratio)
        # R6·5: an expected all-UNVOICED cluster must never ride the pitch
        # path — harvest overcalls voicing on strong sibilants ("sus"'s /s/
        # tracked "voiced", sang as TONE, and whisper heard a syllable:
        # "keys. Sus" → "kisses" with the phrase gap rendered perfectly).
        # …unless the run is too SHORT to fricate audibly (< 30 ms): a
        # 2-frame /s/ rendered as noise is nothing, while the brief tone at
        # least cues the listener ("see"'s clipped /s/ — the descent tail
        # went mute when it flipped).
        if exp and all(not p["voiced"] for p in exp):
            for r in runs:
                if r[2] == "pitch" and r[1] - r[0] >= int(0.03 / FRAME_S):
                    r[2] = "noise"
    can_noise = any(p["cls"] in ("fricative", "affricate") for p in exp)
    can_pitch = any(p["cls"] in ("nasal", "approximant")
                    or (p["cls"] == "fricative" and p["voiced"]) for p in exp)
    if noise_f is None:
        noise_f = NOISE_STRETCH
    plan = []
    for ra, rb, m in runs:
        f = 1.0
        if m == "noise" and can_noise:
            f = min(STRETCH_HARD_CAP, noise_f * scale)
        elif m == "pitch" and can_pitch:
            f = min(STRETCH_HARD_CAP, VOICED_STRETCH * scale)
        plan.append([ra, rb, max(1, int(round((rb - ra) * f))), m])
    raw_total = sum(p[1] - p[0] for p in plan)
    total = sum(p[2] for p in plan)
    cap_f = max(1, int(cap_s / FRAME_S))
    if total > cap_f and total > raw_total:
        # give back the excess proportionally, never below raw length
        excess = total - max(cap_f, raw_total)
        stretchy = [p for p in plan if p[2] > (p[1] - p[0])]
        amt = sum(p[2] - (p[1] - p[0]) for p in stretchy) or 1
        for p in stretchy:
            cut = int(round(excess * (p[2] - (p[1] - p[0])) / amt))
            p[2] = max(p[1] - p[0], p[2] - cut)
        total = sum(p[2] for p in plan)
    return plan, total


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
    for wi_w, w in enumerate(words):
        w0 = max(0, int(round(w["srcFromMs"] / 1000.0 / FRAME_S)))
        w1 = min(n_src, int(round(w["srcToMs"] / 1000.0 / FRAME_S)))
        if w1 <= w0 + 2:
            w1 = min(n_src, w0 + 8)
        w0t, w1t = trim_silence(w0, w1, rms, floor)
        # R4·3: adjacent words must not sing the SAME source audio — clamp the
        # search window past the previous word's last nucleus ("app" re-singing
        # the tail of "mac"'s vowel was round 3's double-hit).
        if segs:
            prev_end = segs[-1]["nuclei"][-1][1]
            if w0t < prev_end:
                w0t = prev_end
                if w1t < w0t + 4:
                    w1t = min(n_src, w0t + 8)
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
        # R5: shrink each nucleus to its longest ENERGETIC run — region
        # merging can hide internal silence, and a held note that maps silent
        # source frames goes dead mid-phrase ("app!"'s clipped vowel mapped
        # 45 ms of silence across a 1 s note)
        for ni in range(len(nuclei)):
            a_n, b_n = nuclei[ni]
            if b_n <= a_n:
                continue
            # energy relative to the region's own peak — quiet voiced tails
            # (breathy decay) count as dead too, harvest voicing does not
            thr_n = max(floor, 0.15 * float(rms[a_n:b_n].max()))
            runs_v = []
            i0 = a_n
            for i in range(a_n, b_n + 1):
                good = i < b_n and rms[i] > thr_n
                if not good:
                    if i > i0:
                        runs_v.append((i0, i))
                    i0 = i + 1
            if runs_v:
                best = max(runs_v, key=lambda r: r[1] - r[0])
                if best[1] - best[0] >= 4 and best != (a_n, b_n):
                    nuclei[ni] = best
        # guided onset / coda from the expected phoneme classes
        exp_onset = exp_notes[0]["phonemes"]["onset"] if exp_notes else None
        exp_coda = exp_notes[-1]["phonemes"]["coda"] if exp_notes else None
        if exp_onset is not None:
            # R5: the consonant-walk floor reaches ~100 ms past the adjacent-
            # word clamp (consonants straddle whisper's word boundaries) —
            # but never across a PHRASE break ("keys. Sus" must not sing
            # keys' /z/ into sus' /s/ → "kisses"); the vowel-crossing seek
            # stays clamped at w0t either way
            w0_ext = w0t
            if exp_onset and not (w.get("phraseStart") and len(segs)):
                w0_ext = max(0, w0t - int(0.10 / FRAME_S))
            onset = guide_onset(nuclei[0][0], w0t, w0_ext, exp_onset, cls, flux,
                                voiced, hf_ratio, rms, floor,
                                prev_coda_end=segs[-1]["coda"][1] if segs else 0,
                                phrase_start=bool(w.get("phraseStart")) and bool(segs))
        else:
            onset = (w0t, nuclei[0][0])
        # R4·2: fully-voiced SONORANT/fricative onsets sing ON the note
        # through the pitch path (m n ŋ l ɹ w j v z ð). Voiced PLOSIVES
        # (b d ɡ) keep their raw closure+burst — synthesizing a stop as tone
        # smears it ("band" → "Ben").
        onset_voiced = bool(exp_onset) and all(
            p["voiced"] and p["cls"] not in ("plosive", "affricate")
            for p in exp_onset)
        max_on = MAX_VOICED_ONSET_S if onset_voiced else MAX_ONSET_S
        if (onset[1] - onset[0]) * FRAME_S > max_on:
            onset = (onset[1] - int(max_on / FRAME_S), onset[1])
        # R5: a leftward onset seek means the vowel truly starts at the
        # onset's end — extend the first nucleus to meet it, but only across
        # frames with real vowel ENERGY (a seek that crossed silence must
        # not hand the note dead source frames)
        if onset[1] < nuclei[0][0]:
            a_n, b_n = nuclei[0]
            gap_sl = rms[onset[1]:a_n]
            thr_x = max(floor, 0.15 * float(rms[a_n:b_n].max())) if b_n > a_n else floor
            if len(gap_sl) and float((gap_sl > thr_x).mean()) > 0.5:
                nuclei[0] = (onset[1], max(b_n, onset[1] + 4))
        if exp_coda is not None:
            # R6·5: the coda may run past the trimmed window edge, but never
            # into the NEXT word's own source audio
            if wi_w + 1 < len(words):
                nxt_w0 = max(0, int(round(
                    words[wi_w + 1]["srcFromMs"] / 1000.0 / FRAME_S)))
                w1_hard = max(w1t, nxt_w0)
            else:
                w1_hard = n_src
            coda = guide_coda(nuclei[-1][0], nuclei[-1][1], w1t, exp_coda, cls,
                              voiced, hf_ratio, rms, floor, w1_hard=w1_hard)
        else:
            coda = (nuclei[-1][1], w1t)
        if (coda[1] - coda[0]) * FRAME_S > MAX_CODA_S:
            coda = (coda[0], coda[0] + int(MAX_CODA_S / FRAME_S))
        # R5: coda walked back into an over-grown nucleus → trim the vowel;
        # coda found past leftover VOICED vowel frames → the vowel extends
        if coda[0] < nuclei[-1][1]:
            a_n, b_n = nuclei[-1]
            nuclei[-1] = (a_n, max(a_n + 6, coda[0]))
        elif coda[0] > nuclei[-1][1]:
            # extend only across frames with real vowel ENERGY — harvest
            # voicing alone also marks near-silent breathy tails
            a_n, b_n = nuclei[-1]
            gap_sl = rms[b_n:coda[0]]
            thr_x = max(floor, 0.15 * float(rms[a_n:b_n].max())) if b_n > a_n else floor
            if len(gap_sl) and float((gap_sl > thr_x).mean()) > 0.5:
                nuclei[-1] = (a_n, coda[0])
        medials = [(nuclei[k][1], nuclei[k + 1][0]) for k in range(n_groups - 1)]
        coda_sonorant = bool(exp_coda) and all(
            p["cls"] in ("nasal", "approximant") for p in exp_coda)
        segs.append({"w": w, "onset": onset, "nuclei": nuclei, "medials": medials,
                     "coda": coda, "sung": sung, "w0": w0t, "w1": w1t,
                     "nucleus_slot": nucleus_slot, "exp": exp_notes,
                     "onset_voiced": onset_voiced, "coda_sonorant": coda_sonorant})
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
            weights = []
            ok = True
            for s, det in zip(segs, det_by_word):
                mids = [sl["midi"] + k for sl in s["w"]["slots"]]
                if min(mids) < 38 or max(mids) > 67:     # keep inside a real voice
                    ok = False
                if det is not None:
                    costs.append(abs(float(np.mean(mids)) - det))
                    # R4: weight by voiced evidence — a 20 ms function word
                    # (whose harvest is least trustworthy) must not outvote
                    # the line's long content words
                    weights.append(sum(b - a for a, b in s["nuclei"]))
            if not ok or not costs:
                continue
            c = float(np.average(costs, weights=weights)) if sum(weights) else float(np.mean(costs))
            if best is None or c < best[1]:
                best = (k, c)
        if best:
            line_transpose = best[0]
    if line_transpose:
        for s in segs:
            for sl in s["w"]["slots"]:
                sl["midi"] += line_transpose
    # R6·1: the register lift rides ON TOP of the minimal-shift fit — the fit
    # finds where the spoken take naturally sits, the register is the ask.
    register = int(plan.get("register", 0))
    if register:
        for s in segs:
            for sl in s["w"]["slots"]:
                sl["midi"] += register

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
    vowel_air = np.zeros(out_n)         # base air on every sung vowel
    seam = np.zeros(out_n, dtype=bool)  # composite seams → energy smoothing
    note_regions = []                   # (o_a, o_b, hold_s) per sung note
    force_voiced = np.zeros(out_n, dtype=bool)  # R4·2 sung voiced consonants
    sp_gain = np.ones(out_n)            # R4·1 shallow bridge energy dips
    cons_gain = np.ones(out_n)          # R5·4 consonant prominence (sp amp)
    raw_gain = np.ones(out_n)           # R6·4 extra boost on onset raw bursts
    cons_mask = np.zeros(out_n, dtype=bool)  # R5 consonant frames (choir gate)
    short_vowel = np.zeros(out_n, dtype=bool)  # R6·5 quick syllables: lead only
    force_unvoiced = np.zeros(out_n, dtype=bool)  # R6·5 expected-unvoiced runs
    gap_env = np.ones(out_n)            # R5·2 pre-plosive glottal-set-up dips
    onset_marks = []                    # (onset_f, vowel_f) voiced-onset QA
    word_spans = []                     # (start_f, end_f, phraseStart) R4·1 QA
    bridges = 0

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

    cons_g = db(CONS_GAIN_DB)

    def place_plan(o_end, plan, midi, med, raw_extra=1.0):
        """R5·1: place a diction plan so its output ENDS at frame o_end.

        Raw runs map 1:1 (integer source steps → the raw composite path picks
        them up with true transients); stretched runs map fractionally and
        render through WORLD (noise for unvoiced fricatives, the pitch path
        for voiced sonorants). raw_extra (R6·4) rides on RAW_BOOST for the
        raw runs — phrase-medial onset bursts sit prouder. Returns the
        plan's output start frame."""
        o = o_end - sum(p[2] for p in plan)
        o_start = o
        for ra, rb, out, m in plan:
            a = max(0, o)
            b = min(out_n, o + out)
            if b > a:
                idx = np.arange(a, b)
                if m != "pitch" and out == rb - ra:
                    src_pos[idx] = ra + (idx - o)      # 1:1 → raw composite
                    natural[idx] = True
                    raw_gain[idx] = raw_extra
                else:
                    pos = (np.linspace(ra, max(ra, rb - 1) + 1e-6, out)
                           if out > 1 else np.array([float(ra)]))
                    src_pos[idx] = pos[idx - o]
                    cons_gain[idx] = cons_g            # R5·4 prominence
                in_word[idx] = True
                target_midi[idx] = midi
                word_med[idx] = med
                cons_mask[idx] = True
                if m == "pitch":
                    force_voiced[idx] = True
                    beta_taper[idx] = 0.0
                elif m == "noise":
                    # R6·5: render as NOISE even where harvest called the
                    # source voiced (sibilant f0 overcall) — see diction_plan
                    force_unvoiced[idx] = True
            seam[max(0, o - 1):min(out_n, o + 1)] = True
            o += out
        seam[max(0, o - 1):min(out_n, o + 1)] = True
        return o_start

    onset_len = [(s["onset"][1] - s["onset"][0]) * FRAME_S for s in segs]

    # R5·3: onset diction plans up front — the reserve a word demands from its
    # predecessor is the STRETCHED onset (+ glottal gap), capped by the room
    # actually available before its beat
    stretch_scale = float(tweaks.get("cons_stretch_scale", 1.0))
    onset_plans = []
    for wi, s in enumerate(segs):
        t0w = s["w"]["slots"][0]["t"]
        if wi > 0:
            avail = t0w - (segs[wi - 1]["w"]["slots"][-1]["t"] + 0.12)
        else:
            # a line-opening word may push its onset INTO the note instead
            # (the vowel arrives late, R4-style) when there is no runway
            avail = max(t0w - line_t0 - 0.02, MAX_VOICED_ONSET_S + 0.02)
        exp_on = s["exp"][0]["phonemes"]["onset"] if s["exp"] else None
        # the glottal set-up: before every plosive/affricate, and at every
        # phrase-initial consonant (a stretched phrase-opening /s/ otherwise
        # welds onto the previous phrase's coda — "keys. Sus" → "kisses").
        # R6·3: a 22 ms dip wasn't enough for phrase-initial FRICATIVES —
        # whisper still heard the /z s/ weld — so those now get a real
        # ~100 ms near-silent phrase breath instead.
        gap_s, gap_amp = 0.0, GAP_FLOOR_AMP
        if exp_on:
            phrase_medial_start = wi > 0 and s["w"].get("phraseStart")
            if phrase_medial_start and exp_on[0]["cls"] == "fricative":
                gap_s, gap_amp = PHRASE_FRIC_GAP_S, PHRASE_FRIC_GAP_AMP
            elif exp_on[0]["cls"] in ("plosive", "affricate"):
                # R6·5: a word-initial plosive MID-phrase ("control"'s /k/
                # inside "Option control.") sets up longer — 22 ms drowned
                # against the leveled vowels around it
                gap_s = PLOSIVE_GAP_MEDIAL_S \
                    if wi > 0 and not s["w"].get("phraseStart") else PLOSIVE_GAP_S
            elif phrase_medial_start:
                gap_s = PLOSIVE_GAP_S
        oplan, out_f = diction_plan(
            s["onset"][0], s["onset"][1], exp_on, cls, voiced, hf_ratio,
            stretch_scale, min(MAX_ONSET_OUT_S, max(0.04, avail - gap_s)),
            force_pitch_all=s["onset_voiced"])
        onset_plans.append({"plan": oplan, "out_f": out_f,
                            "out_s": out_f * FRAME_S,
                            "gap_f": int(round(gap_s / FRAME_S)),
                            "gap_s": gap_s, "gap_amp": gap_amp})

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
        bridge_from = None
        if wi + 1 < len(segs):
            nxt = segs[wi + 1]
            # R5·3: every onset (voiced included) now ends AT its beat, so the
            # reserve is the stretched onset + glottal gap
            reserve = onset_plans[wi + 1]["out_s"] + onset_plans[wi + 1]["gap_s"]
            nxt_start = nxt["w"]["slots"][0]["t"] - reserve
            # R4·1: legato bridge — inside a phrase, sustain through the gap
            if not nxt["w"].get("phraseStart") and \
                    hard_end < nxt_start - 0.005 and \
                    nxt_start - hard_end <= BRIDGE_MAX_S:
                bridge_from = hard_end
                hard_end = nxt_start - 0.005
                bridges += 1
            else:
                hard_end = min(hard_end, nxt_start - 0.01)
        hard_end = max(hard_end, slots[-1]["t"] + 0.10)

        # R5·1+3: the onset plan ENDS at the beat — the vowel lands ON the
        # note; sustained consonants borrow from the previous vowel's tail.
        # Voiced onsets stay on the pitch path (forced voicing, beta 0, one
        # continuous f0 into the vowel), now anticipating the beat the way a
        # choir places sonorants early.
        op = onset_plans[wi]
        o_beat = of(slots[0]["t"])
        # runway check: if the beat sits too close to the line start the plan
        # would clip at frame 0 — push it into the note and delay the vowel
        onset_total = sum(p[2] for p in op["plan"])
        room = o_beat - op["gap_f"]
        v_delay = 0.0
        o_end_on = o_beat
        if onset_total > room:
            push = min(onset_total - room, int(0.14 / FRAME_S))
            o_end_on = o_beat + push
            v_delay = push * FRAME_S
        s["_v_delay"] = v_delay
        onset_start = o_end_on
        if op["plan"]:
            onset_start = place_plan(o_end_on, op["plan"], slots[0]["midi"], med_log,
                                     raw_extra=ONSET_RAW_EXTRA if wi > 0 else 1.0)
            if s["onset_voiced"] and o_end_on > onset_start:
                onset_marks.append((max(0, onset_start), min(out_n, o_end_on)))
        if op["gap_f"] > 0:
            ga = max(0, onset_start - op["gap_f"])
            if onset_start > ga:
                gap_env[ga:onset_start] = np.minimum(
                    gap_env[ga:onset_start], op["gap_amp"])
        s["_onset_start"] = onset_start

        # R5·3: coda diction plan — stretched to full value, stealing time
        # from the vowel tail; word-final codas at phrase ends may borrow up
        # to CODA_EXTEND_S past hardEnd (bridged codas already ride the
        # bridge into the next word's onset).
        nxt_seg = segs[wi + 1] if wi + 1 < len(segs) else None
        exp_coda_ph = s["exp"][-1]["phonemes"]["coda"] if s["exp"] else None
        # R6·2: a coda CLUSTER (≥2 phones — "diminished"'s /ʃt/) needs real
        # articulation room; let it run further into the phrase gap than a
        # single-consonant coda would.
        ext_cap = CODA_CLUSTER_EXTEND_S if exp_coda_ph and len(exp_coda_ph) >= 2 \
            else CODA_EXTEND_S
        ext_s = 0.0
        if nxt_seg is None:
            ext_s = ext_cap
        elif nxt_seg["w"].get("phraseStart") and bridge_from is None:
            nxt_onset_t = nxt_seg["w"]["slots"][0]["t"] - \
                (onset_plans[wi + 1]["out_s"] + onset_plans[wi + 1]["gap_s"])
            ext_s = max(0.0, min(ext_cap, nxt_onset_t - hard_end - 0.06))
        coda_avail = (hard_end + ext_s) - (slots[-1]["t"] + 0.06)
        cp_plan, cp_out_f = diction_plan(
            s["coda"][0], s["coda"][1], exp_coda_ph, cls, voiced, hf_ratio,
            stretch_scale, min(MAX_CODA_OUT_S, max(0.04, coda_avail)),
            force_pitch_all=s["coda_sonorant"])
        coda_len = cp_out_f * FRAME_S - ext_s   # what the vowel tail gives up
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
            # R5·3: the vowel owns the beat (late only when the line opened
            # with no onset runway)
            v_start = slots[k]["t"] + (s["_v_delay"] if k == 0 else 0.0)
            if k + 1 < n_slots:
                same_group = (s["nucleus_slot"][k + 1] if k + 1 < len(s["nucleus_slot"]) else -1) == grp
                if same_group:
                    med_len = 0.0
                else:
                    med_idx = min(grp, len(s["medials"]) - 1)
                    med_len = ((s["medials"][med_idx][1] - s["medials"][med_idx][0]) * FRAME_S
                               if s["medials"] else 0.0)
                    # R6·5: cap the medial at its expected cluster budget —
                    # a carved medial spanning a long closure must not starve
                    # the vowel before it ("con" fell to 30 ms when /ntɹ/'s
                    # 290 ms closure counted as medial). place() keeps the
                    # medial's TAIL — the burst and liquid nearest the vowel.
                    exp_med_ph = (s["exp"][k + 1]["phonemes"]["onset"]
                                  if k + 1 < len(s["exp"]) else None)
                    med_len = min(med_len,
                                  _cluster_budget_s(exp_med_ph)
                                  if exp_med_ph else 0.16)
                    med_len = min(med_len, max(0.0, slots[k + 1]["t"] - v_start - 0.03))
                v_end = slots[k + 1]["t"] - med_len
            else:
                v_end = hard_end - coda_len
            v_end = max(v_end, v_start + 0.03)
            # R6·2: a word-final UNSTRESSED syllable must not starve — its
            # vowel gets a minimum duration, borrowed by anticipating the
            # note into the preceding (stressed) vowel's tail ("diminished"'s
            # final -nished was 3 output frames + no coda room → "deman").
            if k == n_slots - 1 and n_slots >= 2:
                exp_k = s["exp"][k] if k < len(s["exp"]) else None
                if exp_k is not None and not exp_k.get("stress") \
                        and v_end - v_start < FINAL_UNSTRESSED_VOWEL_S:
                    v_start = max(slots[k - 1]["t"] + 0.10,
                                  v_end - FINAL_UNSTRESSED_VOWEL_S)
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
            note_regions.append((o_a, o_b, v_end - v_start))
            # C4: contour retention tapers out at note edges
            tt = (idx - o_a) * FRAME_S
            te = (o_b - 1 - idx) * FRAME_S
            beta_taper[idx] = np.clip(tt / BETA_START_TAPER_S, 0, 1) * \
                np.clip(te / BETA_END_TAPER_S, 0, 1)
            # C3: glide from the NATURAL onset pitch into the plateau
            first_of_group = len(melmates) == 1 or melmates.index(k) == 0
            if first_of_group:
                # voiced onsets carry the natural→plateau glide from the
                # consonant itself so f0 is one continuous path into the vowel
                if k == 0 and s["onset_voiced"]:
                    glide_pending.append((max(0, s["_onset_start"]), na))
                else:
                    glide_pending.append((o_a, na))
            # vibrato + airy sustain on true holds (angelic)
            hold = v_end - v_start
            if hold >= VIB_HOLD_S:
                vib_gain[idx] = np.clip((tt - 0.12) / VIB_ONSET_S, 0, 1)
            # R6·5: the choir tacets on QUICK vowels and on the UNSTRESSED
            # syllables of polysyllabic words — a four-layer detuned unison
            # on a schwa smears it into the previous word ("Option control"
            # → "Option-Troll" in the choired render while the bare lead
            # transcribed fine). Choirs unify on held stressed vowels;
            # passing syllables belong to the lead's diction. (Monosyllables
            # keep their choir — pronounce marks them stress 0.)
            exp_k2 = s["exp"][k] if k < len(s["exp"]) else None
            unstressed_poly = n_slots >= 2 and exp_k2 is not None \
                and not exp_k2.get("stress")
            if hold < 0.20 or unstressed_poly:
                short_vowel[idx] = True
            vowel_air[idx] = 1.0
            if hold >= 0.35:
                hold_air[idx] = np.clip((tt - 0.10) / 0.35, 0, 1)
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

        # R5·1: the coda plan ENDS at hardEnd (+ its phrase-end extension) —
        # stretched sonorant/fricative codas articulate at full value, sung
        # nasal/liquid codas stay on the pitch path (place_plan forces them).
        coda_end_f = of(hard_end)
        if cp_plan:
            coda_end_f = of(hard_end + ext_s)
            place_plan(coda_end_f, cp_plan, slots[-1]["midi"], med_log)

        # R4·1: shallow energy dip over the bridged gap articulates the word
        # boundary without breaking voicing (applied to sp — lead AND choir)
        if bridge_from is not None:
            da_f = max(0, of(bridge_from))
            db_f = min(out_n, of(hard_end))
            if db_f - da_f >= 4:
                dip = 1.0 - (1.0 - BRIDGE_DIP_AMP) * \
                    np.sin(np.linspace(0, np.pi, db_f - da_f))
                sp_gain[da_f:db_f] = np.minimum(sp_gain[da_f:db_f], dip)

        word_spans.append((
            max(0, s["_onset_start"] - op["gap_f"]),
            min(out_n, coda_end_f),
            bool(w.get("phraseStart")),
        ))

        if wi + 1 < len(segs) and segs[wi + 1]["w"].get("phraseStart"):
            ga, gb = s["w1"], segs[wi + 1]["w0"]
            ga, gb = trim_silence(ga, gb, rms, floor * 0.6)
            if gb > ga + 4 and rms[ga:gb].mean() > floor * 0.6:
                blen = min(gb - ga, int(MAX_BREATH_S / FRAME_S))
                nxt_on = of(segs[wi + 1]["w"]["slots"][0]["t"]) - \
                    onset_plans[wi + 1]["out_f"] - onset_plans[wi + 1]["gap_f"]
                ba = nxt_on - blen - 4
                if ba > coda_end_f + 2:
                    place(ba, gb - blen, gb)
        stats_words.append({
            "word": w["w"], "targets": [sl["midi"] for sl in slots],
            "detected_midi": None if detected is None else round(detected, 2),
            "shift_st": None if detected is None else
                round(float(np.mean([sl["midi"] for sl in slots])) - detected, 2),
            "onset_ms": round(onset_len[wi] * 1000),
            "onset_out_ms": round(op["out_s"] * 1000),
            "gap_ms": round(op["gap_s"] * 1000),
            "coda_ms": round((s["coda"][1] - s["coda"][0]) * FRAME_S * 1000),
            "coda_out_ms": round(cp_out_f * FRAME_S * 1000),
            "nuclei": len(s["nuclei"]), "sung": s["sung"],
            "onset_voiced": s["onset_voiced"],
            "bridged": bridge_from is not None,
            # R4·3 per-syllable sanity: every aligned nucleus should hold a
            # genuinely voiced region of the source take
            "nucleus_voiced": [round(float(voiced[a:b].mean()), 2) if b > a else 0.0
                               for a, b in s["nuclei"]],
        })

    # ── continuous target-f0 curve ─────────────────────────────────────────
    mapped = src_pos >= 0
    src_i = np.clip(np.round(src_pos).astype(int), 0, n_src - 1)
    voiced_out = np.zeros(out_n, dtype=bool)
    voiced_out[mapped] = voiced[src_i[mapped]]
    # R4·2: sung voiced consonants are voiced by fiat — harvest missing a
    # nasal murmur must not silence (or raw-composite) the pitch path
    voiced_out |= force_voiced & mapped
    voiced_out &= ~force_unvoiced          # R6·5 sibilant f0 overcall → noise

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
    # human micro-instability — the goalpost references show plateau
    # micro-drift p10 ≈ 8¢ / p50 ≈ 46¢; a frozen plateau fails the band and
    # sounds synthetic. Gaussian-filtered frame noise keeps real CURVATURE
    # inside short notes (piecewise-linear knot noise detrends away).
    def smooth_noise(sigma_s, cents):
        nn = rng.standard_normal(out_n)
        sf_ = sigma_s / FRAME_S
        r = int(max(1, round(3 * sf_)))
        k = np.exp(-0.5 * (np.arange(-r, r + 1) / sf_) ** 2)
        k /= k.sum()
        c = np.convolve(np.pad(nn, (r, r), mode="reflect"), k, mode="valid")
        c = c / (np.std(c) + 1e-9)
        return c * (cents / 1200.0) * np.log(2)
    drift = drift + (smooth_noise(0.030, 9.0) + smooth_noise(0.080, 6.0)) * drift_scale
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
    if plan.get("debug_f0"):
        np.save(plan["debug_f0"], f0_out)

    # R4·2 QA: f0 continuity across every voiced-onset→vowel boundary —
    # no frame-to-frame jump beyond the glide slope through the seam
    onset_jump_max = 0.0
    lf_all = np.where(f0_out > 0, np.log(f0_out), np.nan)
    for (a_f, e_f) in onset_marks:
        d = np.abs(np.diff(lf_all[a_f:min(out_n, e_f + 4)])) * 1200 / np.log(2)
        d = d[~np.isnan(d)]
        if len(d):
            onset_jump_max = max(onset_jump_max, float(d.max()))

    # ── sp/ap streams (formants untouched by any f0 move) ──────────────────
    sp_out = np.full((out_n, sp.shape[1]), 1e-12)
    ap_out = np.ones((out_n, ap.shape[1]))
    w0f = (1 - frac[mapped])[:, None]
    w1f = frac[mapped][:, None]
    sp_out[mapped] = w0f * sp[i0[mapped]] + w1f * sp[i1[mapped]]
    ap_out[mapped] = np.clip(w0f * ap[i0[mapped]] + w1f * ap[i1[mapped]], 0.0, 1.0)
    # R4·1: bridged-gap energy dips (power spectrum → amplitude² scaling)
    sp_out *= (sp_gain ** 2)[:, None]
    # R5·4: consonant prominence — stretched consonant frames sit proud
    sp_out *= (cons_gain ** 2)[:, None]

    # angelic air: breath on every sung vowel, more on sustains. WORLD's noise
    # excitation is scaled by the spectral envelope, so real air needs an HF
    # ENERGY floor (a quiet aspiration bed > 4kHz, tilted down toward nyquist)
    # rendered as noise (ap → high there) — breath, not buzz. This is what
    # conforms the rendered hf_ratio into the reference spectral-balance band.
    air_arr = np.clip((0.30 * vowel_air + 0.55 * hold_air) * air_scale, 0.0, 1.2)
    if air_arr.any():
        nbins = ap_out.shape[1]
        k3 = int(nbins * 3000.0 / (fs / 2.0))
        k4 = int(nbins * 4000.0 / (fs / 2.0))
        airy = np.where(air_arr > 0)[0]
        tilt = np.exp(-3.0 * np.arange(nbins - k4) / max(1, nbins - k4))
        tot = sp_out[airy].sum(axis=1)
        hf_now = sp_out[airy, k4:].sum(axis=1)
        need = 0.010 * air_arr[airy] * tot
        add = np.maximum(0.0, need - hf_now) / tilt.sum()
        sp_out[airy, k4:] += add[:, None] * tilt[None, :]
        # the aspiration bed is noise; presence band gets a gentler lift
        ap_out[airy, k4:] = np.clip(
            ap_out[airy, k4:] + (1.0 - ap_out[airy, k4:]) * 0.85, 0.0, 1.0)
        gain = (0.25 * air_arr[airy])[:, None]
        ap_out[airy, k3:k4] = np.clip(
            ap_out[airy, k3:k4] + (1.0 - ap_out[airy, k3:k4]) * gain, 0.0, 1.0)

    y = pw.synthesize(np.ascontiguousarray(f0_out),
                      np.ascontiguousarray(sp_out),
                      np.ascontiguousarray(ap_out), fs,
                      frame_period=FRAME_S * 1000.0)

    # goalpost energy-arc conformance on held notes (time domain — exact):
    # a stretched vowel keeps the source's slow crescendo and lands outside
    # the reference energy_attack band. Reshape each hold's rendered envelope
    # to the template arc (quick rise → gentle singerly decay), with held
    # notes leveled to the line's median so a quiet vowel gliding into a loud
    # one doesn't read as one long crescendo peaking a note late.
    hop = int(round(fs * FRAME_S))
    att_s = float(bands.get("energy_attack_ms", {}).get("p50", 30.0)) / 1000.0
    att_f = max(2, int(max(att_s, 0.03) / FRAME_S))
    holds = [(o_a, o_b) for (o_a, o_b, hold) in note_regions
             if hold >= 0.4 and o_b - o_a >= att_f * 2]
    if holds:
        n_env = max(1, len(y) // hop)
        env_y = np.array([np.sqrt(np.mean(y[i * hop:(i + 1) * hop] ** 2) + 1e-12)
                          for i in range(n_env)])
        env_sm = smooth_runs(env_y, np.ones(n_env, dtype=bool), 3.0)
        pks = [env_sm[a:min(b, n_env)].max() if a < n_env else 0.0 for a, b in holds]
        live = [p for p in pks if p > 1e-5]
        even_pk = float(np.median(live)) if live else 0.0
        gain_fr = np.ones(n_env)
        for (o_a, o_b), pk in zip(holds, pks):
            if pk <= 1e-5 or o_a >= n_env:
                continue
            b = min(o_b, n_env)
            n_note = b - o_a
            arc = np.ones(n_note)
            arc[:att_f] = np.linspace(0.5, 1.0, min(att_f, n_note))[:min(att_f, n_note)]
            if n_note > att_f:
                arc[att_f:] = np.linspace(1.0, 0.85, n_note - att_f)
            tgt = even_pk if even_pk > 0 else pk
            gain_fr[o_a:b] = np.clip((arc * tgt) / (env_sm[o_a:b] + 1e-9), 0.6, 1.8)
        gain_fr = smooth_runs(gain_fr, np.ones(n_env, dtype=bool), 3.0)
        y *= np.interp(np.arange(len(y)) / hop, np.arange(n_env), gain_fr)


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
                # R4/R5: articulate — the vowel leveling (arc conformance)
                # lifts sung vowels well above the raw spoken consonants;
                # boost the composite so plosive bursts stay legible.
                # R6·4: raw_gain adds onset-burst prominence on top.
                seg = x[sa:sb] * RAW_BOOST * float(raw_gain[i])
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
        # R5·4: each layer sits 1.5 dB lower than round 4 — with the vowel
        # gate this keeps the halo but stops it drowning the lead's diction
        layers = [
            {"ratio": 1.0, "cents": +9.0, "ms": 12, "db": -11.5},
            {"ratio": 1.0, "cents": -12.0, "ms": 19, "db": -12.0},
            {"ratio": 2.0, "cents": -8.0, "ms": 15, "db": -12.5},
            {"ratio": 1.5, "cents": +10.0, "ms": 22, "db": -14.5},
        ]
        ap_choir = ap_out.copy()
        nbins = ap_choir.shape[1]
        k3 = int(nbins * 3000.0 / (fs / 2.0))
        ap_choir[:, k3:] = np.clip(ap_choir[:, k3:] + (1 - ap_choir[:, k3:]) * 0.2, 0, 1)
        # R5·4: the choir sings VOWELS ONLY — consonant frames and glottal
        # gaps belong to the lead alone (a unison bed right on top of the
        # diction is what masked it); ~20 ms hann edges keep it clickless.
        choir_gain = np.ones(out_n)
        choir_gain[cons_mask] = 0.0
        choir_gain[gap_env < 0.999] = 0.0
        choir_gain[short_vowel] = 0.0        # R6·5 quick syllables: lead only
        ck = np.hanning(9)
        ck /= ck.sum()
        choir_gain = np.convolve(np.pad(choir_gain, (4, 4), mode="edge"), ck, mode="valid")
        hop_ch = int(round(fs * FRAME_S))
        for lay in layers:
            f0_c = f0_out * lay["ratio"] * (2.0 ** (lay["cents"] / 1200.0))
            yc = pw.synthesize(np.ascontiguousarray(f0_c),
                               np.ascontiguousarray(sp_out),
                               np.ascontiguousarray(ap_choir), fs,
                               frame_period=FRAME_S * 1000.0)
            yc *= np.interp(np.arange(len(yc)) / hop_ch,
                            np.arange(out_n), choir_gain)
            off = int(lay["ms"] * fs / 1000.0)
            g = db(lay["db"])
            L = min(len(y), len(yc) - 0) - off
            if L > 0:
                y[off:off + L] += g * yc[:L]

    # R5·2: carve the pre-plosive glottal set-up into the final render (lead
    # AND choir) — a short, near-silent breath before every plosive burst
    if (gap_env < 0.999).any():
        gk = np.hanning(5)
        gk /= gk.sum()
        ge = np.convolve(np.pad(gap_env, (2, 2), mode="edge"), gk, mode="valid")
        hop_g = int(round(fs * FRAME_S))
        gs = np.interp(np.arange(len(y)) / hop_g, np.arange(out_n), ge)
        y *= gs
        y_lead *= gs[:len(y_lead)]

    # R4·1 QA: intra-phrase voicing continuity — % of frames inside each
    # phrase span that carry real vocal energy on the FINAL render
    hop_c = int(round(fs * FRAME_S))
    n_env_c = max(1, len(y) // hop_c)
    env_c = np.array([np.sqrt(np.mean(y[i * hop_c:(i + 1) * hop_c] ** 2))
                      for i in range(n_env_c)])
    phrases = []
    for (a_f, e_f, ps) in word_spans:
        if ps or not phrases:
            phrases.append([a_f, e_f])
        else:
            phrases[-1][0] = min(phrases[-1][0], a_f)
            phrases[-1][1] = max(phrases[-1][1], e_f)
    in_span = np.zeros(n_env_c, dtype=bool)
    for a_f, e_f in phrases:
        in_span[max(0, a_f):min(n_env_c, e_f)] = True
    thr_c = 0.05 * (float(np.percentile(env_c[in_span], 95)) if in_span.any() else 0.0)
    # R5·2: deliberate consonant articulation — glottal set-up gaps, stretched
    # closures, unvoiced fricatives — is not broken voicing; the continuity
    # gate watches the VOWEL/bridge stream only
    gap_f_mask = gap_env < 0.999
    art_mask = gap_f_mask | cons_mask
    cont = []
    for a_f, e_f in phrases:
        seg_idx = np.arange(max(0, a_f), min(n_env_c, e_f))
        seg_idx = seg_idx[~art_mask[np.clip(seg_idx, 0, out_n - 1)]]
        if len(seg_idx):
            cont.append(float((env_c[seg_idx] > thr_c).mean()))
    continuity = {
        "per_phrase": [round(c, 3) for c in cont],
        "min": round(min(cont), 3) if cont else None,
        "mean": round(float(np.mean(cont)), 3) if cont else None,
        "phrases": len(phrases), "bridges": bridges,
    }

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
        # R5: the goalposts describe VOWEL notes — trim each note's leading
        # stretched-consonant frames (a 100 ms murmur ramp is deliberate
        # diction, not a slow vowel attack), then drop notes that are still
        # mostly consonant material
        trimmed = []
        for a, b in notes_r:
            a2 = a
            while a2 < min(b, out_n) and cons_mask[a2]:
                a2 += 1
            if (b - a2) * FRAME_S >= 0.12:
                trimmed.append((a2, b))
        notes_r = [(a, b) for a, b in (trimmed or notes_r)
                   if a >= out_n or cons_mask[a:min(b, out_n)].mean() < 0.5]
        feats_r = [note_features(f0r, rms_r, hf_r, a, b) for a, b in notes_r]
        conf = conformance(feats_r, bands, register=register)
        conf["_notes_measured"] = len(feats_r)
    clicks = click_scan(y.astype(np.float64), fs)
    # exonerate flux spikes that land on NATURAL consonant composites — those
    # samples are the original take (plosive bursts are supposed to be there)
    if clicks["flux_spikes"]:
        real = []
        for p in clicks["spike_s"]:
            fr = int(p / FRAME_S)
            a = max(0, fr - 6)
            b = min(out_n, fr + 7)
            if not (comp[a:b].any() or seam[a:b].any()):
                real.append(p)
        clicks["flux_spikes"] = len(real)
        clicks["spike_s"] = real
        clicks["positions_s"] = clicks["click_s"] + real

    # R5·4: consonant spans (line-relative seconds) — the caller ducks the
    # bed under these so the diction isn't masked by the jingle either
    cons_spans = []
    marked = cons_mask | gap_f_mask
    ii = 0
    while ii < out_n:
        if marked[ii]:
            jj = ii
            while jj < out_n and marked[jj]:
                jj += 1
            cons_spans.append([round(ii * FRAME_S, 3), round(jj * FRAME_S, 3)])
            ii = jj
        else:
            ii += 1

    j = np.abs(np.diff(np.log(np.where(f0_out > 0, f0_out, np.nan)))) * 1200 / np.log(2)
    j = j[~np.isnan(j)]
    print(json.dumps({
        "words": stats_words,
        "line_transpose": line_transpose,
        "register": register,
        "beta": round(beta, 4), "harmony": harmony,
        "cons_stretch_scale": stretch_scale,
        "consonant_spans": cons_spans,
        "f0_jump_max_cents": round(float(j.max()), 1) if len(j) else 0,
        "f0_jump_p95_cents": round(float(np.percentile(j, 95)), 1) if len(j) else 0,
        "voicing_continuity": continuity,
        "voiced_onset_jump_max_cents": round(onset_jump_max, 1),
        "conformance": conf,
        "clicks": clicks,
        "out_frames": out_n,
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
