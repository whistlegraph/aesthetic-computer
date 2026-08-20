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

import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
from nervox import waver as nervox_waver, flange as nervox_flange
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
GLIDE_ST_S = 18.0           # above this rate of change she is SLIDING, not
                            # holding a note — and a slide is not out of tune
FORMANT_DB = 1.6
AIR_DB = 2.5
BREATH = 0.14
# THE SIBILANT RESTORE — @jeffrey: "the opening 's' is not hearable".
# It was never missing. In her source the /s/ of "sitting" runs 15 dB under
# the vowel that follows it; in the render that gap opens to 19, because
# the voiced half gets the formant lift, the air shelf and the breath and
# the unvoiced half — which is copied straight from the warped source —
# gets none of them. Four dB does not sound like much until the floor is
# under it, and then the consonant is simply gone.
#
# So: find the frames that are UNVOICED AND BRIGHT (a fricative is dim but
# bright — the same test audit.py uses to call an event FRIC) and give them
# back the level the voiced frames were given, plus a little more, because
# a sung sibilant in a mix needs to beat a hi-hat and not just itself.
# Ramped over 30 ms so it lifts the consonant rather than gating it.
SIB_DB = 8.0                # the lift on an unvoiced bright frame
SIB_HI_HZ = 3000.0          # "bright" is measured above here…
SIB_SHARE = 0.45            # …and means this much of the frame's energy
SIB_RAMP_S = 0.030
HALO_DARK_HZ = 5500.0
HALO_BREATH_X = 1.5
UNVOICED_W = 0.18           # consonant share of any stretch
SILENT_W = 0.04             # a pause inside a word hardly stretches at all
PEAK_LEAD_MAX_S = 0.09      # how far a word's peak may pull its start early
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


CACHE = os.path.join(VOX4, ".cache")


def analyze_cached(path, x, fs):
    """WORLD analysis is 2.3 s a slice and depends only on the audio and
    the analysis constants — never on the chart. Tuning a bar re-ran it
    every time. Cached on disk, keyed by the file's mtime and the
    parameters, so an edit pays for synthesis only."""
    key = (f"{os.path.basename(path)}-{int(os.path.getmtime(path))}"
           f"-{FRAME_MS}-{FLOOR}-{SNAP}-{SMOOTH_MS}-{GLIDE_ST_S}-nervox1-fitfloor1")
    dest = os.path.join(CACHE, key + ".npz")
    if os.path.exists(dest):
        z = np.load(dest)
        return dict(x=x, fs=fs, f0=z["f0"], f0c=z["f0c"],
                    sp=z["sp"].astype(np.float64), ap=z["ap"].astype(np.float64),
                    voiced=z["voiced"], rate=z["rate"])
    a = analyze(x, fs)
    os.makedirs(CACHE, exist_ok=True)
    np.savez(dest, f0=a["f0"], f0c=a["f0c"], voiced=a["voiced"], rate=a["rate"],
             sp=a["sp"].astype(np.float32), ap=a["ap"].astype(np.float32))
    return a


def analyze(x, fs):
    # THE FLOOR HAS TO FIT THE VOICE. FLOOR was set for Camille's take,
    # which sits at 283 Hz; harvest below its floor does not return a low
    # note, it returns a wrong one — usually the second harmonic — or
    # nothing. Half the bank sings lower than she does. rq, sh and pf are
    # all around 121 Hz, well UNDER the 140 Hz floor, so every one of them
    # was analysed from a broken pitch track and everything downstream —
    # the snap, the melody lock, THE HOLD — was reading it. @jeffrey: "it
    # sounds so glitchy". That is what it was.
    #
    # So the range is measured before it is used: one wide probe to find
    # where this voice actually lives, then the real analysis bracketed
    # around it. Wide-open on every take would be worse than a fixed
    # floor — the lower you let harvest look, the more freely it picks a
    # subharmonic on a bright voice — so the bracket is snug.
    # …and it only ever WIDENS. The spine is the record, and a bracket
    # recomputed from a probe would have moved her ceiling from 600 to 849
    # on the next rebuild for no reason at all. Nothing moves unless the
    # voice does not fit: FLOOR/600 stands for anyone it already suits.
    probe, _t = pw.harvest(x, fs, f0_floor=55.0, f0_ceil=900.0,
                           frame_period=FRAME_MS)
    voiced_probe = probe[probe > 0]
    med = float(np.median(voiced_probe)) if len(voiced_probe) else TONIC
    floor, ceil = FLOOR, 600.0
    if med * 0.55 < floor:                      # a voice lower than hers
        floor = max(55.0, med * 0.55)
    if med * 2.0 > ceil:                        # …or higher
        ceil = min(1000.0, med * 3.0)
    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=ceil, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP
        # A SLIDE IS NOT OUT OF TUNE. Smoothing the correction keeps a glide
        # SMOOTH but still drags it onto the grid tone by tone, so a fast
        # scoop comes out as a staircase — @jeffrey on the leap into "pa":
        # "it feels a little extreme · maybe we need better auto tune on
        # it". That leap is 8.4 semitones in 30 ms. So the snap now fades
        # out wherever her pitch is genuinely MOVING. f0 is smoothed over
        # 60 ms before the derivative, so vibrato (±0.5 st at ~5 Hz, ~16
        # st/s instantaneous) averages away and only a real transition
        # survives; unvoiced gaps are interpolated across first, or their
        # edges would read as infinite glides and disable the snap at every
        # onset.
        st = 12.0 * np.log2(np.maximum(f0, 1e-6) / TONIC)
        idx = np.arange(len(f0))
        st = np.interp(idx, idx[voiced], st[voiced])
        rate = np.abs(np.gradient(smooth(st, int(60.0 / FRAME_MS)),
                                  FRAME_MS / 1000.0))
        corr *= np.clip(1.0 - (rate - GLIDE_ST_S) / GLIDE_ST_S, 0.0, 1.0)
    else:
        rate = np.zeros_like(f0)
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    # `rate` travels with the analysis so nervox wavers exactly the frames
    # the snap regulated, and lets go exactly where the snap let go.
    return dict(x=x, fs=fs, f0=f0, f0c=f0c, sp=sp, ap=ap, voiced=voiced, rate=rate)


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
                             "lead": 0.0,        # no pickup — @jeffrey: "remove the
                                                 # prelude beat and space so it just
                                                 # starts immediately". Her /s/ still
                                                 # plays, 1:1, in the few frames before
                                                 # the downbeat rather than stretched
                                                 # across a whole beat of air.
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
                             # curled 2.29 is not a grid value on purpose: up's peak sits
                                       # 0.71 beats into its slot, so the slot has
                                       # to open at 6.29 for the peak to land on
                                       # the bar 1 beat 4 line.
                                       "durs": { 0: 2.0, 1: 2.0, 2: 1.91, 3: 2.09,
                                       # "my" a beat sooner: the warp is one
                                       # sequential frame map, so units cannot
                                       # truly overlap — "in" simply ends where
                                       # "my" begins, which is what overlapping
                                       # would sound like anyway. in lands at
                                       # 0.92x, her own speed. Then "my" moves
                                       # to halfway past beat 3 — in takes the
                                       # half beat (2.0, 1.23×) and my gives it
                                       # back (1.5, 1.07×), so self keeps bar 3
                                       # and nothing downstream moves.
                                       # IN takes two full beats — @jeffrey:
                                       # "'in' should last two full beats and my
                                       # should start on bar 2:3 now". At 1.5 it
                                       # ran 1.10× and handed "my" the 2:2.5 line;
                                       # at 2 it is 1.47×, still her voice and
                                       # nowhere near the 1.8 hold, and "my" opens
                                       # square on bar 2 beat 3.
                                       # SELF grows so "i" opens on bar 3:3 —
                                       # @jeffrey: "'i' should move so it starts
                                       # on bar 3:3 now". self goes 2 → 2.5
                                       # (1.02× → 1.23×), which lands i on 14.0.
                                       4: 2.0, 5: 1.5, 6: 2.5,
                                       # think 1 (1.17×, her own speed — at 2
                                       # it was a 2.3× held tone), so OF keeps
                                       # its 4 beats at 1.01×, her real octave,
                                       # "a" keeps 2, and STONE starts on bar 6
                                       # beat 2 as asked.
                                       # my on bar 2 beat 4; "i" takes the
                                       # freed beat (1.78×, still her voice)
                                       # so "of" keeps beat 19 and stone its
                                       # bar 6 beat 2.
                                       # …and "i" pays for it. Giving IN its
                                       # second beat pushed the whole back half
                                       # off its lines — self 2:4.5, think 3:4.5,
                                       # stone 6:1.5. "i" was the one word with
                                       # slack anywhere near it: 0.71 s stretched
                                       # over 2 beats at 1.39×. At 1.5 it runs
                                       # 1.04×, her own speed, and think · of · a ·
                                       # stone are all back on the lines they were
                                       # tuned to. Only self and i sit a half beat
                                       # later than before.
                                       # THINK on bar 4:1 — @jeffrey: "and think
                                       # should start on bar 4:1". "i" takes its
                                       # second beat back (1.04× → 1.39×, still
                                       # well under the 1.8 hold) so it ends on
                                       # 16.0 and think opens the bar.
                                       # 2.0, and it must STAY a beat value.
                                       # The +0.22 slice shift caught this one
                                       # by accident — it is the only `durs`
                                       # entry that sits alone on a line before
                                       # a comment, which is exactly the shape
                                       # the times-bumping pass looked for — so
                                       # "i" became 2.22 and every word after it
                                       # sat 0.22 of a beat late: @jeffrey,
                                       # "these boundaries are still off · check
                                       # the rest of the words too". durs are
                                       # BEATS; times are SECONDS. Nothing in
                                       # here shifts with the slice.
                                       7: 2.0,
                                       # …and "a" pays this one, for free. Once
                                       # stone took back its own /s/, "a" was
                                       # 0.43 s of audio over 3 beats — 3.4×, far
                                       # past the 1.8 hold line, so it is already
                                       # a synthesized grid tone rather than her
                                       # voice. 2.5 beats is 2.9×: the same
                                       # synthetic hold, half a beat shorter, and
                                       # stone is back on bar 6:1 with everything
                                       # after it.
                                       # "a" pays again — and this is the last
                                       # half beat it has. @jeffrey set its floor
                                       # at two ("a should be at least two beats,
                                       # its too short now"), so anything after
                                       # this has to come from "of" or run
                                       # downstream.
                                       # STONE'S PEAK ON THE BAR 6 EDGE —
                                       # @jeffrey: "'stone' should shift just a
                                       # little · so its peak lands on the bar 6
                                       # edge". The block was already on 6:1, but
                                       # the block now opens with the /s/ it took
                                       # back, and a consonant rides 1:1 through
                                       # the warp — so her NOTE arrived 0.75 of a
                                       # beat late and her peak 1.24 beats late.
                                       # PEAK_LEAD_MAX_S is 90 ms, a lean, so this
                                       # is the grid's job. Aligning the RMS PEAK
                                       # was wrong — @jeffrey: "whoa now stone
                                       # starts too soon". The peak sits 0.21 s
                                       # after her vowel, so peak-on-the-beat
                                       # opened the block 1.24 beats early and the
                                       # /s/ arrived most of a bar before 6. Her
                                       # VOWEL is the anchor a listener hears: it
                                       # starts 0.37 s in, consonants ride ~1:1,
                                       # so the slot opens 0.75 of a beat early at
                                       # 23.25 and the note lands on 24.0 with the
                                       # sibilant leading into it. stone holds
                                       # 3.75 (1.39×) and still hands "just" the
                                       # bar 6:4 line; of keeps its 4, and "a"
                                       # takes 1.25 — 1.43×, her own voice rather
                                       # than the 2.3× synthetic hold it was at 2
                                       # beats.
                                       # …and then right a bit more — @jeffrey:
                                       # "it should move to the right a bit
                                       # more". Half a beat, bought from "of"
                                       # (4 → 4.5, 1.37×; it is the held octave
                                       # and takes the extra) rather than from
                                       # "a", which at 1.75 would tip back over
                                       # the 1.8 hold and go synthetic again.
                                       # stone opens 23.75, its /t/ lands 24.51.
                                       # JUST ON BAR 7 — @jeffrey: "and 'just'
                                       # should start on bar 7 now". stone holds
                                       # the extra beat itself (3.25 → 4.25,
                                       # 1.57×, still under the 1.8 hold) rather
                                       # than moving, so its /t/ stays on 24.51
                                       # and only what follows "just" shifts.
                                       # OF ENDS ON BAR 5:3 — @jeffrey: "'of'
                                       # should end at bar 5:3 start, and 'a'
                                       # should start there". of back to 4
                                       # (18–22, 1.22×). That pins BOTH of a's
                                       # edges — 22 to stone's 23.75 — so it is
                                       # 1.75 beats, 2.0×, just over the hold: "a"
                                       # is a sustained grid tone again rather
                                       # than her voice. That is the cost of the
                                       # two placements, and it is the connective
                                       # in "of a stone", so it holds well.
                                       8: 2.0, 9: 4.0, 10: 1.75, 11: 4.25,
                                       # very longer, patiently slower
                                       # the tail on bars: just and waiting
                                       # a bar each (waiting on bar 8:1),
                                       # very as TWO notes, and "pa" held a
                                       # full bar because at 2 beats it was
                                       # too fast and abrupt
                                       # waiting splits like sitting did, so
                                       # "ing" lands on the half bar
                                       12: 4.0, 13: 2.0, 14: 2.0, 15: 2.0,
                                       # BAR 10 HOLDS pa AND tient, BAR 11 OPENS
                                       # WITH ly — @jeffrey: "can pa and tient be
                                       # just within bar 10?" · "and ly should be
                                       # bar 11 first three beats". tient 2.5 → 2
                                       # ends it on 44.0 (0.94×, a hair quicker
                                       # than she sang it) so pa·tient fill bar 10
                                       # exactly, and ly takes the freed half beat
                                       # itself — 3 beats, 44–47, at 1.76×, just
                                       # under the 1.8 hold, which is right for
                                       # the held last syllable of the word. "for"
                                       # keeps bar 11:4 and nothing downstream
                                       # moves.
                                       16: 2.0, 17: 2.0, 18: 2.0, 19: 2.0,
                                       # for and time were still the auto scale's
                                       # — @jeffrey: "and for is two beats too" ·
                                       # "and time is 4 beats". Pinning them ends
                                       # the auto scale on this line entirely:
                                       # every unit's length is now a decision.
                                       # time at 4 is 1.80×, right on the hold
                                       # line, so it sustains as a grid tone —
                                       # which is what a held "time" wants.
                                       # …then @jeffrey squared the tail off:
                                       # "'ly' should be two beats" · "'for'
                                       # should be two beats" · "and time to pass
                                       # each of those is 1 full bar". ly 3 → 2
                                       # pulls for back to 11:3, and from there
                                       # the last three words own a bar each —
                                       # time bar 12, to bar 13, pass bar 14.
                                       # "to" at 4 beats is 2.05×, over the hold,
                                       # so it sustains as a grid tone; "pass"
                                       # lands at 1.65× and stays her voice.
                                       20: 2.0, 21: 4.0, 22: 4.0, 23: 4.0 },
                             # words whose syllables carry a melody must not
                             # be flattened to one tone by THE HOLD
                             # patiently is pa·tient·ly, three notes
                             # ve|ry and pa|tient were both cut off the
                             # brightness column (audit's zoom), not by ear.
                             # very is legato — no level dip anywhere, because
                             # /r/ is an approximant — so the only edge is the
                             # note: she holds A#3 flat to 15.165 and only then
                             # glides to G#3. The old 15.05 opened "ry" 115 ms
                             # inside "ve"'s pitch, so ry sang ve's note first.
                             # patiently's /ʃ/ runs 16.655–16.825 (hf 0.9+, level
                             # −30); the old 16.80 left 145 ms of it in "pa" and
                             # gave "tient" the last 25 ms of its own consonant.
                             # sit|ting is MEASURED now, not found. The
                             # automatic fricative split put it at 0.865,
                             # 140 ms inside her "sit" vowel while it was
                             # still at −7 dB — it had been fine until unit 0
                             # reached back for the /s/, which moved every
                             # landmark the search leans on. Her vowel holds
                             # to 0.945, decays into the /t/ closure and
                             # bottoms at 1.015; the burst is 1.025 and "ting"
                             # arrives 1.035 on its own note. The closure
                             # belongs to the syllable it opens.
                             "sylls": { 0: [(None, "sitting·a"), (1.00, "sitting·b")],
                                        4: [(None, "my"), (4.72, "self")],
                                        11: [(None, "wait"), (13.92, "ing")],
                                        12: [(None, "ve"), (15.39, "ry")],
                                        13: [(None, "pa"), (16.875, "tient"),
                                             (17.92, "ly")] },
                             # "pa" stays on the F4 she sings — the leap up
                             # from very's A#3 is the point of the phrase,
                             # not something to smooth away. (`shift` is
                             # still there if a unit ever needs moving.)
                             # source seconds, read off her onsets, for the
                             # words whisper-1 mistimed. PRE-split indices.
                             # 24.33 — her /s/ tapers out at 24.30. In the
                             # slice's clock (see the cap below).
                             "end": 24.55,
                             # THE S OF SITTING — @jeffrey: "id love a stronger
                             # 'ssss' sound at start of first english
                             # 'sittting too'". Her sibilant runs 0.065–0.295
                             # in the SOURCE and the slice used to open at
                             # 0.280, so 215 ms of a 230 ms /s/ was thrown
                             # away before anything here ever saw it. The
                             # slice now starts at 0.06 — and every
                             # slice-relative time in this chart moved +0.22
                             # with it — so the whole sibilant exists; this
                             # pin is what makes unit 0 actually HOLD it,
                             # rather than opening at the aligner's word start
                             # 220 ms later. With lead 0 it plays 1:1 as the
                             # pre-roll, so the record opens on her ssss and
                             # lands on the downbeat.
                             "times": { 0: 0.0,
                                        1: 1.73,   # curled — the aligner cut at 1.465,
                                                   # inside ting's decay (18% there,
                                                   # 8% at 1.48), so curled opened
                                                   # holding the end of "sitting".
                                                   # Her /k/ closure is 1.52–1.58 and
                                                   # belongs to curled; ting's decay
                                                   # bottoms out at 1.50.
                                        2: 2.62,
                                        7: 7.37,   # of — her onset is 7.17;
                                                   # the aligner opened its span
                                                   # 115 ms of silence early
    # up — the alignment put this
                                                    # boundary at 2.44, AFTER up's
                                                    # attack had already begun, so
                                                    # curled's block ended on the
                                                    # first 20 ms of "up". Her level
                                                    # valley is 2.38–2.42; the
                                                    # boundary belongs in it.
                                        # A CONSONANT BELONGS TO THE NOTE IT
                                        # LEADS INTO. The aligner hands a word
                                        # over where the transcript does, which
                                        # is at the vowel — so four words were
                                        # opening after their own mouth had
                                        # already played inside the word before.
                                        # Each of these is the brightness column
                                        # (audit's zoom), not a guess.
                                        9: 9.41,     # stone — her /s/ is 290 ms
                                                     # of hf 0.99 running
                                                     # 9.19–9.48, then the /t/
                                                     # closure to 9.56 and the
                                                     # vowel at 9.60. Opening at
                                                     # 9.555 put the WHOLE
                                                     # sibilant in "a" and let
                                                     # stone enter as "-tone".
                                        10: 11.315,  # just — the /dʒ/ affricate
                                                     # steps up at 11.095 and
                                                     # brightens to 0.9 by 11.115;
                                                     # opening at 11.195 caught
                                                     # its last 20 ms and dropped
                                                     # the rest on the floor.
                                        11: 12.92,   # waiting
                                        12: 14.62,   # very
                                        13: 16.12,   # patiently — her /p/ and the
                                                     # breath into it run
                                                     # 15.90–15.98 and her voice
                                                     # arrives at 15.98; "ry" was
                                                     # holding all of it.
                                        14: 18.76,   # for
                                        15: 19.535,  # time — the /t/ burst is
                                                     # 30 ms of hf 0.97 at 19.315
                                                     # with its aspiration to
                                                     # 19.42. At 19.48 the word
                                                     # started on its vowel and
                                                     # "for" said the t.
                                        16: 21.42,   # to
                                        17: 23.03 },  # pass — /p/ + breath from
                                                     # 22.81, voice from 22.945.
                                                     # 23.00 was 55 ms into her
                                                     # own attack.
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
                             "gaps": {
                                       } },
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


def snap_boundaries(a, words, t0, pinned=()):
    """Pull each word start onto the acoustic event nearest it."""
    x, fs, f0 = a["x"], a["fs"], a["f0"]
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    if m < 8 or len(words) < 2:
        return words, []
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    st = np.where(f0[:m] > 0, 12.0 * np.log2(np.maximum(f0[:m], 1e-6) / TONIC), np.nan)
    W = max(2, int(round(SNAP_MED_S / FRAME_S)))
    # sliding-window medians instead of a Python loop with two np.median
    # calls per frame — same numbers, a fraction of the time
    step = np.zeros(m)
    if m > 2 * W:
        win = np.lib.stride_tricks.sliding_window_view(st, W)      # m-W+1 x W
        med = np.nanmedian(win, axis=1)                            # per start k
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
        if i in pinned:          # a measured pin is a decision, not a guess
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
TRIM_HF_DB = -30.0          # …and dull: a fricative is dim but BRIGHT
TRIM_QUIET_RUN_S = 0.120    # silence this long may end the word
TRIM_LEAK_S = 0.150         # audio after it that is just the next word bleeding in
ATTACK_S = 0.030            # pre-roll kept ahead of every word's onset


def keep_attacks(unit_src, rest_src, x=None, fs=None):
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
    # Borrow ONLY silence. @jeffrey: "feels like 'time' has bits of the
    # last word in it". The runway was bounded by the previous unit's
    # span, which is not the same as the previous word's SOUND — where
    # two words run together with no gap, reaching back 30 ms for an
    # attack reached into the neighbour's glide, and the audit found
    # "time" holding 44% of a fragment belonging to "for". Walk back only
    # across frames under the gate, so a word can pick up its own breath
    # and never anyone else's voice.
    quiet = None
    if x is not None:
        n = int(round(fs * FRAME_S))
        m = len(x) // n
        e = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
        quiet = e <= (np.max(np.abs(x)) or 1.0) * 10.0 ** (TRIM_GATE_DB / 20.0)
    out = []
    for u, (s0, s1) in enumerate(unit_src):
        floor = 0 if u == 0 else unit_src[u - 1][1]
        lo = max(floor, s0 - pre)
        if quiet is not None:
            k = s0
            while k > lo and k - 1 < len(quiet) and quiet[k - 1]:
                k -= 1
            lo = k
        out.append((lo, s1))
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
    # A frame is silence only if it is quiet AND dull. @jeffrey: "self
    # feels like i don't feel the 'f' at the end". Her /f/ sits at 1–2%
    # of peak — below the broadband gate — so the trim read the quietest
    # fricative in the language as silence and cut it off the word. But
    # a fricative is not silent, it is DIM AND BRIGHT: little energy,
    # nearly all of it high. Differencing the samples is a crude 6 dB/oct
    # high-pass, and that is enough to tell an /f/ from room tone.
    d = np.diff(seg[:m * n + 1]) if len(seg) > m * n else np.diff(
        np.concatenate([seg[:m * n], seg[-1:]]))
    hf = np.sqrt((d[:m * n].reshape(m, n) ** 2).mean(axis=1))
    quiet = (rms <= peak * 10.0 ** (TRIM_GATE_DB / 20.0)) & \
            (hf <= (np.max(hf) or 1.0) * 10.0 ** (TRIM_HF_DB / 20.0))
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
               nohold=(), tail_end=None):
    """Frame index map with VOWEL-ON-THE-BEAT alignment.

    Each word's voiced onset (its vowel) lands exactly on its chart
    slot; the consonant runway plays 1:1 just AHEAD of the beat — the
    way a singer actually places a word. Stretch lives in the voiced
    body (weight 1.0 vs 0.18 for unvoiced); pre-roll and the slice tail
    ride 1:1. Returns (idx, holds, fade, Z) where Z is the output frame
    of the phrase's beat 0 (the lead-in the C engine subtracts).
    """
    F = len(a["f0c"])
    # Stretch weights: a vowel absorbs it (1.0), a consonant rides near
    # 1:1 (0.18) — and SILENCE inside a word barely moves at all. Her
    # 0.20 s pause between "patient" and "ly" was being stretched to
    # 0.40 s along with the singing, which detached the last syllable so
    # it read as a word of its own sitting next to "for". A pause is not
    # a phoneme; lengthening it does not lengthen the word, it just
    # breaks it in half.
    w = np.where(a["voiced"], 1.0, UNVOICED_W)
    xs, fsr = a["x"], a["fs"]
    spf = int(round(fsr * FRAME_S))
    nf = min(F, len(xs) // spf)
    if nf > 0:
        e = np.sqrt((xs[:nf * spf].reshape(nf, spf) ** 2).mean(axis=1))
        quiet = e <= (np.max(np.abs(xs)) or 1.0) * 10.0 ** (TRIM_GATE_DB / 20.0)
        w[:nf][quiet] = SILENT_W
    # PEAK ON THE BEAT. @jeffrey: "align the peaks". The warp put each
    # word's voiced ONSET on its slot, which is where the note starts —
    # but not where the ear hears the hit. A sung syllable swells, and the
    # beat is felt at the loudest moment, so a word whose peak is 60 ms
    # into the vowel reads as 60 ms late even though its onset is exact.
    # The anticipation now runs to the PEAK, with everything before it
    # playing 1:1 ahead of the beat the way a singer leans in. Capped, so
    # a late-peaking word cannot drag its start into the word before it.
    xs, fsr = a["x"], a["fs"]
    spf = int(round(fsr * FRAME_S))
    nfr = min(F, len(xs) // spf)
    fen = np.sqrt((xs[:nfr * spf].reshape(nfr, spf) ** 2).mean(axis=1))
    lead_cap = int(round(PEAK_LEAD_MAX_S / FRAME_S))
    ants, voiced_at = [], []                    # frames ahead of the beat
    for (s0, s1) in unit_src:
        v0 = s0
        # TWO MECHANISMS, TWO WINDOWS. 0.20 s cannot see a real sibilant —
        # "sitting" opens on 235 ms of /s/ — so for the FIRST unit the
        # search gave up mid-fricative, decided the word had no unvoiced
        # runway, and set Z = 0. The render then carried 330 ms of pre-roll
        # that lead_in never reported and the engine placed the whole vocal
        # that late: @jeffrey, "seems like all other utterances got bumped".
        #
        # But widening it for EVERY unit is a different change, and a worse
        # one. `ants` also decides how far a word leans back into the word
        # before it, and the back half of this line is almost all
        # consonant-initial — just /dʒ/, for /f/, time /t/, to /t/, pass
        # /p/. At 0.40 they all started leaning at once and the whole
        # second half moved: "just waiting very patiently for time to pass
        # is all offset". The pickup is about the take's opening; the lean
        # is a per-word gesture with its own calibration. Wide window for
        # unit 0 only.
        wide = 0.40 if len(voiced_at) == 0 else 0.20
        lim = min(s0 + int(wide / FRAME_S), s1 - 1, F - 1)
        while v0 < lim and not a["voiced"][v0]:
            v0 += 1
        if not a["voiced"][min(v0, F - 1)]:
            v0 = s0
        voiced_at.append(v0)                            # BEFORE the peak search
        # The lead may not exceed the word's own CONSONANT runway.
        # @jeffrey: "the end of up still has the start of in — clip
        # bleed". He is describing peak alignment doing what it was told:
        # pulling a word's start ahead of its beat so the peak lands on
        # it. But "in" opens on a vowel, so there was no consonant to
        # lean with — the 90 ms it borrowed was the vowel itself, sounding
        # inside the previous word's block. A singer leans in with the
        # consonant. A vowel-initial word starts ON the beat.
        cons = max(0, v0 - s0)                          # unvoiced runway
        hi = min(s1, nfr, v0 + int(0.35 / FRAME_S))     # look in the attack
        if hi > v0 and cons > 0:
            pk = v0 + int(np.argmax(fen[v0:hi]))
            v0 = min(pk, v0 + min(lead_cap, cons))
        ants.append(max(0, v0 - s0))
    # THE PICKUP is the UNVOICED prefix only. Peak alignment made ants[0]
    # reach into the vowel, and this stretch used to swallow whatever was
    # in `pre` — so the /s/ window suddenly contained voiced attack, the
    # noise synthesis got coloured by a vowel, and the take opened on a
    # strange tone. Split it: the fricative before her first voiced frame
    # is what stretches across the pickup beat; the peak lead that follows
    # plays 1:1, the way any other word's runway does.
    u0 = unit_src[0][0]
    von = max(u0, min(voiced_at[0], u0 + ants[0]))
    unv = list(range(0, von))                       # fricative, pre-vowel
    lead_tail = list(range(von, u0 + ants[0]))      # attack up to the peak, 1:1
    rise = None
    if lead_b > 0.0 and len(unv):
        want = max(1, int(round(lead_b * SPB / FRAME_S)))
        pos = np.linspace(0, len(unv) - 1, want)
        src_n = len(unv)
        unv = [unv[int(round(p))] for p in pos]
        rise = (0, want, src_n)     # synth_from rebuilds this range as noise
    pre = unv + lead_tail
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
    # The tail plays 1:1 after the last word — but only to `tail_end`.
    # This take carries a second utterance after "pass" (she goes again at
    # 23.0 s), and without a cap the tail played it too, which is the
    # extra syllable heard at the end of the line.
    Fend = min(F, tail_end) if tail_end else F
    tail0 = unit_src[-1][1]
    idx += list(range(min(tail0, Fend), Fend))                # tail, 1:1
    # THE RELEASE — when the source has no tail (she flows straight into
    # the next phrase in the take), synthesize one: a ping-pong read of
    # the final unit's last 120 ms, faded to nothing by the caller.
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
        # full words: rebuild a warped copy of the source for the
        # consonants. This was a Python loop over every 5 ms frame of
        # every render — the single slowest thing in the build. It is one
        # gather: each output frame reads spf consecutive source samples,
        # so the sample indices are idx*spf + arange(spf), raveled.
        spf = int(fs * FRAME_S)
        nf = min(len(idx), (n + spf - 1) // spf)
        pos = (np.asarray(idx[:nf], dtype=np.int64)[:, None] * spf
               + np.arange(spf, dtype=np.int64)[None, :]).ravel()
        np.clip(pos, 0, len(x) - 1, out=pos)
        xw = np.zeros(n)
        take = min(n, pos.size)
        xw[:take] = x[pos[:take]]
        out = mask * y + (1 - mask) * xw
        # …and now give the fricatives back what the voiced half was given.
        # sp_o is already the post-lift envelope, so the brightness test
        # reads the frame as it will be heard.
        band = freqs > SIB_HI_HZ
        share = sp_o[:, band].sum(1) / (sp_o.sum(1) + 1e-12)
        fric = (~voiced_o) & (share > SIB_SHARE)
        if fric.any():
            gf = np.where(fric, 10.0 ** (SIB_DB / 20.0), 1.0)
            gf = smooth(gf, max(1, int(SIB_RAMP_S / FRAME_S)))
            g = np.repeat(gf, int(fs * FRAME_S))
            out *= g[:n] if len(g) >= n else np.pad(g, (0, n - len(g)), constant_values=1.0)
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


# A partial build (PHRASES=...) must not drop the phrases it skipped from
# the receipts or the generated header — the C engine looks every phrase
# up by name. Seed from what is already on disk and overwrite in place.
def _seed(path):
    try:
        return json.load(open(path))
    except Exception:
        return {}

_old_chart = _seed(os.path.join(VOX4, ".chart.json"))
manifest = _seed(os.path.join(VOX4, ".manifest.json"))
chart_c = []   # (phrase, lead_in_s, beats_total, [(beat, dur, st, t, lead)])
VOICING = {}   # per phrase, voiced runs in beats — vowels vs consonants

# TAKECHARTS — a chart per take, not one chart lent around.
# @jeffrey: "our envelopes etc are still fitting like the original samples ·
# we need to like restart the whole process for each actual take · or it
# will sound wonky". bin/takechart.py assembles a take's line from its
# per-word corpus files and measures its OWN onsets, syllable seams and
# notes; it keeps only `durs` from w-whole-line, because unit lengths in
# beats are the song and everything else in that entry is f- on one day.
# Registering the result here rather than warping afterwards is the whole
# point: the take then goes through this file's real pipeline — consonant
# runway, boundary snap, energy trim, note re-measurement, THE HOLD,
# nervox, the sibilant restore — and gets an envelope of its own.
# OPT-IN. These are a BENCH, not the record — @jeffrey: "sounds bad i think
# we should stick to our original". Registered unconditionally they would
# join every bare halo3 run, get built into the bank, and be loaded as
# samples by an engine that never asks for them. bin/tryout-takes.sh sets
# TAKES=1; nothing else does, so the record's pipeline is untouched.
_takes_path = os.path.join(LANE, "samples", ".takecharts.json")
if os.environ.get("TAKES") and os.path.exists(_takes_path):
    _takes = json.load(open(_takes_path))
    for _name, _r in _takes.items():
        SLICES[_r["chart"]["slice"]] = _r["slice"]
        ALIGN[_r["chart"]["slice"]] = _r["align"]
        _c = dict(_r["chart"])
        for _k in ("durs", "times"):
            _c[_k] = {int(_i): _v for _i, _v in (_c.get(_k) or {}).items()}
        _c["sylls"] = {int(_i): [(None if _c0 is None else float(_c0), _l)
                                 for _c0, _l in _cuts]
                       for _i, _cuts in (_c.get("sylls") or {}).items()}
        CHART[_name] = _c
    print(f"→ .takecharts.json registered {len(_takes)} take(s): "
          f"{' '.join(sorted(_takes))}")

# CHART-EDITS — what ChartWizard dragged. The CHART literal above stays the
# score, with its reasons written next to every number; the GUI writes only
# the numbers, into a sidecar merged over the top. Two authors, one file
# each, so a drag can never eat a paragraph explaining why a boundary is
# where it is — and `git diff chart-edits.json` is a readable list of what
# a session moved by hand.
_edits_path = os.path.join(LANE, "chart-edits.json")
if os.path.exists(_edits_path):
    _edits = json.load(open(_edits_path))
    for _name, _e in _edits.items():
        if _name not in CHART:
            continue
        _ch = CHART[_name]
        for _k in ("times", "durs", "gaps", "stretch"):
            if _k in _e:
                _ch[_k] = {**(_ch.get(_k) or {}),
                           **{int(_i): _v for _i, _v in _e[_k].items()}}
        if "sylls" in _e:                      # [[null|seconds, label], …]
            _ch["sylls"] = {**(_ch.get("sylls") or {}),
                            **{int(_i): [(None if _c[0] is None else float(_c[0]), _c[1])
                                         for _c in _cuts]
                               for _i, _cuts in _e["sylls"].items()}}
        if "end" in _e:
            _ch["end"] = _e["end"]
    print(f"→ chart-edits.json merged over {len(_edits)} phrase(s)")

ONLY = {p for p in os.environ.get("PHRASES", "").split(",") if p}
LEAD_ONLY = os.environ.get("LEAD_ONLY") is not None

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
    # PROVENANCE. Every unit's left edge is owned by exactly one knob in
    # the CHART, and ChartWizard has to write a drag back to that knob and
    # no other: `pin` is the pre-split word index, `cut` is None for the
    # word's own start (times[pin]) or k for its k-th syllable cut
    # (sylls[pin][k-1]). The fricative sub-split makes a ·b edge that no
    # knob owns — halo3 finds it in the audio — so it carries cut="auto"
    # and the GUI refuses to drag it.
    if aligned:
        words = [dict(t=w["t"], start=t0_slice + w["start"],
                      end=t0_slice + w["end"], f0_hz=w["f0_hz"],
                      note=w["note"], pin=i, cut=None)
                 for i, w in enumerate(ALIGN[slice_name]["words"])]
    else:
        words = [dict(w, pin=i, cut=None) for i, w in enumerate(entry["word_f0"])]
    for wi, ts in (ch.get("times") or {}).items():
        if 0 <= wi < len(words):
            words[wi]["start"] = t0_slice + ts
            if wi:
                words[wi - 1]["end"] = t0_slice + ts

    # SYLLABLE SPLITS at explicit source times, applied AFTER the time
    # pins so those indices still mean what they say. The chart gives one
    # note per unit, but "patiently" is three syllables on three
    # DIFFERENT pitches — she descends F4 → D#4 → C#4 across it — so a
    # single unit made the pluck play one note against three, and left
    # the word as a lump the bar map could not place. Cutting it into
    # real syllables gives each its own note, slot and beat.
    syll_pins = set()
    for wi in sorted((ch.get("sylls") or {}), reverse=True):
        base = words[wi]
        cuts = ch["sylls"][wi]
        head = next((c[1] for c in cuts if c[0] is None), base["t"])
        cuts = [c for c in cuts if c[0] is not None]
        edges = [base["start"]] + [t0_slice + c[0] for c in cuts] + [base["end"]]
        labels = [head] + [c[1] for c in cuts]
        words[wi:wi + 1] = [dict(base, start=edges[k], end=edges[k + 1],
                                 t=labels[k], pin=wi, cut=(None if k == 0 else k))
                            for k in range(len(labels))]
        syll_pins.update(labels[1:])   # a measured cut is a measurement too

    # words pinned by `times` were measured off her own onsets; the
    # snapper must not then drag them somewhere else. Indices shift
    # by the syllable splits inserted before them.
    # Boundaries we MEASURED — `times` pins and syllable cuts alike — must
    # survive the snapper. It moved the my/self cut from the 4.50 s start
    # of her /s/ to 4.715, back inside the fricative, which is the bug it
    # was supposed to fix.
    pinned = {i for i, w in enumerate(words) if w["t"] in syll_pins}
    for wi in (ch.get("times") or {}):
        extra = sum(len([c for c in cs if c[0] is not None])
                    for si, cs in (ch.get("sylls") or {}).items() if si < wi)
        pinned.add(wi + extra)
    words, snaps = snap_boundaries(a, words, t0_slice, pinned)
    # sub-split units at their internal fricative (e.g. myself → my·self)
    for ui in sorted(ch.get("splits", []), reverse=True):
        w = words[ui]
        f0 = int(round((w["start"] - t0_slice) / FRAME_S))
        f1 = int(round((w["end"] - t0_slice) / FRAME_S))
        # AN ONSET CONSONANT IS NOT AN INTERNAL ONE. The search used to
        # start a quarter of the way into the unit, which was fine while
        # "sitting" began at its vowel — once the block reached back to
        # take her 220 ms /s/, that quarter-point landed INSIDE the
        # sibilant and split sit·ting 110 ms early, inside the first
        # syllable. Skip the word's own opening unvoiced run first, then
        # look a quarter of the way through what is left.
        v0 = f0
        while v0 < f1 - 1 and v0 < len(a["voiced"]) and not a["voiced"][v0]:
            v0 += 1
        lo, run, split_f = v0 + max(3, (f1 - v0) // 4), 0, None
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
        second = dict(w, start=ts, t=w["t"] + "·b", cut="auto")
        words[ui:ui + 1] = [first, second]
    # RE-MEASURE THE NOTE. f0_hz came from the aligner, over the span the
    # aligner thought the word had. Once `times` repins a boundary or a
    # split cuts a word in two, that number describes the wrong audio —
    # which is how "for" kept its old pitch after being moved a whole
    # word later. Read every note off her voice, over the span the chart
    # will actually play.
    for wd in words:
        f_a = int(round((wd["start"] - t0_slice) / FRAME_S))
        f_b = int(round((wd["end"] - t0_slice) / FRAME_S))
        seg = a["f0"][max(0, f_a):max(1, min(f_b, len(a["f0"])))]
        seg = seg[seg > 0]
        if len(seg):
            wd["f0_hz"] = float(np.median(seg))

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
    unit_src = keep_attacks(unit_src, rest_src, x, fs)
    # ONE CLOCK. `end` used to subtract the slice offset while `times` and
    # `sylls` add it, so an `end` written in the same seconds you read off
    # the zoom landed 280 ms early — @jeffrey: "i can't hear the s in
    # pass". Her /s/ is 24.03–24.30 and the cap was falling at 23.97, in
    # the gap before it, so the sibilant was cut every render. It is the
    # slice's clock now, like every other time in this chart.
    if ch.get("end"):                      # the last word stops at the cap too
        # …and it SETS the last unit's end rather than only clipping it.
        # The aligner stopped "pass" at 24.20, inside her /s/, and a cap
        # that can only shorten could never give the rest back.
        cap = int(round(ch["end"] / FRAME_S))
        a0, _ = unit_src[-1]
        unit_src[-1] = (a0, min(F, max(a0 + 1, cap)))

    gapsb = [ch.get("gaps", {}).get(i, 0.0) for i in range(len(ch["units"]))]
    idx, holds, fade, Z, ants, rise, rests = build_warp(
        a, unit_src, [b for (b, d) in ch["units"]],
        [d for (b, d) in ch["units"]], gapsb, rest_src, ch.get("lead", 0.0),
        set(ch.get("nohold", ())),
        int(round((ch["end"] - t0_slice) / FRAME_S)) if ch.get("end") else None)
    f0_o = a["f0c"][idx].copy()
    voiced_o = a["voiced"][idx]

    # THE MELODY LOCK — another take, HER tune.
    # @jeffrey: "whoa the pitches are way off now". A take chart keeps the
    # composition and re-measures the performance, and the first cut of it
    # counted only `durs` as composition. But the snap regulates a voice
    # toward the nearest degree of the A#-minor grid, not toward a NOTE —
    # so with its own notes re-measured, every take sang its own tune, in
    # its own octave. Melody is the song exactly as much as rhythm is.
    #
    # Each unit is moved onto the spine's semitone by an OFFSET rather than
    # by rewriting f0 flat, so the take keeps its own vibrato, scoops and
    # glides inside the note and only the note itself is hers. The line is
    # folded by WHOLE OCTAVES to wherever the take actually sings — several
    # of these voices sit an octave below her — because forcing her
    # register on a low voice is a different song, not the same one.
    #
    # THE OFFSET IS A CURVE, NOT A STAIRCASE. The first version multiplied
    # each unit by a constant and stopped there — @jeffrey: "it sounds so
    # glitchy". It did: the pitch stepped instantly at every unit boundary,
    # by up to several semitones, and WORLD renders a hard f0 step as a
    # click. rq came out with 425 frames jumping more than 2 st against the
    # spine's 132. So the offsets are laid on a per-frame track and
    # smoothed with the same window the snap uses — which is why a real
    # melodic leap survives: it is the CORRECTION that is smoothed, and the
    # octave jump on "of" lives in the take's own contour underneath.
    melody = ch.get("melody")
    locked = None
    if melody and len(melody) == len(ch["units"]):
        want, have = np.array(melody, dtype=float), np.full(len(melody), np.nan)
        bounds = []
        for u, (bt, du) in enumerate(ch["units"]):
            o0 = Z + int(round(bt * SPB / FRAME_S))
            o1 = Z + int(round((bt + du) * SPB / FRAME_S))
            o0, o1 = max(0, o0), min(len(f0_o), o1)
            bounds.append((o0, o1))
            seg = f0_o[o0:o1][voiced_o[o0:o1]]
            seg = seg[seg > 0]
            if len(seg):
                have[u] = 12.0 * np.log2(float(np.median(seg)) / TONIC)
        ok = ~np.isnan(have)
        if ok.any():
            octs = round(float(np.median((have - want)[ok])) / 12.0)
            locked = want + 12.0 * octs
            # a unit with no usable voiced median gets its neighbours'
            # correction rather than a cliff back to zero
            delta_u = np.where(ok, locked - have, np.nan)
            idxs = np.arange(len(delta_u))
            delta_u = np.interp(idxs, idxs[ok], delta_u[ok])
            track = np.zeros(len(f0_o))
            for u, (o0, o1) in enumerate(bounds):
                if o1 > o0:
                    track[o0:o1] = delta_u[u]
            if bounds:
                track[:bounds[0][0]] = delta_u[0]
                track[bounds[-1][1]:] = delta_u[-1]
            track = smooth(track, int(SMOOTH_MS / FRAME_MS))
            f0_o = np.where(voiced_o, f0_o * 2.0 ** (track / 12.0), 0.0)
            print(f"    melody locked: {int(ok.sum())}/{len(want)} units onto the "
                  f"spine, {octs:+d} octave{'' if abs(octs) == 1 else 's'}, "
                  f"median move {np.median(np.abs(delta_u)):.1f} st")

    # THE HOLD — long stretches flatten to the unit's median grid tone.
    # It reads the SOURCE analysis, which is right for the take the chart
    # was measured from and wrong for any other: with a melody lock in
    # front of it, every held note was being flattened back onto the pitch
    # the singer originally sang and the lock silently undone. That is why
    # rq's seven bad units were all its LONG ones, and why "of" — the one
    # octave leap in the lyric — came out 16 semitones under the chart.
    # Where the melody is locked, the hold sustains the note as it will be
    # SUNG. Without a lock it reads the source exactly as before, so the
    # spine is untouched.
    for (o0, o1, s0, s1) in holds:
        if locked is not None:
            seg = f0_o[o0:o1][voiced_o[o0:o1]]
            seg = seg[seg > 0]
        else:
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

    # PER-SYLLABLE TRANSPOSE. Sometimes the note she sang is not the note
    # the score wants — @jeffrey: "the pa in patiently should be pitched
    # down". `shift` moves one unit by N semitones, in her f0 AND in the
    # chart, so the pluck doubles the new note rather than the old one.
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
    # NERVOX — @jeffrey: "i think wavering notes too / flanging and wiggling
    # the pitches · so the voice sounds more nervous · lets call this
    # 'nervox' technique". The snap is what makes her notes NOTES; this is
    # what stops them being a machine's notes. Held frames only — the same
    # glide test the snap uses, so the two never fight. pop/lib/nervox.py.
    rate_o = a["rate"][idx]
    f0_n = nervox_waver(f0_o, FRAME_S, rate=rate_o, voiced=voiced_o)
    out, _ = synth_from(a, idx, f0_n, fade=fade, rise=rise, rests=rests)
    out = nervox_flange(out, a["fs"])
    sf.write(os.path.join(VOX4, f"{name}.wav"), out, fs)
    renders["lead"] = round(len(out) / fs, 3)

    for tag, cents in () if LEAD_ONLY else (("8ve-a", 1200 + 6), ("8ve-b", 1200 - 7)):
        out, _ = synth_from(a, idx, f0_o * 2.0 ** (cents / 1200.0),
                            dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                            vowels_only=True, air=False, fade=fade, rise=rise, rests=rests)
        sf.write(os.path.join(VOX4, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    # THE BACKUP — full words at the 3rd and 5th below, darker, ±det
    for tag, deg, det in () if LEAD_ONLY else (("low3", -2, 5.0), ("low5", -4, -6.0)):
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

    # WHICH LYRIC WORD IS THIS UNIT. The chart plays 24 units but the
    # lyric has 18 words — "patiently" is three units, "sitting" and
    # "myself" and "waiting" and "very" are two each. Anything warping a
    # DIFFERENT take onto this chart has to know that grouping, or it
    # maps 18 source words onto 24 slots by index and sings six of them
    # twice. Both split mechanisms mark a continuation with `cut`, so a
    # unit opens a new word exactly when `cut` is None.
    notes, wi = [], -1
    for u, (wd, (beat, durb)) in enumerate(zip(words, ch["units"])):
        if wd.get("cut") is None:
            wi += 1
        # what the band doubles is what the voice SINGS — the locked note
        # where there is one, the measured note otherwise.
        if locked is not None:
            st = int(round(locked[u]))
        else:
            st = int(np.round(12.0 * np.log2(wd["f0_hz"] / TONIC))) if wd["f0_hz"] else 0
        st += int((ch.get("shift") or {}).get(u, 0))
        notes.append((beat, durb, st, wd["t"].strip(),
                      round(ants[u] * FRAME_S / SPB, 4) if u < len(ants) else 0.0,
                      wi))
    chart_c.append((name, lead_in, beats_total, notes))
    VOICING[name] = voiced_runs
    # THE SPANS ACTUALLY PLAYED — after times, sylls, snapping, attack
    # pre-roll and the energy trim. bin/audit.py checks these against the
    # sung events it finds in the audio, so the check is on what the
    # chart plays rather than on what a transcriber claimed.
    # SLICE-relative seconds, the same clock the slice wav and the
    # alignment use — adding the source offset here silently shifted the
    # audit against the audio it was checking.
    played = [[w["t"], round(a_ * FRAME_S, 3), round(b_ * FRAME_S, 3)]
              for w, (a_, b_) in zip(words, unit_src)]
    pins = [dict(t=w["t"], pin=w.get("pin"), cut=w.get("cut")) for w in words]
    manifest[name] = dict(slice=slice_name, lead_in=round(lead_in, 3),
                          spans=played, pins=pins,
                          beats=beats_total, renders=renders,
                          snaps=snaps, trims=trims, words=entry["words"])
    print(f"  {name:22s} {renders['lead']:5.2f}s  lead·8ve×2·low3·low5  «{entry['words']}»")
    if snaps:
        print(f"    boundaries: {' · '.join(snaps)}")
    if trims:
        print(f"    trimmed: {' · '.join(trims)}")

_new_chart = dict(_old_chart)
_new_chart.update({name: dict(leadIn=round(li, 3), beats=bt, voiced=VOICING[name],
                              notes=[dict(beat=b, dur=d, st=s, t=t, lead=ld, w=wi)
                                     for (b, d, s, t, ld, wi) in ns])
                   for (name, li, bt, ns) in chart_c})

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
    "// `t` is the unit's label — the C addresses a word by name so a",
    "// treatment can be written against \"pa\" rather than against unit 17.",
    "typedef struct { double beat, dur; int st; const char *t; } ChartNote;",
    "typedef struct { const char *name; double leadIn; double beats;",
    "                 int n; const ChartNote *notes; } ChartPhrase;",
    "",
]
_all = [(nm, _new_chart[nm]["leadIn"], _new_chart[nm]["beats"],
         [(q["beat"], q["dur"], q["st"], q["t"], q.get("lead", 0.0), q.get("w", i))
          for i, q in enumerate(_new_chart[nm]["notes"])]) for nm in _new_chart]
for name, lead_in, beats_total, notes in _all:
    ident = name.replace("-", "_")
    lines.append(f"static const ChartNote {ident}_notes[] = {{")
    for (beat, durb, st, _t, _lead, _w) in notes:
        lines.append(f'    {{ {beat:.2f}, {durb:.2f}, {st}, "{_t}" }},')
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
open(os.path.join(CDIR, "loner-chart.h"), "w").write("\n".join(lines))

# the labeled chart, for tooling (the timeline video reads this)
json.dump(_new_chart, open(os.path.join(VOX4, ".chart.json"), "w"), indent=1)
print(f"WROTE {VOX4}/.manifest.json + .chart.json + {CDIR}/loner-chart.h ({len(chart_c)} phrases)")
