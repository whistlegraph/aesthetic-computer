# takechart.py — give a take its OWN chart, instead of borrowing hers.
#
# @jeffrey: "our envelopes etc are still fitting like the original samples ·
# we need to like restart the whole process for each actual take · or it
# will sound wonky".
#
# He is right, and it is a structural thing rather than a mixing one.
# bin/singdub.py warps another take onto the chart halo3 built FROM f-, and
# that chart is two different kinds of number tangled together:
#
#   THE COMPOSITION    beats, and `durs` — how many BEATS each unit gets.
#                      This is the music. It is the same for every take.
#   F-'S PERFORMANCE   `times` (13 hand-pinned onsets), `sylls` (5 measured
#                      syllable cuts), `end`. All SECONDS into
#                      f-whole-line.wav. These describe one singer on one
#                      day, and nothing about them transfers.
#
# singdub also never reads `lead`/`ants`, so every word it places starts AT
# the slot edge — where halo3 puts the VOWEL on the beat and runs the
# consonant 1:1 before it, the way a singer leans in. A borrowed envelope
# and no runway is exactly "wonky".
#
# So this restarts the process. It keeps the composition and re-measures
# everything else against the take's own audio, which is cheap because
# samples/corpus is already cut per word: the 18 onsets that took a session
# to pin by hand for f- are simply the file boundaries here. Only the
# syllable cuts inside the five multi-syllable words have to be found, and
# they are found in THAT take's voice.
#
# The output is a sidecar halo3 reads, so each take then goes through the
# WHOLE pipeline — consonant runway, boundary snap, energy trim, note
# re-measurement, the weighted warp clock, THE HOLD, nervox, the sibilant
# restore — rather than a warp bolted on afterwards.
#
#   pop/.venv/bin/python pop/loner/bin/takechart.py rq sh lg
#   PHRASES=w-rq LEAD_ONLY=1 pop/.venv/bin/python pop/loner/bin/halo3.py
import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
FRAME_MS = 5.0
FRAME_S = FRAME_MS / 1000.0
TONIC = 237.0
# THE ASSEMBLER HAS TO LEAVE ROOM FOR THE RUNWAY. halo3 puts a word's
# VOWEL on its beat and runs the consonant 1:1 before it, so a word needs
# space to lean back into. The measured runways here reach 0.53 beats
# (0.26 s), and butting the corpus files together at 0.10 s meant a
# consonant either did not fit or ate the end of the word before it.
# The head pad is the same thing for word 0: with "sitting" starting at
# sample 0 there was nowhere for its /s/ to go and the phrase came out
# with leadIn 0.000 — the opening sibilant missing, exactly as it went
# missing from the record.
GAP_S = 0.25
HEAD_S = 0.40
# EVERY CUT EDGE IS A CLICK. The corpus files were sliced out of longer
# takes, so each one starts and ends on whatever sample the cut landed on
# — butting eighteen of them together left 754 step discontinuities in
# rq-line against 386 in her own unbroken take, and @jeffrey heard the
# difference: "it sounds so glitchy". A 6 ms cosine at each edge costs
# nothing audible and removes all of them.
EDGE_S = 0.006
NAMES = "C C# D D# E F F# G G# A A# B".split()

LYRIC = ("sitting curled up in myself i think of a stone just waiting "
         "very patiently for time to pass").split()
# how many syllables each word is sung across — the only thing about the
# split that is compositional. WHERE the cut falls is per-take and measured.
SYLLS = {0: ["sitting·a", "sitting·b"], 4: ["my", "self"],
         11: ["wait", "ing"], 12: ["ve", "ry"], 13: ["pa", "tient", "ly"]}


def note_name(hz):
    if not hz:
        return "?"
    m = int(round(69 + 12 * np.log2(hz / 440.0)))
    return f"{NAMES[m % 12]}{m // 12 - 1}"


def take_words(take):
    d = os.path.join(LANE, "samples", "corpus")
    idx = {}
    for f in os.listdir(d):
        if f.endswith(".wav") and f.count("-") >= 2:
            tk, _, w = f[:-4].split("-", 2)
            if tk == take:
                idx[w] = os.path.join(d, f)
    return idx if all(w in idx for w in LYRIC) else None


def syllable_cuts(x, fs, f0, s, e, k):
    """The k−1 seams inside one sung word, found in this take's own voice.

    A syllable boundary is where the voice thins: an unvoiced frame (the
    /t/ of pa|tient|ly), or failing that the deepest dip in energy. Score
    every interior frame for both, then take the best k−1 that are far
    enough apart to be real syllables rather than one wobble counted twice.
    """
    f_a, f_b = int(s / FRAME_S), int(e / FRAME_S)
    f_b = min(f_b, len(f0))
    if k < 2 or f_b - f_a < 8:
        return []
    hop = int(FRAME_S * fs)
    rms = np.array([np.sqrt(np.mean(x[i * hop:(i + 1) * hop] ** 2)) + 1e-9
                    for i in range(f_a, f_b)])
    db = 20 * np.log10(rms / rms.max())
    voiced = f0[f_a:f_b] > 0
    # skip the word's own onset consonant — that is an edge, not a seam
    lo = int(np.argmax(voiced)) if voiced.any() else 0
    lo = max(lo + 2, int(0.12 * len(db)))
    hi = len(db) - max(2, int(0.12 * len(db)))
    if hi - lo < k:
        return []
    score = (~voiced).astype(float) * 6.0 - db / 6.0
    sep = max(2, (hi - lo) // (k + 1))
    picked = []
    order = sorted(range(lo, hi), key=lambda i: -score[i])
    for i in order:
        if len(picked) == k - 1:
            break
        if all(abs(i - j) >= sep for j in picked):
            picked.append(i)
    return [round(s + i * FRAME_S, 4) for i in sorted(picked)]


def build(take, durs, melody, beats):
    idx = take_words(take)
    if not idx:
        print(f"  {take}: does not own all 18 lyric words")
        return None
    y0, fs = sf.read(idx[LYRIC[0]], dtype="float64")
    parts, spans, n = [np.zeros(int(HEAD_S * fs))], [], int(HEAD_S * fs)
    for w in LYRIC:
        y, fs = sf.read(idx[w], dtype="float64")
        if y.ndim > 1:
            y = y.mean(axis=1)
        e = min(int(EDGE_S * fs), len(y) // 4)
        if e > 1:
            ramp = 0.5 - 0.5 * np.cos(np.linspace(0.0, np.pi, e))
            y = y.copy()
            y[:e] *= ramp
            y[-e:] *= ramp[::-1]
        spans.append((n / fs, (n + len(y)) / fs))
        parts.append(y)
        n += len(y)
        g = np.zeros(int(GAP_S * fs))
        parts.append(g)
        n += len(g)
    x = np.concatenate(parts[:-1])          # no trailing breath
    slice_name = f"{take}-line"
    sf.write(os.path.join(LANE, "samples", f"{slice_name}.wav"), x, fs)

    f0r, t = pw.harvest(x, fs, f0_floor=80.0, f0_ceil=600.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0r, t, fs)

    words = []
    for i, (w, (s, e)) in enumerate(zip(LYRIC, spans)):
        seg = f0[int(s / FRAME_S):int(e / FRAME_S)]
        seg = seg[seg > 0]
        hz = float(np.median(seg)) if len(seg) else 0.0
        words.append(dict(t=w, start=round(s, 4), end=round(e, 4),
                          f0_hz=round(hz, 1), note=note_name(hz)))

    sylls = {}
    for wi, labels in SYLLS.items():
        s, e = spans[wi]
        cuts = syllable_cuts(x, fs, f0, s, e, len(labels))
        if len(cuts) != len(labels) - 1:
            print(f"  {take}: could not seam {LYRIC[wi]} — left whole")
            continue
        sylls[str(wi)] = [[None, labels[0]]] + [[c, labels[j + 1]]
                                                for j, c in enumerate(cuts)]

    med = float(np.median(f0[f0 > 0])) if (f0 > 0).any() else 0.0
    return dict(
        slice=dict(source=f"corpus/{take}", start=0.0, end=round(len(x) / fs, 3),
                   words="the whole lyric, assembled from corpus",
                   dur=round(len(x) / fs, 3), median_f0_hz=round(med, 1),
                   word_f0=words),
        align=dict(model="corpus", text=" ".join(LYRIC), words=words),
        chart=dict(slice=slice_name, beats=beats, lead=0.0,
                   durs={str(i): d for i, d in enumerate(durs)},
                   # THE MELODY IS THE SONG, not the singer. The first cut
                   # of this file kept only `durs` as compositional and let
                   # each take's notes be re-measured from its own voice —
                   # @jeffrey: "whoa the pitches are way off now". They
                   # were: every take was being autotuned to the nearest
                   # scale degree of ITS OWN contour, so it sang its own
                   # tune in its own octave. Rhythm AND melody are the
                   # composition; only the TIMING — onsets, syllable
                   # seams, consonant runways — belongs to the take.
                   melody=melody,
                   sylls=sylls,
                   times={str(i): round(s, 4) for i, (s, _) in enumerate(spans)},
                   end=round(spans[-1][1], 4)),
        name=slice_name)


def main():
    takes = sys.argv[1:]
    if not takes:
        print("usage: takechart.py <take> [take…]"); return
    # THE COMPOSITION, read off the built chart rather than restated: unit
    # lengths in BEATS, which is the one part of w-whole-line that is the
    # song and not the singer.
    built = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))["w-whole-line"]
    durs = [n["dur"] for n in built["notes"]]
    melody = [n["st"] for n in built["notes"]]
    beats = round(sum(durs), 4)
    print(f"→ the composition: {len(durs)} units, {beats} beats, "
          f"melody {min(melody)}..{max(melody)} st")

    path = os.path.join(LANE, "samples", ".takecharts.json")
    out = json.load(open(path)) if os.path.exists(path) else {}
    for tk in takes:
        r = build(tk, durs, melody, beats)
        if not r:
            continue
        out[f"w-{tk}"] = r
        cuts = sum(len(v) - 1 for v in r["chart"]["sylls"].values())
        print(f"  ✓ w-{tk:4s} → samples/{r['name']}.wav  "
              f"{r['slice']['dur']:5.2f}s · 18 words pinned exactly · "
              f"{cuts} syllable seams found · median {r['slice']['median_f0_hz']:.0f} Hz")
    json.dump(out, open(path, "w"), indent=1)
    print(f"WROTE {path} ({len(out)} takes)")


main()
