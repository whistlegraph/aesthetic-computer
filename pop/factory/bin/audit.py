# audit.py — the boundary police. THE AUDIO IS THE ONLY AUTHORITY.
#
# @jeffrey, hearing the loner study: "feels like 'time' has bits of the
# last word in it". He was right, and the cause was structural rather
# than a slip: we had been taking word boundaries on trust from a
# transcriber. Every transcriber we have tried has been wrong somewhere.
#
#   whisper.cpp ggml-small -ml 1   returns SUB-WORD TOKENS, not words.
#                                  On loner it cut "curled" into cur+led;
#                                  in THIS lane's own harvest.json take a
#                                  reads ' Spin'@13.85 'ning'@14.45 —
#                                  "spinning", split at its /n/. Every
#                                  label after a split slides by a
#                                  syllable, and nothing about the JSON
#                                  looks wrong.
#   OpenAI whisper-1               hears real words, but drifts at the
#                                  END of a take (0.6–1.4 s on loner) and
#                                  mishears the ones it cannot segment —
#                                  on chant-full it renders "factory" as
#                                  two words, "to read".
#
# So this script does not trust either. It finds the SUNG EVENTS in the
# audio itself — loud AND voiced runs, cut again wherever the de-spiked
# pitch steps to a new plateau and HOLDS there — and then asks, of every
# chart unit, whether its source span sits cleanly on top of them.
#
# THE FLAG THAT MATTERS is PARTIAL: a unit that slices an event, taking
# some of a neighbour's note into its own block. That is exactly the "for
# swallowing time" bug — the word carries a piece of the word beside it,
# and you hear it as a smeared consonant or a phantom syllable.
#
# A unit holding two or three WHOLE events is normal here and is reported,
# not flagged: this is a whistlegraph, and she flips an octave inside a
# single syllable all the time (factory ends on C#4, "from" drops D4→Bb2).
# Multi-event words are the material. Sliced events are the bug.
#
#   pop/.venv/bin/python pop/factory/bin/audit.py
#
# chart.py imports find_events()/despike() from here, so the build and the
# audit look at exactly the same events — one authority, not two.

import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
SAMPLES = os.path.join(LANE, "samples")

# HER D — 148.73 Hz. See chart.py's header for the derivation.
TONIC = 148.73
FRAME_S = 0.005
FLOOR = 90.0

# Event detection, tuned on chant-full and printed with its receipts:
STEP_ST = 1.7        # a pitch move this big may be a new note
PLATEAU_S = 0.090    # …measured as the median each side of the boundary
HOLD_ST = 0.6        # …and it must SETTLE: the new plateau holds this flat
MERGE_ST = 0.9       # two events this close in pitch are one note (vibrato)
MIN_EV_S = 0.10      # nothing shorter is an event
GATE_DB = -34.0      # of the take's peak
BRIDGE_S = 0.060     # a closure shorter than this does not end a run

CHROM = ["D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#"]


def note_name(hz):
    if not hz:
        return ""
    st = int(round(12.0 * np.log2(hz / TONIC)))
    return f"{CHROM[st % 12]}{3 + (st + 2) // 12}"


def despike(f0):
    """Fold octave-class tracking errors before anything reads the pitch.

    The lane already knows about these: v2's post-mortem found 1–3 per
    word at harvest's f0_floor of 100, and an octave jump inside a word is
    precisely what a naive splitter reads as a note change. A frame more
    than 6 semitones off its 45 ms median is a tracking error — fold it by
    whole octaves toward the median, and if it still does not fit, replace
    it with the median. Returns (f0, folds).
    """
    v = f0 > 0
    st = np.where(v, 12.0 * np.log2(np.maximum(f0, 1e-6) / TONIC), np.nan)
    W = int(round(0.045 / FRAME_S))
    med = np.full(len(f0), np.nan)
    for k in range(len(f0)):
        seg = st[max(0, k - W):k + W + 1]
        if np.isfinite(seg).any():
            med[k] = np.nanmedian(seg)
    fix, folds = st.copy(), 0
    for k in range(len(f0)):
        if not v[k] or not np.isfinite(med[k]):
            continue
        d = st[k] - med[k]
        for o in (12.0, -12.0, 24.0, -24.0):
            if abs(d - o) < abs(d):
                d, fix[k], folds = d - o, st[k] - o, folds + 1
        if abs(fix[k] - med[k]) > 6.0:
            fix[k] = med[k]
    return np.where(v, TONIC * 2.0 ** (fix / 12.0), 0.0), folds


def analyze(path):
    x, fs = sf.read(path, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    f0r, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=5.0)
    f0 = pw.stonemask(x, f0r, t, fs)
    f0d, folds = despike(f0)
    return x, fs, f0d, folds


def find_events(x, fs, f0d):
    """Sung events, in FRAMES, as (start, end) pairs.

    Two stages, and the second one exists because the first one lies. A
    sustained-pitch-step split alone returns 39 events for this take's 30
    syllables, because she sings with vibrato and a wide one and the
    splitter reads each swing as a new note. Requiring the new plateau to
    HOLD flat, then merging back any two neighbours within a semitone of
    each other, brings it to 34 — and the nine that remain above the
    syllable count are all real: they are the whistle flips.
    """
    n = int(round(fs * FRAME_S))
    m = min(len(f0d), len(x) // n)
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    on = (rms > (np.max(np.abs(x)) or 1.0) * 10.0 ** (GATE_DB / 20.0)) & (f0d[:m] > 0)
    runs, k = [], 0
    while k < m:
        if on[k]:
            j = k
            while j < m and on[j]:
                j += 1
            if (j - k) * FRAME_S >= 0.060:
                runs.append([k, j])
            k = j
        else:
            k += 1
    merged = []
    for r in runs:
        if merged and (r[0] - merged[-1][1]) * FRAME_S < BRIDGE_S:
            merged[-1][1] = r[1]
        else:
            merged.append(list(r))
    W = max(2, int(round(PLATEAU_S / FRAME_S)))
    out = []
    for a, b in merged:
        st = 12.0 * np.log2(np.maximum(f0d[a:b], 1e-6) / TONIC)
        L = b - a
        if L < int(0.20 / FRAME_S):
            out.append((a, b))
            continue
        md = np.array([np.median(st[max(0, i - W):min(L, i + W + 1)]) for i in range(L)])
        cuts, i = [], W
        while i < L - W:
            lo, hi = md[i - W], md[min(L - 1, i + W)]
            if abs(hi - lo) >= STEP_ST:
                after = md[min(L - 1, i + W):min(L, i + 2 * W)]
                if len(after) and np.max(np.abs(after - hi)) < HOLD_ST:
                    if not cuts or (i - cuts[-1]) * FRAME_S >= 0.14:
                        cuts.append(i)
                    i += int(0.10 / FRAME_S)
                    continue
            i += 1
        e = [0] + cuts + [L]
        segs = [(a + e[k], a + e[k + 1]) for k in range(len(e) - 1)
                if (e[k + 1] - e[k]) * FRAME_S >= MIN_EV_S]
        fin = []
        for s0, s1 in segs:
            if fin:
                p0, p1 = fin[-1]
                q, r = f0d[p0:p1], f0d[s0:s1]
                q, r = q[q > 0], r[r > 0]
                if len(q) and len(r) and \
                        abs(12.0 * np.log2(np.median(r) / np.median(q))) < MERGE_ST:
                    fin[-1] = (p0, s1)
                    continue
            fin.append((s0, s1))
        out += fin
    return out


def event_pitch(f0d, a, b):
    seg = f0d[a:b]
    seg = seg[seg > 0]
    return float(np.median(seg)) if len(seg) else 0.0


# ── the contamination check ───────────────────────────────────────────
PARTIAL_S = 0.040   # an event edge this far inside a unit is a real slice
BORROW_S = 0.010    # …and anything above THIS is still a neighbour's voice
#
# THE HOLE THAT WAS IN HERE. PARTIAL_S forgives a sliver of a neighbouring
# event so that a boundary landing a frame or two off does not cry wolf.
# But chart.py's attack pre-roll (ATTACK_S) is 30 ms — UNDER that
# tolerance — so a pre-roll that reached back into the previous word's
# VOICE instead of the silence in front of it would have been waved
# through by the very check meant to catch it. That is one of exactly two
# ways the loner lane shipped a contaminated boundary. A tolerance wider
# than the thing it is supposed to police is not a tolerance, it is a
# blind spot, so a forgiven sliver is now still counted and reported as a
# BORROW.


def check_units(x, fs, f0d, events, units, names):
    """For every unit, what of the audio does its SOURCE SPAN actually
    contain? Returns a row per unit and a list of flags.

    units are (start_frame, end_frame) on the slice's own clock.
    """
    n = int(round(fs * FRAME_S))
    m = min(len(f0d), len(x) // n)
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    peak = np.max(np.abs(x)) or 1.0
    gate = peak * 10.0 ** (GATE_DB / 20.0)
    rows, flags = [], []
    for u, (s0, s1) in enumerate(units):
        s0, s1 = max(0, s0), min(m, s1)
        whole, part, borrow = [], [], []
        for k, (e0, e1) in enumerate(events):
            lo, hi = max(s0, e0), min(s1, e1)
            if hi <= lo:
                continue
            inside = (hi - lo) * FRAME_S
            outside = ((e1 - e0) - (hi - lo)) * FRAME_S
            if outside <= PARTIAL_S:
                whole.append(k)              # the event is (all but) wholly in
            elif inside <= PARTIAL_S:
                # a sliver of a NEIGHBOUR'S event. Small enough that the
                # slice test forgives it — but it is still that word's
                # voice sitting inside this word's block.
                if inside > BORROW_S:
                    borrow.append((k, round(inside, 3)))
            else:
                part.append((k, round(inside, 3), round(outside, 3)))
        seg = rms[s0:s1]
        sung = float((seg > gate).mean()) if len(seg) else 0.0
        pk = (s0 + int(np.argmax(seg))) if len(seg) else s0
        rows.append(dict(unit=u, name=names[u],
                         start=round(s0 * FRAME_S, 3), end=round(s1 * FRAME_S, 3),
                         events=whole, partial=part, borrow=borrow,
                         sung=round(sung, 2),
                         peak_s=round((pk - s0) * FRAME_S, 3),
                         hz=round(event_pitch(f0d, s0, s1), 1)))
        if part:
            for (k, ins, out) in part:
                flags.append(f"{names[u]}: SLICES event {k} "
                             f"({ins:.2f}s in, {out:.2f}s left outside)")
        for (k, ins) in borrow:
            flags.append(f"{names[u]}: BORROWS {ins*1000:.0f} ms of event {k} "
                         f"— a neighbour's voice inside this word's block")
        if not whole and not part:
            flags.append(f"{names[u]}: NO EVENT in its span — silent unit")
        if sung < 0.35:
            flags.append(f"{names[u]}: only {sung*100:.0f}% of its span is above the gate")
    return rows, flags


def report(slice_name, units=None, names=None, verbose=True):
    """Audit one slice. With no units, just lists the events."""
    path = os.path.join(SAMPLES, f"{slice_name}.wav")
    x, fs, f0d, folds = analyze(path)
    ev = find_events(x, fs, f0d)
    if verbose:
        print(f"── {slice_name}: {len(ev)} sung events "
              f"({folds} octave folds repaired) ──")
        for k, (a, b) in enumerate(ev):
            hz = event_pitch(f0d, a, b)
            print(f"  ev{k:<3d} {a*FRAME_S:6.3f}–{b*FRAME_S:6.3f} "
                  f"({(b-a)*FRAME_S:.3f}s)  {hz:6.1f} Hz  {note_name(hz)}")
    if units is None:
        return ev, [], []
    rows, flags = check_units(x, fs, f0d, ev, units, names)
    if verbose:
        print(f"── {slice_name}: {len(units)} chart units ──")
        for r in rows:
            ev_s = ",".join(f"ev{k}" for k in r["events"]) or "—"
            pt = " ".join([f"⚠SLICES ev{k}" for (k, _, _) in r["partial"]]
                          + [f"⚠BORROWS {i*1000:.0f}ms of ev{k}"
                             for (k, i) in r["borrow"]])
            print(f"  {r['name']:>14s} {r['start']:6.3f}–{r['end']:6.3f}  "
                  f"{ev_s:<14s} sung {r['sung']*100:3.0f}%  "
                  f"peak +{r['peak_s']*1000:4.0f}ms  {note_name(r['hz'])} {pt}")
        if flags:
            print("  FLAGS:")
            for f in flags:
                print(f"    ⚠ {f}")
        else:
            print("  ✓ no contamination: every unit holds whole events only,"
                  " and no pre-roll borrows a neighbour's voice")
    return ev, rows, flags


if __name__ == "__main__":
    for name in (sys.argv[1:] or ["chant-full"]):
        report(name)
        print()
