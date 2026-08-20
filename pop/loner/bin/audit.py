# audit.py — does every word's span hold ITS word, and only its word?
#
# @jeffrey: "pls pay attention to word boundaries vs actual spikes · like
# feels like 'time' has bits of the last word in it, etc · check that per
# word now in the lyrics".
#
# Every transcriber we have used has been wrong somewhere on this take.
# whisper.cpp at -ml 1 returned SUB-WORD tokens and slid the labels a
# syllable. whisper-1 drifted 0.6–1.4 s at the end of the line, which
# made each word's span swallow the head of the next one — that is what
# "for" containing "time" was. So the transcript is never the authority;
# the audio is.
#
# This finds the sung EVENTS with no reference to any transcript, and then
# asks of every charted unit: which events fall inside your span?
#
# THREE KINDS OF EVENT, because a word is not only its vowel. Splitting on
# pitch alone — which is all this did at first — cannot see a consonant: a
# fricative has no pitch, so the estimator hands back a garbage number and
# the /s/ of "self" merges into the vowel of "my". That read every CORRECT
# sibilant boundary in the take as contamination and buried the three real
# ones under sixteen flags. So each run is split where the pitch plateau
# moves OR where the brightness does, and classified:
#
#   NOTE   voiced, dark          — a sung syllable
#   FRIC   bright above 3 kHz    — /s/ /f/ /ʃ/, an onset or a coda
#   PUFF   unvoiced, dark        — a breath, a stop closure, a burst
#
# and the rule is the singer's, not the transcript's: a consonant belongs
# to the note it LEADS INTO. A unit may open with a fricative — that is
# its own onset — and may end with one when nothing follows it closely,
# which is a coda. A unit that ends with a fricative belonging to the next
# unit's note is carrying the next word's mouth, which is the bug that had
# "stone" entering as "-tone".
#
# ZOOM: given a time window, it stops summarising and prints the frames —
# level, pitch and brightness, one line per 5 ms — which is the view every
# boundary in this take has actually been pinned from. A fricative is DIM
# but BRIGHT (fd31ffd10), a stop closure is dim and DARK, and a note change
# is a step in the pitch column; those three shapes are the whole method.
#
#   pop/.venv/bin/python pop/loner/bin/audit.py [phrase]
#   pop/.venv/bin/python pop/loner/bin/audit.py [phrase] <from_s> <to_s>

import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
FRAME_S = 0.005
TONIC = 237.0
SPB = 60.0 / 122.0
GATE_DB = -34.0          # a run must clear this to be a sung event
MIN_EVENT_S = 0.070      # shorter than this is a transient, not a syllable
MIN_NOTE_S = 0.150       # a real sung syllable here; below it, a glide
MIN_CONS_S = 0.070       # below this, a click rather than a consonant
HELD_FRAC = 0.25         # a unit HOLDS an event at this share of it…
HELD_S = 0.100           # …or at this many seconds of it, whichever first
COVERED = 0.75           # an event played below this share is partly lost
PITCH_STEP = 0.7         # semitones of sustained change that split an event
BRIGHT_STEP = 0.45       # change in >3 kHz share that splits an event
BRIGHT = 0.50            # above this share of energy, the frame is a fricative
VOICED = 0.50            # below this fraction of pitched frames, it is unvoiced
ONSET_GAP_S = 0.200      # a fricative this close to the next note is its onset


def columns(x, fs):
    """f0, level and brightness per frame — the three columns every boundary
    in this take has been read from, and the only measurements here."""
    f0r, t = pw.harvest(x, fs, f0_floor=70.0, f0_ceil=600.0, frame_period=FRAME_S * 1000)
    f0 = pw.stonemask(x, f0r, t, fs)
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    fr = x[:m * n].reshape(m, n)
    rms = np.sqrt((fr ** 2).mean(axis=1))
    peak = np.max(np.abs(x)) or 1.0

    # the share of frame energy above 3 kHz. /s/ and /f/ run over 0.9 at
    # levels a pure level gate calls silence; a stop closure runs low in
    # BOTH columns, which is how the two are told apart.
    win = np.hanning(n)
    mag = np.abs(np.fft.rfft(fr * win, axis=1)) ** 2
    fk = np.fft.rfftfreq(n, 1.0 / fs)
    hi = mag[:, fk >= 3000.0].sum(axis=1) / np.maximum(mag.sum(axis=1), 1e-20)

    f0 = f0[:m]
    st = np.where(f0 > 0, 12.0 * np.log2(np.maximum(f0, 1e-6) / TONIC), np.nan)
    return f0, st, rms, hi, peak, m


def classify(st, hi, u, v):
    """NOTE, FRIC or PUFF — and the pitch, when it has one."""
    voiced = st[u:v][~np.isnan(st[u:v])]
    if float(np.median(hi[u:v])) >= BRIGHT:
        return "FRIC", float("nan")
    if len(voiced) < VOICED * (v - u):
        return "PUFF", float("nan")
    return "NOTE", float(np.median(voiced))


def events(x, fs):
    """Sung events, straight from the audio. No transcript involved."""
    f0, st, rms, hi, peak, m = columns(x, fs)
    on = rms > peak * 10.0 ** (GATE_DB / 20.0)

    runs, k = [], 0
    while k < m:
        if on[k]:
            j = k
            while j < m and on[j]:
                j += 1
            if (j - k) * FRAME_S >= MIN_EVENT_S:
                runs.append((k, j))
            k = j
        else:
            k += 1

    out = []              # split each run where pitch OR brightness steps
    W = int(round(0.080 / FRAME_S))
    for (a, b) in runs:
        cuts = [a]
        for k in range(a + W, b - W):
            if k - cuts[-1] <= int(0.12 / FRAME_S):
                continue
            plo, phi = st[k - W:k], st[k:k + W]
            plo, phi = plo[~np.isnan(plo)], phi[~np.isnan(phi)]
            moved = (len(plo) >= W // 2 and len(phi) >= W // 2
                     and abs(np.median(phi) - np.median(plo)) > PITCH_STEP)
            lit = abs(np.median(hi[k:k + W]) - np.median(hi[k - W:k])) > BRIGHT_STEP
            if moved or lit:
                cuts.append(k)
        cuts.append(b)
        for u, v in zip(cuts[:-1], cuts[1:]):
            kind, pitch = classify(st, hi, u, v)
            out.append((u * FRAME_S, v * FRAME_S, pitch, kind))
    return out


def zoom(x, fs, t0, t1, spans):
    """Every frame in a window, so a boundary can be read rather than guessed."""
    f0, st, rms, hi, peak, m = columns(x, fs)
    k0, k1 = max(0, int(t0 / FRAME_S)), min(m, int(t1 / FRAME_S) + 1)
    edges = {round(a, 3): f"{t} starts" for (t, a, b) in spans}
    for (t_, a, b) in spans:
        edges.setdefault(round(b, 3), f"{t_} ends")

    print(f"\nframes {t0:.2f}–{t1:.2f}s   "
          f"(level dB rel take peak · pitch st vs {TONIC:.0f} Hz · >3 kHz share)")
    for k in range(k0, k1):
        tt = k * FRAME_S
        db = 20.0 * np.log10(max(rms[k], 1e-9) / peak)
        bar = "#" * int(max(0, (db + 60.0)) / 3.0)
        mark = next((f"   <-- {lab}" for e, lab in edges.items()
                     if abs(e - tt) < FRAME_S / 2), "")
        pitch = f"{st[k]:+6.1f}st" if not np.isnan(st[k]) else "     ---"
        print(f"  {tt:6.3f}  {db:6.1f} {bar:<20} {pitch}  hf {hi[k]:.2f}{mark}")


def main():
    phrase = sys.argv[1] if len(sys.argv) > 1 else "w-whole-line"
    man = json.load(open(os.path.join(LANE, "vox4", ".manifest.json")))[phrase]
    chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))[phrase]
    slice_name = man["slice"]
    x, fs = sf.read(os.path.join(LANE, "samples", f"{slice_name}.wav"), dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    ev = events(x, fs)
    print(f"{phrase} · {slice_name} · {len(ev)} events, "
          f"{len(chart['notes'])} charted units\n")
    print("detected events (audio only):")
    for i, (a, b, s, kind) in enumerate(ev):
        pitch = f"{s:+5.1f}st" if not np.isnan(s) else ""
        print(f"  {i:2d}  {a:6.2f}–{b:6.2f}s  {b - a:.2f}s  {kind}  {pitch}")

    # the spans the chart ACTUALLY plays — emitted by halo3 after times,
    # sylls, snapping, attack pre-roll and the trim. Checking the raw
    # alignment instead would flag boundaries we already corrected.
    spans = [(t, a, b) for (t, a, b) in man["spans"]]

    # Which units hold which events, and how much. A boundary that splits
    # one event between two ADJACENT units is legato, not damage — she
    # slides from note to note and the cut has to land somewhere inside
    # the slide. What is damage is audio no unit plays at all.
    held = [[] for _ in ev]
    for si, (t, a, b) in enumerate(spans):
        for i, (ea, eb, es, kind) in enumerate(ev):
            ov = max(0.0, min(b, eb) - max(a, ea))
            if ov > 0.0:
                held[i].append((si, ov / (eb - ea), ov))

    def owns(i, si):
        """Does unit si hold enough of event i to be playing it?"""
        return any(s_ == si and (f >= HELD_FRAC or o >= HELD_S) for s_, f, o in held[i])

    print("\nper word — which events fall in its span:")
    flags = 0
    for si, (t, a, b) in enumerate(spans):
        mine = [i for i in range(len(ev)) if owns(i, si)]
        notes = [i for i in mine if ev[i][3] == "NOTE" and ev[i][1] - ev[i][0] >= MIN_NOTE_S]

        bad = ""
        if len(notes) > 1:
            bad = f"   <== TWO NOTES: carrying #{notes[0]} as well as #{notes[-1]}"
        elif mine:
            # a trailing consonant is this word's coda UNLESS a note starts
            # right after it — then it is that note's onset, and this unit
            # is holding the next word's mouth.
            last = ev[mine[-1]]
            if last[3] in ("FRIC", "PUFF") and last[1] - last[0] >= MIN_CONS_S:
                nxt = next((j for j, e in enumerate(ev)
                            if e[3] == "NOTE" and e[0] >= last[1] - 0.01), None)
                if nxt is not None and ev[nxt][0] - last[1] < ONSET_GAP_S and nxt not in mine:
                    bad = "   <== TRAILING CONSONANT: the next word's onset"
        if not bad:
            # anything of hers this unit starts but nobody finishes
            for i in mine:
                floor = MIN_NOTE_S if ev[i][3] == "NOTE" else MIN_CONS_S
                if ev[i][1] - ev[i][0] < floor:
                    continue
                cover = sum(f for _, f, _ in held[i])
                if cover < COVERED:
                    bad = f"   <== CLIPPED: only {100 * cover:.0f}% of {ev[i][3].lower()} #{i} is played"
                    break
        if not mine:
            bad = "   <== NO EVENT in this span"
        if bad:
            flags += 1

        desc = " · ".join(
            f"#{i} {100 * f:.0f}% {ev[i][3]}"
            + (f"@{ev[i][2]:+.1f}st" if not np.isnan(ev[i][2]) else "")
            for i in range(len(ev))
            for s_, f, o in held[i] if s_ == si and o > 0.02) or "—"
        print(f"  {t:>10} {a:6.2f}–{b:6.2f}s   {desc}{bad}")
    print(f"\n{flags} flagged of {len(spans)} words")

    if len(sys.argv) > 3:
        zoom(x, fs, float(sys.argv[2]), float(sys.argv[3]), spans)


if __name__ == "__main__":
    main()
