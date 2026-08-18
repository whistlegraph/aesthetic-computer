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
# This finds the sung EVENTS with no reference to any transcript — voiced,
# loud runs, split wherever the pitch plateau moves — and then asks of
# every charted unit: which events fall inside your span, and how much of
# each? A unit holding more than one event is carrying a neighbour's
# word. A unit holding a fraction of one is clipping it.
#
#   pop/.venv/bin/python pop/loner/bin/audit.py [phrase]

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
PITCH_STEP = 0.7         # semitones of sustained change that split an event


def events(x, fs):
    """Sung events, straight from the audio. No transcript involved."""
    f0r, t = pw.harvest(x, fs, f0_floor=70.0, f0_ceil=600.0, frame_period=FRAME_S * 1000)
    f0 = pw.stonemask(x, f0r, t, fs)
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    gate = (np.max(np.abs(x)) or 1.0) * 10.0 ** (GATE_DB / 20.0)
    on = rms > gate
    st = np.where(f0[:m] > 0, 12.0 * np.log2(np.maximum(f0[:m], 1e-6) / TONIC), np.nan)

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

    out = []                       # split each run where the pitch plateau moves
    W = int(round(0.080 / FRAME_S))
    for (a, b) in runs:
        cuts = [a]
        for k in range(a + W, b - W):
            lo, hi = st[k - W:k], st[k:k + W]
            lo, hi = lo[~np.isnan(lo)], hi[~np.isnan(hi)]
            if len(lo) >= W // 2 and len(hi) >= W // 2:
                if abs(np.median(hi) - np.median(lo)) > PITCH_STEP and k - cuts[-1] > int(0.12 / FRAME_S):
                    cuts.append(k)
        cuts.append(b)
        for u, v in zip(cuts[:-1], cuts[1:]):
            seg = st[u:v][~np.isnan(st[u:v])]
            out.append((u * FRAME_S, v * FRAME_S,
                        float(np.median(seg)) if len(seg) else float("nan")))
    return out


def main():
    phrase = sys.argv[1] if len(sys.argv) > 1 else "w-whole-line"
    man = json.load(open(os.path.join(LANE, "vox4", ".manifest.json")))[phrase]
    align = json.load(open(os.path.join(LANE, "samples", ".align.json")))
    chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))[phrase]
    slice_name = man["slice"]
    x, fs = sf.read(os.path.join(LANE, "samples", f"{slice_name}.wav"), dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    ev = events(x, fs)
    words = align[slice_name]["words"] if slice_name in align else []
    print(f"{phrase} · {slice_name} · {len(ev)} sung events, "
          f"{len(chart['notes'])} charted units\n")
    print("detected events (audio only):")
    for i, (a, b, s) in enumerate(ev):
        print(f"  {i:2d}  {a:6.2f}–{b:6.2f}s  {b - a:.2f}s  {s:+5.1f}st")

    # the spans the chart ACTUALLY plays — emitted by halo3 after times,
    # sylls, snapping, attack pre-roll and the trim. Checking the raw
    # alignment instead would flag boundaries we already corrected.
    spans = [(t, a, b) for (t, a, b) in man["spans"]]

    print("\nper word — which events fall in its span:")
    flags = 0
    for (t, a, b) in spans:
        inside = []
        for i, (ea, eb, es) in enumerate(ev):
            ov = max(0.0, min(b, eb) - max(a, ea))
            if ov > 0.02:
                inside.append((i, ov / (eb - ea), es))
        desc = " · ".join(f"#{i} {100 * f:.0f}%@{s:+.1f}st" for i, f, s in inside) or "—"
        bad = ""
        if len(inside) > 1:
            bad = "   <== TWO EVENTS: carrying a neighbour"
            flags += 1
        elif inside and inside[0][1] < 0.75:
            bad = "   <== PARTIAL: clipping this event"
            flags += 1
        elif not inside:
            bad = "   <== NO EVENT in this span"
            flags += 1
        print(f"  {t:>10} {a:6.2f}–{b:6.2f}s   {desc}{bad}")
    print(f"\n{flags} flagged of {len(spans)} words")


if __name__ == "__main__":
    main()
