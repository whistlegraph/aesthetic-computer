# tryout.py — audition every version of a word she actually sang.
#
# @jeffrey: "can we try using other takes for the lead too / go back to
# vocal tests with that? · especially i wanna try out new iterations of
# 'patiently'".
#
# bin/takes.py says WHICH takes hold a word and at what pitch; this one
# renders them, so the choice is made by ear instead of from a table. Each
# candidate goes through the same WORLD chain the chart uses — snap to the
# A# minor grid in her own 237 Hz frame, nervox tremor on the held frames —
# and they are laid out one after another with a gap, loudest-first, so a
# swap can be auditioned before it is charted.
#
# Every variant here is HER, from a take she sang. Generating syllables she
# never sang (an ElevenLabs IVC of Camille, the way the Prutti dub kept
# Prutti's voice) is a different thing and wants her sign-off first — the
# klokkentales gate: label synthetic voice, collab-invite is the consent.
#
#   pop/.venv/bin/python pop/loner/bin/tryout.py patiently
#   → out/tryout-patiently.wav  + a printed legend

import json, os, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
from audit import columns, TONIC, FRAME_S
from nervox import waver as nervox_waver, flange as nervox_flange

MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
SNAP = 0.92
GAP_S = 0.45


def cents_to_grid(hz):
    cents = 1200.0 * np.log2(hz / TONIC)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([MINOR * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def render(x, fs, snap=SNAP, nervy=True):
    """The chart's chain, on one word."""
    f0r, t = pw.harvest(x, fs, f0_floor=140.0, f0_ceil=600.0, frame_period=5.0)
    f0 = pw.stonemask(x, f0r, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=140.0)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=140.0)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    v = f0 > 0
    corr = np.zeros_like(f0)
    if v.any():
        corr[v] = -cents_to_grid(f0[v]) * snap
    f0c = np.where(v, f0 * 2.0 ** (corr / 1200.0), 0.0)
    if nervy:
        f0c = nervox_waver(f0c, 0.005, voiced=v)
    y = pw.synthesize(f0c, sp, ap, fs, frame_period=5.0)
    return nervox_flange(y, fs) if nervy else y


def main():
    word = (sys.argv[1] if len(sys.argv) > 1 else "patiently").lower()
    idx_path = os.path.join(LANE, "samples", ".takes.json")
    if not os.path.exists(idx_path):
        print("run bin/takes.py first"); return
    index = json.load(open(idx_path))
    if word not in index:
        print(f"'{word}' is not in the bank — bin/takes.py lists what is"); return

    out, legend, sr = [], [], None
    for e in sorted(index[word], key=lambda e: (e["take"], e["slice"])):
        wav = os.path.join(LANE, "samples", f"{e['slice']}.wav")
        x, fs = sf.read(wav, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        sr = fs
        seg = x[int(e["start"] * fs):int(e["end"] * fs)]
        if len(seg) < int(0.05 * fs):
            continue
        for nervy in (False, True):
            y = render(seg, fs, nervy=nervy)
            pk = np.max(np.abs(y)) or 1.0
            out.append(y / pk * 0.72)
            out.append(np.zeros(int(GAP_S * fs)))
            legend.append(f"{e['take']}-take {e['slice']:<20} {e['note']:>4} "
                          f"{e['st']:+5.2f}st  {e['dur']:.2f}s  "
                          f"{'nervox' if nervy else 'plain '}")

    if not out:
        print("nothing renderable"); return
    y = np.concatenate(out)
    dest = os.path.join(LANE, "out", f"tryout-{word}.wav")
    sf.write(dest, y, sr)
    print(f"'{word}' — {len(legend)} variants, in order:\n")
    at = 0.0
    for i, l in enumerate(legend):
        print(f"  {at:5.1f}s  {i + 1}. {l}")
        at += len(out[i * 2]) / sr + GAP_S
    print(f"\nWROTE {dest}  ({len(y) / sr:.1f}s)")


if __name__ == "__main__":
    main()
