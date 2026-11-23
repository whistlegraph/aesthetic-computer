"""Print the six revised vocal passages into the original bell-break stereo master.

Use the weirdest score and the two WAV masters rendered with the same --accel value.
"""
import argparse
import math
from pathlib import Path

import numpy as np
import soundfile as sf


def windows(score, accel, full_line=False, stems=None):
    rows = [line.split() for line in score.read_text().splitlines() if line.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    frames = int(next(r[1] for r in rows if r[0] == 'frames'))
    bpm = float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    bar = 240 / bpm
    count = math.ceil(frames / (sr * bar)) + 1
    # The same piecewise-linear time map as c/run-cutie.mjs --accel.
    bars = np.arange(count + 1)
    u = np.clip((bars[:-1] - 8) / max(1, count - 24), 0, 1)
    edges = np.r_[0, np.cumsum(240 / (bpm + (accel - bpm) * u*u*(3 - 2*u)))]
    words = stems or (('lip-weirdest',) if full_line else ('weird', 'weirdest'))
    ids = {r[1] for r in rows if r[0] == 'smp' and
           any(Path(r[2]).stem == f'{r[1]}-{word}' for word in words)}
    cues = sorted((float(r[2]), float(r[2]) + float(r[3])) for r in rows if r[0] == 'sung' and r[1] in ids)
    if not cues:
        raise ValueError('No weirdest cut vocals in score')
    spans = []
    for start, end in cues:
        a, b = np.interp([start, end], bars * bar, edges)
        if spans and a - spans[-1][1] < 2:
            spans[-1][1] = max(spans[-1][1], b)
        else:
            spans.append([a, b])
    return sr, spans


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('score', 'original', 'revision', 'output'):
        p.add_argument(name, type=Path)
    p.add_argument('--accel', type=float, default=124)
    p.add_argument('--liner-opening', type=Path,
                   help='Restore the five-bar liner opening and punch only the full lead lines over the existing weird choir.')
    p.add_argument('--stems', nargs='+',
                   help='Punch the samples named <id>-<stem> instead (color-liner.py writes <id>-color).')
    args = p.parse_args()
    sr, spans = windows(args.score, args.accel, full_line=bool(args.liner_opening), stems=args.stems)
    original, rate = sf.read(args.original, dtype='int32')
    revised, new_rate = sf.read(args.revision, dtype='int32')
    if original.shape != revised.shape or rate != sr or new_rate != sr:
        raise ValueError('The two masters must have identical lengths, channels and rates')
    result = original.copy()
    n = round(.075 * sr)
    ramp = np.linspace(0, 1, n)[:, None]

    def seam(lo, hi, other=revised):
        # Pick 75 ms where the two performances agree, away from the sung consonants.
        def difference(i):
            a = original[i:i+n].astype(float)
            b = other[i:i+n].astype(float)
            return np.sum((a-b)**2) / max(1, np.sum(a*a) + np.sum(b*b))
        return min(range(max(0, round(lo*sr)), min(len(result)-n+1, round(hi*sr)), sr//100),
                   key=difference)

    if args.liner_opening:
        opening, intro_rate = sf.read(args.liner_opening, dtype='int32')
        if intro_rate != sr or opening.shape != original.shape:
            raise ValueError('The liner opening must come from the matching stereo master')
        first_lead = spans[0][0]
        intro_end = seam(first_lead - 3.0, first_lead - 1.5, opening)
        result[:intro_end] = opening[:intro_end]
        result[intro_end:intro_end+n] = opening[intro_end:intro_end+n]*(1-ramp) + original[intro_end:intro_end+n]*ramp
        print(f'Opening through {(intro_end+n)/sr:.3f} s')
    for start, end in spans:
        a = seam(start - .55, start - .08)
        b = seam(end + .65, end + 1.55)
        result[a:a+n] = original[a:a+n]*(1-ramp) + revised[a:a+n]*ramp
        result[a+n:b] = revised[a+n:b]
        result[b:b+n] = revised[b:b+n]*(1-ramp) + original[b:b+n]*ramp
        print(f'{a/sr:.3f}–{(b+n)/sr:.3f} s')
    with sf.SoundFile(args.output, 'x', samplerate=sr, channels=result.shape[1],
                      subtype=sf.info(args.original).subtype) as f:
        f.write(result)


if __name__ == '__main__':
    main()
