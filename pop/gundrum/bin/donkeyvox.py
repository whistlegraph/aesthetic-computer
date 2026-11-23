"""donkeyvox: a donkey's bray sung onto notes, the aesthetivox way.

A bray is a run of voiced "haw"s (and noisy "hee"s between). Each voiced run of the take is one
syllable: WORLD (pyworld) reads its pitch, spectral envelope and aperiodicity, the pitch is
replaced by the note (keeping `--nat` of the donkey's own movement around its median, with a
slow vibrato), and it is synthesised again, so the throat and breath stay the donkey's and the
pitch is the tune's. The unvoiced edges of each syllable are kept as they were.

  donkeyvox.py <take>[,<take> …] <out dir> --notes D4,A4,F4,E4,G4,F4,E4,D4 [--nat .2] [--vib 18] [--min .18]
              [--hold .5,.5,…]
Several takes are read one after another, so a long tune can use the syllables of more than one bray.
--hold gives each syllable a length in seconds: its voiced middle is stretched (or shortened) to fill
it, scanning back and forth through the vowel so a long note does not freeze into a buzz.
Writes <out dir>/syl-NN.f32 (mono float32, 48 kHz) and <out dir>/donkeyvox.json with each
syllable's note, length and the measured pitch error after tuning.
"""
import argparse
import json
from pathlib import Path
import subprocess
import warnings

import numpy as np
import pyworld as pw

warnings.filterwarnings('ignore')
SR, FP = 48000, 5.0
NOTE = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7, 'A': 9, 'B': 11}


def midi(name):
    n = NOTE[name[0]] + (1 if '#' in name else -1 if 'b' in name[1:] else 0)
    return n + 12 * (int(name.lstrip('ABCDEFG#b')) + 1)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('take')
    p.add_argument('out', type=Path)
    p.add_argument('--notes', required=True)
    p.add_argument('--nat', type=float, default=.2, help='how much of the donkey\'s own pitch movement stays')
    p.add_argument('--vib', type=float, default=18, help='vibrato depth in cents (5.2 Hz)')
    p.add_argument('--min', type=float, default=.18, help='shortest voiced run that counts as a syllable (s)')
    p.add_argument('--skip', default='', help='voiced runs (counted from 0 across the takes) to pass over')
    p.add_argument('--hold', help='seconds for each syllable, comma separated')
    a = p.parse_args()
    a.out.mkdir(parents=True, exist_ok=True)
    runs = []                                           # (signal, first voiced frame, end frame)
    for take in a.take.split(','):
        raw = subprocess.check_output(['ffmpeg', '-v', 'error', '-i', take, '-ac', '1', '-ar', str(SR), '-f', 'f64le', '-'])
        x = np.frombuffer(raw, '<f8').copy()
        f0, t = pw.harvest(x, SR, f0_floor=70, f0_ceil=1100, frame_period=FP)
        f0 = pw.stonemask(x, f0, t, SR)
        start = None
        for i, v in enumerate(np.r_[f0 > 0, False]):
            if v and start is None:
                start = i
            elif not v and start is not None:
                if (i - start) * FP / 1000 >= a.min:
                    runs.append((x, len(f0), start, i))
                start = None
    holds = [float(h) for h in a.hold.split(',')] if a.hold else None
    skip = {int(k) for k in a.skip.split(',') if k}
    runs = [r for i, r in enumerate(runs) if i not in skip]
    notes = [midi(n) for n in a.notes.split(',')]
    if len(runs) < len(notes):
        raise SystemExit(f'{a.take}: {len(runs)} voiced runs for {len(notes)} notes')
    report = []
    for k, (target, (x, frames, r0, r1)) in enumerate(zip(notes, runs)):
        # 60 ms of the unvoiced edge each side, less on a short note so it keeps its vowel
        pad = int(min(60, (holds[k] * 1000 * .12) if holds else 60) / FP)
        s0, s1 = max(0, r0 - pad), min(frames, r1 + pad)
        seg = x[int(s0 * FP / 1000 * SR):int(s1 * FP / 1000 * SR)]
        g0, gt = pw.harvest(seg, SR, f0_floor=70, f0_ceil=1100, frame_period=FP)
        g0 = pw.stonemask(seg, g0, gt, SR)
        sp, ap = pw.cheaptrick(seg, g0, gt, SR), pw.d4c(seg, g0, gt, SR)
        if holds:
            # stretch the voiced middle to the hold; the 60 ms edges stay as they were
            want = max(len(g0), round(holds[k] * 1000 / FP)) if holds[k] * 1000 / FP > len(g0) else round(holds[k] * 1000 / FP)
            a0, a1 = min(r0 - s0, len(g0) - 1), max(min(r1 - s0, len(g0)) - 1, min(r0 - s0, len(g0) - 1) + 1)
            middle = max(2, want - a0 - (len(g0) - a1))
            u = np.linspace(0, 1, middle)
            ratio = middle / max(1, a1 - a0)
            # a long hold scans forward and back through the vowel instead of freezing on one frame
            pos = a0 + (a1 - a0 - 1) * (u if ratio < 1.6 else .5 - .5 * np.cos(np.pi * u * max(1, ratio / 1.6)))
            idx = np.r_[np.arange(a0), np.round(pos).astype(int), np.arange(a1, len(g0))]
            g0, sp, ap = g0[idx], np.ascontiguousarray(sp[idx]), np.ascontiguousarray(ap[idx])
            seg = np.zeros(len(g0) * int(SR * FP / 1000))
        on = g0 > 0
        st = 12 * np.log2(np.where(on, g0, 1) / 440) + 69
        centre = np.median(st[on]) if on.any() else target
        tt = np.arange(len(g0)) * FP / 1000
        cents = a.vib * np.sin(2 * np.pi * 5.2 * tt) * np.clip((tt - .12) / .2, 0, 1) / 100
        new = np.where(on, 440 * 2 ** ((target + a.nat * np.clip(st - centre, -3, 3) + cents - 69) / 12), 0)
        y = pw.synthesize(np.ascontiguousarray(new), sp, ap, SR, FP)
        y = y[:len(seg)] if len(y) >= len(seg) else np.pad(y, (0, len(seg) - len(y)))
        # keep the recorded breath above 4 kHz and the unvoiced edges as they were
        n = round(.006 * SR)
        w = np.ones(len(y))
        w[:n] = np.linspace(0, 1, n); w[-n:] = np.linspace(1, 0, n)
        y = (y / (np.abs(y).max() + 1e-9) * w).astype('<f4')
        y.tofile(a.out / f'syl-{k:02d}.f32')
        h0, _ = pw.harvest(y.astype(float), SR, f0_floor=70, f0_ceil=1100, frame_period=FP)
        hv = h0[h0 > 0]
        err = float(np.median(1200 * np.log2(hv / (440 * 2 ** ((target - 69) / 12))))) if len(hv) else None
        report.append({'file': f'syl-{k:02d}.f32', 'midi': target, 'seconds': round(len(y) / SR, 3),
                       'voicedFrom': round((r0 - s0) * FP / 1000, 3), 'pitchErrorCents': None if err is None else round(err, 1)})
    (a.out / 'donkeyvox.json').write_text(json.dumps({'take': str(a.take), 'notes': a.notes, 'syllables': report}, indent=1))
    for r in report:
        print(r['file'], r['midi'], r['seconds'], 's', r['pitchErrorCents'], 'c')


if __name__ == '__main__':
    main()
