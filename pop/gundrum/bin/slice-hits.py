"""Cut recordings into one-shots: each event from its attack to where it has died away.

  slice-hits.py <out dir> --max 1.5 --min .04 --gap .06 <file> [<file> …]
Finds attacks in a smoothed level envelope (a rise of 12 dB over the floor), cuts each event from
3 ms before it to where it falls 36 dB under its peak (or --max seconds), and keeps those at least
--min long. Writes <out dir>/<stem>-NN.f32 (mono float32, 48 kHz, peak 1) and <out dir>/hits.json
with each hit's file, source, length, peak position and spectral centroid.
"""
import argparse
import json
from pathlib import Path
import subprocess

import numpy as np

SR = 48000


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('out', type=Path)
    p.add_argument('files', nargs='+', type=Path)
    p.add_argument('--max', type=float, default=1.5)
    p.add_argument('--min', type=float, default=.04)
    p.add_argument('--gap', type=float, default=.06, help='shortest time between two attacks')
    p.add_argument('--per', type=int, default=8, help='most hits kept from one file (the loudest)')
    a = p.parse_args()
    a.out.mkdir(parents=True, exist_ok=True)
    index = []
    for f in a.files:
        raw = subprocess.check_output(['ffmpeg', '-v', 'error', '-i', str(f), '-ac', '1', '-ar', str(SR), '-f', 'f32le', '-'])
        x = np.frombuffer(raw, '<f4').astype(float)
        hop = round(.002 * SR)
        env = np.sqrt(np.convolve(x * x, np.ones(hop * 3) / (hop * 3), 'same'))[::hop] + 1e-9
        db = 20 * np.log10(env)
        floor = np.percentile(db, 20)
        hits, last = [], -1e9
        for i in range(1, len(db)):
            if db[i] > floor + 12 and db[i] - db[max(0, i - 5)] > 6 and (i - last) * hop / SR > a.gap:
                hits.append(i)
                last = i
        found = []
        for i in hits:
            s0 = max(0, i * hop - round(.003 * SR))
            seg = x[s0:s0 + round(a.max * SR)]
            e = np.abs(seg)
            pk = int(np.argmax(e[:round(.08 * SR)])) if len(e) else 0
            top = e[pk] if len(e) else 0
            if top <= 0:
                continue
            smooth = np.convolve(e, np.ones(240) / 240, 'same')
            quiet = np.flatnonzero(smooth[pk:] < top * 10 ** (-36 / 20))
            n = pk + (quiet[0] if len(quiet) else len(seg) - pk)
            seg = seg[:n].copy()
            if len(seg) < a.min * SR:
                continue
            fade = min(len(seg) // 4, round(.02 * SR))
            seg[-fade:] *= np.linspace(1, 0, fade)
            spec = np.abs(np.fft.rfft(seg * np.hanning(len(seg))))
            centroid = float(np.sum(np.fft.rfftfreq(len(seg), 1 / SR) * spec) / (np.sum(spec) + 1e-9))
            sm = np.convolve(np.abs(seg), np.ones(480) / 480, 'same')
            found.append((top, seg, centroid, int(np.argmax(sm)) / SR))
        found.sort(key=lambda h: -h[0])
        for k, (top, seg, centroid, peak_at) in enumerate(found[:a.per]):
            name = f'{f.stem}-{k:02d}.f32'
            (seg / np.abs(seg).max()).astype('<f4').tofile(a.out / name)
            index.append({'file': name, 'source': f.name, 'seconds': round(len(seg) / SR, 3), 'centroidHz': round(centroid), 'peakSeconds': round(peak_at, 3)})
    (a.out / 'hits.json').write_text(json.dumps(index, indent=1))
    by = {}
    for h in index:
        by.setdefault(h['source'], []).append(h['seconds'])
    for k, v in by.items():
        print(f'{k}: {len(v)} hits, {min(v):.2f}–{max(v):.2f} s')


if __name__ == '__main__':
    main()
