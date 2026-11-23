"""A timeline of a baked cutie score: one lane a part, across the time you hear.

  timeline.py <liner.score.txt> <out.png> [--accel 124] [--audio master.wav --mp4 out.mp4] [--jobs jobs.json ...]
The x axis is heard time (after c/run-cutie.mjs --accel). With --audio and --mp4 the picture is
also written as a video: a playhead crosses it with the sound.
"""
import argparse
import json
from pathlib import Path
import subprocess

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

AT = {'sung': 2, 'bell': 2, 'vox': 2}
# kind → (lane, colour, column of length or seconds, column of level)
PARTS = {'kick': ('kick', '#e4572e', .12, 2), 'snare': ('snare', '#f3a712', .1, 2), 'spray': ('hats', '#a8c686', .05, 8),
         'tick': ('ticks', '#669bbc', .05, 3), 'bass': ('bass', '#8f2d56', 2, 4), 'pad': ('pad', '#3a7ca5', 2, 4),
         'air': ('air chords', '#81c3d7', 2, 4), 'pluck': ('plucks', '#ffbf69', .12, 3), 'lead': ('synth lead', '#ff5d8f', 2, 5),
         'bloop': ('bloops', '#06d6a0', .15, 3), 'bell': ('bells', '#ffd166', 3, 4), 'riser': ('riser', '#c77dff', 2, 3),
         'elastic': ('bomb', '#ff0054', 2, 3), 'flyby': ('flyby', '#bdb2ff', 2., None), 'acidchord': ('acid chord', '#9ef01a', 2, 3)}
VOICE = [('grand', 'grand piano', '#f8f9fa'), ('piano', 'grand piano', '#f8f9fa'), ('gun', 'guns', '#ff4d4d'),
         ('scream', 'screams', '#ff9e00'), ('ah-', 'ah / oo', '#90e0ef'), ('oo-', 'ah / oo', '#90e0ef'),
         ('nothing-word', 'small nothings', '#b8f2e6'), ('nothing-six', 'lead voice', '#ffffff'),
         ('lip', 'colour choir', '#f15bb5'), ('modem', 'colour choir', '#f15bb5'), ('superstar', 'ah / oo', '#90e0ef'),
         ('raw', 'ah / oo', '#90e0ef')]
ORDER = ['lead voice', 'colour choir', 'small nothings', 'ah / oo', 'screams', 'grand piano', 'guns', 'synth lead',
         'plucks', 'acid', 'acid chord', 'air chords', 'pad', 'bells', 'bloops', 'riser', 'bomb', 'flyby', 'bass', 'ticks',
         'hats', 'snare', 'kick']


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('png', type=Path)
    p.add_argument('--accel', type=float, default=124)
    p.add_argument('--audio', type=Path)
    p.add_argument('--mp4', type=Path)
    p.add_argument('--jobs', type=Path, nargs='*', default=[])
    p.add_argument('--title', default='wada jetter QD UR')
    args = p.parse_args()
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    bpm = float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    dur = int(next(r[1] for r in rows if r[0] == 'frames')) / sr
    bar = 240 / bpm
    bars = int(np.ceil(dur / bar)) + 1
    u = np.clip((np.arange(bars) - 8) / max(1, bars - 24), 0, 1)
    edges = np.r_[0, np.cumsum(bar * bpm / (bpm + (args.accel - bpm) * u*u*(3 - 2*u)))] if args.accel else np.arange(bars + 1) * bar
    heard = lambda t: float(np.interp(t, np.arange(bars + 1) * bar, edges))
    end = heard(dur)
    names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
    words = {}
    for f in args.jobs:
        spec = json.loads(f.read_text())
        words.update({j['key']: spec['lines'][j['line']].replace('-', '') for j in spec['jobs'] if j['line'] in spec['lines']})

    marks, labels = [], []
    acid = 0.
    for r in rows:
        if r[0] == 'acidrow':
            acid = float(r[2])
            continue
        if r[0] == '.':
            if acid > 0:
                t = float(r[1])
                marks.append(('acid', heard(t), max(.1, heard(t + float(r[6]) * bar / 16) - heard(t)),
                              acid * float(r[10]), '#c6f432'))
            continue
        if r[0] == 'sung':
            name = names[r[1]]
            lane, colour = next(((l, c) for k, l, c in VOICE if name.startswith(k)), ('lead voice', '#ffffff'))
            t, length, level = float(r[2]), float(r[3]), float(r[4])
            if lane == 'lead voice':
                labels.append((heard(t), words.get(name, 'nothing, nothing, nooothing' if 'six' in name else '')))
        elif r[0] in PARTS:
            lane, colour, length, col = PARTS[r[0]]
            t = float(r[AT.get(r[0], 1)])
            level = float(r[col]) if col else .6
            length = float(r[length]) if isinstance(length, int) else length
        else:
            continue
        marks.append((lane, heard(t), max(.12, heard(t + length) - heard(t)), level, colour))
    lanes = [l for l in ORDER if any(m[0] == l for m in marks)]
    top = {l: max(m[3] for m in marks if m[0] == l) for l in lanes}

    fig = plt.figure(figsize=(19.2, 10.8), dpi=100, facecolor='#101014')
    ax = fig.add_axes([.085, .06, .905, .86], facecolor='#101014')
    for lane, t, w, level, colour in marks:
        y = len(lanes) - 1 - lanes.index(lane)
        ax.add_patch(plt.Rectangle((t, y + .12), w, .76, color=colour, alpha=.25 + .75 * min(1, level / top[lane]), lw=0))
    seen = {}
    for t, text in sorted(labels):
        if text and seen.get(text, -9) < t - 1.5:
            seen[text] = t
            ax.text(t, len(lanes) + .08 + .42 * (len(seen) % 2), text, color='#ffffff', fontsize=6.5, va='bottom', family='Menlo')
    for b in range(0, bars, 8):
        ax.axvline(heard(b * bar), color='#ffffff', alpha=.22, lw=.7)
        ax.text(heard(b * bar) + .2, -.55, f'bar {b}', color='#888', fontsize=7, family='Menlo')
    ax.set_xlim(0, end); ax.set_ylim(-.7, len(lanes) + 1.1)
    ax.set_yticks(np.arange(len(lanes)) + .5); ax.set_yticklabels(lanes[::-1], color='#ddd', fontsize=10, family='Menlo')
    ticks = np.arange(0, end, 10)
    ax.set_xticks(ticks); ax.set_xticklabels([f'{int(t//60)}:{int(t%60):02d}' for t in ticks], color='#ddd', fontsize=9, family='Menlo')
    ax.tick_params(length=0)
    for s in ax.spines.values(): s.set_visible(False)
    fig.text(.085, .955, f'{args.title} · {int(end//60)}:{int(end%60):02d} · brighter is louder within a lane', color='#fff', fontsize=13, family='Menlo')
    fig.savefig(args.png, facecolor=fig.get_facecolor())
    print(args.png)
    if args.audio and args.mp4:
        x0, x1 = .085 * 1920, (.085 + .905) * 1920
        subprocess.run(['ffmpeg', '-y', '-loglevel', 'error', '-loop', '1', '-framerate', '30', '-i', str(args.png), '-i', str(args.audio),
                        '-filter_complex', f"color=c=white:s=3x1080:r=30[bar];[0:v][bar]overlay=x='{x0}+({x1}-{x0})*t/{end}':y=0:shortest=1,format=yuv420p[v]",
                        '-map', '[v]', '-map', '1:a', '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '20', '-c:a', 'aac', '-b:a', '256k',
                        '-t', f'{end:.2f}', str(args.mp4)], check=True)
        print(args.mp4)


if __name__ == '__main__':
    main()
