"""Sing the colour lip-liner lines again on the liner arrangement, with the adjective at full length.

liner-cut.py made room for the long "line-er" by squeezing "put the reddest lip" to 64 % of its
time, and the "-est" was lost (read back as "red slip", "green slip"). Here each line is sung
again from its spoken takes: the words before "line" keep `--liner` beats (5 is the tune as
written), the adjective's last syllable is given `--est` seconds, and "line-er on" shares what
is left of the slot.

  color-liner.py <liner.score.txt> <jobs.json> <new dir> [--liner 5] [--est .34]
Writes <new dir>/liner.score.txt and the takes it names. Render it with c/run-cutie.mjs
--accel 124, then punch-weirdest.py --stems color.
"""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys

import numpy as np


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('score', 'jobs', 'out'):
        p.add_argument(name, type=Path)
    p.add_argument('--liner', type=float, default=5, help='the beat "line" lands on')
    p.add_argument('--est', type=float, default=.34, help='seconds for the adjective\'s last syllable')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [line.split() for line in args.score.read_text().splitlines()]
    spec = json.loads(args.jobs.read_text())
    jobs = {j['key']: j for j in spec['jobs']}
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    lyrics = json.loads((Path(__file__).resolve().parents[1] / 'lyrics/cutie.json').read_text())
    new_jobs, leads, lines = [], [], {}
    for row in rows:
        if row[0] != 'smp' or not Path(row[2]).stem.endswith('-liner'):
            continue
        k, src = row[1], Path(row[2])
        job = jobs[src.stem.removesuffix('-liner')]
        duration = src.stat().st_size / (4*sr)
        liner = args.liner * beat
        on = duration - .47
        head = [s * liner / job['starts'][-3] for s in job['starts'][:-3]]
        # head[-1] is "lip"; the syllable before it ends the adjective.
        head[-2] = min(head[-2], head[-1] - args.est)
        starts = head + [liner, (liner+on)/2, on]
        key = f'{k}-color'
        lines[job['line']] = lyrics[job['line']]
        new_jobs.append({**job, 'key': key, 'starts': starts,
                         'pin': job.get('pin') or spec.get('pin', {}).get(job['line'], ['cheeky']),
                         'end_hold': duration - on - .09})
        dest = out / (key + '.f32')
        leads.append((src, dest))
        row[2] = str(dest)
    jobfile = out / 'color-jobs.json'
    jobfile.write_text(json.dumps({'cache': str(out), 'sr': sr, 'lines': lines,
                                   'pin': {line: ['cheeky'] for line in lines}, 'jobs': new_jobs}))
    subprocess.run([sys.executable, str(Path(__file__).with_name('sing-lines.py')), str(jobfile)], check=True)
    for src, dest in leads:
        original = np.fromfile(src, '<f4')
        voice = np.fromfile(dest, '<f4')
        if not np.isfinite(voice).all() or not np.any(voice):
            raise ValueError(f'Invalid take {dest}')
        voice = np.pad(voice[:len(original)], (0, max(0, len(original)-len(voice))))
        voice *= np.sqrt(np.sum(original**2) / np.sum(voice**2))
        voice.tofile(dest)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(row) for row in rows) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')


if __name__ == '__main__':
    main()
