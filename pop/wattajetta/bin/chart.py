"""The critique's score fixes that are not the piano's or the guns': thirds, hooks, a higher hook.

From the 2026-11-22 critique (measured from the score; papers/composition-platter/bach digests):
- the old machine-row piano's G natural sounded against the choir's G sharp: those notes go;
- the plucked top line's G sharps sounded against the piano's and bells' G: they come down to G,
  so the turn to major happens once, at the finale;
- the title line was not heard for 49 seconds: it returns twice over the choir, and once,
  whispered, in the outro;
- the last two full statements get the line again an octave higher on top (one new take).

  chart.py <liner.score.txt> <jobs.json> <new dir> --hook <sample id> [--whisper <id>] [--lift <id> ...]
Bars count from the top of the score given (before soft-open.py cuts the opening).
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
    p.add_argument('--hook', required=True, help='the sample id of the title line to bring back')
    p.add_argument('--at', type=int, nargs='*', default=[45, 49], help='bars it returns on')
    p.add_argument('--whisper', help='the whispered title line\'s sample id')
    p.add_argument('--whisper-at', type=int, default=83)
    p.add_argument('--lift', nargs='*', default=[], help='sample ids of statements that get the octave on top')
    p.add_argument('--clash', type=int, nargs=2, default=[41, 52], help='bars where the old piano\'s G goes')
    p.add_argument('--minor', type=int, nargs=2, default=[53, 60], help='bars where plucked G sharps become G')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    bar = 240 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
    within = lambda t, span: span[0] * bar - .04 <= t < span[1] * bar - .04

    before = len(rows)
    rows = [r for r in rows if not (r[0] == 'sung' and names[r[1]].startswith('piano-55-') and within(float(r[2]), args.clash))]
    lowered = 0
    for r in rows:
        if r[0] == 'pluck' and within(float(r[1]), args.minor) and round(float(r[2])) % 12 == 8:
            r[2] = str(float(r[2]) - 1)
            lowered += 1

    first = {}
    for r in rows:
        if r[0] == 'sung':
            first.setdefault(r[1], r)
    new = []
    hook = first[args.hook]
    for b in args.at:
        new.append(['sung', args.hook, str(b * bar - .03), hook[3], str(float(hook[4]) * .7), '0', '0', '0.18', '0.04'])
    if args.whisper:
        w = first[args.whisper]
        new.append(['sung', args.whisper, str(args.whisper_at * bar - .03), w[3], str(float(w[4]) * .8), '0', '0', '0.25', '0.15'])

    if args.lift:
        job = next(j for j in json.loads(args.jobs.read_text())['jobs'] if j['line'] == 'lead' and j.get('mode') == 'angel')
        high = {**job, 'key': 'hook-high', 'midis': [m + 12 for m in job['midis']]}
        jobfile = out / 'high-jobs.json'
        jobfile.write_text(json.dumps({'cache': str(out), 'sr': sr, 'lines': {'lead': 'what a jet-ter cu-tie you are'},
                                       'jobs': [high]}))
        subprocess.run([sys.executable, str(Path(__file__).with_name('sing-lines.py')), str(jobfile)], check=True)
        take = np.fromfile(out / 'hook-high.f32', '<f4').astype(float)
        base = np.fromfile(next(r[2] for r in rows if r[0] == 'smp' and r[1] == args.lift[0]), '<f4').astype(float)
        (take * np.sqrt(np.mean(base**2) / np.mean(take**2))).astype('<f4').tofile(out / 'hook-high.f32')
        hid = str(max(int(k) for k in names) + 1)
        new.append(['smp', hid, str(out / 'hook-high.f32')])
        for sid in args.lift:
            r = first[sid]
            new.append(['sung', hid, r[2], str(len(take) / sr), str(float(r[4]) * .55), '0', '0', '0.2', '0.04'])

    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in rows + new) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    print(f'{before - len(rows)} piano Gs out · {lowered} plucks lowered · {len(args.at)} hooks back · {len(args.lift)} lifted')


if __name__ == '__main__':
    main()
