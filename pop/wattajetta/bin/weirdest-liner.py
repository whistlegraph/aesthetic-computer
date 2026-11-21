"""Set the full weirdest lyric on the earlier liner arrangement, keeping the weird choir."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys

import numpy as np


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('score', 'jobs', 'choir_score', 'out'):
        p.add_argument(name, type=Path)
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [line.split() for line in args.score.read_text().splitlines()]
    choir = {r[1]: r[2] for line in args.choir_score.read_text().splitlines()
             if (r := line.split()) and r[0] == 'smp'}
    spec = json.loads(args.jobs.read_text())
    jobs = {j['key']: j for j in spec['jobs']}
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    line = 'lip-weirdest'
    lyric = json.loads((Path(__file__).resolve().parents[1] / 'lyrics/cutie.json').read_text())[line]
    new_jobs, leads = [], []
    for row in rows:
        if row[0] != 'smp':
            continue
        k, src = row[1], Path(row[2])
        if not src.stem.endswith('-liner'):
            if src.stem.startswith('lip-') and src.stem.endswith(('-legato', '-scream', '-right', '-left')):
                if Path(choir[k]).stem != f'{k}-weird':
                    raise ValueError(f'Missing weird choir voice {k}')
                row[2] = choir[k]
            continue
        job = jobs[src.stem.removesuffix('-liner')]
        starts = job['starts'].copy()
        notes = job['midis'].copy()
        if len(starts) == 9:  # The orange and yellow lines had a third adjective syllable.
            del starts[3], notes[3]
        if len(notes) != 8:
            raise ValueError(f'Unexpected phrase shape in {src.name}')
        duration = src.stat().st_size / (4*sr)
        # Give "weird-est" its two original notes, with the extra time on "line-er".
        liner = 4.6 * beat
        on = duration - .47
        starts = [s * liner / starts[-3] for s in starts[:-3]] + [liner, (liner+on)/2, on]
        key = f'{k}-{line}'
        new_jobs.append({**job, 'line': line, 'key': key, 'starts': starts, 'midis': notes,
                         'pin': job.get('pin') or spec['pin'][job['line']],
                         'end_hold': duration - on - .09})
        dest = out / (key + '.f32')
        leads.append((src, dest))
        row[2] = str(dest)
    jobfile = out / 'liner-jobs.json'
    jobfile.write_text(json.dumps({'cache': str(out), 'sr': sr, 'lines': {line: lyric},
                                   'pin': {line: ['cheeky']}, 'jobs': new_jobs}))
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
