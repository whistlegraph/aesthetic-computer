"""Sing the leads again, slippery: slower slides, more of his own pitch, a flute in the whispers.

@jeffrey 2026-11-22: "some of jeffrey's vox sounds a bit BOTTY / quantized … i wish it was smoother
slippery like yogurt", "if the whisper parts sounded a bit flute like", and the last line is
"put the nicest money on". Every lead line in the score that a jobs file knows is sung again by
sing-lines.py with `--glide` seconds of slide into each note and `--nat` of the speaker's own
pitch movement left on it (the first verse, mode "angel", is left for soft-open.py). The whispered
lines get the flute. The line named by `--money` (a sample id) is sung to the new words.

  resing.py <liner.score.txt> <new dir> --jobs <jobs.json> [...] [--glide .11] [--nat .45]
            [--vib .05] [--flute .5] [--money 89] [--leave 81] [--words 50=lip-orange]
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
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--jobs', type=Path, nargs='+', required=True)
    p.add_argument('--glide', type=float, default=.11)
    p.add_argument('--nat', type=float, default=.45)
    p.add_argument('--vib', type=float, default=.05)
    p.add_argument('--flute', type=float, default=.5)
    p.add_argument('--whistle', type=float, help='dB: a whistle over every line, that far under the voice (-20)')
    p.add_argument('--money', help='the sample id of the last lip-liner line')
    p.add_argument('--money-take', nargs='+', default=['cheeky'],
                   help='its readings (cheeky is the one whose "money" is heard; kind and warm read as "movie", "one")')
    p.add_argument('--words', nargs='+', default=[], metavar='ID=LINE',
                   help='a line sung to another line\'s words (50=lip-orange): the syllable times come from a '
                        'job that already sings those words, the tune stays the line\'s own')
    p.add_argument('--pin', nargs='+', default=[], metavar='ID=READING',
                   help='one reading for a line, where the best-fitting one loses a word (48=warm2)')
    p.add_argument('--grid', nargs='+', default=[], metavar='ID',
                   help='lines whose syllables are put on the nearest sixteenth (color-liner.py scaled some off it)')
    p.add_argument('--mode', help='sing every line (but the whispers) in this mode: angel is the first verse\'s '
                                  'high sing-song voice, with its slides, vibrato, halo and whistle')
    p.add_argument('--wet', type=float, help='the room send of the lines sung again')
    p.add_argument('--echo', type=float, help='their echo send')
    p.add_argument('--plain', nargs='+', default=[], metavar='ID',
                   help='lines sung with no mode at all, as the shouted line after the whisper is')
    p.add_argument('--long', nargs='+', default=[], metavar='ID',
                   help='lines sung long and held (@jeffrey 2026-11-22: "after the aesthetic computer audio stamp those '
                        'utterances should be longly held and upper octave sung"): the syllables spread through the '
                        'slot by the square root of their lengths, the last one held over into the next line')
    p.add_argument('--up', type=int, default=0, help='semitones the --long lines are moved (12: an octave up)')
    p.add_argument('--stay', nargs='+', default=[], metavar='ID', help='--long lines that keep their octave (the money line: '
                   'an octave up, no reading of it is heard as "money")')
    p.add_argument('--shift', nargs='+', default=[], metavar='ID=SEMIS', help='--long lines moved by their own amount')
    p.add_argument('--fx-like', help='a sample id whose room and echo the --plain lines take (57: the line after the whisper)')
    p.add_argument('--leave', nargs='+', default=[], help='sample ids left as they are')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    step = 15 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    jobs, lines, pins = {}, {}, {}
    for f in args.jobs:
        spec = json.loads(f.read_text())
        jobs.update({j['key']: j for j in spec['jobs']})
        lines.update(spec['lines'])
        pins.update(spec.get('pin', {}))
    lyrics = json.loads((Path(__file__).resolve().parents[1] / 'lyrics/cutie.json').read_text())
    words = dict(w.split('=') for w in args.words)
    pinned = dict(w.split('=') for w in args.pin)
    shifts = {k: int(v) for k, v in (w.split('=') for w in args.shift)}
    new_jobs, again = [], []
    for row in rows:
        if row[0] != 'smp' or Path(row[2]).stem not in jobs:
            continue
        job = dict(jobs[Path(row[2]).stem])
        if job.get('mode') == 'angel' or row[1] in args.leave:
            continue
        job['pin'] = job.get('pin') or pins.get(job['line']) or ['cheeky']
        if args.mode and job.get('mode') != 'whisper':
            job['mode'] = args.mode
        if row[1] in args.plain:
            job.pop('mode', None)
        if row[1] in pinned:
            job['pin'] = [pinned[row[1]]]
        if row[1] in words:
            donor = next(j for j in jobs.values() if j['line'] == words[row[1]] and len(j['starts']) > 4)
            m = job['midis']
            extra = len(donor['starts']) - len(m)
            if extra not in (0, 1):
                raise ValueError(f'{words[row[1]]} does not fit the tune of sample {row[1]}')
            job.update(line=words[row[1]], starts=donor['starts'], midis=m[:3] + m[2:3] * extra + m[3:])
        if row[1] in args.grid:
            total = job['starts'][-1] + job['end_hold']
            snapped = []
            for t in job['starts']:
                q = round(t / step) * step
                snapped.append(q if not snapped or q > snapped[-1] else snapped[-1] + step)
            job.update(starts=snapped, end_hold=max(.2, total - snapped[-1]))
        if row[1] in args.long:
            s0, total = job['starts'], job['starts'][-1] + job['end_hold']
            lengths = [b - a for a, b in zip(s0, s0[1:] + [total])]
            weights = [x ** .5 for x in lengths]
            at, starts = 0., []
            for w in weights:
                starts.append(at)
                at += total * .9 * w / sum(weights)
            job.update(starts=starts, end_hold=total - starts[-1] + .6, midis=[m + (shifts[row[1]] if row[1] in shifts else 0 if row[1] in args.stay else args.up) for m in job['midis']])
        if row[1] == args.money:
            # "put the nic-est mon-ey on": "mon-" takes the time of "lip lin-", on the note of "lin-"
            s, m = job['starts'], job['midis']
            job.update(line='money', starts=s[:4] + s[4:5] + s[6:], midis=m[:4] + m[5:], pin=args.money_take)
        job['key'] = f'{row[1]}-smooth'
        lines[job['line']] = lyrics[job['line']]
        new_jobs.append(job)
        again.append((row, out / f'{row[1]}-smooth.f32'))
    jobfile = out / 'smooth-jobs.json'
    jobfile.write_text(json.dumps({'cache': str(out), 'sr': sr, 'lines': lines, 'jobs': new_jobs,
                                   'pin': {j['line']: j['pin'] for j in new_jobs},
                                   'glide': args.glide, 'nat': args.nat, 'vib': args.vib, 'flute': args.flute,
                                   **({'whistle': args.whistle} if args.whistle is not None else {})}))
    subprocess.run([sys.executable, str(Path(__file__).with_name('sing-lines.py')), str(jobfile)], check=True)
    for row, dest in again:
        original = np.fromfile(row[2], '<f4')
        voice = np.fromfile(dest, '<f4')
        if not np.isfinite(voice).all() or not np.any(voice):
            raise ValueError(f'Invalid take {dest}')
        voice = np.pad(voice[:len(original)], (0, max(0, len(original)-len(voice))))
        (voice * np.sqrt(np.sum(original**2) / np.sum(voice**2))).astype('<f4').tofile(dest)
        row[2] = str(dest)
    sung_again = {row[1] for row, _ in again}
    for r in rows:
        if r[0] == 'sung' and r[1] in sung_again:
            like = next((x for x in rows if x[0] == 'sung' and x[1] == args.fx_like), None) if args.fx_like else None
            if like and r[1] in args.plain:
                r[7], r[8] = like[7], like[8]
            else:
                if args.wet is not None:
                    r[7] = str(args.wet)
                if args.echo is not None:
                    r[8] = str(args.echo)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in rows) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    how = f'mode {args.mode}' if args.mode else f'slide {args.glide} s · own pitch {args.nat}'
    print(f'{len(again)} lines sung again · {how}')


if __name__ == '__main__':
    main()
