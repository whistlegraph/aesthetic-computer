"""Open on the lyrics, softly: keep the last bars of the opening and sing the first verse warm.

The liner single spends five bars before the first line. This keeps `--keep` of them (the bar
that leads into the verse), so the voice comes in about two seconds from the top, and sings
the first verse again (the lines in mode "angel") from the warm and gentle readings instead
of the mischievous ones, `--gain` times as loud.

  soft-open.py <liner.score.txt> <new dir> --jobs <jobs.json> [<jobs.json> ...]
               [--keep 1] [--gain .75] [--pin warm gentle] [--take lead=warm2]
               [--mode bedroom] [--wet .05] [--echo 0] [--hush .45]
               [--roll] [--bed pad spray tick] [--spoken lead=tender]
Writes <new dir>/liner.score.txt and the takes it names; render it with c/run-cutie.mjs --accel 124.
"""
import argparse
import json
from pathlib import Path
import subprocess
import sys

import numpy as np

AT = {'sung': 2, 'bell': 2, 'vox': 2}          # the column an event's time is in (1 otherwise)
FIXED = ('sr', 'frames', 'cfg', 'smp', 'material', 'under', '.')


def say(line, reading, sr):
    """The spoken take as it was said: its silence trimmed, the rumble out, the very top a little down."""
    from scipy.signal import butter, sosfilt
    vault = Path(__file__).resolve().parents[3] / 'aesthetic-computer-vault/voice/jeffrey-phonemes/phrases'
    raw = subprocess.check_output(['ffmpeg', '-v', 'error', '-i', str(vault / f'cutie-{line}-{reading}.mp3'),
                                   '-ac', '1', '-ar', str(sr), '-f', 'f32le', '-'])
    x = np.frombuffer(raw, '<f4').astype(float)
    loud = np.flatnonzero(np.abs(x) > .03 * np.abs(x).max())
    x = x[max(0, loud[0] - round(.02*sr)):loud[-1] + round(.15*sr)]
    x = sosfilt(butter(2, 80, 'highpass', fs=sr, output='sos'), x)
    x = x - .2 * sosfilt(butter(1, 6000, 'highpass', fs=sr, output='sos'), x)
    return (x / (np.abs(x).max() + 1e-9)).astype('<f4')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--jobs', type=Path, nargs='+', required=True)
    p.add_argument('--keep', type=int, default=1, help='bars kept before the first line')
    p.add_argument('--gain', type=float, default=.75)
    p.add_argument('--pin', nargs='+', default=['warm', 'gentle'])
    p.add_argument('--mode', help='sing the verse in this mode of sing-lines.py (bedroom)')
    p.add_argument('--wet', type=float, help='the verse\'s room send')
    p.add_argument('--echo', type=float, help='the verse\'s echo send')
    p.add_argument('--hush', type=float, default=1,
                   help='the band under the first verse, against now (drums, bass, acid, bells, bloops)')
    p.add_argument('--roll', action='store_true',
                   help='the band comes in one part at a time across the verse, up to --hush, and the '
                        'voice starts dry and reaches --wet and --echo by its last line')
    p.add_argument('--glide', type=float, help='seconds of slide into each note (sing-lines.py)')
    p.add_argument('--nat', type=float, help='how much of his own pitch movement stays on the note')
    p.add_argument('--vib', type=float)
    p.add_argument('--whistle', type=float, help='dB: a whistle over the verse, that far under the voice')
    p.add_argument('--from-bar', type=int,
                   help='start the record at this bar instead (21: the second verse); nothing is sung again, '
                        'and --hush is the band for the eight bars after')
    p.add_argument('--vox', type=float, default=1, help='the voices in the first eight bars, against now')
    p.add_argument('--acid', type=float,
                   help='bring the acid line back, at this much of its written level. Its steps carry absolute '
                        'times that liner-cut.py and short-nothing.py never moved, so every row has been silent '
                        'since the pocket single: here each row\'s steps are put back under their row')
    p.add_argument('--bed', nargs='+', default=[],
                   help='parts there from the first bar under --roll, at half of --hush and growing (pad spray tick)')
    p.add_argument('--spoken', nargs='+', default=[], metavar='LINE=READING',
                   help='a line said, not sung: the spoken take as it is (lead=tender)')
    p.add_argument('--take', nargs='+', default=[], metavar='LINE=READING',
                   help='one reading for a line, where the best-fitting one loses a word (lead=warm2)')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [line.split() for line in args.score.read_text().splitlines() if line.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    bar = 240 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    jobs, lines = {}, {}
    for f in args.jobs:
        spec = json.loads(f.read_text())
        jobs.update({j['key']: j for j in spec['jobs']})
        lines.update(spec['lines'])

    # the first verse, sung again
    verse = {r[1]: jobs[Path(r[2]).stem] for r in rows
             if r[0] == 'smp' and jobs.get(Path(r[2]).stem, {}).get('mode') == 'angel'}
    said = dict(t.split('=') for t in args.spoken)
    spoken = {k: said[job['line']] for k, job in verse.items() if job['line'] in said}
    if args.from_bar is not None:
        verse = {}
    own = dict(t.split('=') for t in args.take)
    new_jobs = [{**job, **({'mode': args.mode} if args.mode else {}), 'key': f'{k}-soft', 'pin': [own[job['line']]] if job['line'] in own else args.pin}
                for k, job in verse.items() if k not in spoken]
    jobfile = out / 'soft-jobs.json'
    jobfile.write_text(json.dumps({'cache': str(out), 'sr': sr, 'jobs': new_jobs,
                                   'lines': {j['line']: lines[j['line']] for j in new_jobs},
                                   'pin': {j['line']: args.pin for j in new_jobs},
                                   **{k: getattr(args, k) for k in ('glide', 'nat', 'vib', 'whistle')
                                      if getattr(args, k) is not None}}))
    if new_jobs:
        subprocess.run([sys.executable, str(Path(__file__).with_name('sing-lines.py')), str(jobfile)], check=True)
    order = [r[1] for r in sorted((r for r in rows if r[0] == 'sung' and r[1] in verse), key=lambda r: float(r[2]))]
    for row in rows:
        if row[0] == 'smp' and row[1] in verse:
            original = np.fromfile(row[2], '<f4')
            dest = out / f'{row[1]}-soft.f32'
            if row[1] in spoken:
                say(verse[row[1]]['line'], spoken[row[1]], sr).tofile(dest)
            voice = np.fromfile(dest, '<f4')
            if not np.isfinite(voice).all() or not np.any(voice):
                raise ValueError(f'Invalid take {dest}')
            voice = np.pad(voice[:len(original)], (0, max(0, len(original)-len(voice))))
            (voice * np.sqrt(np.sum(original**2) / np.sum(voice**2))).astype('<f4').tofile(dest)
            row[2] = str(dest)
        elif row[0] == 'sung' and row[1] in verse:
            row[4] = str(float(row[4]) * args.gain)
            far = order.index(row[1]) / max(1, len(order) - 1) if args.roll else 1
            if args.wet is not None:
                row[7] = str(args.wet * far)
            if args.echo is not None:
                row[8] = str(args.echo * far)

    # the opening, cut to its last bars
    first = min(float(r[2]) for r in rows if r[0] == 'sung')
    cut = (round(first / bar) - args.keep) * bar
    if args.from_bar is not None:
        cut = args.from_bar * bar
        first = cut
    kept, acid, stale = [], True, None
    for row in rows:
        if row[0] == '.' and not acid:       # the steps of an acid row that was cut go with it
            continue
        if row[0] == '.' and args.acid is not None:
            if stale is None:                # the first step tells how far the row's steps were left behind
                stale = round((float(row[1]) - at_row) / bar) * bar
            row[1] = str(float(row[1]) - stale - cut)
            kept.append(row)
            continue
        if row[0] == 'acidrow':
            acid = float(row[1]) >= cut - .04
            at_row, stale = float(row[1]), None
            if args.acid is not None:
                row[2] = str(float(row[2]) * args.acid)
        if row[0] in FIXED:
            if row[0] == 'frames':
                row[1] = str(int(row[1]) - round(cut*sr))
            kept.append(row)
            continue
        ti = AT.get(row[0], 1)
        t = float(row[ti])
        if t < cut - .04:
            # the held air that is still sounding at the cut comes in with the bar
            if row[0] == 'air' and t + float(row[2]) > cut + .5:
                row[2] = str(t + float(row[2]) - cut)
                t = cut
            else:
                continue
        row[ti] = str(max(0, t - cut))
        kept.append(row)
    # the band under the first verse steps back; the pad and the air stay
    level = {'kick': 2, 'snare': 2, 'spray': 8, 'tick': 3, 'bloop': 3, 'bass': 4, 'bell': 4,
             'pluck': 3, 'lead': 5, 'acidrow': 2, 'acidchord': 3}
    top = max(0, first - cut)
    quiet = top + 8*bar - .04
    # --roll: each part has a bar it enters on and grows from nothing to --hush by the end of the verse,
    # so no two arrive together: air, pad, bloops, bells, plucks, ticks, hats, bass, acid, kick, snare
    level.update(pad=4, air=4)
    enter = {'air': .5, 'pad': 1.25, 'bloop': 2, 'bell': 2.5, 'pluck': 3, 'lead': 3, 'tick': 3.5, 'spray': 4,
             'bass': 4.5, 'acidchord': 5, 'kick': 5.5, 'snare': 6.5}
    for row in kept:
        if row[0] in level and float(row[AT.get(row[0], 1)]) < quiet:
            amount = args.hush
            at = (float(row[AT.get(row[0], 1)]) - top) / bar
            if args.roll and row[0] in args.bed:
                amount *= .5 + .5 * float(np.clip(at / 8, 0, 1))
            elif args.roll and row[0] in enter:
                amount *= float(np.clip((at - enter[row[0]]) / (8 - enter[row[0]]), 0, 1)) ** .8
            elif row[0] in ('pad', 'air'):
                amount = 1
            if amount <= 0 and row[0] != 'acidrow':   # an acid row's steps follow it: it is never dropped here
                row[0] = None
            else:
                row[level[row[0]]] = str(float(row[level[row[0]]]) * amount)
    kept = [row for row in kept if row[0] is not None]
    for row in kept:
        if row[0] == 'sung' and float(row[2]) < quiet:
            row[4] = str(float(row[4]) * args.vox)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in kept) + '\n')
    width = json.loads((args.score.parent / 'wattajetta-cutie.width.json').read_text())
    (out / 'wattajetta-cutie.width.json').write_text(json.dumps(
        [[0, width[0][1]]] + [[t - cut, w] for t, w in width if t > cut]))
    print(f'{len(verse)} lines sung again · {cut:.3f} s cut · the first line at {first-cut:.2f} s')


if __name__ == '__main__':
    main()
