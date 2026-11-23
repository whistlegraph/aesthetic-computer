"""The two synth blasts, made into lines: an accordion arpeggio and a top line.

@jeffrey 2026-11-22: the synth blasts after the first NOTHING should "stretch and squash /
accordianize / 56k modemize arpeggiate a bit", and the ones at 1:10 are "way too much …
attenuate those / kill them or make them into a top line melody".

- `--air A B` (bars): the big held air chords there fall to a bed, and their notes are plucked
  one at a time up and down the chord. The steps squash from half a beat to an eighth of one and
  stretch back out, like a bellows; each chord opens on a short two-tone modem chatter.
- `--top A` (bar): in the eight bars from there the bass stabs are left out but one low note a
  bar, and the tune of the first two sung lines is plucked two octaves up, twice, a little
  nearer the second time.

  blasts.py <liner.score.txt> <jobs.json> <new dir> [--air 29 37] [--top 53] [--single .4]
Bars count from the top of the score given (before soft-open.py cuts the opening).
"""
import argparse
import json
from pathlib import Path
import shutil


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('score', 'jobs', 'out'):
        p.add_argument(name, type=Path)
    p.add_argument('--air', type=int, nargs=2)
    p.add_argument('--top', type=int)
    p.add_argument('--single', type=float,
                   help='less doubling of the words: where several voices sing a line together only the loudest '
                        'stays in the middle, and the ones at the sides are this much of what they were (.4)')
    p.add_argument('--no-screams', type=int, nargs=2, metavar=('A', 'B'),
                   help='leave the screamed syllables out between these bars')
    p.add_argument('--angel', nargs='+', default=[], metavar='ID',
                   help='sample ids of "what a jetter cutie you are" lines that go back to --angel-take')
    p.add_argument('--angel-take', type=Path,
                   help='the first verse\'s high, sing-song take of the line (mode "angel"), which @jeffrey '
                        'asked to keep for the line wherever it comes (2026-11-22: "lets stick to those")')
    p.add_argument('--take', nargs='+', default=[], metavar='ID=FILE',
                   help='a line replaced by a recording as it is (a screamed ElevenLabs take): trimmed, '
                        'set to the level of the line it replaces')
    p.add_argument('--bed', type=float, default=.3, help='what is left of the held chords')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    bar = 4 * beat
    new = []

    def pluck(t, midi, g, pan, decay):
        new.append(['pluck', str(t), str(midi), str(g), str(pan), str(decay)])

    if args.air:
        a0, a1 = args.air[0] * bar - .04, args.air[1] * bar - .04
        chords = {}
        for r in rows:
            if r[0] == 'air' and a0 <= float(r[1]) < a1:
                chords.setdefault(round(float(r[1]), 2), []).append((float(r[3]), float(r[2])))
                r[4] = str(float(r[4]) * args.bed)
        bellows = [1/2, 1/3, 1/4, 1/6, 1/8, 1/8, 1/8, 1/6, 1/4, 1/3]
        for t0, notes in sorted(chords.items()):
            tones = sorted({round(m) for m, _ in notes if m < 90})
            length = max(d for _, d in notes)
            ladder = tones + tones[-2:0:-1]
            for k in range(12):           # the modem: two tones, 22 a second, falling away
                pluck(t0 + k * .045, tones[-1] + (12 if k % 2 else 7), .16 * (1 - k / 14), .6 if k % 2 else -.6, .03)
            t, k = t0 + .6, 0
            while t < t0 + length - .1:
                step = bellows[k % len(bellows)] * beat
                tight = 1 - step / (beat / 2)          # 0 when wide, near 1 when squashed
                pluck(t, ladder[k % len(ladder)] + (12 if tight > .7 and k % 2 else 0), .34 - .1 * tight,
                      .5 * (-1) ** k * (.3 + .7 * tight), .07 + .22 * (1 - tight))
                t += step
                k += 1

    if args.top is not None:
        b0, b1 = args.top * bar - .04, (args.top + 8) * bar - .04
        downbeat = lambda t: abs(t / bar - round(t / bar)) < .03
        rows = [r for r in rows if not (r[0] == 'bass' and b0 <= float(r[1]) < b1 and not downbeat(float(r[1])))]
        jobs = json.loads(args.jobs.read_text())['jobs']
        tune = []
        for line, at in (('lead', 0), ('lip-yellow', 2 * bar)):
            job = next(j for j in jobs if j['line'] == line and j.get('mode') == 'clear')
            ends = job['starts'][1:] + [job['starts'][-1] + job['end_hold']]
            tune += [(at + s, m + 24, e - s) for s, e, m in zip(job['starts'], ends, job['midis'])]
        for again in range(2):
            for s, m, length in tune:
                pluck(args.top * bar + again * 4 * bar + s, m, .3 + .1 * again, .18 if again else -.18,
                      min(.5, .12 + .3 * length))

    if args.angel:
        if not args.angel_take:
            p.error('--angel needs --angel-take')
        import numpy as np
        sr = int(next(r[1] for r in rows if r[0] == 'sr'))
        take = np.fromfile(args.angel_take, '<f4').astype(float)
        for r in rows:
            if r[0] == 'smp' and r[1] in args.angel:
                was = np.fromfile(r[2], '<f4').astype(float)
                dest = out / f'{r[1]}-angel.f32'
                (take * np.sqrt(np.mean(was**2) / np.mean(take**2))).astype('<f4').tofile(dest)
                r[2] = str(dest)
            elif r[0] == 'sung' and r[1] in args.angel:
                r[3] = str(len(take) / sr)
                r[7], r[8] = '0.18', '0.04'      # the angel's room and echo
        print(f'{len(args.angel)} lines back on the angel take')

    if args.take:
        import subprocess
        import numpy as np
        sr = int(next(r[1] for r in rows if r[0] == 'sr'))
        takes = dict(t.split('=', 1) for t in args.take)
        length = {}
        for r in rows:
            if r[0] == 'smp' and r[1] in takes:
                raw = subprocess.check_output(['ffmpeg', '-v', 'error', '-i', takes[r[1]], '-ac', '1', '-ar', str(sr), '-f', 'f32le', '-'])
                x = np.frombuffer(raw, '<f4').astype(float)
                loud = np.flatnonzero(np.abs(x) > .03 * np.abs(x).max())
                x = x[max(0, loud[0] - round(.02*sr)):loud[-1] + round(.2*sr)]
                was = np.fromfile(r[2], '<f4').astype(float)
                x = x * np.sqrt(np.sum(was**2) / np.sum(x**2))
                dest = out / f'{r[1]}-take.f32'
                x.astype('<f4').tofile(dest)
                r[2] = str(dest)
                length[r[1]] = len(x) / sr
        for r in rows:
            if r[0] == 'sung' and r[1] in length:
                r[3] = str(length[r[1]])
                r[7], r[8] = '0.2', '0.1'
        print(f'{len(length)} lines replaced by recordings: ' + ', '.join(f'{k} {v:.2f} s' for k, v in length.items()))

    if args.no_screams:
        names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
        s0, s1 = args.no_screams[0] * bar - .04, args.no_screams[1] * bar - .04
        before = len(rows)
        rows = [r for r in rows if not (r[0] == 'sung' and names[r[1]].startswith('scream') and s0 <= float(r[2]) < s1)]
        print(f'{before - len(rows)} screamed syllables left out')

    if args.single is not None:
        import re
        names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
        line = re.compile(r'line-|\d+-(color|smooth|soft|angel|take)$|nothing-six-bars')
        voices = sorted((r for r in rows if r[0] == 'sung' and line.match(names[r[1]])), key=lambda r: float(r[2]))
        groups = []
        for r in voices:
            if groups and float(r[2]) - float(groups[-1][0][2]) < .1:
                groups[-1].append(r)
            else:
                groups.append([r])
        gone = set()
        for g in groups:
            main = max(g, key=lambda r: float(r[4]))
            for r in g:
                if r is main:
                    continue
                if abs(float(r[5])) > .5:
                    r[4] = str(float(r[4]) * args.single)
                else:
                    gone.add(id(r))
        rows = [r for r in rows if id(r) not in gone]
        print(f'{len(gone)} doubling voices left out, {sum(len(g) - 1 for g in groups) - len(gone)} set back')

    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in rows + new) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    print(f'{len(new)} plucked notes')


if __name__ == '__main__':
    main()
