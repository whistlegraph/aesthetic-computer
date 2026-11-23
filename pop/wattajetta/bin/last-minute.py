"""The last minute: a grand piano, and the guns cut short and played as percussion.

@jeffrey 2026-11-22: "can we add grand piano using natural grand piano in the last 60 seconds",
"rethink the sample cut / grooving there", "if we have some guns now lets bring those in".

The piano is the AC grand (Salamander V3, fedac/native/samples/piano), a note at a time, as
render-wattajetta-cutie.mjs bakes it for the machine row. It reads the harmony from the pads.
From `--from` bars it comes in on rolled chords, plays broken chords over a left hand through the
two finale rows, and thins to rolled chords that ring through the outro.

The guns (assets/guns, bin/fetch-guns.mjs) are cut at their attack and kept short so they sit on
the grid like drum hits. They tell one story: loaded in the row before the finale, dry clicks on
the last sixteenth of each beat in the first finale row, shots on two and four with racks and a
burst for fills in the second, and one shot left to ring where the outro begins.

  last-minute.py <liner.score.txt> <new dir> [--from 53] [--piano .4] [--guns 1] [--guns-dir assets/guns]
                 [--simple] [--tail 5]
Bars count from the top of the score given (before soft-open.py cuts the opening).
"""
import argparse
from pathlib import Path
import shutil
import subprocess

import numpy as np

HERE = Path(__file__).resolve().parent
GRAND = HERE.parents[2] / 'fedac/native/samples/piano'


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--from', dest='start', type=int, default=53, help='the bar the piano enters')
    p.add_argument('--piano', type=float, default=.4)
    p.add_argument('--guns', type=float, default=1, help='the guns\' level, against the written one (0: none)')
    p.add_argument('--space', type=float, default=1,
                   help='the bass in the row the piano enters on, against now (.3: the drop '
                        'stays a space until the finale brings it up)')
    p.add_argument('--fractal', type=int, default=0, help='octave-up, double-speed copies of the piano\'s entrance (3)')
    p.add_argument('--critique', action='store_true',
                   help='the 2026-11-22 critique: the left hand rides an octave over the bass and never under it, '
                        'the right hand is an isorhythm (the hook\'s rhythm, a six-note walk), shots on four only, '
                        'the clicks Euclidean E(5,16), the fractal layers out of phase')
    p.add_argument('--riser', type=float, default=1, help='every long riser, against now (.15)')
    p.add_argument('--simple', action='store_true', help='thin the synths from the finale on')
    p.add_argument('--tail', type=float, default=0, help='seconds added at the end, with a last chord, to end in')
    p.add_argument('--guns-dir', type=Path, default=HERE.parent / 'assets/guns')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    bar = 4 * beat
    end = int(next(r[1] for r in rows if r[0] == 'frames')) / sr
    ids = {}
    next_id = [max(int(r[1]) for r in rows if r[0] == 'smp') + 1]
    rng = np.random.default_rng(22)

    def sample(key, make):
        if key not in ids:
            f32 = out / f'{key}.f32'
            make().astype('<f4').tofile(f32)
            ids[key] = next_id[0]
            rows.append(['smp', str(next_id[0]), str(f32)])
            next_id[0] += 1
        return ids[key]

    def play(key, make, t, hold, gain, pan=0., wet=.1, echo=0.):
        if 0 <= t < end - .2:
            rows.append(['sung', str(sample(key, make)), str(t), str(hold), str(gain), str(pan), '0', str(wet), str(echo)])

    # ── the grand ────────────────────────────────────────────────────────
    def grand(midi, dur):
        anchor = min(96, max(21, 21 + 3 * round((midi - 21) / 3)))
        raw = np.fromfile(GRAND / f'{anchor}.raw', '<i2').reshape(-1, 2).sum(1) / 65536
        rate = 2 ** ((midi - anchor) / 12)
        n = min(int((len(raw) - 2) / rate), int((dur + .9) * sr))
        x = np.arange(n) * rate
        j = x.astype(int)
        y = raw[j] * (1 - (x - j)) + raw[j + 1] * (x - j)
        t = np.arange(n) / sr
        y = y * np.where(t < dur, 1, np.exp(-(t - dur) / .22))
        if n < int((dur + .9) * sr):         # the sample ran out before the note did: let it go, not stop dead
            k = min(n, int(.25 * sr))
            y[-k:] *= np.linspace(1, 0, k) ** 2
        return y / (np.abs(y).max() + 1e-9)

    def note(t, midi, beats, vel, pan=0.):
        ms = round(beats * beat * 1000)
        vel = float(np.clip(vel + rng.normal(0, .03), .12, 1))
        play(f'grand-{midi}-{ms}', lambda: grand(midi, ms / 1000), t + rng.normal(0, .005), ms / 1000 + .8,
             args.piano * vel, pan, wet=.1)

    pads = {}
    for r in rows:
        if r[0] == 'pad':
            pads.setdefault(round(float(r[1]) / beat), []).append(round(float(r[3])))

    def chord(b):
        at = [k for k in pads if k <= b * 4 + .2]
        return sorted(set(pads[max(at)])) if at and max(at) >= args.start * 4 else [52, 55, 59, 62]

    basses = sorted((float(r[1]), float(r[2]), round(float(r[3]))) for r in rows if r[0] == 'bass')

    def bass_under(b, otherwise):
        """The bass note sounding on bar b's downbeat: struck there, or still held from before."""
        at = b * bar + .05
        held = [m for t, d, m in basses if t <= at < t + d + .1]
        return held[-1] if held else otherwise

    def hands(b):
        tones = chord(b)
        root = tones[0] - 12 if tones[0] - 12 >= 28 else tones[0]
        right = sorted({m + 12 * int(np.ceil((57 - m) / 12)) if m < 57 else m - 12 * int(np.ceil((m - 76) / 12)) if m > 76 else m
                        for m in tones})
        return root, right

    def rolled(t, right, beats, vel, root=None):
        if root is not None:
            note(t, root, beats, vel + .1, -.2)
        for k, m in enumerate(right):
            note(t + .034 * (k + 1), m, beats, vel + .03 * k, -.05 + .12 * k)

    last = int(end / bar)
    finale, outro = args.start + 8, args.start + 24
    for b in range(args.start, last):
        t0 = b * bar
        root, right = hands(b)
        if b < finale:                       # coming in: a rolled chord every two bars, each a little nearer
            # (it came in on four-note chords held seven beats, loud in an empty row and cut dead where the
            # three-second samples ran out: @jeffrey heard "a big car horn". Now one quiet note every two beats.)
            k = b - args.start
            walk = right + right[-2:0:-1]
            for q in (0, 1):
                note(t0 + 2 * q * beat, walk[(2 * k + q) % len(walk)], 1.6, .2 + .02 * k, -.1 + .2 * q)
            # fractal (@jeffrey: "we could fractalize it, make it cuter there"): the same walk again at twice,
            # four and eight times the speed, each an octave higher, smaller and shorter, entering two bars
            # apart, so the little figure is made of itself
            for level in range(1, 1 + args.fractal):
                if k < 2 * level:
                    continue
                per = 2 ** level                      # notes a half bar
                shift = 2 * level if args.critique else 0     # each layer enters further into the walk
                for i in range(2 * per):
                    m = walk[((2 * k) * per + i + shift) % len(walk)] + 12 * level
                    if m <= 96:
                        note(t0 + i * 2 * beat / per, m, max(.3, 1.6 / per), (.2 + .015 * k) * (.9 if args.critique else .72) ** level,
                             (.25 + .15 * level) * (-1) ** (i + level))
            if b == finale - 1:              # and a walk up into the finale
                for q, m in enumerate((59, 62, 64, 68)):
                    note(t0 + (2 + q / 2) * beat, m, .9, .45 + .07 * q, -.1 + .1 * q)
        elif b < outro:                      # the two finale rows: a left hand, and the chord broken in eighths
            second = b >= finale + 8
            lift = (b - finale) / 16
            if args.critique:
                # left hand: the bass's own note an octave up (the sine bass keeps the bottom and its line)
                under = bass_under(b, root)
                left = under + 12 if under < 45 else under
                note(t0, left, 3.6, .62 + .12 * lift, -.22)
                note(t0 + 2 * beat, left + 7, 1.8, .42 + .1 * lift, -.18)
                # right hand: talea = the hook's rhythm over two bars, color = a six-note walk through the chord;
                # eight against six, so the two only meet again after six bars. No leading tone over a D# bass.
                walk6 = [m for m in (right + right[-2:0:-1] + right)[:6] if not (under % 12 == 3 and m % 12 == 3)]
                talea = (0, 6, 8, 13) if (b - finale) % 2 == 0 else (0, 6, 8, 12)
                for q, six in enumerate(talea):
                    i = (b - finale) * 4 + q
                    m = walk6[i % len(walk6)] + (12 if second and q == 0 and walk6[i % len(walk6)] + 12 <= 84 else 0)
                    note(t0 + six / 4 * beat, m, 1.6, .46 + .16 * lift + (.06 if q == 0 else 0), .05 + .12 * (i % len(walk6)) / len(walk6))
                continue
            note(t0, root, 3.6, .7 + .15 * lift, -.22)
            if second and root - 12 >= 28:
                note(t0, root - 12, 3.6, .6, -.25)
            note(t0 + 2 * beat, root + 7, 1.8, .48 + .1 * lift, -.18)
            order = (0, 1, 2, 1, 3, 2, 1, 2) if b % 2 == 0 else (0, 2, 1, 3, 2, 1, 2, 1)
            for q, ix in enumerate(order):
                m = right[ix % len(right)] + (12 if second and q in (0, 5) and right[ix % len(right)] + 12 <= 84 else 0)
                note(t0 + q / 2 * beat, m, 1.3, .4 + .18 * lift + (.08 if q in (0, 4) else 0), .05 + .1 * (ix % len(right)) / len(right))
        else:                                # the outro and the mist: chords left to ring, further apart
            k = b - outro
            if k < 4:
                rolled(t0, right, 3.8, .48 - .03 * k, root)
            elif k % 2 == 0:
                rolled(t0, right, 7.5, max(.2, .36 - .03 * (k - 4)), root)

    # ── the space after the drop ─────────────────────────────────────────
    # (@jeffrey: "i like the drop into the space but … we dont need all that super blasting synth, its too
    # insane … we can bring it up around 1:24"). In the row before the finale the pumping bass keeps only its
    # low note, far quieter.
    if args.space != 1:
        a0, a1 = args.start * bar - .04, (args.start + 8) * bar - .04

        def spare(r):
            if r[0] == 'bass' and a0 <= float(r[1]) < a1:
                if float(r[3]) >= 48:
                    return False
                r[4] = str(float(r[4]) * args.space)
            return True
        rows[:] = [r for r in rows if spare(r)]

    # ── the risers ───────────────────────────────────────────────────────
    # The long risers are noise rung through three tuned resonators, swelling to their loudest as they end:
    # a held chord that reads as a car horn (@jeffrey: "the stuff that sounds like a car horn"). All of them
    # come down; the short one at the very end is left.
    if args.riser != 1:
        for r in rows:
            if r[0] == 'riser' and float(r[2]) > 1:
                r[3] = str(float(r[3]) * args.riser)

    # ── the synths step aside ────────────────────────────────────────────
    # (@jeffrey: "making the synths at the end a bit simpler … would not powerload the mix there")
    # From the finale on: no plucks or acid chords, the lead only where it lands on a beat, the acid lower.
    if args.simple:
        t1 = finale * bar - .04

        def stays(r):
            if r[0] in ('pluck', 'acidchord') and float(r[1]) >= t1:
                return False
            if r[0] == 'lead' and float(r[1]) >= t1:
                on = float(r[1]) / beat
                if abs(on - round(on)) > .06:
                    return False
                r[5] = str(float(r[5]) * .8)
            if r[0] == 'acidrow' and float(r[1]) >= t1:
                r[2] = str(float(r[2]) * .6)
            return True
        rows[:] = [r for r in rows if stays(r)]

    # ── the ending ───────────────────────────────────────────────────────
    # The record used to stop while it was still sounding. One last chord, and `--tail` seconds for it and the
    # room to die away in before the master's own fade.
    if args.tail:
        final = int(end / bar) * bar
        root, right = hands(int(end / bar) - 1)
        for r in rows:
            if r[0] == 'frames':
                r[1] = str(int(r[1]) + round(args.tail * sr))
        end += args.tail
        rolled(final, right, 10, .34, root)

    # ── the guns ─────────────────────────────────────────────────────────
    def gun(name, seconds, semis=0, fade=.03):
        def make():
            src = next(iter(sorted(args.guns_dir.glob(name + '.*'))), None)
            if src is None:
                raise FileNotFoundError(name)
            raw = subprocess.check_output(['ffmpeg', '-v', 'error', '-i', str(src), '-ac', '1', '-ar', str(sr), '-f', 'f32le', '-'])
            x = np.frombuffer(raw, '<f4').astype(float)
            on = np.flatnonzero(np.abs(x) > .12 * np.abs(x).max())[0]
            x = x[max(0, on - round(.002 * sr)):]
            if semis:                        # played higher or lower, as a sampler would: pitch and length together
                at = np.arange(0, len(x) - 1, 2 ** (semis / 12))
                x = np.interp(at, np.arange(len(x)), x)
            x = x[:round(seconds * sr)].copy()
            n = min(len(x), round(fade * sr))
            x[-n:] *= np.linspace(1, 0, n) ** 2
            return x / (np.abs(x).max() + 1e-9)
        return f'gun-{name}-{round(seconds*1000)}-{semis}', make

    def hit(name, seconds, b, at, gain, pan=0., wet=.06, echo=0., semis=0):
        try:
            key, make = gun(name, seconds, semis)
            play(key, make, b * bar + at * beat, seconds, gain * args.guns, pan, wet, echo)
        except (FileNotFoundError, IndexError, subprocess.CalledProcessError):
            print(f'  no gun sample {name}')

    if args.guns:
        s = args.start
        # loading: the row before the finale
        hit('pistol-9mm-mag-insert-01', .5, s, 0, .5, -.3)
        hit('ak47-mag-detach-01', .5, s + 2, 2.5, .4, .35)
        hit('pistol-heavy-slide-rack-01', .45, s + 3, 3.5, .5, .2)
        hit('smg-9mm-bolt-lock-01', .4, s + 5, 3.5, .45, -.25)
        hit('shotgun-pump-rack-01', .55, s + 7, 3, .6, 0, .12)
        # the first finale row: dry clicks late in each beat, a rack every fourth bar
        euclid = (0, 3, 7, 10, 13)             # E(5,16) turned to miss the snare (4, 12) and its ghost (11)
        for b in range(finale, finale + 8):
            for q in (range(5) if args.critique else range(4)):
                hit('pistol-9mm-dry-fire-01', .09, b, euclid[q] / 4 if args.critique else q + .78, .26 + .05 * (q % 2),
                    -.4 if q % 2 else .4, .03, semis=(0, 3, 7, 12, 7, 3, 0, -5)[(b * 4 + q) % 8])
            if b % 2:
                hit('pistol-9mm-mag-insert-01', .3, b, 2.5, .3, .3)
            if (b - finale) % 4 == 3:
                hit('pistol-heavy-slide-rack-01', .4, b, 3.5, .5, -.2)
        # the second: shots on two and four, clicks still, racks and a burst to turn the phrase
        hit('shotgun-pump-shot-rack-01', 1.3, finale + 8, 0, .62, 0, .22)
        for b in range(finale + 8, finale + 16):
            for q in ((3,) if args.critique else (1, 3)):
                hit('m4-shot-01' if (q == 1 or b % 2) else 'pistol-9mm-shot-01', .15, b, q, .4, .12 if b % 2 else -.12, .1,
                    semis=(-5, 0, -3, 4)[(b % 2) * 2 + (q == 3)])
            for q in (range(5) if args.critique else range(4)):
                hit('pistol-9mm-dry-fire-01', .09, b, euclid[q] / 4 if args.critique else q + .78, .24, .4 if q % 2 else -.4, .03,
                    semis=(12, 7, 3, 0, 3, 7, 12, 15)[(b * 4 + q) % 8])
            if (b - finale) % 4 == 3:
                hit('smg-9mm-burst-01', beat * .95, b, 3, .42, .25, .08)
                hit('shotgun-pump-rack-01', .5, b, 2.5, .45, -.3)
        # brass on the floor after the shotgun and after each burst
        hit('casing-shotgun-shell-drop-01', .9, finale + 8, 2.6, .3, .3, .1)
        hit('casing-drop-02', .8, finale + 12, .3, .28, -.35, .1)
        hit('casing-drop-01', .9, finale + 16, 1.2, .3, .3, .15)
        # the outro begins on one shot, left to ring in the room
        hit('pistol-heavy-shot-03', 2.4, outro, 0, .7, 0, .5, .2)

    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in rows) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    print(f'piano from bar {args.start} ({args.start*bar:.1f} s) · {len(ids)} new samples')


if __name__ == '__main__':
    main()
