"""The acid line, played two other ways (@jeffrey 2026-11-22: "can we make the acid bass more slidey … and
cooler … more like grand piano playing … staccato").

  acid-voice.py <liner.score.txt> <new dir> --slide [--share .6] [--reso .86]
      more of a 303: about `share` of the notes slide into the next (the gate held over so the glide is
      legato), the filter's resonance up, the slid-into offbeats accented, and a note that ends a phrase
      (a gap of more than a beat after it) diving an octave as it lets go.
  acid-voice.py <liner.score.txt> <new dir> --piano [--gain .34] [--short .13]
      the same notes, rhythm and accents on the AC grand (Salamander, fedac/native/samples/piano),
      staccato: each note held `short` seconds and let go over 80 ms; the acid synth is left out.

Steps are `. t midi accent slide atk len fdec out whis lvl` (wattajetta-cutie.c), len in sixteenths.
"""
import argparse
from pathlib import Path
import shutil

import numpy as np

HERE = Path(__file__).resolve().parent
GRAND = HERE.parents[2] / 'fedac/native/samples/piano'
SR = 48000


def grand(midi, dur):
    anchor = min(96, max(21, 21 + 3 * round((midi - 21) / 3)))
    raw = np.fromfile(GRAND / f'{anchor}.raw', '<i2').reshape(-1, 2).sum(1) / 65536
    rate = 2 ** ((midi - anchor) / 12)
    n = min(int((len(raw) - 2) / rate), int((dur + .4) * SR))
    x = np.arange(n) * rate
    j = x.astype(int)
    y = raw[j] * (1 - (x - j)) + raw[j + 1] * (x - j)
    t = np.arange(n) / SR
    y = y * np.where(t < dur, 1, np.exp(-(t - dur) / .08))
    return (y / (np.abs(y).max() + 1e-9)).astype('<f4')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--slide', action='store_true')
    p.add_argument('--piano', action='store_true')
    p.add_argument('--share', type=float, default=.6)
    p.add_argument('--reso', type=float, default=.86)
    p.add_argument('--gain', type=float, default=.34)
    p.add_argument('--short', type=float, default=.13)
    a = p.parse_args()
    out = a.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in a.score.read_text().splitlines() if l.strip()]
    six = 15 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    blocks, kept, i = [], [], 0
    while i < len(rows):
        r = rows[i]
        if r[0] == 'acidrow':
            n = int(r[6])
            blocks.append((r, rows[i + 1:i + 1 + n]))
            kept.append(('acid', len(blocks) - 1))
            i += 1 + n
            continue
        kept.append(r)
        i += 1
    first_free = max(int(r[1]) for r in rows if r[0] == 'smp') + 1      # sample ids must stay under the engine's 4096
    rng = np.random.default_rng(303)
    made, slid, dives = {}, 0, 0
    extra = []
    for head, steps in blocks:
        steps.sort(key=lambda s: float(s[1]))
        level = float(head[2])
        for k, s in enumerate(steps):
            t, gap = float(s[1]), (float(steps[k + 1][1]) - float(s[1])) if k + 1 < len(steps) else 9
            if a.slide:
                if gap <= 4 * six + .01 and rng.random() < a.share:
                    s[4] = '1'; s[6] = f'{gap / six + .6:.2f}'; slid += 1
                    if k + 1 < len(steps) and round((float(steps[k + 1][1]) / six)) % 2:
                        steps[k + 1][3] = '1'
                if gap > 4 * six + .01:
                    s[8] = '-12'; dives += 1
            if a.piano:
                midi = round(float(s[2]))
                if midi not in made:
                    sid = str(first_free + len(made))
                    path = out / f'grand-staccato-{midi}.f32'
                    grand(midi, a.short).tofile(path)
                    made[midi] = sid
                    extra.append(['smp', sid, str(path)])
                vel = (1 if s[3] == '1' else .78) * min(1.4, .5 + level) * (.9 + .2 * rng.random())
                pan = max(-.5, min(.5, (midi - 52) / 30))
                extra.append(['sung', made[midi], f'{t:.5f}', f'{a.short + .1:.3f}', f'{a.gain * vel:.4f}', f'{pan:.3f}', '0', '0.1', '0'])
        if a.slide:
            head[5] = str(a.reso)
    lines = []
    for item in kept:
        if isinstance(item, tuple):
            if a.piano:
                continue
            head, steps = blocks[item[1]]
            lines += [head, *steps]
        else:
            lines.append(item)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in lines + extra) + '\n')
    shutil.copyfile(a.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    if a.slide:
        print(f'{slid} notes slide into the next · {dives} dive an octave at a phrase end · resonance {a.reso}')
    if a.piano:
        print(f'{sum(len(s) for _, s in blocks)} acid notes on the grand, staccato {a.short} s · {len(made)} pitches')


if __name__ == '__main__':
    main()
