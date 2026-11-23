"""Smooth the liner single: a drier room, softer bells, a colour choir that blends, more tape.

@jeffrey 2026-11-22: the high gongy overtones "can be toned down or waver", the colour harmonies
"all feel super smooth and gradient like", "the whole track is a tad too reverby", and the master
printed to tape "with a tiny bit of hiss".

- the room: reverb and delay returns come down, the tail is shorter.
- the bells: every glass and handbell sample loses `--bells` dB above 2.5 kHz, and what is left
  up there wavers slowly; each strike is a little quieter.
- the colours: the screamed colour is left out; each sung colour swells in and sits lower; the
  held harmonies come up over seconds instead of arriving.
- the tape: the material's amount goes up (more wow, saturation and hiss, less top).

  smooth.py <liner.score.txt> <new dir> [--room .65] [--bells 7] [--tape 1.3]
Writes <new dir>/liner.score.txt and the bell samples it names.
"""
import argparse
from pathlib import Path
import shutil

import numpy as np
from scipy.signal import butter, sosfiltfilt


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--room', type=float, default=.65, help='reverb and delay returns, against now')
    p.add_argument('--bells', type=float, default=7, help='dB taken from the bells above 2.5 kHz')
    p.add_argument('--tape', type=float, default=1.3)
    p.add_argument('--vox', type=float, default=1,
                   help='every voice, against now (.78: the vocals sit back in the mix, as in a pop record)')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    sr = int(next(r[1] for r in rows if r[0] == 'sr'))
    names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
    kept = []
    for r in rows:
        if r[0] == 'material':
            r[2] = str(args.tape)
        elif r[:2] == ['cfg', 'reverb.level'] or r[:2] == ['cfg', 'delay.level']:
            r[2] = str(float(r[2]) * args.room)
        elif r[:2] == ['cfg', 'reverb.feedback']:
            r[2] = str(float(r[2]) - .035)
        elif r[0] == 'smp' and ('-glass-glass-' in r[2] or '-stone-handbell-' in r[2]):
            # bell samples are planar stereo (the left half, then the right): each ear is filtered on its own
            x = np.fromfile(r[2], '<f4').astype(float).reshape(2, -1)
            high = sosfiltfilt(butter(2, 2500, 'highpass', fs=sr, output='sos'), x, axis=1)
            t = np.arange(x.shape[1]) / sr
            # each bell wavers at its own rate, so a chord of them shimmers instead of pulsing
            rate = 2.3 + (sum(map(ord, Path(r[2]).stem)) % 17) / 10
            waver = 1 + .35 * np.sin(2*np.pi*rate*t + rate)
            y = x - high + high * 10 ** (-args.bells/20) * waver
            dest = out / Path(r[2]).name
            y.ravel().astype('<f4').tofile(dest)
            r[2] = str(dest)
        elif r[0] == 'bell':
            r[4] = str(float(r[4]) * .85)
        elif r[0] == 'sung':
            name = names[r[1]]
            if not name.startswith('piano-'):
                r[4] = str(float(r[4]) * args.vox)
            if name.startswith('lip') and name.endswith('-scream'):
                continue
            if name.startswith('lip') and name.endswith('-legato'):
                r[4] = str(float(r[4]) * .7)
                r[6] = '0.9'
            elif name.startswith('lip') and name.endswith(('-left', '-right')):
                r[6] = '2.5'
        kept.append(r)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in kept) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    print(f'{len(rows)-len(kept)} screamed colours left out · room ×{args.room} · bells −{args.bells:g} dB over 2.5 kHz · tape {args.tape}')


if __name__ == '__main__':
    main()
