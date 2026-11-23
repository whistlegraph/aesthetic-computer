"""The release critique's safe fixes, on the final score (after soft-open.py, so bars count from the top heard).

From the Bach-informed critique of 2026-11-22 (papers/composition-platter/bach digests 02–03):
- bar 39, the seam into the last chorus: the piano is still on E minor while the plucks and the bell run
  have turned to E major (24 G/G# and 16 D/D# overlaps). The bar's plucked G#s come down to G, its bell
  G#s and D#s are left out, and piano notes that would ring into bar 40 stop at the barline, so the turn
  to major lands once, on the downbeat with the hook.
- bars 42–43 (the purple line over the bass D#): the leading tone was tripled (bass, pad, piano). The pad's
  D#3 moves to F#3 and the piano's D#3 is left out.

  bach-fixes.py <liner.score.txt> <new dir>
"""
import argparse
from pathlib import Path
import re
import shutil

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('score', type=Path)
p.add_argument('out', type=Path)
a = p.parse_args()
out = a.out.resolve()
out.mkdir(parents=True, exist_ok=False)
rows = [l.split() for l in a.score.read_text().splitlines() if l.strip()]
bar = 240 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
note = lambda n: (m := re.match(r'([A-G])(s?)(-?\d)', n)) and 'C D EF G A B'.index(m[1]) + (m[2] == 's') + 12 * (int(m[3]) + 1)
inbar = lambda t, b0, b1: b0 * bar - .02 <= t < b1 * bar - .02
counts = {'plucks': 0, 'bells': 0, 'piano cut': 0, 'pad': 0, 'piano out': 0}
kept = []
for r in rows:
    if r[0] == 'pluck' and inbar(float(r[1]), 39, 40) and round(float(r[2])) % 12 == 8:
        r[2] = str(float(r[2]) - 1); counts['plucks'] += 1
    elif r[0] == 'bell' and inbar(float(r[2]), 39, 40):
        m = note(names[r[1]])
        if m is not None and m % 12 in (3, 8):
            counts['bells'] += 1
            continue
    elif r[0] == 'sung' and names[r[1]].startswith(('grand-', 'piano-')):
        t, hold = float(r[2]), float(r[3])
        midi = int(names[r[1]].split('-')[1])
        if inbar(t, 39, 40) and t + hold > 40 * bar - .05:
            r[3] = str(max(.05, 40 * bar - .05 - t)); counts['piano cut'] += 1
        if inbar(t, 42, 44) and midi % 12 == 3 and midi < 60:
            counts['piano out'] += 1
            continue
    elif r[0] == 'pad' and inbar(float(r[1]), 42, 44) and round(float(r[3])) == 51:
        r[3] = '54'; counts['pad'] += 1
    kept.append(r)
(out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in kept) + '\n')
shutil.copyfile(a.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
print(' · '.join(f'{k} {v}' for k, v in counts.items()))
