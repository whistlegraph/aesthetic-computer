"""The voices alone: a score with every sung voice and nothing else, to render as an a cappella.

  acapella.py <liner.score.txt> <new dir> [--leads] [--click]
Keeps the settings, the samples and the `sung` rows that are voices (not the grand piano, not the
guns); `--leads` keeps only the worded lead lines (no choir, ah/oo, screams or small nothings).
Render <new dir>/liner.score.txt with c/run-cutie.mjs --accel 124: same room, tape and master,
so it lines up with the full mix sample for sample in time.
"""
import argparse
from pathlib import Path
import re
import shutil

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('score', type=Path)
p.add_argument('out', type=Path)
p.add_argument('--leads', action='store_true')
p.add_argument('--click', action='store_true', help='keep the kick, quietly, and the ticks: a click track under the voices')
a = p.parse_args()
out = a.out.resolve()
out.mkdir(parents=True, exist_ok=False)
rows = [l.split() for l in a.score.read_text().splitlines() if l.strip()]
names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
lead = re.compile(r'line-|\d+-(color|smooth|soft|angel)$|nothing-six-bars|hook-high')
kept = [r for r in rows if r[0] in ('sr', 'frames', 'material', 'cfg', 'smp')
        or (r[0] == 'sung' and not names[r[1]].startswith(('grand-', 'piano-', 'gun-'))
            and (not a.leads or lead.match(names[r[1]])))
        or (a.click and r[0] in ('kick', 'tick'))]
for r in kept:
    if r[0] == 'kick':
        r[2] = str(float(r[2]) * .4)
(out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in kept) + '\n')
shutil.copyfile(a.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
print(sum(r[0] == 'sung' for r in kept), 'voices')
