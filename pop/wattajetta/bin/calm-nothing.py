"""The six-bar "nothing", calmer and said three times: "nothing, nothing, nooooothing".

short-nothing.py threw the held vowel up a two-octave arpeggio that reached eight notes a beat
(@jeffrey 2026-11-22: "a little too extreme … can we double up the nothings on the arpeggio and be
like 'nothing nothing nooooothing' so we hear many nothings"). This makes the hold again from the
same take. The word is said whole, said again two beats on, and held the third time; the figure
stays inside `--span` semitones and never runs faster than `--rate` notes a beat. While it climbs,
the word comes back every `--every` beats on the note the figure has reached, small, one ear then
the other.

  calm-nothing.py <score with nothing-six-bars> <jobs.json> <new dir>
                  [--span 12] [--rate 4] [--gain .8] [--every 2] [--echo .3]
Writes <new dir>/liner.score.txt and the samples it names.
"""
import argparse
import json
from pathlib import Path
import runpy
import shutil

import numpy as np
from scipy.ndimage import gaussian_filter1d

voice = runpy.run_path(str(Path(__file__).with_name('choir-cutie.py')))
p = argparse.ArgumentParser(description=__doc__)
for arg in ('score', 'jobs', 'out'):
    p.add_argument(arg, type=Path)
p.add_argument('--span', type=int, default=12)
p.add_argument('--rate', type=float, default=4)
p.add_argument('--gain', type=float, default=.8)
p.add_argument('--every', type=float, default=2, help='beats between the small nothings (0: none)')
p.add_argument('--echo', type=float, default=.3, help='a small nothing, against the held one')
a = p.parse_args()
out = a.out.resolve()
out.mkdir(parents=True, exist_ok=False)
rows = [l.split() for l in a.score.read_text().splitlines() if l.strip()]
sr = int(next(r[1] for r in rows if r[0] == 'sr'))
beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
jobs = {j['key']: j for j in json.loads(a.jobs.read_text())['jobs'] if j['line'] == 'nothing'}
samples = {r[1]: Path(r[2]) for r in rows if r[0] == 'smp'}
held = next(r for r in rows if r[0] == 'smp' and Path(r[2]).name == 'nothing-six-bars.f32')
row = next(r for r in rows if r[0] == 'sung' and r[1] == held[1])
# the take the hold was cut from: the "nothing" whose opening the old hold still begins with
old = np.fromfile(held[2], '<f4')
head = round(.8*sr)


def same(path):
    take = np.fromfile(path, '<f4')[:head]
    return abs(np.corrcoef(take, old[:len(take)])[0, 1]) if len(take) == head else 0


stem = max((p for p in samples.values() if p.stem in jobs), key=same)
if same(stem) < .9:
    raise ValueError('No "nothing" take opens like the hold')
job = jobs[stem.stem]
x = np.fromfile(stem, '<f4')
start, end = job['starts'][-2:]
syll = x[round(start*sr):round(end*sr)]
t, f, sp, ap = voice['analyze'](syll)
core = np.where((f > 0) & (t > .075) & (t < end-start-.09))[0]
if not len(core):
    raise ValueError('Missing voiced nucleus of nothing')

# the word, whole and short: "noth-" as it was sung and half a second of "-ing"
said = 2 * beat
word = x[round(start*sr):round((end+.5)*sr)].copy()
fade = round(.06*sr)
word[-fade:] *= np.linspace(1, 0, fade)
if len(word) > said*sr:
    raise ValueError('The word does not fit two beats')

held_beats = 24 - 2 * 2          # the two said words take four beats from the hold
ph = np.arange(0, held_beats*beat, .005)
b = ph / beat * 24 / held_beats  # the figure keeps its shape over the shorter hold
up = [s for s in (0, 4, 7, 12, 16, 19, 24) if s <= a.span]
figure = np.array(up + up[-2:0:-1])
# held two beats, then a note a beat, two, and `rate` at the top; it slows and lands on its own note
rate = a.rate * held_beats / 24       # the figure is laid over 24 beats and played in held_beats
steps = np.cumsum([0, 0, 4, 6, 8, 4*rate, rate])
steps = np.r_[steps[:-1], np.ceil(steps[-1] / len(figure)) * len(figure)]
degrees = np.interp(b, [0, 2, 6, 12, 16, 20, 22, 24], np.r_[steps, steps[-1]])
pitch = job['midis'][-1] + figure[np.floor(degrees+1e-6).astype(int) % len(figure)]
ix = core[np.rint((.5-.46*np.cos(2*np.pi*ph*.41))*(len(core)-1)).astype(int)]
y = voice['synth'](440*np.exp2((gaussian_filter1d(pitch.astype(float), 1.3)-69)/12), sp[ix], ap[ix])
y = voice['level'](y, voice['rms'](syll)*.91)


def join(x, y):
    n = round(.025*sr)
    w = np.linspace(0, 1, n)
    return np.r_[x[:-n], x[-n:]*(1-w)+y[:n]*w, y[n:]]


def slot(x, seconds):
    return np.r_[x, np.zeros(max(0, round(seconds*sr) - len(x)))]


first = slot(np.r_[x[:round(start*sr)], word], start + said)
third = join(join(x[round(start*sr):round((start+.13)*sr)], y), x[round((end-.045)*sr):])
clip = np.r_[first, slot(word, said), third]
if abs(len(clip) - len(old)) > .08*sr:
    raise ValueError(f'The new hold is {len(clip)/sr:.2f} s, the old one {len(old)/sr:.2f} s')
path = out / 'nothing-six-bars.f32'
clip.astype('<f4').tofile(path)
held[2] = str(path)
row[3] = str(len(clip)/sr)
row[4] = str(float(row[4]) * a.gain)

# the small nothings: the word again on the note the figure has reached
count = 0
if a.every:
    take = voice['analyze'](word)
    ids = {}
    next_id = max(int(k) for k in samples) + 1
    at = np.arange(a.every, held_beats - 2, a.every)
    for n, hb in enumerate(at):
        note = int(pitch[min(len(pitch)-1, round(hb*beat/.005))])
        if note not in ids:
            small = voice['word_at_note'](word, take, note)
            f32 = out / f'nothing-word-{note}.f32'
            small.astype('<f4').tofile(f32)
            ids[note] = next_id
            rows.append(['smp', str(next_id), str(f32)])
            next_id += 1
        when = float(row[2]) + start + 2*said + .13 + hb*beat
        level = float(row[4]) * a.echo * (.7 + .3 * n / max(1, len(at)-1))
        rows.append(['sung', str(ids[note]), str(when), str(len(word)/sr), str(level),
                     str(-.6 if n % 2 else .6), '0.01', '0.12', '0.1'])
        count += 1
(out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in rows) + '\n')
shutil.copyfile(a.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
print(f'hold {len(clip)/sr:.2f} s · said twice, held {held_beats} beats · up {max(figure)} semitones · '
      f'at most {a.rate:g} notes a beat · {count} small nothings · from {stem.name}')
