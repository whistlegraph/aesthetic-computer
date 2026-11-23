"""The acid line as a continuo: its pedal on the bass, its upper notes in counterpoint with the voice.

@jeffrey 2026-11-22: "make sure the acid line is bachian'ed in and has a nice part in the mix …
lets not have it in the start so much but have it come in around 0:27".

The acid figure was already a compound line: a low pedal with notes thrown above it. Its rhythm,
accents and slides are kept. The pedal now follows the bass note sounding in each bar. The upper
notes become one voice, chosen beat by beat (a shortest-path search over the bar's scale) by the
rules in papers/composition-platter/bach/digest/02 and pop/lib/counterpoint.mjs: consonant with
the sung note on the beat, a chord tone on strong beats, no perfect fifth or octave with the
voice reached by the same motion twice, contrary motion preferred, steps preferred to leaps.
Every step is also given its true time (the rows' steps had been left 19 bars behind their row).

  bach-acid.py <liner.score.txt> <new dir> --jobs <jobs.json> [...] [--enter 33] [--open .3]
`--enter` adds a row of the line at that bar (33 is 0:27 once soft-open.py starts at bar 21);
`--open` is the level of any row before it, against now. Bars count from the top of the score.
"""
import argparse
import json
from pathlib import Path
import re
import shutil

CONSONANT = {0, 3, 4, 7, 8, 9}
MAJOR = {4, 6, 8, 9, 11, 1, 3}          # E major, bent to the chord of the bar


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('out', type=Path)
    p.add_argument('--jobs', type=Path, nargs='+', required=True)
    p.add_argument('--enter', type=int)
    p.add_argument('--open', type=float, default=1)
    p.add_argument('--align', action='store_true',
                   help='where a line is sung the upper notes are struck with its syllables, and the pedal '
                        'keeps the beats between them (@jeffrey: "more aligned acid bass with the lyrics")')
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    rows = [l.split() for l in args.score.read_text().splitlines() if l.strip()]
    beat = 60 / float(next(r[2] for r in rows if r[:2] == ['cfg', 'bpm']))
    bar = 4 * beat
    names = {r[1]: Path(r[2]).stem for r in rows if r[0] == 'smp'}
    jobs = {}
    for f in args.jobs:
        jobs.update({j['key']: j for j in json.loads(f.read_text())['jobs']})
    angel = next((j for j in jobs.values() if j['line'] == 'lead' and j.get('mode') == 'angel'), None)

    # what the voice sings, and when
    sung = []
    for r in rows:
        if r[0] != 'sung':
            continue
        name = names[r[1]]
        job = angel if name.endswith('-angel') else jobs.get(name) or jobs.get(re.sub(r'-soft$', '-smooth', name))
        if not job or float(r[4]) < .3:
            continue
        ends = job['starts'][1:] + [job['starts'][-1] + job['end_hold']]
        sung += [(float(r[2]) + .03 + s, float(r[2]) + .03 + e, m) for s, e, m in zip(job['starts'], ends, job['midis'])]
    voice = lambda t: next((m for s, e, m in sung if s - .06 <= t < e - .06), None)
    pads = {}
    for r in rows:
        if r[0] == 'pad':
            pads.setdefault(round(float(r[1]) / beat), set()).add(round(float(r[3])) % 12)
    basses = sorted((float(r[1]), float(r[2]), round(float(r[3]))) for r in rows if r[0] == 'bass')

    def chord(b):
        at = [k for k in pads if b * 4 - 32 < k <= b * 4 + .2]
        return pads[max(at)] if at else {4, 8, 11}

    def under(b):
        held = [m for t, d, m in basses if t <= b * bar + .05 < t + d + .1]
        low = held[-1] if held else 40
        return low - 12 if low > 47 else low

    def scale(b):
        s, c = set(MAJOR), chord(b)
        for flat, sharp in ((2, 3), (7, 8), (0, 1), (10, 11), (5, 6)):
            if flat in c and sharp not in c:
                s.discard(sharp); s.add(flat)
        return s

    def upper(t0):
        """One note a beat for 32 beats: the cheapest path through the scale."""
        best = {None: (0, [])}
        for k in range(32):
            b, t = int((t0 + k * beat) / bar + .02), t0 + k * beat
            ped, sung_now, before = under(b), voice(t), voice(t - beat) if k else None
            strong = k % 2 == 0
            nxt = {}
            for n in range(ped + 3, ped + 20):
                if n % 12 not in scale(b):
                    continue
                here = 0
                if sung_now is not None and abs(n - sung_now) % 12 not in CONSONANT:
                    here += 6 if strong else 3
                if n % 12 not in chord(b):
                    here += 3 if strong else 1
                if abs(n - ped) % 12 not in CONSONANT:
                    here += 2
                for prev, (cost, path) in best.items():
                    c = cost + here
                    if prev is not None:
                        leap = abs(n - prev)
                        c += 1.5 if leap == 0 else 0 if leap <= 2 else 1 if leap <= 4 else 2.5 if leap <= 7 else 6
                        if sung_now is not None and before is not None and sung_now != before and n != prev:
                            same = (n - prev > 0) == (sung_now - before > 0)
                            c += 1 if same else -.5
                            if same and abs(n - sung_now) % 12 in (0, 7) and abs(prev - before) % 12 == abs(n - sung_now) % 12:
                                c += 20
                    if n not in nxt or c < nxt[n][0]:
                        nxt[n] = (c, path + [n])
            best = nxt
        return min(best.values())[1]

    # the rows, their steps under them, their true times
    blocks, kept, block = [], [], None
    for r in rows:
        if r[0] == 'acidrow':
            block = [r, []]
            blocks.append(block)
            kept.append(block)
        elif r[0] == '.':
            block[1].append(r)
        else:
            kept.append(r)
    for head, steps in blocks:
        t0 = float(head[1])
        stale = round((float(steps[0][1]) - t0) / bar) * bar
        for s in steps:
            s[1] = str(float(s[1]) - stale)
    template = blocks[min(1, len(blocks) - 1)]
    if args.enter is not None:
        t0, was = args.enter * bar, float(template[0][1])
        for head, _ in blocks:
            if float(head[1]) < t0 - .04:
                head[2] = str(float(head[2]) * args.open)
        new = [['acidrow', str(t0), '0.8', *template[0][3:]],
               [[s[0], str(float(s[1]) - was + t0), *s[2:]] for s in template[1]]]
        new[0][-1] = str(len(new[1]))
        blocks.append(new)
        kept.append(new)

    report = []
    for head, steps in blocks:
        t0 = float(head[1])
        line = upper(t0)
        floor = min(round(float(s[2])) for s in steps)
        for s in steps:
            at = float(s[1]) - t0
            was, b = round(float(s[2])), int((t0 + at) / bar + .02)
            if was <= floor:
                s[2] = str(under(b))
            else:
                n = line[min(31, int(at / beat + .02))]
                s[2] = str(n + (12 if was >= 64 else 0))
        if args.align:
            inside = sorted((s0, e0, m) for s0, e0, m in sung if t0 - .02 <= s0 < t0 + 8 * bar - .1)
            spans = []
            for s0, e0, m in inside:
                if spans and s0 - spans[-1][1] < .3:
                    spans[-1][1] = max(spans[-1][1], e0)
                else:
                    spans.append([s0, e0])
            sings = lambda t: any(a0 - .05 <= t < a1 for a0, a1 in spans)
            free = [s for s in steps if not sings(float(s[1]))]
            like = next((s for s in steps if s[3] == '1'), steps[0])
            struck = []
            for s0, e0, m in inside:
                k = min(31, int((s0 - t0) / beat + .02))
                b = int(s0 / bar + .02)
                n = min((c for c in range(line[k] - 4, line[k] + 5) if c % 12 in scale(b) and abs(c - m) % 12 in CONSONANT),
                        key=lambda c: abs(c - line[k]), default=line[k])
                gap = min(e0 - s0, 2 * beat)
                struck.append(['.', str(s0), str(n), '1', '0', like[5], str(max(.6, gap / (beat / 4) * .8)), like[7], '0', '0', '0.45'])
            onsets = [float(x[1]) for x in struck]
            for k in range(32):
                t = t0 + k * beat
                if sings(t) and all(abs(t - o) > .12 for o in onsets):
                    struck.append(['.', str(t), str(under(int(t / bar + .02))), '0', '0', like[5], '1.6', like[7], '0', '0', '0.45'])
            steps[:] = sorted(free + struck, key=lambda x: float(x[1]))
            head[-1] = str(len(steps))
        beats = [(line[k], voice(t0 + k * beat)) for k in range(32)]
        with_voice = [(n, v) for n, v in beats if v is not None]
        cons = sum(abs(n - v) % 12 in CONSONANT for n, v in with_voice)
        steps_ = sum(abs(a - b) <= 2 for a, b in zip(line, line[1:]))
        moves = [(a[0] - b[0], a[1] - b[1]) for a, b in zip(beats[1:], beats) if a[1] is not None and b[1] is not None and a[0] != b[0] and a[1] != b[1]]
        contrary = sum((x > 0) != (y > 0) for x, y in moves)
        report.append(f'bar {round(t0/bar)}: {cons}/{len(with_voice)} beats consonant with the voice · '
                      f'{steps_}/31 moves by step · {contrary}/{len(moves)} contrary')
    flat = []
    for item in kept:
        if isinstance(item[1], list):
            flat += [item[0], *item[1]]
        else:
            flat.append(item)
    (out / 'liner.score.txt').write_text('\n'.join(' '.join(r) for r in flat) + '\n')
    shutil.copyfile(args.score.parent / 'wattajetta-cutie.width.json', out / 'wattajetta-cutie.width.json')
    print('\n'.join(report))


if __name__ == '__main__':
    main()
