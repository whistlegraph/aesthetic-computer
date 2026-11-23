"""Sheep arpeggios over the passacaglia's ground (@jeffrey 2026-11-22: "can the sheeps be arpeggiated").

Each bar of the ground (D C Bb G A) has its chord (D minor, C, Bb, G minor, A major). Each voice
breaks it in its own rhythm:
  solo  (variation 1)  quarters, up and back down          root 3rd 5th 3rd
  low   (variation 5)  quarters, up                        root 3rd 5th 8ve
  mid   (variation 5)  quarters an eighth late (a canon)   3rd 5th 8ve 5th
  high  (variation 5)  dotted eighths, down (3:4 drift)    8ve 5th 3rd ... (from the octave over the mid)
Every note is a separate real bleat sung onto its pitch by bin/donkeyvox.py. A voice draws its bleats
from several takes; any bleat that lands more than 30 cents off is dropped and the next run used.

  sheep-arps.py <out dir>      writes <out dir>/<voice>/ (donkeyvox output) and arps.json
"""
import json
from pathlib import Path
import subprocess
import sys

HERE = Path(__file__).resolve().parent
SHEEP = HERE.parents[1] / 'wattajetta/assets/sheep'
STEP = 60 / 172 / 4
CHORDS = [(62, 65, 69), (60, 64, 67), (58, 62, 65), (55, 58, 62), (57, 61, 64)]      # D4 octave
NAMES = ['C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B']
name = lambda m: NAMES[m % 12] + str(m // 12 - 1)


def voice(octave, steps, pick):
    """notes for 5 bars: steps are the onsets in a bar (16ths), pick(chord, i) the pitch of the i-th."""
    out = []
    for b, chord in enumerate(CHORDS):
        for i, st in enumerate(steps):
            out.append((16 * b + st, pick(chord, i) + 12 * octave))
    return out


up_down = lambda c, i: [c[0], c[1], c[2], c[1]][i % 4]
up = lambda c, i: [c[0], c[1], c[2], c[0] + 12][i % 4]
canon = lambda c, i: [c[1], c[2], c[0] + 12, c[2]][i % 4]
down = lambda c, i: [c[0] + 12, c[2], c[1], c[0], c[1]][i % 5]
VOICES = {
    'solo': (voice(0, [0, 4, 8, 12], up_down), ['sheep-bleat-long-04', 'sheep-bleat-long-05', 'sheep-bleat-long-02']),
    'low': (voice(-1, [0, 4, 8, 12], up), ['sheep-bleat-long-06', 'sheep-bleat-long-01', 'sheep-bleat-02', 'sheep-bleat-06',
                                           'sheep-bleat-08', 'sheep-bleat-09', 'sheep-bleat-10', 'sheep-bleat-11', 'ram-bleat-01']),
    'mid': (voice(0, [2, 6, 10, 14], canon), ['sheep-bleat-long-05', 'sheep-bleat-long-04', 'sheep-bleat-long-02']),
    'high': (voice(0, [0, 3, 6, 9, 12], down), ['sheep-bleat-long-03', 'ewe-bleat-01', 'sheep-bleat-03', 'sheep-bleat-04',
                                               'sheep-bleat-05', 'lamb-bleat-02', 'lamb-bleat-03', 'lamb-bleat-04', 'lamb-bleat-05',
                                               'sheep-bleat-07', 'sheep-bleat-long-02']),
}


def main():
    out = Path(sys.argv[1]).resolve()
    out.mkdir(parents=True, exist_ok=True)
    summary = {}
    for v, (notes, takes) in VOICES.items():
        files = ','.join(str(next(SHEEP.glob(t + '.*'))) for t in takes)
        onsets = [n[0] for n in notes]
        holds = [((onsets[i + 1] if i + 1 < len(onsets) else 80) - onsets[i]) * STEP * .95 for i in range(len(onsets))]
        holds = [min(h, .6) for h in holds]
        skip, kept = [], None
        files += ',' + str(SHEEP / 'sheep-flock-02.mp3')          # a sheepfold of ewes: spare bleats for any voice
        for attempt in range(12):
            r = subprocess.run([sys.executable, str(HERE / 'donkeyvox.py'), files, str(out / v),
                                '--notes', ','.join(name(m) for _, m in notes), '--min', '.2', '--nat', '.15', '--vib', '12',
                                '--hold', ','.join(f'{h:.3f}' for h in holds), '--skip', ','.join(map(str, skip))],
                               capture_output=True, text=True)
            if r.returncode:
                print(v, 'failed:', r.stdout[-300:], r.stderr[-300:])
                break
            syl = json.loads((out / v / 'donkeyvox.json').read_text())['syllables']
            bad = [k for k, s in enumerate(syl) if s['pitchErrorCents'] is None or abs(s['pitchErrorCents']) > 30]
            if not bad:
                kept = syl
                break
            # the k-th syllable used the k-th run not yet skipped: skip that run and try again
            used = [i for i in range(10000) if i not in skip]
            skip = sorted(set(skip) | {used[k] for k in bad})
            kept = syl
        errs = [999 if s['pitchErrorCents'] is None else abs(s['pitchErrorCents']) for s in kept or []]
        summary[v] = {'onsets': onsets, 'syllables': kept, 'skipped': skip,
                      'worstCents': max(errs) if errs else None, 'over30': sum(e > 30 for e in errs)}
        print(f'{v}: {len(onsets)} notes · worst {summary[v]["worstCents"]} c · {summary[v]["over30"]} over 30 c · skipped runs {skip}')
    (out / 'arps.json').write_text(json.dumps(summary, indent=1))


if __name__ == '__main__':
    main()
