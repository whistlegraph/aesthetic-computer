"""Add a one-bar handbell pickup and lengthen 'line-er' in the short single."""
import argparse
import json
from pathlib import Path
import subprocess
import tempfile

import numpy as np
from scipy.io import wavfile


def held_line(source, job, sr, beat, room):
    raw = np.fromfile(source, dtype='<f4')
    pre = json.loads(source.with_suffix('.json').read_text())['pre']
    line, er, on = [pre+t for t in job['starts'][-3:]]
    end = len(raw)/sr
    # Give each syllable its own sustain, keeping n/r boundaries at normal speed.
    line_at = pre+3.20*beat
    on_at = min(room-.40, pre+7.42*beat if room<6 else pre+15.5*beat)
    er_at = line_at+(on_at-line_at)*.48
    points = [(0,0),(line,line_at),(line+.055,line_at+.055),
              (er-.045,er_at-.045),(er,er_at),(er+.045,er_at+.045),
              (on-.035,on_at-.035),(on,on_at),(end,on_at+min(.42,end-on))]
    with tempfile.TemporaryDirectory() as scratch:
        base=Path(scratch)
        wavfile.write(base/'take.wav',sr,raw)
        (base/'map').write_text(''.join(f'{round(x*sr)} {round(y*sr)}\n' for x,y in points[1:]))
        subprocess.run(['rubberband-r3','-q','--timemap',str(base/'map'),'-D',str(points[-1][1]),
                        str(base/'take.wav'),str(base/'held.wav')], check=True)
        rate,data=wavfile.read(base/'held.wav')
        if rate!=sr or abs(len(data)/sr-points[-1][1])>.08:
            raise ValueError('Vocal stretch missed its cue')
    return data.astype('<f4')


def bell_break(rows, samples, beat):
    """Five bars. The little bell pickups and the ghost kick share the swung eighth."""
    end = 20 * beat
    def in_break(row):
        if row[0] in ('kick', 'snare', 'spray'):
            return float(row[1]) < end - .04
        return row[0] == 'bell' and float(row[2]) < end - .04

    rows = [row for row in rows if not in_break(row)]
    stone = {p.stem.split('-')[0]:k for k,p in samples.items() if 'stone-handbell' in p.name}
    gold = next(k for k,p in samples.items() if p.name.startswith('E2-gold-church-'))
    def bell(k, at, length, gain, pan):
        rows.append(['bell', str(k), str(at*beat), str(length*beat), str(gain), str(pan), '.21'])
    # The low bell and kick announce bar one together. Leave space on two and four
    # for the snare; the high bells answer its tail, sitting where the open hat would.
    bell(gold, 0, 7.5, .27, 0)
    motifs = [
        [('E4',0,.35), ('G4',1.58,.24), ('B4',2.58,.28)],
        [('E4',0,.27), ('A4',1.58,.23), ('G4',3.58,.19)],
        [('B4',.58,.25), ('G4',2.58,.23)],
        [('E4',0,.32), ('G4',1.58,.24), ('B4',2.58,.28)],
        [('A4',.58,.24), ('G4',2,.21), ('E4',3.58,.20)],
    ]
    for bar, phrase in enumerate(motifs):
        down = bar * 4
        for j,(note,pos,gain) in enumerate(phrase):
            bell(stone[note], down+pos, 2.6 if j==0 else 1.5, gain, -.15 if j%2==0 else .19)
        def drum(kind, pos, *params):
            rows.append([kind, str((down+pos)*beat), *map(str,params)])
        drum('kick', 0, .72 if bar==0 else .88, 0)
        drum('kick', 2.58 if bar%2==0 else 2, .64, 0)
        if bar in (1,3): drum('kick', 3.78, .34, 0)
        for pos in (1.03,3.03):
            drum('snare', pos, .43 if bar==0 else .54)
        # The hat drops out wherever a handbell sounds, giving its attack the accent.
        for pos in (.58,1.58,2.58,3.58):
            if all(abs(pos-p[1])>.01 for p in phrase):
                drum('spray',pos,6500,.88,1,.001,.021,.10,.14,.12,0)
    return rows


def main():
    p=argparse.ArgumentParser(description=__doc__)
    for field in ('score','jobs','out'):
        p.add_argument(field,type=Path)
    args=p.parse_args()
    rows=[line.split() for line in args.score.read_text().splitlines() if line.strip()]
    samples={int(v[1]):Path(v[2]) for v in rows if v[0]=='smp'}
    jobs={j['key']:j for j in json.loads(args.jobs.read_text())['jobs'] if j['line'].startswith('lip')}
    out=args.out.resolve()
    out.mkdir(parents=True,exist_ok=False)
    bpm=float(next(v[2] for v in rows if v[:2]==['cfg','bpm']))
    sr=int(next(v[1] for v in rows if v[0]=='sr'))
    beat=60/bpm
    bar=4*beat
    sizes={}
    for row in rows:
        if row[0]!='smp': continue
        sid=int(row[1])
        src=samples[sid]
        if src.stem not in jobs: continue
        job=jobs[src.stem]
        room=16*beat if job['line']=='lip-black' else 8*beat
        y=held_line(src,job,sr,beat,room)
        path=out/(src.stem+'-liner.f32')
        y.tofile(path)
        row[2]=str(path)
        sizes[sid]=len(y)/sr
    for v in rows:
        kind=v[0]
        if kind=='frames':
            v[1]=str(int(v[1])+round(bar*sr))
        elif kind not in ('sr','cfg','smp','material','under','.'):
            ti=2 if kind in ('sung','bell','vox') else 1
            v[ti]=str(float(v[ti])+bar)
            if kind=='sung' and int(v[1]) in sizes:
                v[3]=str(sizes[int(v[1])])
    rows=bell_break(rows,samples,beat)
    (out/'liner.score.txt').write_text('\n'.join(' '.join(v) for v in rows)+'\n')
    width=json.loads((args.score.parent/'wattajetta-cutie.width.json').read_text())
    (out/'wattajetta-cutie.width.json').write_text(json.dumps([[0,width[0][1]]]+[[t+bar,w] for t,w in width]))
    print(f'{len(sizes)} lip liner takes; pickup {bar:.3f}s')

if __name__=='__main__':
    main()
