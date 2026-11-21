"""Make the short 56k single from modem-choir.py's score and the source line jobs."""
import argparse
import json
from pathlib import Path
import runpy

import numpy as np
from scipy.ndimage import gaussian_filter1d

voice = runpy.run_path(str(Path(__file__).with_name('choir-cutie.py')))
p = argparse.ArgumentParser(description=__doc__)
for arg in ('score', 'jobs', 'out'):
    p.add_argument(arg, type=Path)
a = p.parse_args()
a.out.mkdir(parents=True, exist_ok=False)
lines = [l.split() for l in a.score.read_text().splitlines() if l.strip()]
jobs = {j['key']:j for j in json.loads(a.jobs.read_text())['jobs'] if j['line']=='nothing'}
samples = {int(v[1]):Path(v[2]) for v in lines if v[0]=='smp'}
takes = [v for v in lines if v[0]=='sung' and samples[int(v[1])].stem in jobs]
last = max(float(v[2]) for v in takes)
group = [v for v in takes if abs(last-float(v[2])) < .1]
lead = max(group,key=lambda v:float(v[4]))
job = jobs[samples[int(lead[1])].stem]
sr = 48000
beat = 60/112
bar = 4*beat
# One continuous throat through the whole run. The original 'th-ing' arrives on the landing.
x = np.fromfile(samples[int(lead[1])],'<f4')
start, end = job['starts'][-2:]
syll = x[round(start*sr):round(end*sr)]
t, f, sp, ap = voice['analyze'](syll)
core = np.where((f>0)&(t>.075)&(t<end-start-.09))[0]
if not len(core):
    raise ValueError('Missing voiced nucleus of nothing')
ph = np.arange(0, 24*beat, .005)
b = ph/beat
base = job['midis'][-1]
figure = np.array([0,4,7,12,16,19,24,19,16,12,7,4])
degrees = np.interp(b,[0,2,6,12,16,20,22,24],[0,0,8,20,36,68,72,72])
pitch = base + figure[np.floor(degrees+1e-6).astype(int)%len(figure)]
ix = core[np.rint((.5-.46*np.cos(2*np.pi*ph*.41))*(len(core)-1)).astype(int)]
y = voice['synth'](440*np.exp2((gaussian_filter1d(pitch.astype(float),1.3)-69)/12),sp[ix],ap[ix])
y = voice['level'](y,voice['rms'](syll)*.91)
# Crossfade into the pitched vowel after the n-, then into the recorded ending.
def join(x,y):
    n=round(.025*sr)
    w=np.linspace(0,1,n)
    return np.r_[x[:-n],x[-n:]*(1-w)+y[:n]*w,y[n:]]
clip=join(join(x[:round((start+.13)*sr)],y),x[round((end-.045)*sr):])
ext_id=max(samples)+1
path=a.out.resolve()/'nothing-six-bars.f32'
clip.astype('<f4').tofile(path)
written=[]; skip_steps=False
for v in lines:
    k=v[0]
    if k=='.':
        if not skip_steps: written.append(v)
        continue
    skip_steps=False
    if k in ['cfg','material','sr','smp','under']: written.append(v); continue
    if k=='frames':
        written.append([k,str(int(v[1])-round(20*bar*sr))]);continue
    ti=2 if k in ('sung','vox','bell') else 1
    at=float(v[ti])
    # Keep the tiny pickups attached to their musical downbeats.
    if 4*bar-.04 <= at < 24*bar-.04:
        skip_steps=k=='acidrow';continue
    if at < 4*bar and k in ('air','bloop','bell'):
        gi={'air':4,'bloop':3,'bell':4}[k]
        v[gi]=str(float(v[gi])*(.08 if k=='air' else .22))
        if k=='bell': v[3]=str(min(float(v[3]),.85))
    if v is lead:
        written.append(['smp',str(ext_id),str(path)])
        v[1]=str(ext_id);v[3]=str(len(clip)/sr)
    elif any(v is other for other in group):
        v[3]=str(start+.14)
    if at >= 24*bar-.04:v[ti]=str(at-20*bar)
    if k=='elastic':
        # The old chorus hit becomes a quick ripple. Bar 74 is 2:29.4 after the tempo ramp.
        written.append(['elastic',v[1],'1.25','.34','.92','1.85','.04','.03','.30','.10'])
        v[1]=str(73*bar)
    written.append(v)
for n in range(4):
    def hit(kind,pos,*values):
        written.append([kind,str((4*n+pos)*beat),*map(str,values)])
    for pos,vel in [(0,.94),(1.66,.52),(2.45,.76)]:hit('kick',pos,vel,0)
    for pos in (1.032,3.038):hit('snare',pos,.59)
    for i in range(8):
        hit('spray',i*.5+(.075 if i%2 else 0),6800,.88,1,.001,.021,.10,.13 if i%2 else .20,.12,0)
    if n in (1,3):hit('snare',3.78,.16)
(a.out/'short.score.txt').write_text('\n'.join(' '.join(v) for v in written)+'\n')
width=json.loads((a.score.parent/'wattajetta-cutie.width.json').read_text())
(a.out/'wattajetta-cutie.width.json').write_text(json.dumps([[t if t<4*bar else t-20*bar,w] for t,w in width if t<4*bar or t>=24*bar]))
