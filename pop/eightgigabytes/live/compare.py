#!/usr/bin/env python3
"""Compare the live callback output with the studio's pre-master signal."""
from pathlib import Path
import json,numpy as np
from scipy.io import wavfile
from scipy.signal import lfilter
from numba import njit
P=Path(__file__).resolve().parents[1]/'out';SR=48000;OFFSET=1.45;TRIM=round(2.05*SR)
def read(p):
 sr,x=wavfile.read(p);assert sr==SR
 if x.dtype==np.int32:x=x.astype(np.float32)/2147483648
 elif x.dtype==np.int16:x=x.astype(np.float32)/32768
 return x
@njit
def room(x,delays):
 y=np.zeros(len(x),np.float32)
 for d in delays:
  b=np.zeros(round(d*48000),np.float32);k=0;lp=0.
  for i in range(len(x)):
   v=b[k];lp=lp*.35+v*.65;b[k]=x[i]+lp*.79;k=(k+1)%len(b);y[i]+=v/4
 for d in [.0051,.0017]:
  b=np.zeros(round(d*48000),np.float32);k=0
  for i in range(len(y)):
   v=b[k];u=y[i]+v*.5;b[k]=u;k=(k+1)%len(b);y[i]=v-.5*u
 return y*10**(-15/20)
tl=json.loads((P/'timeline.json').read_text());mix={'drums':-10.5,'bass':-17.5,'pad':-9,'keys':-9.5,'whistle':-11.5}
x=read(P/'stems/drums.wav');n=len(x);del x
t=(np.arange(n)/SR-.6)/.6;want=np.ones(n);want[t<4]=.72;want[(t>=84)&(t<116)]=.6;want[t>=148]=.75
scale,_=lfilter([.00004],[1,-.99996],want,zi=[.72*.99996]);scale=scale.astype(np.float32)
# Score section boundaries are derived, not duplicated.
import subprocess
sections=json.loads(subprocess.check_output(['node','--input-type=module','-e',"import {SECTIONS} from './pop/eightgigabytes/score.mjs';console.log(JSON.stringify(SECTIONS))"]))
at={s['id']:s['start'] for s in sections}
want[:]=1;want[t<at['r1']]=.72;want[(t>=at['bridge'])&(t<at['r3'])]=.6;want[t>=at['outro']]=.75
scale,_=lfilter([.00004],[1,-.99996],want,zi=[.72*.99996]);scale=scale.astype(np.float32)
rows={};summed=None;expected=None
for member,buses in {'neo':['keys','whistle'],'blueberry':['bass','pad'],'frisbee':['drums']}.items():
 ref=np.zeros((n,2),np.float32);vox=np.zeros_like(ref)
 for bus in buses:ref+=read(P/f'stems/{bus}.wav')*10**(mix[bus]/20)*(scale[:,None] if bus!='whistle' else 1)
 for line in tl['lines']:
  if line['m']!=member:continue
  role=line['role'];db={'neo':0,'blueberry':-3.5,'frisbee':-3.5,'hum':-12,'echo':-3}[role]
  sig=read(line['wav'].replace('.wav','.vx.wav'))
  pan={'neo':0,'blueberry':.38,'frisbee':-.38}[member]*(1.6 if role=='hum' else 1)
  g=10**(db/20)*.9;start=round((.6+line['spanOffset'])*SR);end=min(n,start+len(sig))
  vox[start:end,0]+=sig[:end-start]*g*np.cos((pan+1)*np.pi/4);vox[start:end,1]+=sig[:end-start]*g*np.sin((pan+1)*np.pi/4)
 mono=(vox[:,0]+vox[:,1])*.5
 vox[:,0]+=room(mono,[.0297,.0371,.0411,.0437]);vox[:,1]+=room(mono,[.0313,.0353,.0427,.0449]);ref+=vox*10**(.5/20)
 got=np.fromfile(P/f'live/{member}-match/mix.f32',dtype='<f4').reshape(-1,2)
 ref=ref[TRIM:TRIM+len(got)]
 delta=got-ref;rms=lambda a:float(np.sqrt(np.mean(a.astype(np.float64)**2)))
 rows[member]={'rmsError':rms(delta),'referenceRms':rms(ref),'errorDb':20*np.log10(max(rms(delta),1e-15)/rms(ref)),'peakError':float(abs(delta).max())}
 print(member,rows[member],flush=True)
 if summed is None:summed=got.copy();expected=ref.copy()
 else:summed+=got;expected+=ref
(P/'live/comparison.json').write_text(json.dumps(rows,indent=2))
summed.astype('<f4').tofile(P/'live/combined-premaster.f32')
expected.astype('<f4').tofile(P/'live/reference-premaster.f32')
