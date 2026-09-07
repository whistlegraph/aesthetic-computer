import numpy as np
import os
S=os.environ.get("RAPID_WORK") or os.path.expanduser("~/.cache/ac/rapid")
os.makedirs(S,exist_ok=True)
sr=48000
BPM=float(os.environ.get("RAPID_BPM","156"))
BEAT=60.0/BPM; BAR=4*BEAT
G=0.10
BARS=88
DURATION=G+BARS*BAR+1.0
NT=int(DURATION*sr)
def T(b): return G+b*BAR
root0=58.27                  # A#1 — the squeezed bass stem carries the changes

# ---- SUB PEDAL: only where the record's own bass is absent ----
PED=[(0,4,"eighths"),(84,88,"sparse")]
sub=np.zeros(NT)
def note(t,freq,dur,g):
    n=int(dur*sr)
    a=int(t*sr); z=min(NT,a+n)
    if a<0 or a>=NT: return
    tt=np.arange(z-a)/sr
    env=np.minimum(tt/0.005,1.0)*np.exp(-tt/(dur*0.55))
    sub[a:z]+=np.tanh(2.2*np.sin(2*np.pi*freq*tt))*env*g
for (b0,b1,mode) in PED:
    step=0.5 if mode=="sparse" else 0.25
    b=float(b0)
    while b<b1:
        fade=1.0
        if b0==0: fade=0.4+0.6*(b-b0)/(b1-b0)       # the intake swells up
        if b0==84: fade=1.0-0.8*(b-b0)/(b1-b0)      # the outro lets go
        note(T(b)+BEAT/2,root0,BEAT*0.34,0.28*fade)
        b+=step
np.stack([sub,sub],1).astype(np.float32).tofile(f"{S}/rapid-sub.raw")
print("bass: pedal at the mouth and the tail")

# ---- WUB: enters late in sprint2, wild from the drop onward ----
wub=np.zeros(NT)
for (b0,b1,depth,g) in [(28,36,0.40,0.11),(44,56,0.55,0.14),
                        (56,72,0.60,0.15),(72,80,0.65,0.16)]:
    a=int(T(b0)*sr); z=min(NT,int(T(b1)*sr))
    n=z-a
    t=np.arange(n)/sr
    bar_idx=((t+T(b0)-G)/BAR).astype(int)
    wob_rate=np.where(bar_idx%2,6.10,4.07)
    wob=(1-depth)+depth*np.sin(2*np.pi*wob_rate*(t%BAR)-np.pi/2)
    ph=2*np.pi*root0*t
    y=np.tanh(2.6*np.sin(ph))*wob
    duck=np.ones(n)
    for bt in np.arange(0,n/sr,BEAT):
        i=int(bt*sr); j=min(n,i+int(0.08*sr))
        duck[i:j]*=1-0.7*np.exp(-np.arange(j-i)/(0.03*sr))
    y*=duck*g
    e_=int(BAR*sr)
    y[:e_]*=np.linspace(0,1,e_); y[-e_:]*=np.linspace(1,0,e_)
    wub[a:z]+=y
np.stack([wub,wub],1).astype(np.float32).tofile(f"{S}/rapid-wub.raw")
print("bass: wub in four passes")
