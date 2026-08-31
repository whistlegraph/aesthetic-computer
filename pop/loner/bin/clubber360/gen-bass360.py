import numpy as np
import os
S=os.environ.get("CLUB360_WORK") or os.path.expanduser("~/.cache/ac/clubber360")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT
G=0.10
DURATION=360.0
NT=int(DURATION*sr)
def T(b): return G+b*BAR
root0=58.27                  # A#1 — the pedal; the record's own bass carries the changes
DROP=88

# ---- SUB PEDAL: plays only between her passes, and grows up as it goes ----
# (during the real-speed passes the v4pid bass stem takes the low road)
PED=[(8,16,"sparse"),(32,40,"eighths"),(56,72,"eighths"),
     (104,120,"octaves"),(152,168,"sparse")]
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
        bar=int(b); beat=round((b-bar)*4)
        oct_up=(mode=="octaves" and bar%2==1 and beat==3)
        note(T(b)+BEAT/2,root0*(2 if oct_up else 1),BEAT*0.34,0.30 if not oct_up else 0.22)
        b+=step
np.stack([sub,sub],1).astype(np.float32).tofile(f"{S}/c360-sub.raw")
print("bass: pedal laid between the passes")

# ---- WUB: shallow first pass, deeper as she rises, wild after the drop ----
# (the vocal-keyed sidechain now lives in the assembler, against the whole voice bus)
wub=np.zeros(NT)
for (b0,b1,depth,g) in [(32,40,0.40,0.11),(56,72,0.50,0.13),(DROP,152,0.60,0.15)]:
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
        i=int(bt*sr); j=min(n,i+int(0.09*sr))
        duck[i:j]*=1-0.7*np.exp(-np.arange(j-i)/(0.03*sr))
    y*=duck*g
    e_=int(BAR*sr)
    y[:e_]*=np.linspace(0,1,e_); y[-e_:]*=np.linspace(1,0,e_)
    wub[a:z]+=y
np.stack([wub,wub],1).astype(np.float32).tofile(f"{S}/c360-wub.raw")
print("bass: wub in three passes")
