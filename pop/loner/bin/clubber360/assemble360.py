import numpy as np, subprocess
import os
from scipy.signal import lfilter
S=os.environ.get("CLUB360_WORK") or os.path.expanduser("~/.cache/ac/clubber360")
V=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
sr=48000
BEAT=60.0/122; BAR=4*BEAT
G=0.10
DURATION=360.0
NT=int(DURATION*sr)
def T(b): return G+b*BAR
T0=G+4*BAR-4*0.3654
DROP=88
def raw2(p): return np.fromfile(p,np.float32).reshape(-1,2).astype(np.float64)
def wav2(p,dur=None):
    cmd=["ffmpeg","-v","error","-i",p]
    if dur: cmd+=["-t",f"{dur:.3f}"]
    cmd+=["-ar",str(sr),"-ac","2","-f","f32le","-"]
    r=subprocess.run(cmd,capture_output=True).stdout
    return np.frombuffer(r,np.float32).reshape(-1,2).astype(np.float64)
def envelope(pts):
    t=np.arange(NT)/sr
    return np.interp(t,[p[0] for p in pts],[p[1] for p in pts])[:,None]
def place(x,deg=0.0,depth=0.0):
    m=x.mean(axis=1)
    itd=int(abs(deg)/40.0*0.0006*sr)
    ild=10**(-abs(deg)/40.0*3.0/20.0)
    L=m.copy(); R=m.copy()
    if deg>0:
        L=np.concatenate([np.zeros(itd),m[:-itd]]) if itd else m.copy()
        L*=ild
    elif deg<0:
        R=np.concatenate([np.zeros(itd),m[:-itd]]) if itd else m.copy()
        R*=ild
    out=np.stack([L,R],1)
    if depth>0:
        a=1-np.exp(-2*np.pi*(9000-6500*depth)/sr)
        out=lfilter([a],[1,-(1-a)],out,axis=0)*(1-0.25*depth)
    return out

# ---- PUMP: the floor breathes everything melodic; the break floats free ----
nb=int(BEAT*sr)
tb=np.arange(nb)/sr
beatduck=1-np.exp(-tb/0.11)
beatduck[:int(0.008*sr)]=np.linspace(1,beatduck[int(0.008*sr)],int(0.008*sr))
kick_on=np.zeros(NT,bool)
b=0.0
while b<176:
    if not (72<=b<DROP):
        a=int(T(b)*sr); kick_on[a:min(NT,a+nb)]=True
    b+=0.25
def pump(depth):
    g=np.ones(NT)
    b=0.0
    while b<176:
        if not (72<=b<DROP):
            a=int(T(b)*sr); z=min(NT,a+nb)
            g[a:z]=1-depth*(1-beatduck[:z-a])
        b+=0.25
    return g[:,None]

# ---- BREATH: a rest in the last half-bar of each 8-bar phrase ----
def gate(windows,ramp_s=0.05):
    g=np.ones(NT)
    ramp=int(ramp_s*sr)
    for (t0,t1,depth) in windows:
        lo=max(0,int(t0*sr)); hi=min(NT,int(t1*sr))
        if hi<=lo: continue
        seg=np.full(hi-lo,depth)
        r=min(ramp,(hi-lo)//2)
        if r:
            seg[:r]=np.linspace(1,depth,r); seg[-r:]=np.linspace(depth,1,r)
        g[lo:hi]=np.minimum(g[lo:hi],seg)
    return g[:,None]
RESTS=[]
k=0
for ph in range(1,22):
    t8=T(ph*8)
    RESTS.append((t8-BAR,t8-BAR/2,0.34 if k%2==0 else 0.52)); k+=1
BREATH=gate(RESTS)

mix=np.zeros((NT,2),np.float32)
def add(x,env=None,gain=1.0,at=0.0):
    a=int(at*sr)
    n=min(len(x),NT-a)
    seg=x[:n]*gain
    if env is not None: seg=seg*env[a:a+n]
    mix[a:a+n]+=seg.astype(np.float32)

STRDUR=DURATION-T0
add(raw2(f"{S}/c360-kick.raw"),envelope([(0,0.76),(T(16),0.82),(T(48),0.86),(T(DROP),0.92),(T(152),0.88),(T(176),0.82)]))
add(place(raw2(f"{S}/c360-perc.raw"),+18,0.05),
    envelope([(0,0),(T(4),0.10),(T(8),0.45),(T(16),0.68),(T(48),0.75),(T(72),0.30),(T(DROP),0.78),(T(152),0.62),(T(168),0.45),(T(176),0.22),(DURATION,0.12)])*BREATH)
add(raw2(f"{S}/c360-fills.raw"))
add(raw2(f"{S}/c360-sub.raw"),envelope([(0,0.95),(DURATION,0.95)]))
add(raw2(f"{S}/c360-wub.raw"))
add(wav2(f"{S}/str-vocalsFX.wav",STRDUR),
    envelope([(T0,0),(T(8),0.50),(T(12),0.92),(T(72),1.00),(T(DROP),0.92),(DURATION,0.92)])*pump(0.18),1.02,at=T0)
add(place(wav2(f"{S}/str-pads.wav",STRDUR),0,0.35),
    envelope([(T0,0.15),(T(16),0.45),(T(32),0.60),(T(48),0.70),(T(72),0.92),(T(DROP),0.75),(T(152),0.82),(DURATION,0.85)])*pump(0.40)*BREATH,0.95,at=T0)
add(place(wav2(f"{S}/str-bells.wav",STRDUR),+30,0.25),
    envelope([(0,0),(T(32)-0.05,0),(T(32),0.40),(T(DROP),0.52),(DURATION,0.52)])*pump(0.30)*BREATH,at=T0)
add(place(wav2(f"{S}/str-pluck.wav",STRDUR),-25,0.15),
    envelope([(T0,0.20),(T(16),0.52),(T(72),0.30),(T(DROP),0.62),(DURATION,0.55)])*pump(0.35)*BREATH,at=T0)
add(place(wav2(f"{S}/str-piano.wav",STRDUR),-15,0.20),
    envelope([(0,0),(T(48),0),(T(48)+0.1,0.35),(T(152),0.35),(T(160),0.20),(DURATION,0.20)])*pump(0.30)*BREATH,at=T0)
add(place(wav2(f"{S}/str-bass.wav",STRDUR),0,0),
    envelope([(T0,0.40),(T(8),0.55),(T(72),0.72),(T(DROP),0.55),(DURATION,0.55)])*pump(0.50),at=T0)

# jeffrey's spoken stamp arrives at the last full section, unslowed
st=raw2(f"{V}/stem-stamp.raw")
nz=np.nonzero(np.abs(st).max(axis=1)>1e-4)[0]
if len(nz):
    seg=st[max(0,nz[0]-int(0.05*sr)):nz[-1]+int(0.2*sr)]
    add(seg,at=T(166))

fade_in=int(0.025*sr)
mix[:fade_in]*=np.linspace(0,1,fade_in)[:,None].astype(np.float32)
fa=int(T(178)*sr); fz=int(359.5*sr)
mix[fa:fz]*=np.linspace(1,0,fz-fa)[:,None].astype(np.float32)**1.5
mix[fz:]=0
pk=float(np.abs(mix).max())
if pk>0.85: mix*=0.85/pk
print(f"premaster peak {pk:.3f} -> normalized to 0.85")
mix.astype(np.float32).tofile(f"{S}/premaster360.raw")
print("clubber360 assembled: 183 bars, six minutes exactly")
