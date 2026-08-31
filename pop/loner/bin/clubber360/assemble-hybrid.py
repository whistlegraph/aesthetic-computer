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
ORG=0.3654                    # v4pid's grid origin inside the sacred audio
P2,P3,END=31.83,63.30,94.2
T0=G+4*BAR-4*ORG              # where the 0.25x pad bed sits, grid-locked
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
def cut(x,t0,t1,fi=0.05,fo=0.30):
    a=max(0,int(t0*sr)); z=min(len(x),int(t1*sr))
    s=x[a:z].copy()
    ni=int(fi*sr); no=int(fo*sr)
    if ni and len(s)>ni: s[:ni]*=np.linspace(0,1,ni)[:,None]
    if no and len(s)>no: s[-no:]*=np.linspace(1,0,no)[:,None]
    return s

# ---- PUMP: the floor breathes everything melodic; the break floats free ----
nb=int(BEAT*sr)
tb_=np.arange(nb)/sr
beatduck=1-np.exp(-tb_/0.11)
beatduck[:int(0.008*sr)]=np.linspace(1,beatduck[int(0.008*sr)],int(0.008*sr))
def pump(depth):
    g=np.ones(NT)
    b=0.0
    while b<176:
        if not (72<=b<DROP):
            a=int(T(b)*sr); z=min(NT,a+nb)
            g[a:z]=1-depth*(1-beatduck[:z-a])
        b+=0.25
    return g[:,None]
PUMP_L=pump(0.12); PUMP_M=pump(0.30); PUMP_H=pump(0.40)

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
for ph in range(1,22):
    t8=T(ph*8)
    RESTS.append((t8-BAR,t8-BAR/2,0.34 if ph%2==1 else 0.52))
BREATH=gate(RESTS)

mix=np.zeros((NT,2),np.float32)
vbus=np.zeros((NT,2),np.float32)
def addto(buf,x,env=None,gain=1.0,at=0.0):
    a=int(at*sr)
    n=min(len(x),NT-a)
    if n<=0: return
    seg=x[:n]*gain
    if env is not None: seg=seg*env[a:a+n]
    buf[a:a+n]+=seg.astype(np.float32)
def add(x,env=None,gain=1.0,at=0.0): addto(mix,x,env,gain,at)

# ---- HER: real-speed passes at the act doors; smears between ----
vox=wav2(f"{V}/vocalsFX.wav")
PASSES=[((0,P2),T(16)-ORG),((P2-0.2,P3),T(40)-0.2),((P3-0.2,END),T(88)-0.2),
        ((0,P2),T(120)-ORG),((P3-0.2,END),T(136)-0.2)]
for (w,at) in PASSES:
    addto(vbus,cut(vox,*w),PUMP_L,1.02,at=at)
del vox
str2=wav2(f"{S}/str2-vocalsFX.wav")
# the smears run hot: vocalsFX is a stem voiced to sit inside a full mix,
# and out here it is the only singer in the room
addto(vbus,cut(str2,0,2*7.87,fi=1.0,fo=1.5),PUMP_M,1.10,at=T(8)-2*ORG)          # the tease
addto(vbus,cut(str2,2*P2,2*(P2+15.74),fi=1.0,fo=1.5),PUMP_M,1.00,at=T(104))     # the weave
addto(vbus,cut(str2,2*(ORG+44*BAR),2*END,fi=1.0,fo=2.0),PUMP_M,1.30,at=T(152))  # the tail
del str2
str4=wav2(f"{S}/str-vocalsFX.wav")
arps=cut(str4,4*P3,4*(P3+15.74),fi=1.5,fo=1.5)                                   # her arps, 0.25x
# the record's own ending, quarter speed, carries the last minute out
addto(vbus,cut(str4,4*(ORG+44*BAR),4*END,fi=2.0,fo=0.5),PUMP_L,1.60,at=T(168))
del str4
arps_env=envelope([(T(56),1.10),(T(72),2.10),(T(DROP)-0.1,2.40),(T(DROP),0.0)])
addto(vbus,arps,arps_env,1.0,at=T(56))
del arps
mix+=vbus

# ---- THE RECORD UNDER HER: pads/pluck/bells/piano/bass ride each pass ----
pads=raw2(f"{V}/st-pads.raw"); pluck=raw2(f"{V}/st-pluck.raw")
bells=raw2(f"{V}/st-bells.raw"); piano=raw2(f"{V}/st-piano.raw")
bass=wav2(f"{V}/sep4/htdemucs/v4pid-trim/bass.wav")
for (w,at) in PASSES:
    finale=w[0]>=P3-0.5
    add(place(cut(pads,*w),0,0.35),PUMP_H*BREATH,0.55,at=at)
    add(place(cut(pluck,*w),-25,0.15),PUMP_M*BREATH,0.50,at=at)
    add(cut(bass,*w),PUMP_M,0.85 if not finale else 0.95,at=at)
    if finale:
        add(place(cut(bells,*w),+30,0.25),PUMP_M*BREATH,0.50,at=at)
        add(place(cut(piano,*w),-15,0.20),PUMP_M*BREATH,0.35,at=at)
del pads,pluck,bells,piano,bass

# ---- the 0.25x pad bed drifts under the whole night, grid-locked ----
strpads=wav2(f"{S}/str-pads.wav",DURATION-T0)
add(place(strpads,0,0.35),
    envelope([(T0,0.12),(T(8),0.30),(T(16),0.18),(T(32),0.40),(T(40),0.22),
              (T(56),0.80),(T(72),1.30),(T(DROP),0.28),(T(104),0.48),(T(120),0.28),
              (T(152),0.70),(T(168),0.95),(DURATION,1.0)])*PUMP_H,0.95,at=T0)
del strpads

# ---- THE FLOOR ----
add(raw2(f"{S}/c360-kick.raw"),envelope([(0,0.78),(T(16),0.82),(T(56),0.86),(T(DROP),0.92),(T(152),0.86),(T(176),0.80)]))
add(place(raw2(f"{S}/c360-perc.raw"),+18,0.05),
    envelope([(0,0.55),(T(16),0.75),(T(40),0.85),(T(DROP),0.95),(T(160),0.7),(T(176),0.4),(DURATION,0.2)])*BREATH)
add(raw2(f"{S}/c360-fills.raw"))
add(raw2(f"{S}/c360-sub.raw"),envelope([(0,0.95),(DURATION,0.95)]))
# the wub swells only in the gaps the whole voice bus leaves
hop=int(0.001*sr)
e=np.abs(vbus.mean(axis=1))[:NT//hop*hop].reshape(-1,hop).max(axis=1)
atk=1-np.exp(-1/15.0); rel=1-np.exp(-1/230.0)
f=0.0; fo=np.empty(len(e))
for i in range(len(e)):
    c=atk if e[i]>f else rel
    f+=c*(e[i]-f); fo[i]=f
fo/=max(1e-9,np.percentile(fo,98))
duckvox=np.interp(np.arange(NT),np.arange(len(fo))*hop,np.clip(fo,0,1))[:,None]
add(raw2(f"{S}/c360-wub.raw")*(1-0.28*duckvox))

# jeffrey's spoken stamp arrives in the last descent, unslowed
st=raw2(f"{V}/stem-stamp.raw")
nz=np.nonzero(np.abs(st).max(axis=1)>1e-4)[0]
if len(nz):
    add(st[max(0,nz[0]-int(0.05*sr)):nz[-1]+int(0.2*sr)],at=T(166))

fade_in=int(0.025*sr)
mix[:fade_in]*=np.linspace(0,1,fade_in)[:,None].astype(np.float32)
fa=int(T(178)*sr); fz=int(359.5*sr)
mix[fa:fz]*=(np.linspace(1,0,fz-fa)[:,None]**1.5).astype(np.float32)
mix[fz:]=0
pk=float(np.abs(mix).max())
if pk>0.85: mix*=0.85/pk
print(f"premaster peak {pk:.3f} -> normalized to 0.85")
mix.astype(np.float32).tofile(f"{S}/premaster360.raw")
print("hybrid assembled: her passes at the doors, her smears between")
