import numpy as np, subprocess
import os
from scipy.signal import lfilter
S=os.environ.get("RAPID_WORK") or os.path.expanduser("~/.cache/ac/rapid")
V=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
sr=48000
BPM=float(os.environ.get("RAPID_BPM","156"))
BEAT=60.0/BPM; BAR=4*BEAT
G=0.10
BARS=88
NT=int((G+BARS*BAR+1.0)*sr)
def T(b): return G+b*BAR
RT=122.0/BPM                  # the squeeze: one v4pid bar = one rapid bar
ORG=0.3654                    # v4pid's grid origin inside the sacred audio
B122=4*60.0/122
def SQ(s): return RT*(ORG+s*B122)   # source bar s in the squeezed files
def RS(s): return ORG+s*B122        # source bar s at real speed
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
    if len(m)==0: return np.zeros((0,2))
    itd=int(abs(deg)/40.0*0.0006*sr)
    if itd>=len(m): itd=0
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
def cut(x,t0,t1,fi=0.05,fo=0.20):
    a=max(0,int(t0*sr)); z=min(len(x),int(t1*sr))
    s=x[a:z].copy()
    ni=int(fi*sr); no=int(fo*sr)
    if ni and len(s)>ni: s[:ni]*=np.linspace(0,1,ni)[:,None]
    if no and len(s)>no: s[-no:]*=np.linspace(1,0,no)[:,None]
    return s

# ---- PUMP: the floor breathes everything melodic; the break floats free ----
nb=int(BEAT*sr)
tb_=np.arange(nb)/sr
beatduck=1-np.exp(-tb_/0.085)
beatduck[:int(0.008*sr)]=np.linspace(1,beatduck[int(0.008*sr)],int(0.008*sr))
def pump(depth):
    g=np.ones(NT)
    b=0.0
    while b<BARS:
        if not (36<=b<44):
            a=int(T(b)*sr); z=min(NT,a+nb)
            g[a:z]=1-depth*(1-beatduck[:z-a])
        b+=0.25
    return g[:,None]
PUMP_L=pump(0.12); PUMP_M=pump(0.30); PUMP_H=pump(0.42)

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
for ph in range(1,11):
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

# ---- the map: which squeezed source bars land at which door ----
# (src0, src1, dest, vox, bass, pads, pluck, bells, piano)
SEGS=[(0,16,4,   1.02,0.85,0.32,0.50,0.0 ,0.0 ),   # sprint I
      (16,32,20, 1.02,0.85,0.55,0.50,0.0 ,0.0 ),   # sprint II
      (32,44,44, 1.05,0.95,0.55,0.50,0.50,0.35),   # the drop
      (0,16,56,  1.02,0.90,0.50,0.55,0.50,0.35),   # reprise: the hook at speed
      (36,44,72, 1.08,0.95,0.50,0.50,0.55,0.0 ),   # climb: the wildest bars
      (44,47.9,80,1.00,0.40,0.45,0.0 ,0.35,0.0 )]  # outro: the natural ending

# ---- HER: squeezed onto the sprint grid; real speed once, in the break ----
sqvox=wav2(f"{S}/sq-vocalsFX.wav")
addto(vbus,cut(sqvox,SQ(0),SQ(2),fi=0.30,fo=0.40),None,0.90,at=T(2))   # the tease
for (s0,s1,d,gv,*_) in SEGS:
    addto(vbus,cut(sqvox,SQ(s0),SQ(s1)),PUMP_L,gv,at=T(d))
del sqvox
vox=wav2(f"{V}/vocalsFX.wav",RS(7))
# the break: "sitting curled up in myself" unhurried while the world waits
addto(vbus,cut(vox,RS(0),RS(6.25),fi=0.40,fo=0.80),None,1.10,at=T(36))
del vox
mix+=vbus

# ---- THE RECORD UNDER HER: squeezed beds ride each segment ----
beds={"bass":wav2(f"{S}/sq-bass.wav"),"pads":wav2(f"{S}/sq-pads.wav"),
      "pluck":wav2(f"{S}/sq-pluck.wav"),"bells":wav2(f"{S}/sq-bells.wav"),
      "piano":wav2(f"{S}/sq-piano.wav")}
for (s0,s1,d,gv,gb,gpa,gpl,gbe,gpi) in SEGS:
    w=(SQ(s0),SQ(s1))
    if gb: add(cut(beds["bass"],*w),PUMP_M,gb,at=T(d))
    if gpa: add(place(cut(beds["pads"],*w),0,0.35),PUMP_H*BREATH,gpa,at=T(d))
    if gpl: add(place(cut(beds["pluck"],*w),-25,0.15),PUMP_M*BREATH,gpl,at=T(d))
    if gbe: add(place(cut(beds["bells"],*w),+30,0.25),PUMP_M*BREATH,gbe,at=T(d))
    if gpi: add(place(cut(beds["piano"],*w),-15,0.20),PUMP_M*BREATH,gpi,at=T(d))
# bells and piano join sprint II halfway, at the bar-28 door
add(place(cut(beds["bells"],SQ(24),SQ(32)),+30,0.25),PUMP_M*BREATH,0.45,at=T(28))
add(place(cut(beds["piano"],SQ(24),SQ(32)),-15,0.20),PUMP_M*BREATH,0.30,at=T(28))
# the break's bed: squeezed pads droning deep while she takes her time
add(place(cut(beds["pads"],SQ(32),SQ(40),fi=0.8,fo=0.8),0,0.55),BREATH,0.40,at=T(36))
del beds

# ---- THE FLOOR ----
add(raw2(f"{S}/rapid-kick.raw"),envelope([(0,0.80),(T(20),0.84),(T(44),0.92),(T(72),0.94),(T(80),0.86),(T(88),0.80)]))
add(place(raw2(f"{S}/rapid-perc.raw"),+18,0.05),
    envelope([(0,0.6),(T(20),0.8),(T(44),0.95),(T(80),0.6),(T(88),0.4)])*BREATH)
add(raw2(f"{S}/rapid-fills.raw"))
add(raw2(f"{S}/rapid-sub.raw"))
# the wub swells only in the gaps the voice bus leaves
hop=int(0.001*sr)
e=np.abs(vbus.mean(axis=1))[:NT//hop*hop].reshape(-1,hop).max(axis=1)
atk=1-np.exp(-1/15.0); rel=1-np.exp(-1/230.0)
f=0.0; fo=np.empty(len(e))
for i in range(len(e)):
    c=atk if e[i]>f else rel
    f+=c*(e[i]-f); fo[i]=f
fo/=max(1e-9,np.percentile(fo,98))
duckvox=np.interp(np.arange(NT),np.arange(len(fo))*hop,np.clip(fo,0,1))[:,None]
add(raw2(f"{S}/rapid-wub.raw")*(1-0.28*duckvox))

# jeffrey's spoken stamp arrives in the outro, unslowed
st=raw2(f"{V}/stem-stamp.raw")
nz=np.nonzero(np.abs(st).max(axis=1)>1e-4)[0]
if len(nz):
    add(st[max(0,nz[0]-int(0.05*sr)):nz[-1]+int(0.2*sr)],at=T(82))

fade_in=int(0.025*sr)
mix[:fade_in]*=np.linspace(0,1,fade_in)[:,None].astype(np.float32)
# the last click cuts the record: 180 ms past the bar-88 door, 30 ms fade
end=int((T(88)+0.18)*sr)
fz=int(0.03*sr)
mix[end-fz:end]*=np.linspace(1,0,fz)[:,None].astype(np.float32)
mix=mix[:end]
pk=float(np.abs(mix).max())
if pk>0.85: mix*=0.85/pk
print(f"premaster peak {pk:.3f} -> normalized to 0.85, {end/sr:.2f}s")
mix.astype(np.float32).tofile(f"{S}/premaster-rapid.raw")
print("rapid assembled: the sprint, her one patient breath, the cut")
