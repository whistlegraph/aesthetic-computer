import numpy as np
import os
from scipy.signal import lfilter
S=os.environ.get("RAPID_WORK") or os.path.expanduser("~/.cache/ac/rapid")
os.makedirs(S,exist_ok=True)
sr=48000
BPM=float(os.environ.get("RAPID_BPM","156"))
BEAT=60.0/BPM; BAR=4*BEAT; SIX=BEAT/4
G=0.10
BARS=88
DURATION=G+BARS*BAR+1.0
NT=int(DURATION*sr)
def T(b): return G+b*BAR
# ---- the story in floor bars: a sprint that holds its breath once ----
KICKLESS=(36,44)            # the break: she sings at real speed, world waits
DOORS=[4,20,28,36,44,56,72,80,88]
TURNS={19,35,55,71,79}      # the bar before a door leans in
def act(b):
    if b<4: return "intake"     # shaker seed + the rush in
    if b<20: return "sprint1"   # full floor from the first door
    if b<36: return "sprint2"   # kit opens: exhales, rim answers
    if b<44: return "break"     # kickless; the shaker ticks alone with her
    if b<56: return "drop"      # the finale material lands at speed
    if b<72: return "reprise"   # the hook again, every offbeat open
    if b<80: return "climb"     # wildest material, floor at full
    return "outro"              # peel in reverse until the kick stands alone
rng=np.random.default_rng(156)
def eager(): return float(np.clip(rng.normal(-0.005,0.006),-0.015,0.005))
def vel(b,s=0.25): return b*float(np.clip(rng.normal(1.0,s),0.5,1.6))

# ---- the v4pid instrument voices, unchanged, at a new speed ----
def kick5():
    dur=0.40; n=int(dur*sr); t=np.arange(n)/sr
    f=40+80*np.exp(-t/0.034)
    ph=2*np.pi*np.cumsum(f)/sr
    body=np.sin(ph)*np.exp(-t/0.17)
    second=np.sin(2*ph)*np.exp(-t/0.05)*0.22
    knock=np.sin(2*np.pi*150*t)*np.exp(-t/0.024)*0.30
    k=np.tanh(2.0*(body+second+knock))
    aa=1-np.exp(-2*np.pi*2200/sr)
    return lfilter([aa],[1,-(1-aa)],k)
def hat(dur,bright,seed=None):
    r=np.random.default_rng(seed if seed is not None else rng.integers(1e9))
    n=int(dur*sr)
    y=r.standard_normal(n)
    a=1-np.exp(-2*np.pi*bright/sr)
    hp=y-lfilter([a],[1,-(1-a)],y)
    return hp*np.exp(-np.arange(n)/(dur*sr*0.25))
def repitch(y,rate):
    return np.interp(np.arange(0,len(y)-1,rate),np.arange(len(y)),y)
def rim():
    n=int(0.05*sr); t=np.arange(n)/sr
    return np.tanh(2.5*(np.sin(2*np.pi*820*t)*np.exp(-t/0.006)+np.sin(2*np.pi*1750*t)*np.exp(-t/0.004)*0.6))
def shaker(seed):
    n=int(0.07*sr)
    r=np.random.default_rng(seed)
    y=r.standard_normal(n)
    a=1-np.exp(-2*np.pi*9500/sr)
    hp=y-lfilter([a],[1,-(1-a)],y)
    return hp*np.exp(-np.arange(n)/(n*0.35))
def snare():
    n=int(0.16*sr); t=np.arange(n)/sr
    noise=rng.standard_normal(n)*np.exp(-t/0.045)
    body=np.sin(2*np.pi*195*t)*np.exp(-t/0.03)*0.5
    a=1-np.exp(-2*np.pi*4200/sr)
    hp=noise-lfilter([a],[1,-(1-a)],noise)
    return np.tanh(1.5*(hp*0.8+body))
def click():
    n=int(0.035*sr); t=np.arange(n)/sr
    x=np.sin(2*np.pi*2600*t)+0.35*rng.standard_normal(n)
    a=1-np.exp(-2*np.pi*1800/sr)
    hp=x-lfilter([a],[1,-(1-a)],x)
    a=1-np.exp(-2*np.pi*7000/sr)
    band=lfilter([a],[1,-(1-a)],hp)
    tick=np.tanh(1.4*band*np.exp(-t/0.006))
    return tick/max(1e-9,np.max(np.abs(tick)))

# ---- KICK: four on the floor at a sprint, turns leaning into the doors ----
K=kick5()
kickbuf=np.zeros(NT)
def putk(b,g):
    a=int(T(b)*sr); z=min(NT,a+len(K))
    if 0<=a<NT: kickbuf[a:z]+=K[:z-a]*g
b=4.0
while b<BARS:
    bar=int(b); beat=round((b-bar)*4)
    if KICKLESS[0]<=b<KICKLESS[1]:
        b+=0.25; continue
    g=0.56*(1.05 if beat==0 else 1.0)
    if bar>=84: g*=1.0-0.3*(b-84)/4          # the peel: kick softens, stays
    putk(b,g)
    if bar in TURNS and beat==3:
        putk(b+0.125,0.40)
    b+=0.25
np.stack([kickbuf,kickbuf],1).astype(np.float32).tofile(f"{S}/rapid-kick.raw")
print("floor: kick sprinting, turns at the doors")

# ---- PERC: closed 16ths are the sprint's signature ----
lay=np.zeros((NT,2))
def put(y,t,g,pan=0.0):
    a=int(t*sr); z=min(NT,a+len(y))
    if a<0 or a>=NT: return
    s=y[:z-a]*g
    lay[a:z,0]+=s*(1-max(0,pan)); lay[a:z,1]+=s*(1-max(0,-pan))
SW=0.54
RATES=[1.0,1.26,0.84,1.12,0.94,1.34,1.0,0.89]
base=hat(0.04,7800,seed=5); ghost=hat(0.03,7800,seed=6); openh=hat(0.18,6200,seed=7)
k=0
while True:                          # closed 16ths, accents breathing in fours
    ts=G+k*SIX
    if ts>T(BARS): break
    bnow=(ts-G)/BAR; a_=act(bnow)
    if a_ in ("intake","break","outro") and not (a_=="outro" and bnow<84):
        k+=1; continue
    if a_=="outro" and bnow>=84:
        k+=1; continue
    r=RATES[(k//4)%8]
    accent=1.3 if k%4==0 else (1.1 if k%4==2 else 0.8)
    swung=SIX*SW*0.5 if (k%2==1 and a_ in ("sprint2","reprise","climb")) else 0.0
    put(repitch(base,r),ts+swung+eager(),vel(0.052*accent),0.12 if k%2 else -0.08)
    if a_ in ("sprint2","drop") and k%32==8:
        put(repitch(openh,0.92),ts+eager(),vel(0.06),0.0)      # the exhale
    if a_ in ("reprise","climb") and k%8==4:
        put(repitch(openh,1.06),ts+eager(),vel(0.055),0.06)    # offbeats open
    k+=1
SHK=[shaker(100+i) for i in range(12)]
k=0
while True:                          # the shaker: opens the record, never leaves
    ts=G+k*SIX
    if ts>T(BARS): break
    bnow=(ts-G)/BAR; a_=act(bnow)
    depth={"intake":0.5,"sprint1":0.45,"sprint2":0.6,"break":0.5,
           "drop":0.7,"reprise":0.7,"climb":0.75,"outro":0.45}[a_]
    wave=(1-depth/2)+depth/2*np.sin(k*np.pi/8+0.7)
    g=(0.026+0.028*wave)
    if a_=="intake": g*=0.5+0.5*bnow/4          # fades up out of nothing
    if 40<=bnow<44: g*=1.0+1.5*(bnow-40)/4      # gathering toward the drop
    if a_=="break": g*=0.85                      # alone with her, ticking
    if bnow>=84: g*=max(0.0,1-(bnow-84)/3.5)     # gone before the last click
    put(SHK[int(rng.integers(12))],ts+eager(),vel(g,0.35),0.22)
    k+=1
RIM=rim()
k=0
while True:                          # rims: answers, only once the kit is open
    tb=G+k*BAR
    if tb>T(BARS): break
    bnow=(tb-G)/BAR; a_=act(bnow)
    if a_ not in ("sprint2","reprise","climb"):
        k+=1; continue
    if k%2==1:
        put(repitch(RIM,[1.0,1.19,0.89,1.33][k%4]),tb+2.5*BEAT+eager(),vel(0.07,0.3),-0.3)
    if k%8==6:
        put(repitch(RIM,1.5),tb+3.75*BEAT+eager(),vel(0.055),0.32)
    k+=1
lay.astype(np.float32).tofile(f"{S}/rapid-perc.raw")
print("floor: sixteenths laid act by act")

# ---- DOORS: click rushes; the very last click CUTS the record ----
fills=np.zeros((NT,2))
def putf(y,t,g,pan=0.0):
    a=int(t*sr); z=min(NT,a+len(y))
    if a<0 or a>=NT: return
    s=y[:z-a]*g
    fills[a:z,0]+=s*(1-max(0,pan)); fills[a:z,1]+=s*(1-max(0,-pan))
CL=click()
def rush(door_bar,gain,width,n=9,span=1.25):
    d=T(door_bar)
    for i in range(n):
        frac=(i/(n-1))**1.6
        t=d-span*(1-frac)-0.02
        putf(CL,t,gain*(0.5+0.5*frac),width*np.sin(i*2.4))
rush(4,0.10,0.15,n=11,span=2.2)     # the way in: a long rush from silence
rush(20,0.095,0.15)
rush(28,0.07,0.15,n=7)
rush(36,0.075,0.3)                  # into the break: softer, wider
rush(44,0.115,0.25,n=12,span=1.6)   # the drop
rush(56,0.095,0.2)
rush(72,0.09,0.2)
rush(80,0.085,0.25,n=7)
rush(88,0.12,0.1,n=10,span=1.8)     # the cut: loudest, dead centre
SN=snare(); RK=K[::-1]
b=42.0
while b<44:                          # the roll under the break's last bars
    step=0.5 if b<43 else 0.25
    frac=(b-42)/2
    putf(SN,T(b)+eager(),0.04+0.13*frac**1.5,rng.normal(0,0.1))
    b+=step
putf(RK,T(44)-0.40,0.24)
fills.astype(np.float32).tofile(f"{S}/rapid-fills.raw")
print("floor: doors clicked, the last one armed to cut")
