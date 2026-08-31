import numpy as np
import os
from scipy.signal import lfilter
S=os.environ.get("CLUB360_WORK") or os.path.expanduser("~/.cache/ac/clubber360")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT; SIX=BEAT/4
G=0.10
DURATION=360.0
NT=int(DURATION*sr)
def T(b): return G+b*BAR
# ---- the story in floor bars: the rhythm assembles, transcends, disassembles
DROP=88
KICKLESS=(72,DROP)          # v4's identity: the record keeps its kickless break
KICK_END=176
DOORS=[16,32,40,56,72,88,104,120,136,152,176]
TURNS={31,55,103,135,151}   # the bar before an act door leans in
SYNCO=112                   # one winking offbeat bar mid-peak
def act(b):
    if b<32: return "restrained"   # closed hats only; groove holds its breath
    if b<56: return "swing"        # the kit opens: exhale hats, rim answers
    if b<72: return "rising"       # her slow arps climb; floor sharpens
    if b<DROP: return "break"      # rhythm reduced to its seed: the shaker
    if b<152: return "club"        # everything it learned, at once
    return "outro"                 # peel in reverse order of arrival
rng=np.random.default_rng(360)
def eager(): return float(np.clip(rng.normal(-0.006,0.007),-0.018,0.006))
def vel(b,s=0.25): return b*float(np.clip(rng.normal(1.0,s),0.5,1.6))

# ---- the v4pid instruments, unchanged voices at a new length ----
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

# ---- KICK: four on the floor, with turns at the doors ----
K=kick5()
kickbuf=np.zeros(NT)
def putk(b,g):
    a=int(T(b)*sr); z=min(NT,a+len(K))
    if 0<=a<NT: kickbuf[a:z]+=K[:z-a]*g
b=0.0
while b<KICK_END:
    bar=int(b); beat=round((b-bar)*4)
    if KICKLESS[0]<=b<KICKLESS[1]:
        b+=0.25; continue
    if bar==SYNCO:
        if beat==0:
            for off in (0.125,0.375,0.625,0.875):
                putk(bar+off,0.44)
        b+=0.25; continue
    g=0.56*(1.05 if beat==0 else 1.0)
    putk(b,g)
    if bar in TURNS and beat==3:
        putk(b+0.125,0.40)          # the extra eighth that leans into the door
    b+=0.25
np.stack([kickbuf,kickbuf],1).astype(np.float32).tofile(f"{S}/c360-kick.raw")
print("floor: kick laid, turns at the doors")

# ---- PERC: the eager hand, learning the room act by act ----
lay=np.zeros((NT,2))
def put(y,t,g,pan=0.0):
    a=int(t*sr); z=min(NT,a+len(y))
    if a<0 or a>=NT: return
    s=y[:z-a]*g
    lay[a:z,0]+=s*(1-max(0,pan)); lay[a:z,1]+=s*(1-max(0,-pan))
SW=0.60
RATES=[1.0,1.26,0.84,1.12,0.94,1.34,1.0,0.89]
base=hat(0.05,7500,seed=5); ghost=hat(0.035,7500,seed=6); openh=hat(0.22,6000,seed=7)
k=0
while True:                          # closed eighths + swung ghosts
    tb=G+k*SIX*2
    if tb>T(KICK_END): break
    bnow=(tb-G)/BAR; a_=act(bnow)
    if a_=="break" or bnow<8:
        k+=1; continue
    r=RATES[k%8]
    accent=1.25 if (k%4==2) else 1.0
    put(repitch(base,r),tb+eager(),vel(0.075*accent),0.12)
    put(repitch(ghost,r*1.19),tb+SIX*2*(SW+rng.normal(0,0.02))+eager(),vel(0.04),-0.18)
    if a_ in ("swing","rising","club") and k%8==4:
        put(repitch(openh,0.92),tb+eager(),vel(0.065),0.0)     # the 2-bar exhale
    if a_=="rising" and k%4==1:
        put(repitch(openh,1.06),tb+eager(),vel(0.05),0.06)
    if a_=="club" and k%2==1:
        put(repitch(openh,1.06),tb+eager(),vel(0.058),0.06)    # every offbeat opens
    k+=1
SHK=[shaker(100+i) for i in range(12)]
k=0
while True:                          # the shaker: the rhythm's seed, never leaves
    ts=G+k*SIX
    if ts>T(KICK_END): break
    bnow=(ts-G)/BAR
    if bnow<8: k+=1; continue
    a_=act(bnow)
    depth={"restrained":0.30,"swing":0.5,"rising":0.7,"break":0.45,"club":0.7,"outro":0.4}[a_]
    wave=(1-depth/2)+depth/2*np.sin(k*np.pi/8+0.7)
    g=(0.028+0.030*wave)
    if 68<=bnow<72: g*=1.0+1.4*(bnow-68)/4     # gathering toward the break
    if 84<=bnow<DROP: g*=1.0+1.6*(bnow-84)/4   # gathering toward the drop
    if a_=="break": g*=0.85                     # alone with her, ticking
    if bnow>=160: g*=max(0.3,1-(bnow-160)/16)
    put(SHK[int(rng.integers(12))],ts+eager(),vel(g,0.35),0.22)
    k+=1
RIM=rim()
k=0
while True:                          # rims: sparse questions, then answers
    tb=G+k*BAR
    if tb>T(KICK_END): break
    bnow=(tb-G)/BAR; a_=act(bnow)
    if a_ in ("break",) or bnow<16 or bnow>=160:
        k+=1; continue
    if k%2==1:
        put(repitch(RIM,[1.0,1.19,0.89,1.33][k%4]),tb+2.5*BEAT+eager(),vel(0.075,0.3),-0.3)
    if k%8==6:
        put(repitch(RIM,1.5),tb+3.75*BEAT+eager(),vel(0.06),0.32)
    if a_=="club" and k%4==3:
        put(repitch(RIM,1.12),tb+1.75*BEAT+eager(),vel(0.065,0.3),0.28)
    k+=1
lay.astype(np.float32).tofile(f"{S}/c360-perc.raw")
print("floor: perc laid act by act")

# ---- DOORS: click rushes (a tick, eases in, no roll) + the drop build ----
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
rush(16,0.095,0.15)
rush(32,0.07,0.15,n=7)
rush(40,0.09,0.18)
rush(56,0.095,0.2)
rush(72,0.075,0.3)                  # into the break: softer, wider
rush(DROP,0.11,0.25,n=12,span=1.6)
rush(104,0.07,0.18,n=7)
rush(120,0.09,0.2)
rush(136,0.095,0.2)
rush(152,0.085,0.25)
rush(176,0.055,0.5,span=1.8)        # the last door: quietest and widest
SN=snare(); RK=K[::-1]
b=DROP-4.0
while b<DROP:                        # the roll under the last bars of the break
    step=0.5 if b<DROP-2 else 0.25
    frac=(b-(DROP-4))/4
    putf(SN,T(b)+eager(),0.04+0.13*frac**1.5,rng.normal(0,0.1))
    b+=step
putf(RK,T(DROP)-0.40,0.24)
fills.astype(np.float32).tofile(f"{S}/c360-fills.raw")
print("floor: doors clicked, drop rolled")
