import numpy as np
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT; SIX=BEAT/4
NT=int(97.2*sr)
rng=np.random.default_rng(21)
def eager():
    # ahead of the beat: mean -6ms, sd 7ms — an excited hand, never late-lazy
    return float(np.clip(rng.normal(-0.006,0.007),-0.018,0.006))
def vel(base,spread=0.25):
    return base*float(np.clip(rng.normal(1.0,spread),0.5,1.6))
def hat(dur,bright,seed=None):
    r=np.random.default_rng(seed if seed is not None else rng.integers(1e9))
    n=int(dur*sr)
    y=r.standard_normal(n)
    a=1-np.exp(-2*np.pi*bright/sr)
    hp=np.empty(n); acc=0.0
    for i in range(n): acc+=a*(y[i]-acc); hp[i]=y[i]-acc
    return hp*np.exp(-np.arange(n)/(dur*sr*0.25))
def repitch(y,rate):
    return np.interp(np.arange(0,len(y)-1,rate),np.arange(len(y)),y)
def rim():
    n=int(0.05*sr); t=np.arange(n)/sr
    return np.tanh(2.5*(np.sin(2*np.pi*820*t)*np.exp(-t/0.006)+np.sin(2*np.pi*1750*t)*np.exp(-t/0.004)*0.6))
def shaker(dur=0.07):
    n=int(dur*sr)
    y=rng.standard_normal(n)
    a=1-np.exp(-2*np.pi*9500/sr)
    hp=np.empty(n); acc=0.0
    for i in range(n): acc+=a*(y[i]-acc); hp[i]=y[i]-acc
    env=np.exp(-np.arange(n)/(dur*sr*0.35))
    return hp*env
lay=np.zeros((NT,2))
def put(y,t,g,pan=0.0):
    a=int(t*sr); b=min(NT,a+len(y))
    if a<0 or a>=NT: return
    s=y[:b-a]*g
    lay[a:b,0]+=s*(1-max(0,pan)); lay[a:b,1]+=s*(1-max(0,-pan))
SW=0.60
RATES=[1.0,1.26,0.84,1.12,0.94,1.34,1.0,0.89]
base=hat(0.05,7500,seed=5); ghost=hat(0.035,7500,seed=6)
GRID0=0.3654
k=0
while True:
    tb=GRID0+k*SIX*2
    if tb>92.6: break
    r=RATES[k%8]
    accent=1.25 if (k%4==2) else 1.0          # the and-of-2/4 leans hot
    put(repitch(base,r),tb+eager(),vel(0.075*accent),0.12)
    put(repitch(ghost,r*1.19),tb+SIX*2*(SW+rng.normal(0,0.02))+eager(),vel(0.04),-0.18)
    if k%8==4:
        put(repitch(hat(0.22,6000),0.92),tb+eager(),vel(0.065),0.0)
    k+=1
# SHAKER — every 16th, velocity waves like a wrist, eagerly ahead
k=0
while True:
    ts=GRID0+k*SIX
    if ts>92.6: break
    wave=0.5+0.5*np.sin(k*np.pi/8+0.7)
    g=0.028+0.030*wave
    put(shaker(),ts+eager(),vel(g,0.35),0.22)
    k+=1
# RIM accents — sparse, syncopated, walking pitches
k=0
while True:
    tb=GRID0+k*BAR
    if tb>91.0: break
    if k%2==1:
        put(repitch(rim(),[1.0,1.19,0.89,1.33][k%4]),tb+2.5*BEAT+eager(),vel(0.075,0.3),-0.3)
    if k%8==6:
        put(repitch(rim(),1.5),tb+3.75*BEAT+eager(),vel(0.06),0.32)
    k+=1
env=np.interp(np.arange(NT)/sr,[0,15.7,19.7,31.83,33.8,97.2],[0,0,0.5,0.6,1,1])[:,None]
(lay*env).astype(np.float32).tofile(f"{S}/st-swing.raw")
print("perc: eager, stochastic, swung, shaker+rim")
# (the 5ms fills nudge that followed here was overwritten by the fills rebuild)
