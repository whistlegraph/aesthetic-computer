import numpy as np
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000; BEAT=60.0/122; BAR=4*BEAT; SIX=BEAT/4
DURATION=94.2
NT=int(DURATION*sr)
CH={'i':[0,3,7,10],'III':[3,7,10,14],'VI':[-4,0,3,6],'VII':[-2,2,5,9]}
VERSE=['i','i','VI','VI','III','III','VII','VII']
HOOK=['VI','VII','i','i','VI','VII','III','VII']
LINE=[(0,7),(2,5),(4,3),(5.91,2),(8,0),(10,5),(11.5,2),(14,-2),(16,-5),(18,12),
      (22,10),(23.75,5),(28,2),(32,3),(34,2),(36,0),(38,-2),(40,7),(42,5),(44,3),
      (46,5),(48,7),(52,3),(56,3)]
def chord_of(t):
    if t<31.83: bar=16+(t-0.3654)/BAR; row=HOOK
    elif t<63.30: bar=48+(t-31.83)/BAR; row=VERSE
    else: bar=64+(t-63.30)/BAR; row=HOOK
    return CH[row[int(bar)%8]]
def melody_st(t):
    for p0 in (63.30,31.83,0.3654):
        if t>=p0:
            lb=(t-p0)/BEAT
            if lb>=60: return None
            st=None
            for (b,s) in LINE:
                if lb>=b: st=s
            return st
    return None
def pnote(f0,dur,gain):
    n=int(dur*sr); t=np.arange(n)/sr
    y=np.zeros(n)
    for m,a in ((1,1.0),(2,0.42),(3,0.2),(4.2,0.09),(5.5,0.05)):
        y+=a*np.sin(2*np.pi*f0*m*t)*np.exp(-t*(1.2+0.8*m))
    atk=int(0.008*sr); y[:atk]*=np.linspace(0,1,atk)
    acc=0.0; out=np.empty(n)
    aa=1-np.exp(-2*np.pi*3200/sr)
    for i in range(n): acc+=aa*(y[i]-acc); out[i]=acc
    return out*gain
rng=np.random.default_rng(9)
piano=np.zeros((NT,2))
BASE=233.08/2
# LEFT HAND — held low chords, continuous, even lower (A#1-2)
t=0.3654+4*BAR
while t<91.0:
    tones=chord_of(t)
    for st,g,off in ((tones[0]-24,0.06,0.0),(tones[2]-24,0.045,0.03)):
        y=pnote(BASE*2**(st/12.0),min(2*BAR,4.5),g)
        aa=int((t+off)*sr); bb=min(NT,aa+len(y))
        piano[aa:bb,0]+=y[:bb-aa]*0.55; piano[aa:bb,1]+=y[:bb-aa]*0.45
    t+=2*BAR
# RIGHT HAND — continuous wander that leans on the sung melody
t=0.3654+8*BAR
while t<91.0:
    tones=chord_of(t)
    scale=sorted(set([s%12 for s in tones]+[0,3,5,7,10]))
    ms=melody_st(t)
    cur=int(ms)+12 if ms is not None else int(rng.choice(tones))+12
    tt=t
    nn=rng.integers(7,13)
    for i in range(nn):
        last=(i==nn-1)
        dur=(rng.choice([0.5,0.5,1.0,1.5]) if not last else rng.choice([1.5,2.0,3.0]))*BEAT
        f0=BASE*2**(cur/12.0)
        y=pnote(f0,min(dur*2.2,3.6),0.055 if not last else 0.07)
        aa=int(tt*sr); bb=min(NT,aa+len(y))
        pan=0.15*np.sin(i*1.3)
        piano[aa:bb,0]+=y[:bb-aa]*(1-max(0,pan)); piano[aa:bb,1]+=y[:bb-aa]*(1-max(0,-pan))
        tt+=dur
        ms=melody_st(tt)
        if ms is not None and rng.random()<0.6:
            cur=int(ms)+12            # lean back onto her line
        else:
            step=rng.choice([-2,-1,-1,1,1,2])
            idx=scale.index(cur%12) if cur%12 in scale else 0
            cur=(cur//12)*12+scale[(idx+step)%len(scale)]
        cur=int(np.clip(cur,1,17))
    t=tt+rng.choice([0.5,1.0])*BEAT     # barely a breath — continuous
piano.astype(np.float32).tofile(f"{S}/st-piano.raw")
print("piano: continuous, low chords, melody-leaning, quieter")

# pitched-around swing hats, quieter
rng2=np.random.default_rng(5)
def hat(dur,bright):
    n=int(dur*sr)
    y=rng2.standard_normal(n)
    a=1-np.exp(-2*np.pi*bright/sr)
    hp=np.empty(n); acc=0.0
    for i in range(n): acc+=a*(y[i]-acc); hp[i]=y[i]-acc
    return hp*np.exp(-np.arange(n)/(dur*sr*0.25))
def repitch(y,rate):
    return np.interp(np.arange(0,len(y)-1,rate),np.arange(len(y)),y)
SW=0.58
RATES=[1.0,1.26,0.84,1.12,0.94,1.34,1.0,0.89]   # the hats walk around in pitch
lay=np.zeros((NT,2))
t0=0.3654; k=0
base=hat(0.05,7500)
ghost=hat(0.035,7500)
while True:
    tb=t0+k*SIX*2
    if tb>92.6: break
    r=RATES[k%8]
    for (off,g,src,pan) in [(0.0,0.075,base,0.12),(SIX*2*SW,0.038,ghost,-0.18)]:
        y=repitch(src,r if off==0 else r*1.19)*g
        aa=int((tb+off)*sr); bb=min(NT,aa+len(y))
        if aa>=NT: break
        lay[aa:bb,0]+=y[:bb-aa]*(1-max(0,pan)); lay[aa:bb,1]+=y[:bb-aa]*(1-max(0,-pan))
    if k%8==4:
        y=repitch(hat(0.22,6000),0.92)*0.065
        aa=int(tb*sr); bb=min(NT,aa+len(y))
        lay[aa:bb,0]+=y[:bb-aa]; lay[aa:bb,1]+=y[:bb-aa]
    k+=1
env=np.interp(np.arange(NT)/sr,[0,15.7,19.7,31.83,33.8,97.2],[0,0,0.5,0.6,1,1])[:,None]
(lay*env).astype(np.float32).tofile(f"{S}/st-swing.raw")
print("hats: pitched around, quieter")
# (the drum-envelope edits that followed here now live in assemble.py)
