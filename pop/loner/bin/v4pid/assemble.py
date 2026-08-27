import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT
NT=int(97.2*sr)
def raw2(p): return np.fromfile(p,np.float32).reshape(-1,2).astype(np.float64)
def wav2(p):
    r=subprocess.run(["ffmpeg","-v","error","-i",p,"-ar",str(sr),"-ac","2","-f","f32le","-"],capture_output=True).stdout
    return np.frombuffer(r,np.float32).reshape(-1,2).astype(np.float64)
def envelope(pts):
    t=np.arange(NT)/sr
    return np.interp(t,[p[0] for p in pts],[p[1] for p in pts])[:,None]

def place(x, deg=0.0, depth=0.0):
    """The stage: azimuth via ITD (<=0.6ms) + ILD, distance via a gentle
    high shelf cut and level — placement, not ping-pong."""
    m=x.mean(axis=1)
    itd=int(abs(deg)/40.0*0.0006*sr)
    ild=10**(-abs(deg)/40.0*3.0/20.0)
    L=m.copy(); R=m.copy()
    if deg>0:   # to the right: left ear later + quieter
        L=np.concatenate([np.zeros(itd),m[:-itd]]) if itd else m.copy()
        L*=ild
    elif deg<0:
        R=np.concatenate([np.zeros(itd),m[:-itd]]) if itd else m.copy()
        R*=ild
    out=np.stack([L,R],1)
    if depth>0:
        a=1-np.exp(-2*np.pi*(9000-6500*depth)/sr)
        acc=np.zeros(2)
        y=np.empty_like(out)
        for i in range(len(out)):
            acc+=a*(out[i]-acc); y[i]=acc
        out=y*(1-0.25*depth)
    return out

P2,P3=31.83,63.30
ENVS={
 "st-kick":[(0,0.72),(29.9,0.77),(31.83,0.9),(97.2,0.9)],
 "st-hats":[(0,0),(15.7,0),(19.7,0.55),(P2,0.55),(P2+2*BAR,0.75),(97.2,0.75)],
 "bass":[(0,0.7),(P2,0.75),(P2+2*BAR,1),(97.2,1)],
 "st-pluck":[(0,0.18),(15.7,0.18),(19.7,0.6),(P2,0.6),(P2+2*BAR,1),(97.2,1)],
 "st-pads":[(0,0.24),(P2-2*BAR,0.26),(P2,0.9),(47.6,1),(97.2,1)],
 "st-bells":[(0,0),(45.6,0),(49.6,0.42),(P3,0.42),(P3+2*BAR,0.60),(97.2,0.60)],
}
# SPACE — the beat never stops (kick, bass and her voice are never gated), but
# everything decorative is allowed to shut up. Two kinds of hole:
#   · a rest in the last half-bar of each 8-bar phrase, so the turnaround fill
#     arrives into a gap instead of onto a full band;
#   · whole sections where a layer simply does not play, so the arrangement
#     has somewhere left to go.
GRID0=0.3654
def gate(windows, floor=0.0, ramp_s=0.05):
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
t8=GRID0+8*BAR; k=0
while t8<93.0:
    RESTS.append((t8-BAR, t8-BAR/2, 0.0 if k%2==0 else 0.22))
    t8+=8*BAR; k+=1
BREATH=gate(RESTS)

# the verse opens thin: the eager hand and the piano sit out its first four bars
THIN_VERSE=gate([(P2, P2+4*BAR, 0.0)])
# and the finale takes one bar off from the sustained pads before its last push
THIN_PADS =gate([(P3+4*BAR, P3+6*BAR, 0.12)])
# the top of the verse also pulls the pattern layers back — not out, but
# far enough that her line has the room to itself for four bars
THIN_TOP  =gate([(P2, P2+4*BAR, 0.45)])

mix=np.zeros((NT,2))
def add(x,env=None,gain=1.0):
    n=min(len(x),NT)
    if env is None: mix[:n]+=x[:n]*gain
    else: mix[:n]+=x[:n]*env[:n]*gain
add(place(raw2(f"{S}/st-kick.raw"),0,0),envelope(ENVS["st-kick"]))
add(place(raw2(f"{S}/st-hats.raw"),+20,0.1),envelope(ENVS["st-hats"])*BREATH*THIN_TOP)
add(place(wav2(f"{S}/sep4/htdemucs/v4pid-trim/bass.wav"),0,0),envelope(ENVS["bass"]))
add(place(raw2(f"{S}/st-pluck.raw"),-25,0.15),envelope(ENVS["st-pluck"])*BREATH*THIN_TOP)
add(place(raw2(f"{S}/st-pads.raw"),0,0.35),envelope(ENVS["st-pads"])*BREATH*THIN_PADS,0.95)
add(place(raw2(f"{S}/st-bells.raw"),+30,0.25),envelope(ENVS["st-bells"]))
VOXENV=[(0,0.86),(29.9,0.9),(31.83,1.0),(97.2,1.0)]   # close to the mic   # quiet, calm entrance
add(wav2(f"{S}/vocalsFX.wav"),envelope(VOXENV),1.02)   # she stays center, forward
add(place(raw2(f"{S}/st-piano.raw"),-15,0.20),BREATH*THIN_VERSE)
add(place(raw2(f"{S}/st-swing.raw"),+12,0.05),BREATH*THIN_VERSE)
add(place(raw2(f"{S}/st-fills.raw"),-8,0.05))
add(raw2(f"{S}/stem-wub.raw"))
add(place(raw2(f"{S}/stem-gongs.raw"),0,0.5))
add(raw2(f"{S}/stem-stamp.raw"))
pk=np.abs(mix).max()
if pk>0.85: mix*=0.85/pk          # the s24 write must never clip
print(f"premaster peak {pk:.3f} -> normalized to 0.85")
mix.astype(np.float32).tofile(f"{S}/premaster.raw")
print("spatial stage assembled")
