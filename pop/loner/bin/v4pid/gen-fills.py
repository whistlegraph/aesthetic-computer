import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT; SIX=BEAT/4
NT=int(97.2*sr)
def wav1(p):
    r=subprocess.run(["ffmpeg","-v","error","-i",p,"-ar",str(sr),"-ac","1","-f","f32le","-"],capture_output=True).stdout
    return np.frombuffer(r,np.float32).astype(np.float64)
out=np.zeros((NT,2))
# no bowl on the last door — the ending belongs to her voice and the stamp
for (t,n,g,pan) in [(0.05,"A",0.16,0.0),(31.83,"F",0.14,0.2),(63.30,"D",0.14,-0.2)]:
    y=wav1(f"{S}/bowl{n}.wav")
    tt=np.arange(len(y))/sr
    y=y*np.exp(-tt/2.6)          # the bowl blooms, then truly dies away
    y*=g
    a=int(t*sr); b=min(NT,a+len(y))
    out[a:b,0]+=y[:b-a]*(1-max(0,pan)); out[a:b,1]+=y[:b-a]*(1-max(0,-pan))
out.astype(np.float32).tofile(f"{S}/stem-gongs.raw")
print("bowls: octave up, decaying")

# fills v2: rush is ONE bar and gentle
rng=np.random.default_rng(13)
def eager(): return float(np.clip(rng.normal(-0.006,0.007),-0.018,0.006))
def vel(b,s=0.25): return b*float(np.clip(rng.normal(1.0,s),0.5,1.5))
def kick(strength):
    dur=0.40; n=int(dur*sr); t=np.arange(n)/sr
    f=40+80*np.exp(-t/0.034)
    ph=2*np.pi*np.cumsum(f)/sr
    body=np.sin(ph)*np.exp(-t/0.17)
    second=np.sin(2*ph)*np.exp(-t/0.05)*0.22
    knock=np.sin(2*np.pi*150*t)*np.exp(-t/0.024)*0.30
    k=np.tanh(2.0*(body+second+knock))
    o=np.empty_like(k); acc=0.0
    aa=1-np.exp(-2*np.pi*2200/sr)
    for j in range(len(k)): acc+=aa*(k[j]-acc); o[j]=acc
    return o*strength
def snare(strength):
    n=int(0.16*sr); t=np.arange(n)/sr
    noise=rng.standard_normal(n)*np.exp(-t/0.045)
    body=np.sin(2*np.pi*195*t)*np.exp(-t/0.03)*0.5
    a=1-np.exp(-2*np.pi*4200/sr)
    hp=np.empty(n); acc=0.0
    for i in range(n): acc+=a*(noise[i]-acc); hp[i]=noise[i]-acc
    return np.tanh(1.5*(hp*0.8+body))*strength
K=kick(1.0); RK=K[::-1]; SN=snare(1.0)
fills=np.zeros((NT,2))
def put(y,t,g,pan=0.0):
    a=int(t*sr); b=min(NT,a+len(y))
    if a<0 or a>=NT: return
    s=y[:b-a]*g
    fills[a:b,0]+=s*(1-max(0,pan)); fills[a:b,1]+=s*(1-max(0,-pan))
GRID0=0.3654
t8=GRID0+8*BAR
while t8<91.0:
    bar8=t8+8*BAR
    kind=rng.integers(0,3)
    if kind==0:
        put(K,bar8-BEAT+SIX*2+eager(),vel(0.34))
        put(K,bar8-SIX+eager(),vel(0.27))
    elif kind==1:
        put(RK,bar8-0.40,vel(0.38,0.15))
    else:
        for i,g in ((3,0.13),(2,0.16),(1,0.21)):
            put(SN,bar8-i*SIX+eager(),vel(g),0.1*(-1)**i)
    t8=bar8
# The run into each door is a CLICK rush, not a snare rush: a tick that
# accelerates a little and stops well short of a roll. The last door gets the
# quietest, widest-spaced one — the track is already leaving by then.
def click(strength):
    n=int(0.035*sr); t=np.arange(n)/sr
    tick=np.sin(2*np.pi*2600*t)+0.5*rng.standard_normal(n)
    return np.tanh(1.4*tick*np.exp(-t/0.006))*strength
CL=click(1.0)
for d,(g0,gcap,floor) in ((31.83,(0.05,0.13,SIX)),
                          (63.30,(0.05,0.13,SIX)),
                          (92.55,(0.035,0.075,SIX*2))):
    tt=d-1*BAR                    # one bar only, and polite
    step=BEAT/2; g=g0
    while tt<d-0.02:
        put(CL,tt+eager(),vel(g,0.2),0.15*np.sin(tt*7))
        step=max(floor,step*0.90) # eases in, never doubles into a buzz
        g=min(gcap,g*1.10)
        tt+=step
    put(RK,d-0.40,0.40)
env=np.interp(np.arange(NT)/sr,[0,15.7,19.7,97.2],[0.5,0.6,1,1])[:,None]
(fills*env).astype(np.float32).tofile(f"{S}/st-fills.raw")
print("fills v2: one-bar gentle rushes")
