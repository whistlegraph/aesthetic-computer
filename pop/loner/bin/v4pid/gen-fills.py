import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT; SIX=BEAT/4
DURATION=94.2
NT=int(DURATION*sr)
def wav1(p):
    r=subprocess.run(["ffmpeg","-v","error","-i",p,"-ar",str(sr),"-ac","1","-f","f32le","-"],capture_output=True).stdout
    return np.frombuffer(r,np.float32).astype(np.float64)
out=np.zeros((NT,2))
# Glass is a club-pass color, not a repeated door label.
for (t,n,g,pan) in [(63.30,"D",0.14,-0.2)]:
    y=wav1(f"{S}/bowl{n}.wav")
    tt=np.arange(len(y))/sr
    y=y*np.exp(-tt/2.6)          # the bowl blooms, then truly dies away
    y*=g
    a=int(t*sr); b=min(NT,a+len(y))
    out[a:b,0]+=y[:b-a]*(1-max(0,pan)); out[a:b,1]+=y[:b-a]*(1-max(0,-pan))
out.astype(np.float32).tofile(f"{S}/stem-gongs.raw")
print("bowls: octave up, decaying")

# Restrained interior turns; the two formal doors are scored separately below.
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
put(RK,47.57-0.40,0.24)
put(K,78.99-BEAT+2*SIX,0.20)
put(K,78.99-SIX,0.16)

# One centered, filtered click identifies the clock door. The club door gets
# the spoken stamp instead, so neither transition has to explain itself twice.
def click(strength):
    n=int(0.035*sr); t=np.arange(n)/sr
    x=np.sin(2*np.pi*2600*t)+0.35*rng.standard_normal(n)
    lp=np.empty(n); acc=0.0
    a=1-np.exp(-2*np.pi*1800/sr)
    for i in range(n): acc+=a*(x[i]-acc); lp[i]=acc
    hp=x-lp
    band=np.empty(n); acc=0.0
    a=1-np.exp(-2*np.pi*7000/sr)
    for i in range(n): acc+=a*(hp[i]-acc); band[i]=acc
    tick=np.tanh(1.4*band*np.exp(-t/0.006))
    return tick/max(1e-9,np.max(np.abs(tick)))*strength
CL=click(1.0)
put(CL,31.50,0.095,0.0)
env=np.interp(np.arange(NT)/sr,[0,15.7,19.7,DURATION],[0.5,0.6,1,1])[:,None]
(fills*env).astype(np.float32).tofile(f"{S}/st-fills.raw")
print("fills v4: one clock click, spoken club pickup")
