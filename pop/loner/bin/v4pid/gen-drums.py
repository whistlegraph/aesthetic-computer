import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
# thicker, round kick
raw=subprocess.run(["ffmpeg","-v","error","-i",f"{S}/sep4/htdemucs/v4pid-trim/drums.wav",
                    "-ar",str(sr),"-ac","2","-f","f32le","-"],capture_output=True).stdout
drums=np.frombuffer(raw,np.float32).reshape(-1,2).astype(np.float64).copy()
ND=len(drums)
low=drums.mean(axis=1)
F=np.fft.rfft(low); fr=np.fft.rfftfreq(len(low),1/sr); F[fr>110]=0
env=np.abs(np.fft.irfft(F,len(low)))
hop=int(0.005*sr)
e=np.array([env[i:i+hop].max() for i in range(0,len(env)-hop,hop)])
th=e.max()*0.25
onsets=[]; i=0
while i<len(e):
    if e[i]>th: onsets.append(i*hop/sr); i+=int(0.28/0.005)
    else: i+=1
def kick5(strength):
    dur=0.40; n=int(dur*sr); t=np.arange(n)/sr
    f=40+80*np.exp(-t/0.034)
    ph=2*np.pi*np.cumsum(f)/sr
    body=np.sin(ph)*np.exp(-t/0.17)          # longer, thicker body
    second=np.sin(2*ph)*np.exp(-t/0.05)*0.22 # warm 2nd harmonic
    knock=np.sin(2*np.pi*150*t)*np.exp(-t/0.024)*0.30
    k=np.tanh(2.0*(body+second+knock))
    out=np.empty_like(k); acc=0.0
    aa=1-np.exp(-2*np.pi*2200/sr)
    for j in range(len(k)):
        acc+=aa*(k[j]-acc); out[j]=acc
    return out*strength
for t0 in onsets:
    if t0<0.30: continue
    aa=int(t0*sr)
    st=min(1.0, env[aa:aa+int(0.05*sr)].max()*2.2) if aa+int(0.05*sr)<len(env) else 0.6
    k=kick5(0.56*st)
    b3=min(ND,aa+len(k))
    drums[aa:b3,0]+=k[:b3-aa]; drums[aa:b3,1]+=k[:b3-aa]
cut=int(0.30*sr); ramp=int(0.02*sr)
drums[:cut]=0; drums[cut:cut+ramp]*=np.linspace(0,1,ramp)[:,None]
drums.astype(np.float32).tofile(f"{S}/drums-cool.raw")
print("thick round kick layered")
