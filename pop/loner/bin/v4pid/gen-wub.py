import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; BAR=4*BEAT
NT=int(97.2*sr)
root0=58.27
ROW=[0,0,-4,-4,3,3,-2,-2]
t_start,t_end=31.826,63.30
a,b=int(t_start*sr),int(t_end*sr)
n=b-a
t=np.arange(n)/sr
bar_idx=(t/BAR).astype(int)
st=np.array([ROW[k%8] for k in bar_idx],float)
hop=int(0.03*sr)
st=np.convolve(st,np.ones(hop)/hop,mode="same")
freq=root0*2**(st/12.0)
wob_rate=np.where(bar_idx%2,6.10,4.07)
wob=0.55+0.45*np.sin(2*np.pi*wob_rate*(t%BAR)-np.pi/2)
ph=2*np.pi*np.cumsum(freq)/sr
sub=np.tanh(2.6*np.sin(ph))*wob
duck=np.ones(n)
for bt in np.arange(0,t_end-t_start,BEAT):
    i=int(bt*sr); j=min(n,i+int(0.09*sr))
    duck[i:j]*=1-0.7*np.exp(-np.arange(j-i)/(0.03*sr))
sub*=duck*0.16
# SIDECHAIN TO HER — the wub swells only in the gaps she leaves
r=subprocess.run(["ffmpeg","-v","error","-i",f"{S}/vocalsFX.wav","-ac","1","-ar",str(sr),"-f","f32le","-"],capture_output=True).stdout
vx=np.frombuffer(r,np.float32).astype(np.float64)
env=np.abs(vx[a:b]) if len(vx)>=b else np.abs(np.pad(vx,(0,b-len(vx)))[a:b])
atk=1-np.exp(-1/(0.015*sr)); rel=1-np.exp(-1/(0.25*sr))
f=0.0; fo=np.empty(n)
for i in range(n):
    c=atk if env[i]>f else rel
    f+=c*(env[i]-f); fo[i]=f
fo/=max(1e-9,np.percentile(fo,98))
fo=np.clip(fo,0,1)
sub*=(1-0.58*fo)
e=int(BAR*sr)
sub[:e]*=np.linspace(0,1,e); sub[-e:]*=np.linspace(1,0,e)
out=np.zeros((NT,2))
out[a:b,0]+=sub; out[a:b,1]+=sub
out.astype(np.float32).tofile(f"{S}/stem-wub.raw")
print("wub sidechained to the vocal")
