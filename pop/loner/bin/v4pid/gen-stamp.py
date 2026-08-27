import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122
r=subprocess.run(["ffmpeg","-v","error","-i",f"{S}/stamp-jsnapped.wav","-ar",str(sr),"-ac","1","-f","f32le","-"],capture_output=True).stdout
st=np.frombuffer(r,np.float32).astype(np.float64)
idx=np.arange(0,len(st)-1,0.88)     # a touch slow, not syrup
slow=np.interp(idx,np.arange(len(st)),st)
d=int(0.75*BEAT*sr)
tail=np.zeros(len(slow)+3*d)
tail[:len(slow)]+=slow; tail[d:d+len(slow)]+=slow*0.35; tail[2*d:2*d+len(slow)]+=slow*0.14
tail*=0.55
NT=int(97.2*sr)
stamp=np.zeros((NT,2))
sa=int(93.2*sr); sb=min(NT,sa+len(tail))
stamp[sa:sb,0]+=tail[:sb-sa]; stamp[sa:sb,1]+=tail[:sb-sa]
stamp.astype(np.float32).tofile(f"{S}/stem-stamp.raw")
print(f"jeffrey stamp {len(tail)/sr:.2f}s at 93.2")
