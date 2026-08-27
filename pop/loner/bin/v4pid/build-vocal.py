import numpy as np, subprocess
import os
S=os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S,exist_ok=True)
sr=48000
BEAT=60.0/122; D8=0.75*BEAT; SIX=BEAT/4
def load(p,ch=2):
    r=subprocess.run(["ffmpeg","-v","error","-i",p,"-ar",str(sr),"-ac",str(ch),"-f","f32le","-"],capture_output=True).stdout
    a=np.frombuffer(r,np.float32).astype(np.float64)
    return a.reshape(-1,ch) if ch==2 else a
vox=load(f"{S}/sep2/htdemucs/v4pid-trim/vocals.wav").copy()
N=len(vox); mono=vox.mean(axis=1)
take=load("pop/loner/vox4/w-whole-line.wav",ch=1)
p1a=mono[int(1.0*sr):int(30*sr)]; act=p1a[np.abs(p1a)>0.01]; tact=take[np.abs(take)>0.01]
g=np.sqrt((act**2).mean())/max(1e-9,np.sqrt((tact**2).mean()))
take=take*g
t_ins=31.83-0.330
a=int(t_ins*sr); b=min(N,a+len(take))
xf=int(0.12*sr)
env=np.ones(b-a); env[:xf]=np.linspace(0,1,xf); env[-xf:]=np.linspace(1,0,xf)
for ch in range(2):
    vox[a:b,ch]=vox[a:b,ch]*(1-env)+take[:b-a]*env
mono=vox.mean(axis=1)
def tail_arp(v0,steps,lead=1.00):
    src_t=v0+0.85
    src=mono[int(src_t*sr):int((src_t+3.6)*sr)].copy()
    idx=np.arange(len(src))
    out=np.zeros(int(len(steps)*D8*sr)); pos=0.0
    for k,s in enumerate(steps):
        r=2**(s/12.0); aa=int(k*D8*sr); bb=int((k+1)*D8*sr)
        p=np.clip(pos+np.arange(bb-aa)*r,0,len(src)-2)
        out[aa:bb]=np.interp(p,idx,src); pos=min(p[-1]+r,len(src)-2)
    t0=v0+lead
    aa=int(t0*sr); bb=min(N,aa+len(out)); out=out[:bb-aa]
    xin=int(0.06*sr); xout=int(0.08*sr)
    e=np.ones(bb-aa); e[:xin]=np.linspace(0,1,xin); e[-xout:]=np.linspace(1,0,xout)
    for ch in range(2):
        vox[aa:bb,ch]=vox[aa:bb,ch]*(1-e)+out*e
P1,P2,P3=0.3654,31.83,63.30
# (arpeggiation retired 2026-08-26 — her held notes stay her held notes)
mono=vox.mean(axis=1)
g0=int((P3+32*BEAT)*sr); g1=int((P3+40*BEAT)*sr)
t=np.arange(g1-g0)/sr
ph=(t/(SIX*2))%1.0
gate=0.35+0.65*(ph<0.58)          # swung: the open hangs long, closes late
edge=int(0.004*sr)
gate=np.convolve(gate,np.ones(edge)/edge,mode="same")
for ch in range(2): vox[g0:g1,ch]*=gate
ms=mono[int((P3+10*BEAT)*sr):int((P3+13*BEAT)*sr)].copy()[::-1]*0.5
f=int(0.02*sr); ms[:f]*=np.linspace(0,1,f); ms[-f:]*=np.linspace(1,0,f)
s0=int((P3+14.5*BEAT)*sr); s1=min(N,s0+len(ms))
vox[s0:s1,0]+=ms[:s1-s0]*0.75; vox[s0:s1,1]+=ms[:s1-s0]*0.35
mono=vox.mean(axis=1)
def scratch_play(chop,cl):
    tt=np.arange(cl)/cl
    rate=np.where(tt<0.4,1.3,np.where(tt<0.75,-1.15,1.0))
    pos=np.clip(np.cumsum(rate)*len(chop)/cl*0.9,0,len(chop)-2)
    return np.interp(pos,np.arange(len(chop)),chop)
for p0,gains in ((P1,(0.8,0.55)),(P2,(0.8,0.55)),(P3,(0.95,0.75))):
    t_up=p0+5.91*BEAT; aa=int(t_up*sr)
    chop=mono[aa:aa+int(0.35*sr)].copy()
    ff=int(0.008*sr); chop[:ff]*=np.linspace(0,1,ff); chop[-ff:]*=np.linspace(1,0,ff)
    for (dt,gg,pan) in [(D8,gains[0],0.3),(2*D8,gains[1],-0.35)]:
        y=chop if dt==D8 else scratch_play(chop,int(0.32*sr))
        q0=int((t_up+dt)*sr); q1=min(N,q0+len(y)); ss=y[:q1-q0]*gg
        vox[q0:q1,0]+=ss*(1-max(0,pan)); vox[q0:q1,1]+=ss*(1-max(0,-pan))
# bandmate harmonies — present, not angelic-choir loud
lead_rms=np.sqrt((vox[int(35*sr):int(60*sr)]**2).mean())
def layer(name,leadIn,gain,pan,passT0,fromBeat):
    t=load(f"{S}/at-{name}.wav",ch=1)
    gg=lead_rms/max(1e-9,np.sqrt((t[np.abs(t)>0.01]**2).mean()))
    t=t*gg*gain
    aa=int((passT0-leadIn)*sr); bb=min(N,aa+len(t))
    seg=t[:bb-aa].copy()
    ent=int((fromBeat*BEAT+leadIn)*sr); fd=int(2*BEAT*sr)
    e=np.zeros(bb-aa)
    lo=min(ent,len(e)); hi=min(ent+fd,len(e))
    if hi>lo: e[lo:hi]=np.linspace(0,1,hi-lo)
    e[hi:]=1
    seg*=e
    vox[aa:bb,0]+=seg*(1-max(0,pan)); vox[aa:bb,1]+=seg*(1-max(0,-pan))
# the hums hold their entrances to the FINALE only, and each take is
# measure-aligned against her lead before it joins
lead8=vox.mean(axis=1)[::6]
def align_take(name,leadIn):
    t8=load(f"{S}/at-{name}.wav",ch=1)[::6]
    exp=(P3-leadIn)
    lo=int((exp-0.45)*8000); probe=t8[:int(3*8000)]
    win=lead8[lo:int((exp+0.45+3)*8000)]
    if len(win)<=len(probe): return 0.0
    w=win-win.mean(); p=probe-probe.mean()
    n=len(w)+len(p)
    c=np.fft.irfft(np.fft.rfft(w,n)*np.conj(np.fft.rfft(p,n)),n)[:len(w)-len(p)]
    d=(lo/8000.0)+int(np.argmax(c))/8000.0-exp
    return float(np.clip(d,-0.4,0.4))
for name,leadIn,gain,pan,fromBeat in (
        ("w-cp",0.430,0.22,-0.4,23.75),("w-o",0.550,0.26,0.45,32.0),
        ("w-lg",0.575,0.17,-0.25,40.0),("w-s",0.370,0.14,0.25,46.0),
        ("w-sh",0.840,0.10,0.15,23.75),("w-rd",0.405,0.09,-0.15,40.0)):
    d=align_take(name,leadIn)
    layer(name,leadIn-d,gain,pan,P3,fromBeat)
# "pa" sits down ~4 dB in every pass
for p0 in (P1,P2,P3):
    a=int((p0+40*BEAT-0.15)*sr); b=int((p0+41.6*BEAT)*sr)
    if a>=N: continue
    b=min(b,N)
    env=np.ones(b-a)*0.62
    r=int(0.06*sr)
    env[:r]=np.linspace(1,0.62,r); env[-r:]=np.linspace(0.62,1,r)
    vox[a:b]*=env[:,None]
vox.astype(np.float32).tofile(f"{S}/vox-arped.raw")
print("vocal v7")
