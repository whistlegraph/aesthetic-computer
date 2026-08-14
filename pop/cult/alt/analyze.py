import json, glob, os, sys
import numpy as np, librosa, soundfile as sf

SR=48000
def load(id):
    y,_=librosa.load(f'alt/wav/{id}.wav', sr=SR, mono=True)
    return y

def rms_env(y, hop=256):
    return librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0], hop

def refine(y, t0, t1, pad=0.20, thresh_ratio=0.10):
    """Trim leading/trailing silence inside a padded window, keeping internal gaps."""
    a=max(0,int((t0-pad)*SR)); b=min(len(y),int((t1+pad)*SR))
    seg=y[a:b]
    if len(seg)<1024: return t0,t1
    e,hop=rms_env(seg)
    peak=e.max()
    if peak<=0: return t0,t1
    th=peak*thresh_ratio
    idx=np.where(e>=th)[0]
    if len(idx)==0: return t0,t1
    s=idx[0]; t=idx[-1]
    ns=(a+s*hop)/SR; nt=(a+min(len(seg),(t+2)*hop))/SR
    return ns, nt

def f0_of(y, s, t, fmin=60, fmax=3000):
    seg=y[int(s*SR):int(t*SR)]
    if len(seg)<2048: return None, 0.0
    f0,vflag,vprob=librosa.pyin(seg, fmin=fmin, fmax=fmax, sr=SR, frame_length=2048)
    v=f0[~np.isnan(f0)]
    if len(v)==0: return None, 0.0
    return float(np.median(v)), float(len(v)/len(f0))

def noisefloor(y, s, t):
    seg=y[int(s*SR):int(t*SR)]
    return float(np.sqrt(np.mean(seg**2))) if len(seg) else 0.0

def write(name, y, s, t, head=0.004, tail=0.008):
    a=max(0,int((s-head)*SR)); b=min(len(y),int((t+tail)*SR))
    sf.write(f'alt/samples/{name}.wav', y[a:b], SR, subtype='PCM_16')
    return (b-a)/SR
