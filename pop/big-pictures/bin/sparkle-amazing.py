#!/usr/bin/env python3
"""Add scored blips, stereo orbits, and pitched transition bursts to a premix.
Usage: python sparkle-amazing.py SOURCE_PRE OUT_DIR [VOCAL_STEM]
All effects are synthesized here; no samples or mastered audio are used.
"""
import json
from pathlib import Path
import sys
import numpy as np
import soundfile as sf
from scipy.ndimage import maximum_filter1d, gaussian_filter1d
from scipy.signal import butter, sosfilt

src, out = Path(sys.argv[1]), Path(sys.argv[2])
out.mkdir(parents=True, exist_ok=True)
mix, sr = sf.read(src, always_2d=True)
assert sr == 48000 and mix.shape[1] == 2
fx = np.zeros_like(mix)
rng = np.random.default_rng(5775)
beat = 60 / 70
events = []

def put(y, at, pan, gain, kind, note=None):
    i = round(at * sr)
    size = min(len(y), len(fx) - i)
    if size <= 0:
        return
    p = np.broadcast_to(pan, y.shape)[:size]
    angle = (np.clip(p, -1, 1) + 1) * np.pi / 4
    fx[i:i+size] += gain * y[:size, None] * np.stack([np.cos(angle), np.sin(angle)], axis=1)
    events.append({'kind': kind, 'at': round(at, 4), 'seconds': size/sr, 'midi': note, 'gain': gain})

def blip(at, midi, gain, direction, duration=.24):
    t = np.arange(round(duration*sr))/sr
    f = 440*2**((midi-69)/12)
    # FM bell chirp settles into its scored pitch rather than random bleeps.
    phase = 2*np.pi*f*t + 1.5*np.exp(-t/.035)*np.sin(2*np.pi*f*2*t)
    y = np.sin(phase)*np.exp(-t/.075)*(1-np.exp(-t/.002))
    y *= np.clip((duration-t)/.025,0,1)
    pan = direction*np.sin(2*np.pi*(t/duration*.7-.25))
    put(y, at, pan, gain, 'blip', midi)
    put(y, at+beat/3, -pan, gain*.30, 'blip-return', midi)

# Answers fit between the sung lines; the second pass gets longer cascades.
for at, notes, gain in [(3.0,[79,83,86],.075), (19.5,[74,79,83],.060),
                        (31.4,[79,74,71],.058), (46.4,[76,79,83],.060),
                        (59.0,[79,83,86,90],.072), (77.8,[83,86,90,86],.070),
                        (89.6,[79,83,86,91],.070), (104.6,[76,79,83,86],.074),
                        (116.0,[79,83,86,90,91,95],.083)]:
    for k, midi in enumerate(notes):
        blip(at+k*beat/6, midi, gain, (-1)**k)

for at, duration, gain, turns in [(5.7,1.8,.065,1.0), (61.5,3.2,.105,1.8),
                                 (93.5,2.6,.070,-1.5), (118.8,4.3,.115,2.2)]:
    t=np.arange(round(duration*sr))/sr
    noise=sosfilt(butter(2,[420,6200],btype='bandpass',fs=sr,output='sos'),rng.normal(size=len(t)))
    env=(1-np.exp(-t/.012))*np.exp(-t/(duration*.24))*np.clip((duration-t)/.3,0,1)
    # Falling resonant shell + tuned G/D shimmer: explosion with a harmonic tail.
    phase=2*np.pi*(98*t+240*.12*(1-np.exp(-t/.12)))
    y=(noise*.30+np.sin(phase)*.28)*env
    for midi in [67,74,79,83]:
        y += .07*np.sin(2*np.pi*440*2**((midi-69)/12)*t)*env
    angle=2*np.pi*turns*t/duration
    y *= .72+.28*np.cos(angle)
    put(y,at,np.sin(angle),gain,'orbit-burst')

# Let consonants lead: brief effect dips follow the actual vocal envelope.
if len(sys.argv)>3:
    vox, vsr=sf.read(sys.argv[3],always_2d=True)
    assert vsr==sr and len(vox)==len(mix)
    hop=240
    pad=(-len(vox))%hop
    v=np.pad(np.max(np.abs(vox),axis=1),(0,pad)).reshape(-1,hop)
    env=np.sqrt(np.mean(v*v,axis=1))
    env=gaussian_filter1d(maximum_filter1d(env,size=15),sigma=6)
    duck=np.clip(1-.55*np.clip(env/.13,0,1),.45,1)
    fx *= np.repeat(duck,hop)[:len(fx),None]
fx[-round(2.5*sr):] *= np.linspace(1,0,round(2.5*sr))[:,None]
assert np.isfinite(fx).all()
sf.write(out/'sparkle-spins.wav',fx,sr,subtype='FLOAT')
sf.write(out/'pre.wav',mix+fx,sr,subtype='FLOAT')
(out/'effects-receipt.json').write_text(json.dumps({'source':str(src),'seed':5775,
    'events':events,'spatial':'equal-power stereo amplitude orbits, positive polarity',
    'vocalDuck':len(sys.argv)>3},indent=2)+'\n')
print(f'{len(events)} effects; peak {np.max(np.abs(fx)):.4f}; {len(mix)/sr:.3f}s')
