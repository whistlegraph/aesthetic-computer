#!/usr/bin/env python3
"""probe.py — is this take screamed? duration, pitch, how much of it is voiced, brightness, roughness."""
import sys, warnings; warnings.filterwarnings("ignore")
import numpy as np, librosa
for p in sys.argv[1:]:
    y, sr = librosa.load(p, sr=22050, mono=True)
    f0, vf, _ = librosa.pyin(y, fmin=70, fmax=700, sr=sr)
    cen = librosa.feature.spectral_centroid(y=y, sr=sr)[0]
    flat = librosa.feature.spectral_flatness(y=y)[0]
    rms = librosa.feature.rms(y=y)[0]
    loud = rms > rms.max() * 0.2
    v = f0[~np.isnan(f0)]
    print(f"{p.split('/')[-1]:28s} {len(y)/sr:5.2f}s  f0 med {np.median(v) if len(v) else 0:5.0f} Hz  max {v.max() if len(v) else 0:4.0f}  voiced {np.mean(~np.isnan(f0[:len(loud)][loud[:len(f0)]])):.2f}  centroid {np.median(cen[loud[:len(cen)]]):5.0f} Hz  flatness {np.median(flat[loud[:len(flat)]]):.3f}")
