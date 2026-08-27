#!/usr/bin/env python3
"""Print arrangement-scale descriptors for a lonerclub master."""

import argparse
import json

import librosa
import numpy as np
import soundfile as sf

N_FFT = 4096
SECTIONS = (
    ("body", 0.00, 31.83),
    ("clock", 31.83, 63.30),
    ("club", 63.30, 93.20),
    ("tag", 61.55, 63.30),
    ("outro", 93.20, 94.20),
    ("door_1", 30.216, 31.83),
    ("door_2", 61.662, 63.30),
)


def measure(stereo, sr, start, end):
    x = stereo[int(start * sr):int(end * sr)]
    mono = x.mean(axis=1)
    mid = (x[:, 0] + x[:, 1]) / 2
    side = (x[:, 0] - x[:, 1]) / 2
    spectrum = np.abs(librosa.stft(mono, n_fft=N_FFT, hop_length=1024))
    centroid = librosa.feature.spectral_centroid(S=spectrum, sr=sr)[0]
    onset_env = librosa.onset.onset_strength(y=mono, sr=sr, hop_length=512)
    onsets = librosa.onset.onset_detect(
        onset_envelope=onset_env, sr=sr, hop_length=512, units="time"
    )
    rms = np.sqrt(np.mean(mono * mono) + 1e-20)
    side_mid = 20 * np.log10(
        (np.sqrt(np.mean(side * side)) + 1e-12)
        / (np.sqrt(np.mean(mid * mid)) + 1e-12)
    )
    return {
        "rms_dbfs": round(float(20 * np.log10(rms)), 2),
        "onsets_per_s": round(float(len(onsets) / (end - start)), 2),
        "centroid_median_hz": round(float(np.median(centroid))),
        "side_mid_db": round(float(side_mid), 2),
        "correlation": round(float(np.corrcoef(x[:, 0], x[:, 1])[0, 1]), 3),
    }


parser = argparse.ArgumentParser()
parser.add_argument("audio")
args = parser.parse_args()
stereo, sr = sf.read(args.audio, always_2d=True, dtype="float32")
if sr != 48000 or stereo.shape[1] != 2:
    raise SystemExit(f"expected stereo 48 kHz, got {stereo.shape[1]}ch at {sr} Hz")
duration = len(stereo) / sr
result = {
    "audio": args.audio,
    "duration_s": round(duration, 3),
    "sections": {
        name: measure(stereo, sr, start, min(end, duration))
        for name, start, end in SECTIONS
        if start < duration
    },
}
print(json.dumps(result, indent=2))
