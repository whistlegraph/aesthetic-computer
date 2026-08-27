#!/usr/bin/env python3
"""Measure and draw the evidence used in the lonerclub arrangement critique.

Run from the repository root:
  pop/.venv/bin/python papers/arxiv-loner-arrangement-critique/analysis.py
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path

import librosa
import numpy as np
import soundfile as sf

# Pillow is installed in the host Python rather than pop/.venv.
sys.path.append("/opt/homebrew/lib/python3.14/site-packages")
from PIL import Image, ImageDraw, ImageFont  # noqa: E402


ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
AUDIO = ROOT / "pop/loner/out/lonerclub-v4pid-release.wav"
FIGURE = HERE / "figures/arrangement-evidence.png"
WINDOWS_CSV = HERE / "analysis-windows.csv"
SECTIONS_CSV = HERE / "section-metrics.csv"

SR_EXPECTED = 48_000
HOP = 2_048
N_FFT = 4_096
SECTIONS = [
    ("body", 0.0, 31.83),
    ("clock", 31.83, 63.30),
    ("club", 63.30, 93.20),
    ("stamp", 93.20, 97.20),
]


def smooth(values: np.ndarray, frames: int) -> np.ndarray:
    frames = max(1, int(frames))
    kernel = np.ones(frames, dtype=np.float64) / frames
    return np.convolve(values, kernel, mode="same")


def section_metrics(stereo: np.ndarray, sr: int, start: float, end: float) -> dict[str, float]:
    x = stereo[int(start * sr) : int(end * sr)]
    mono = x.mean(axis=1)
    mid = (x[:, 0] + x[:, 1]) / 2
    side = (x[:, 0] - x[:, 1]) / 2
    rms = float(np.sqrt(np.mean(mono * mono) + 1e-20))
    peak = float(np.max(np.abs(x)))
    spectrum = np.abs(librosa.stft(mono, n_fft=N_FFT, hop_length=1_024, window="hann"))
    centroid = librosa.feature.spectral_centroid(S=spectrum, sr=sr)[0]
    rolloff = librosa.feature.spectral_rolloff(S=spectrum, sr=sr, roll_percent=0.85)[0]
    rms_frames = librosa.feature.rms(S=spectrum, frame_length=N_FFT)[0]
    onset_env = librosa.onset.onset_strength(y=mono, sr=sr, hop_length=512)
    onset_times = librosa.onset.onset_detect(
        onset_envelope=onset_env,
        sr=sr,
        hop_length=512,
        units="time",
        backtrack=False,
    )
    width = 20 * np.log10(
        (np.sqrt(np.mean(side * side)) + 1e-12)
        / (np.sqrt(np.mean(mid * mid)) + 1e-12)
    )
    return {
        "start_s": start,
        "end_s": end,
        "rms_dbfs": 20 * np.log10(rms),
        "crest_db": 20 * np.log10(peak / rms),
        "side_mid_db": float(width),
        "correlation": float(np.corrcoef(x[:, 0], x[:, 1])[0, 1]),
        "centroid_median_hz": float(np.median(centroid)),
        "rolloff_85_median_hz": float(np.median(rolloff)),
        "onsets_per_s": float(len(onset_times) / (end - start)),
        "rms_spread_db": float(
            20
            * np.log10(
                np.percentile(rms_frames, 90) / max(np.percentile(rms_frames, 10), 1e-12)
            )
        ),
    }


def font(name: str, size: int) -> ImageFont.FreeTypeFont:
    path = ROOT / f"system/public/type/webfonts/{name}.ttf"
    return ImageFont.truetype(str(path), size=size)


stereo, sr = sf.read(AUDIO, always_2d=True, dtype="float32")
if sr != SR_EXPECTED or stereo.shape[1] != 2:
    raise SystemExit(f"Expected stereo {SR_EXPECTED} Hz audio, got {stereo.shape[1]}ch at {sr} Hz")

duration = len(stereo) / sr
mono = stereo.mean(axis=1)
spectrum = np.abs(librosa.stft(mono, n_fft=N_FFT, hop_length=HOP, window="hann"))
times = librosa.frames_to_time(np.arange(spectrum.shape[1]), sr=sr, hop_length=HOP)
rms_db = librosa.amplitude_to_db(
    librosa.feature.rms(S=spectrum, frame_length=N_FFT)[0], ref=1.0, top_db=70
)
centroid = librosa.feature.spectral_centroid(S=spectrum, sr=sr)[0]
onset = librosa.onset.onset_strength(y=mono, sr=sr, hop_length=HOP)

# One-second smoothing exposes the arrangement-scale motion while preserving
# the two transition bursts.
frames_per_second = sr / HOP
rms_s = smooth(rms_db, round(frames_per_second * 0.45))
centroid_s = smooth(centroid, round(frames_per_second * 0.35))
onset_s = smooth(onset, round(frames_per_second * 0.65))

with WINDOWS_CSV.open("w", newline="") as handle:
    writer = csv.writer(handle)
    writer.writerow(["time_s", "rms_dbfs", "spectral_centroid_hz", "onset_strength"])
    for row in zip(times, rms_s, centroid_s, onset_s):
        writer.writerow([f"{row[0]:.4f}", f"{row[1]:.4f}", f"{row[2]:.3f}", f"{row[3]:.5f}"])

metrics: list[tuple[str, dict[str, float]]] = []
for label, start, end in SECTIONS:
    metrics.append((label, section_metrics(stereo, sr, start, min(end, duration))))

with SECTIONS_CSV.open("w", newline="") as handle:
    fieldnames = ["section", *metrics[0][1].keys()]
    writer = csv.DictWriter(handle, fieldnames=fieldnames)
    writer.writeheader()
    for label, values in metrics:
        writer.writerow({"section": label, **{k: f"{v:.4f}" for k, v in values.items()}})

W, H = 1800, 900
BG = (248, 246, 240)
INK = (55, 49, 64)
GRAY = (119, 119, 119)
GRID = (215, 210, 215)
PINK = (180, 72, 135)
PURPLE = (120, 80, 180)
BLUE = (48, 118, 170)
ORANGE = (226, 135, 67)
SECTION_COLORS = [PURPLE, BLUE, PINK, GRAY]

image = Image.new("RGB", (W, H), BG)
draw = ImageDraw.Draw(image)
f_title = font("ywft-processing-bold", 48)
f_label = font("ywft-processing-bold", 25)
f_small = font("ywft-processing-regular", 21)
f_tiny = font("ywft-processing-regular", 18)

left, right = 145, W - 55
plot_w = right - left


def x_at(seconds: float) -> float:
    return left + plot_w * seconds / duration


draw.text((left, 25), "lonerclub — arrangement evidence", font=f_title, fill=INK)
draw.text(
    (left, 78),
    "97.2 s · 122 BPM · release candidate 2026-08-27 · vocal entries at 0.84 / 32.28 / 63.78 s",
    font=f_small,
    fill=GRAY,
)

# Section rail.
rail_y0, rail_y1 = 120, 174
for idx, (label, start, end) in enumerate(SECTIONS):
    x0, x1 = x_at(start), x_at(min(end, duration))
    color = SECTION_COLORS[idx]
    draw.rounded_rectangle((x0, rail_y0, x1 - 3, rail_y1), radius=8, fill=color)
    text_label = label.upper()
    bbox = draw.textbbox((0, 0), text_label, font=f_label)
    if x1 - x0 > bbox[2] - bbox[0] + 16:
        draw.text(((x0 + x1) / 2, rail_y0 + 27), text_label, font=f_label, fill=BG, anchor="mm")
    elif label == "stamp":
        draw.text(((x0 + x1) / 2, rail_y0 + 27), text_label, font=f_tiny, fill=BG, anchor="mm")

# Rush windows identified from the full-spectrum and 2 s feature pass.
for start, end, text_label in [(30.0, 31.83, "rush 1"), (61.4, 63.30, "rush 2")]:
    x0, x1 = x_at(start), x_at(end)
    draw.rectangle((x0, 194, x1, 766), fill=(239, 220, 230))
    draw.text(((x0 + x1) / 2, 202), text_label, font=f_tiny, fill=PINK, anchor="ma")

panels = [
    ("RMS", 230, 408, -34.0, -8.0),
    ("CENTROID", 446, 624, 0.0, 4_500.0),
    ("ONSET FLUX", 662, 802, 0.0, max(1.0, float(np.percentile(onset_s, 99)))),
]

for label, top, bottom, low, high in panels:
    draw.text((left - 18, (top + bottom) / 2), label, font=f_small, fill=GRAY, anchor="rm")
    for frac in (0.0, 0.5, 1.0):
        y = bottom - frac * (bottom - top)
        draw.line((left, y, right, y), fill=GRID, width=2)

for tick in range(0, 100, 10):
    x = x_at(tick)
    draw.line((x, 214, x, 816), fill=GRID, width=1)
    draw.text((x, 828), f"{tick // 60}:{tick % 60:02d}", font=f_tiny, fill=GRAY, anchor="ma")


def plot_line(values: np.ndarray, panel_index: int, color: tuple[int, int, int], width: int) -> None:
    _, top, bottom, low, high = panels[panel_index]
    clipped = np.clip(values, low, high)
    ys = bottom - (clipped - low) / (high - low) * (bottom - top)
    points = [(x_at(float(t)), float(y)) for t, y in zip(times, ys)]
    draw.line(points, fill=color, width=width, joint="curve")


# Energy as a filled silhouette, then centroid/onset as lines.
_, e_top, e_bottom, e_low, e_high = panels[0]
energy_y = e_bottom - (np.clip(rms_s, e_low, e_high) - e_low) / (e_high - e_low) * (e_bottom - e_top)
energy_points = [(left, e_bottom), *[(x_at(float(t)), float(y)) for t, y in zip(times, energy_y)], (right, e_bottom)]
draw.polygon(energy_points, fill=(184, 166, 207))
draw.line(energy_points[1:-1], fill=PURPLE, width=4, joint="curve")
plot_line(centroid_s, 1, PINK, 5)
plot_line(onset_s, 2, ORANGE, 5)

for seconds, label in [(31.83, "0:31.83"), (63.30, "1:03.30"), (93.20, "1:33.20")]:
    x = x_at(seconds)
    draw.line((x, 112, x, 816), fill=INK, width=3)
    draw.text((x + 6, 180), label, font=f_tiny, fill=INK)

FIGURE.parent.mkdir(parents=True, exist_ok=True)
image.save(FIGURE, optimize=True)

print(f"audio: {AUDIO}")
print(f"figure: {FIGURE}")
for label, values in metrics:
    print(
        f"{label:5s}  rms {values['rms_dbfs']:.1f} dBFS  "
        f"onsets {values['onsets_per_s']:.1f}/s  "
        f"centroid {values['centroid_median_hz']:.0f} Hz  "
        f"side/mid {values['side_mid_db']:.1f} dB"
    )
