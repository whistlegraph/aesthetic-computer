#!/usr/bin/env python3
"""fit.py — CMA-ES fit of Pink Trombone steady-state params to a target WAV.

The minimum-viable version: fit one steady-state vowel pose to a target
recording by minimizing MFCC distance. Per-frame trajectory fitting and
per-phoneme corpus iteration come next.

How it works:
    target.wav → librosa MFCCs → time-mean vector mfcc_target
    objective(x):
        params = denormalize(x)
        wav = subprocess(render-pt.mjs --params <json> --out - --quiet)
        mfcc_render = librosa MFCCs of wav → time-mean
        return ||mfcc_render - mfcc_target||_2
    CMA-ES minimizes objective over a bounded normalized space.

The PT param envelope is bounded by `jeffrey-anthropometry.json` if
present (clamps tract length, lip aperture, etc); otherwise PT defaults.

Usage:
    python bin/fit.py --target ~/Desktop/pt-iy.wav --max-evals 200
    python bin/fit.py --target ~/Desktop/pt-iy.wav --init iy --max-evals 200
    python bin/fit.py --self-test                       # PT→PT, recovers iy
"""
from __future__ import annotations

import argparse
import io
import json
import subprocess
import sys
import time
import wave
from dataclasses import dataclass
from pathlib import Path

import cma                          # type: ignore
import librosa                      # type: ignore
import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
RENDER = HERE / "render-pt.mjs"
ANTHRO_PATH = ROOT / "jeffrey-anthropometry.json"
FITS = ROOT / "fits"

# ─── Parameter space ────────────────────────────────────────────────────
# Order is fixed (CMA-ES sees a numpy vector). Each entry: name, lo, hi,
# initial-pose-defaults map (per-pose) to seed CMA-ES near a sensible spot.
PARAMS = [
    ("tongueIndex",     12.0, 29.0),
    ("tongueDiameter",   2.05, 3.5),
    ("lipMul",           0.0,  1.0),
    ("velumOpen",        0.01, 0.4),
    ("f0",              80.0, 200.0),
    ("tenseness",        0.0,  1.0),
]

POSE_INITS = {
    "iy":    [12.9, 2.43, 1.0, 0.01, 120, 0.6],
    "ah":    [22.0, 3.5,  1.0, 0.01, 120, 0.6],
    "uw":    [27.0, 2.3,  0.6, 0.01, 120, 0.6],
    "schwa": [17.0, 2.8,  1.0, 0.01, 120, 0.6],
    "m":     [17.0, 2.8,  0.0, 0.4,  110, 0.55],
}


def normalize(values: list[float]) -> np.ndarray:
    out = np.zeros(len(PARAMS))
    for i, ((_, lo, hi), v) in enumerate(zip(PARAMS, values)):
        out[i] = (v - lo) / (hi - lo)
    return out


def denormalize(x: np.ndarray) -> dict:
    return {
        name: float(np.clip(x[i], 0, 1) * (hi - lo) + lo)
        for i, (name, lo, hi) in enumerate(PARAMS)
    }


def apply_anthropometry_bounds(bounds_lo: list[float], bounds_hi: list[float]) -> tuple[list[float], list[float]]:
    """Tighten lo/hi using anthropometry priors if available."""
    if not ANTHRO_PATH.exists():
        return bounds_lo, bounds_hi
    anthro = json.loads(ANTHRO_PATH.read_text())
    derived = anthro.get("derived", {})
    lip_mul_max = derived.get("lip_mul_upper_bound")
    if lip_mul_max is not None:
        idx = next(i for i, (n, _, _) in enumerate(PARAMS) if n == "lipMul")
        bounds_hi[idx] = min(bounds_hi[idx], float(lip_mul_max))
    return bounds_lo, bounds_hi


# ─── Render bridge ──────────────────────────────────────────────────────
@dataclass
class RenderConfig:
    duration_s: float = 1.0
    sample_rate: int = 22050
    seed: int = 1


def render_pt(params: dict, cfg: RenderConfig) -> tuple[np.ndarray, int]:
    """Spawn render-pt.mjs as a subprocess; return (mono float32, sr)."""
    body = {**params, "durationS": cfg.duration_s, "sampleRate": cfg.sample_rate, "seed": cfg.seed}
    proc = subprocess.run(
        ["node", str(RENDER), "--params", json.dumps(body), "--out", "-", "--quiet"],
        capture_output=True, check=True,
    )
    wav_bytes = proc.stdout
    with wave.open(io.BytesIO(wav_bytes), "rb") as w:
        sr = w.getframerate()
        n = w.getnframes()
        raw = w.readframes(n)
    samples = np.frombuffer(raw, dtype="<i2").astype(np.float32) / 32767.0
    return samples, sr


def load_target(path: Path, window_start: float = 0.0,
                window_end: float = 1.0) -> tuple[np.ndarray, int]:
    y, sr = librosa.load(str(path), sr=None, mono=True)
    if (window_start > 0.0) or (window_end < 1.0):
        n = y.shape[0]
        a = int(window_start * n)
        b = int(window_end * n)
        y = y[a:b]
    return y.astype(np.float32), int(sr)


# ─── Spectral distance ──────────────────────────────────────────────────
# v0.1 used MFCC time-mean — 20 cepstral coefficients averaged. Lost
# discriminative power between front-close and back-close vowels in the
# first jeffrey-pvc fit (landed at tongueIndex=28 instead of ~12 for /iy/).
#
# v0.2 uses log-mel-spectrogram time-mean *and* time-std — 64 mel bins
# preserve the spectral envelope (formant pattern) better than 20
# cepstral coefficients, and the std term captures dynamic variation
# vs. steady-state. Plus a per-frame MFCC distance via DTW for time-
# robustness, weighted lighter than the spectral envelope term.

def crop_ramps(y: np.ndarray, sr: int) -> np.ndarray:
    head = int(0.1 * sr)
    tail = int(0.15 * sr)
    if y.shape[0] > head + tail + sr * 0.2:
        y = y[head:-tail] if tail else y[head:]
    return y


def spectral_envelope_features(y: np.ndarray, sr: int, n_mels: int = 64) -> np.ndarray:
    """64-bin log-mel time-mean + time-std → 128-dim feature."""
    y = crop_ramps(y, sr)
    if y.shape[0] < int(sr * 0.2):
        return np.zeros(n_mels * 2, dtype=np.float32)
    S = librosa.feature.melspectrogram(y=y, sr=sr, n_mels=n_mels,
                                       n_fft=2048, hop_length=512, fmax=sr / 2)
    log_S = librosa.power_to_db(S, ref=np.max)
    mean = log_S.mean(axis=1)
    std = log_S.std(axis=1)
    return np.concatenate([mean, std]).astype(np.float32)


def estimate_f0(y: np.ndarray, sr: int) -> float | None:
    """librosa pyin → median F0 over voiced frames."""
    y = crop_ramps(y, sr)
    if y.shape[0] < int(sr * 0.2):
        return None
    try:
        f0, voiced, _ = librosa.pyin(y, fmin=70, fmax=350, sr=sr,
                                     frame_length=2048)
    except Exception:
        return None
    f0v = f0[~np.isnan(f0)]
    if f0v.size < 10:
        return None
    return float(np.median(f0v))


def spectral_distance(y_target: np.ndarray, sr_target: int,
                      y_render: np.ndarray, sr_render: int,
                      f0_target: float | None) -> float:
    """Composite distance: log-mel envelope (load-bearing) + F0 penalty.

    The F0 penalty is needed because the log-mel mean is largely insensitive
    to small pitch shifts — but mismatched F0 audibly breaks identity.
    """
    if sr_target != sr_render:
        y_render = librosa.resample(y_render, orig_sr=sr_render, target_sr=sr_target)
        sr_render = sr_target

    a = spectral_envelope_features(y_target, sr_target)
    b = spectral_envelope_features(y_render, sr_render)
    envelope_dist = float(np.linalg.norm(a - b)) / np.sqrt(a.size)  # per-bin RMS

    f0_pen = 0.0
    if f0_target is not None:
        f0_render = estimate_f0(y_render, sr_render)
        if f0_render is not None:
            # cents-style log distance, scaled to roughly match envelope dist
            f0_pen = abs(np.log2(f0_render / f0_target)) * 10.0

    return envelope_dist + f0_pen


# ─── Fit driver ─────────────────────────────────────────────────────────
def fit(target_path: Path, init_pose: str | None, max_evals: int,
        population: int, sigma0: float, duration_s: float,
        window_start: float = 0.0, window_end: float = 1.0) -> dict:
    y_target, sr_target = load_target(target_path, window_start, window_end)
    cfg = RenderConfig(duration_s=duration_s, sample_rate=sr_target)
    f0_target = estimate_f0(y_target, sr_target)

    bounds_lo = [lo for _, lo, _ in PARAMS]
    bounds_hi = [hi for _, _, hi in PARAMS]
    bounds_lo, bounds_hi = apply_anthropometry_bounds(bounds_lo, bounds_hi)

    if init_pose and init_pose in POSE_INITS:
        x0 = normalize(POSE_INITS[init_pose])
    else:
        # midpoint of bounds in normalized space
        x0 = np.full(len(PARAMS), 0.5)

    es = cma.CMAEvolutionStrategy(
        x0, sigma0,
        {
            "bounds": [[0.0] * len(PARAMS), [1.0] * len(PARAMS)],
            "popsize": population,
            "maxfevals": max_evals,
            "verbose": -9,
        },
    )

    history = []
    n_evals = 0
    t0 = time.time()
    best = {"loss": float("inf"), "params": None, "eval": -1}

    while not es.stop() and n_evals < max_evals:
        xs = es.ask()
        losses = []
        for x in xs:
            params = denormalize(np.asarray(x))
            y_r, sr_r = render_pt(params, cfg)
            loss = spectral_distance(y_target, sr_target, y_r, sr_r, f0_target)
            losses.append(loss)
            n_evals += 1
            if loss < best["loss"]:
                best = {"loss": loss, "params": params, "eval": n_evals}
            if n_evals >= max_evals:
                break
        es.tell(xs[: len(losses)], losses)
        history.append({"eval": n_evals, "best_loss": best["loss"], "gen_min": float(min(losses))})
        elapsed = time.time() - t0
        eps = n_evals / elapsed
        sys.stdout.write(
            f"\r  eval {n_evals:4d}/{max_evals}  best {best['loss']:7.3f}  gen-min {min(losses):7.3f}  ({eps:4.1f} eval/s)"
        )
        sys.stdout.flush()

    print()
    return {
        "target": str(target_path),
        "init_pose": init_pose,
        "evals": n_evals,
        "elapsed_s": round(time.time() - t0, 2),
        "best_loss": round(best["loss"], 4),
        "best_params": best["params"],
        "best_eval": best["eval"],
        "history": history,
        "f0_target_hz": round(f0_target, 2) if f0_target else None,
        "loss_version": "log_mel_envelope+f0_log_cents (v0.2)",
        "bounds_used": {n: [lo, hi] for (n, _, _), lo, hi in zip(PARAMS, bounds_lo, bounds_hi)},
    }


# ─── Self-test (PT → PT) ────────────────────────────────────────────────
def self_test(max_evals: int) -> int:
    """Render a known-pose vowel via PT, then fit it back. The recovered
    params should be close to the source (or at minimum, the loss should
    drop substantially below a random baseline)."""
    cfg = RenderConfig(duration_s=1.0)
    truth = {"tongueIndex": 12.9, "tongueDiameter": 2.43, "lipMul": 1.0,
             "velumOpen": 0.01, "f0": 120.0, "tenseness": 0.6, "loudness": 0.8}
    print(f"→ self-test: render truth /iy/, then fit it")
    print(f"  truth: {truth}")
    y, sr = render_pt(truth, cfg)
    target = Path("/tmp/pt-self-test-target.wav")
    with wave.open(str(target), "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(sr)
        w.writeframes((np.clip(y, -1, 1) * 32767).astype("<i2").tobytes())
    print(f"  target wav: {target}")

    result = fit(target, init_pose=None, max_evals=max_evals,
                 population=8, sigma0=0.4, duration_s=cfg.duration_s)
    print(f"  best loss: {result['best_loss']}")
    print(f"  best params: {result['best_params']}")
    # Diagnostic — distance per param dimension
    for name, _, _ in PARAMS:
        recovered = result["best_params"][name]
        truth_v = truth[name]
        print(f"    {name:18s}  truth {truth_v:7.3f}  fit {recovered:7.3f}  Δ {recovered - truth_v:+.3f}")
    return 0


# ─── CLI ────────────────────────────────────────────────────────────────
def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--target", type=Path, help="path to target WAV")
    parser.add_argument("--init", type=str, default=None,
                        choices=list(POSE_INITS.keys()),
                        help="seed pose for CMA-ES start point")
    parser.add_argument("--max-evals", type=int, default=200)
    parser.add_argument("--population", type=int, default=8)
    parser.add_argument("--sigma0", type=float, default=0.3)
    parser.add_argument("--duration", type=float, default=1.0,
                        help="render duration to compare; should match target")
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--out", type=Path, default=None,
                        help="write fit JSON here (default: fits/<basename>.json)")
    parser.add_argument("--window-start", type=float, default=0.0,
                        help="crop target to [start, end] (fraction of duration); "
                             "use to fit one pose to part of a transitional word")
    parser.add_argument("--window-end", type=float, default=1.0)
    args = parser.parse_args()

    if args.self_test:
        return self_test(args.max_evals)

    if args.target is None:
        parser.error("--target is required (or use --self-test)")

    win_label = (f"  window=[{args.window_start:.2f}, {args.window_end:.2f}]"
                 if (args.window_start > 0 or args.window_end < 1) else "")
    print(f"→ fit {args.target}  init={args.init}  max-evals={args.max_evals}  pop={args.population}{win_label}")
    result = fit(args.target, args.init, args.max_evals, args.population,
                 args.sigma0, args.duration,
                 args.window_start, args.window_end)
    out_path = args.out or FITS / f"{args.target.stem}.json"
    out_path.parent.mkdir(exist_ok=True)
    out_path.write_text(json.dumps(result, indent=2) + "\n")
    print(f"✓ wrote {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
