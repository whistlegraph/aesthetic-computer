#!/usr/bin/env python3
"""measure-jeffrey.py — anthropometric measurement pass over the platter.

Reads photographs (default: `portraits/jeffrey/corpus/shoot/*.jpg`),
runs MediaPipe Face Mesh with iris-landmarks (468 + 4 iris), extracts
the head-anthropometry features defined in PHYSIOLOGY.md, anchors
absolute scale via interpupillary distance (population mean 6.30 cm),
applies the Lammert+Narayanan VTL regression, and writes
`pop/voice/jeffrey-anthropometry.json` — the prior file that `fit.py`
clamps Pink Trombone's parameter envelope against.

Identity validation is skipped on the default corpus (the AV shoot is
all jeffrey by construction). When run against the IG archive — where
non-jeffrey faces appear — wire `--identity-jsonl` to a face-match.py
output and we'll filter to confirmed-jeffrey rows.

Run:
    pip install mediapipe opencv-python numpy   # in pop/.venv
    python bin/measure-jeffrey.py
    python bin/measure-jeffrey.py --glob 'portraits/jeffrey/corpus/shoot/*.jpg'
    python bin/measure-jeffrey.py --debug-out /tmp/measure-debug
"""
from __future__ import annotations

import argparse
import glob as globlib
import json
import math
import sys
from dataclasses import dataclass, field
from pathlib import Path
from statistics import median, quantiles
from typing import Iterator

import cv2                      # type: ignore
import mediapipe as mp          # type: ignore
from mediapipe.tasks import python as mp_python    # type: ignore
from mediapipe.tasks.python import vision as mp_vision    # type: ignore
import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent                       # pop/voice/
REPO = ROOT.parent.parent                # aesthetic-computer/
PLATTER = REPO / "papers" / "jeffrey-platter"
OUT_PATH = ROOT / "jeffrey-anthropometry.json"
DEFAULT_GLOB = str(REPO / "portraits" / "jeffrey" / "corpus" / "shoot" / "*.jpg")
MODEL_PATH = ROOT / ".cache" / "face_landmarker.task"

# ─── MediaPipe Face Mesh landmark indices ──────────────────────────────
# 468-point canonical face mesh. These indices are stable across mediapipe
# versions. Cross-reference against
# https://github.com/google/mediapipe/blob/master/mediapipe/python/solutions/face_mesh.py
LMK = {
    "forehead":      10,    # vertex (top of head proxy — actual vertex is above mesh)
    "chin":          152,   # menton
    "left_zygion":   234,
    "right_zygion":  454,
    "left_gonion":   172,
    "right_gonion":  397,
    "left_cheilion": 61,
    "right_cheilion": 291,
    "subnasale":     2,
    "upper_lip_top": 0,
    "upper_lip_cupid": 13,
    "lower_lip_bot": 17,
    "left_alar":     219,
    "right_alar":    438,
    "left_iris_center":  468,    # iris landmarks (require refined-landmarks=True)
    "right_iris_center": 473,
}

# Population priors (used when the corresponding measurement is absent
# from a frame — never as a substitute for measurement).
INTERPUPILLARY_DISTANCE_CM = 6.30   # adult mean (Dodgson 2004)


# ─── VTL estimation ────────────────────────────────────────────────────
# We do NOT carry placeholder Lammert+Narayanan coefficients here — the
# actual paper's regression uses internal MRI-derived measurements that
# we can't reproduce from external photographs alone. Instead we use
# Fitch+Giedd 1999's body-height regression as the central estimate
# (population median when no height is stated), and treat the photo-
# derived measurements as *diagnostics* + bounds on PT's articulator
# range — not as inputs to a VTL closed-form.
#
# When jeffrey provides --height-cm, we tighten the central estimate
# accordingly. A real Lammert-style multi-feature fit needs the actual
# paper coefficients, which are deferred to a future pass.

def vtl_fitch_giedd(height_cm: float | None) -> tuple[float, float]:
    """Body-height VTL regression. Fitch & Giedd 1999.

    Adult male median: 17.5 cm at 175 cm body height, slope ~0.05 per cm.
    Population residual: ~0.8 cm at 1σ.
    """
    if height_cm is None:
        return 17.5, 0.8
    return 17.5 + 0.05 * (height_cm - 175.0), 0.8


# ─── Schema ────────────────────────────────────────────────────────────
@dataclass
class FrameMeasurements:
    source: str
    # NB. mediapipe FaceMesh has no skull-vertex landmark — landmark 10 is
    # the upper forehead surface. So "head_height" is really
    # forehead-to-chin. Keep that name explicit so we don't accidentally
    # treat it as full cranial height in downstream regressions.
    forehead_to_chin_cm: float | None = None
    bizygomatic_width_cm: float | None = None
    mandible_length_cm: float | None = None
    lip_width_cm: float | None = None
    lip_aperture_cm: float | None = None
    philtrum_cm: float | None = None
    nose_to_chin_cm: float | None = None
    nostril_width_cm: float | None = None
    ipd_px: float | None = None           # diagnostic — pixel anchor used for cm scaling
    rejected_reason: str | None = None    # e.g. "identity", "no_face", "oblique"
    raw: dict = field(default_factory=dict)  # all raw 3D landmarks for debug


# ─── Photo iterator ────────────────────────────────────────────────────
def iter_glob_photos(pattern: str) -> Iterator[tuple[str, Path]]:
    """Yield (bucket-tag, path) for files matching the glob. Bucket is
    inferred from the parent directory name."""
    for p in sorted(globlib.glob(pattern)):
        path = Path(p)
        if not path.is_file():
            continue
        yield path.parent.name, path


def iter_paths_file(paths_file: Path) -> Iterator[tuple[str, Path]]:
    """Yield (bucket-tag, path) for each line in a list-of-paths file."""
    for line in paths_file.read_text().splitlines():
        s = line.strip()
        if not s or s.startswith("#"):
            continue
        path = Path(s)
        if not path.is_file():
            continue
        yield path.parent.name, path


# ─── Landmark → measurement extraction ─────────────────────────────────
def _dist3(a: np.ndarray, b: np.ndarray) -> float:
    return float(np.linalg.norm(a - b))


def _resize_to_max(image: np.ndarray, max_side: int) -> np.ndarray:
    h, w = image.shape[:2]
    long_side = max(h, w)
    if long_side <= max_side:
        return image
    scale = max_side / long_side
    return cv2.resize(image, (int(w * scale), int(h * scale)), interpolation=cv2.INTER_AREA)


def measure_frame(image: np.ndarray, source: str, landmarker,
                  debug_out: Path | None = None) -> FrameMeasurements:
    """Run MediaPipe FaceLandmarker on the image and extract anthropometry."""
    # mediapipe's detector scales input to ~256-512 internally; on a
    # 6000x4000 master headshot the face occupies <13% of the frame and
    # gets shrunk to <50px which the detector misses. Pre-resize.
    image = _resize_to_max(image, 1280)
    h, w = image.shape[:2]
    rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    mp_image = mp.Image(image_format=mp.ImageFormat.SRGB, data=rgb)
    result = landmarker.detect(mp_image)
    if not result.face_landmarks:
        return FrameMeasurements(source=source, rejected_reason="no_face")
    lms = result.face_landmarks[0]
    if len(lms) < 478:
        return FrameMeasurements(source=source, rejected_reason="no_iris")

    # Pull all required landmarks as (x_px, y_px, z_relative).
    pt = lambda i: np.array([lms[i].x * w, lms[i].y * h, lms[i].z * w], dtype=np.float64)
    P = {name: pt(idx) for name, idx in LMK.items()}

    # Anchor scale via interpupillary distance.
    ipd_px = _dist3(P["left_iris_center"][:2], P["right_iris_center"][:2])
    if ipd_px < 10:
        return FrameMeasurements(source=source, rejected_reason="oblique_or_small")
    cm_per_px = INTERPUPILLARY_DISTANCE_CM / ipd_px

    def cm(a: np.ndarray, b: np.ndarray) -> float:
        return _dist3(a[:2], b[:2]) * cm_per_px

    fm = FrameMeasurements(
        source=source,
        forehead_to_chin_cm   = cm(P["forehead"],     P["chin"]),
        bizygomatic_width_cm  = cm(P["left_zygion"],  P["right_zygion"]),
        mandible_length_cm    = cm(P["left_gonion"],  P["chin"]),
        lip_width_cm          = cm(P["left_cheilion"], P["right_cheilion"]),
        lip_aperture_cm       = cm(P["upper_lip_top"], P["lower_lip_bot"]),
        philtrum_cm           = cm(P["subnasale"],     P["upper_lip_cupid"]),
        nose_to_chin_cm       = cm(P["subnasale"],     P["chin"]),
        nostril_width_cm      = cm(P["left_alar"],     P["right_alar"]),
        ipd_px                = ipd_px,
    )

    if debug_out is not None:
        debug_out.mkdir(parents=True, exist_ok=True)
        annotated = image.copy()
        for name, p in P.items():
            x, y = int(p[0]), int(p[1])
            cv2.circle(annotated, (x, y), 4, (0, 255, 200), -1)
            cv2.putText(annotated, name, (x + 6, y - 6),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.4, (0, 255, 200), 1)
        cv2.imwrite(str(debug_out / (Path(source).stem + "__landmarks.jpg")), annotated)
    return fm


def load_image(path: Path) -> np.ndarray | None:
    img = cv2.imread(str(path))
    return img


# ─── Aggregation ───────────────────────────────────────────────────────
def aggregate(frames: list[FrameMeasurements]) -> dict:
    """Median + IQR for each measurement, ignoring None values."""
    fields = [
        "forehead_to_chin_cm", "bizygomatic_width_cm", "mandible_length_cm",
        "lip_width_cm", "lip_aperture_cm", "philtrum_cm",
        "nose_to_chin_cm", "nostril_width_cm",
    ]
    out = {}
    for f in fields:
        values = [getattr(fr, f) for fr in frames if getattr(fr, f) is not None]
        if not values:
            out[f] = {"median": None, "iqr": None, "n": 0}
            continue
        if len(values) >= 4:
            q = quantiles(values, n=4, method="inclusive")
            iqr = [round(q[0], 2), round(q[2], 2)]
        else:
            iqr = [round(min(values), 2), round(max(values), 2)]
        out[f] = {"median": round(median(values), 2), "iqr": iqr, "n": len(values)}
    return out


def derive_pt_priors(measurements: dict, height_cm: float | None) -> dict:
    """Convert anthropometry into Pink Trombone parameter priors.

    For now, VTL central is from Fitch-Giedd (body-height-based).
    The photo-derived measurements bound the *articulator* params
    (lip aperture, etc.) but are not yet fed into a closed-form VTL.
    """
    vtl, sigma = vtl_fitch_giedd(height_cm)

    # Lip aperture max (cm, from corpus) → PT's lipMul upper bound.
    # PT's restDiameter[lipStart..n-1] = 1.5 * lipMul (where 1.5 cm is the
    # PT-default rest aperture). So a measured 2.02 cm max → lipMul ≤ 2.02/1.5 ≈ 1.35.
    # In normalized [0..1] CMA-ES space we still cap at 1.0 so we don't
    # over-open relative to PT's calibration.
    lip_p75 = (measurements.get("lip_aperture_cm", {}).get("iqr") or [None, None])[1]
    lip_max_cm = lip_p75 if lip_p75 is not None else 1.5
    return {
        "vtl_cm": {
            "central": round(vtl, 2),
            "ci95": [round(vtl - 1.96 * sigma, 2), round(vtl + 1.96 * sigma, 2)],
            "model": "fitch-giedd-1999",
            "input": ("body-height-stated" if height_cm is not None else "population-median-male"),
        },
        # PT default: 44 segments × 0.396 cm each = 17.4 cm. Personalize
        # by varying segment length, keeping segment count constant.
        "tract_segments_44": {"segment_length_cm": round(vtl / 44.0, 4)},
        "lip_aperture_max_cm": round(lip_max_cm, 2),
        "lip_mul_upper_bound": round(min(1.0, lip_max_cm / 1.5), 3),
        "mandible_rotation_max_deg": 22,  # placeholder; real value needs jaw-open vs jaw-closed pair
        "notes": [
            "VTL central uses Fitch+Giedd 1999 body-height regression.",
            "Photo-derived measurements (forehead-to-chin, bizygomatic, mandible) are not yet wired into a closed-form VTL — Lammert-Narayanan 2015 needs MRI-derived inputs we don't have.",
            "Lip aperture upper bound comes from corpus 75th percentile (max would be too sensitive to selfie outliers).",
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--glob", default=DEFAULT_GLOB,
                        help=f"glob of input images (default: AV shoot cache)")
    parser.add_argument("--paths-file", type=Path, default=None,
                        help="read newline-separated image paths from file (overrides --glob)")
    parser.add_argument("--limit", type=int, default=None,
                        help="cap on photos processed")
    parser.add_argument("--height-cm", type=float, default=None,
                        help="jeffrey's stated body height; tightens VTL prior via Fitch-Giedd")
    parser.add_argument("--debug-out", type=Path, default=None,
                        help="write annotated landmark images here")
    parser.add_argument("--out", type=Path, default=OUT_PATH)
    args = parser.parse_args()

    if not MODEL_PATH.exists():
        print(f"✗ model file missing: {MODEL_PATH}", file=sys.stderr)
        print("  download:  curl -sSLfo .cache/face_landmarker.task https://storage.googleapis.com/mediapipe-models/face_landmarker/face_landmarker/float16/latest/face_landmarker.task",
              file=sys.stderr)
        return 1
    options = mp_vision.FaceLandmarkerOptions(
        base_options=mp_python.BaseOptions(model_asset_path=str(MODEL_PATH)),
        running_mode=mp_vision.RunningMode.IMAGE,
        num_faces=1,
        output_face_blendshapes=False,
        output_facial_transformation_matrixes=False,
        min_face_detection_confidence=0.5,
    )
    landmarker = mp_vision.FaceLandmarker.create_from_options(options)

    frames: list[FrameMeasurements] = []
    rejected: dict[str, int] = {}
    counts: dict[str, int] = {}

    photo_iter = (iter_paths_file(args.paths_file) if args.paths_file
                  else iter_glob_photos(args.glob))
    for i, (bucket, path) in enumerate(photo_iter):
        if args.limit is not None and i >= args.limit:
            break
        counts[bucket] = counts.get(bucket, 0) + 1
        img = load_image(path)
        if img is None:
            fm = FrameMeasurements(source=str(path), rejected_reason="read_failed")
        else:
            fm = measure_frame(img, str(path), landmarker, debug_out=args.debug_out)
        frames.append(fm)
        if fm.rejected_reason:
            rejected[fm.rejected_reason] = rejected.get(fm.rejected_reason, 0) + 1
            print(f"  - {path.name}  rejected: {fm.rejected_reason}")
        else:
            print(f"  ✓ {path.name}  ipd_px={fm.ipd_px:.1f}  "
                  f"forehead-chin={fm.forehead_to_chin_cm:.1f}cm  bizy={fm.bizygomatic_width_cm:.1f}cm")

    landmarker.close()

    if not frames:
        print("✗ no images matched", file=sys.stderr)
        return 1

    measurements = aggregate(frames)
    derived = derive_pt_priors(measurements, args.height_cm)

    out = {
        "version": 1,
        "generated": "2026-05-04",
        "generator": "pop/voice/bin/measure-jeffrey.py v0.2",
        "source_glob": args.glob,
        "source_corpus": counts,
        "rejected": rejected,
        "measurements_cm": measurements,
        "derived": derived,
        "scale_anchor": {
            "method": "interpupillary distance",
            "ipd_cm_used": INTERPUPILLARY_DISTANCE_CM,
            "ipd_px_median": round(median([f.ipd_px for f in frames if f.ipd_px]), 1)
                if any(f.ipd_px for f in frames) else None,
            "rationale": "Adult-population mean (Dodgson 2004). Replace with stated height + Lammert 2015 if/when --height-cm is provided.",
        },
        "notes": [
            "Single-pass mediapipe FaceMesh on local AV-shoot cache.",
            "Identity validation skipped (AV shoot is jeffrey by construction). Wire --identity-jsonl when running against IG archive.",
        ],
    }
    args.out.write_text(json.dumps(out, indent=2) + "\n")
    n_ok = sum(1 for f in frames if not f.rejected_reason)
    print(f"\n✓ wrote {args.out}")
    print(f"  frames: {len(frames)}  measured: {n_ok}  rejected: {sum(rejected.values())}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
