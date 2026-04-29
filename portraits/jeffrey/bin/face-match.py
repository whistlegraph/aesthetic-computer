#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Local face-match: identify which images contain jeffrey, with similarity scores.

Pipeline:
  1. Build a reference embedding-set from confirmed-jeffrey photos
     (the shoot/--01..--10 master-tier headshots by default).
  2. For each input image: detect faces (RetinaFace via insightface),
     embed each face (ArcFace), compute cosine similarity to the closest
     reference, threshold to a yes/no decision.
  3. Emit one JSONL record per image.

This is the IDENTITY step. No API calls, no costs. ~50–100 ms per image
on CPU. Run before the optional description layer (face-describe.py),
which calls a vision API only on the matched subset.

Usage:
  python face-match.py \\
      --input "$VAULT/jeffrey-platter/ig-archive/whistlegraph/*.jpg" \\
      --output $VAULT/jeffrey-platter/curated/jeffrey-match.jsonl

Args:
  --input <glob>          glob of input image paths. Required.
  --output <path>         JSONL output. Resumes by skipping already-tagged paths. Required.
  --references <glob>     glob of reference photos. Default: shoot/--01..--10 masters.
  --threshold <float>     cosine-similarity threshold for is_jeffrey. Default: 0.5
                          (insightface default). Lower = more recall, more false positives.
  --limit <int>           process at most N images (smoke testing). Default: all.
  --min-face-size <int>   reject faces smaller than this (pixels). Default: 60.

Output JSONL record:
  {
    "path": "/abs/path",
    "rel_path": "filename.jpg",
    "date": "YYYY-MM-DD" or null,
    "shortcode": "..." or null,
    "n_faces": 2,
    "best_similarity": 0.71,
    "best_match_ref": "jeffery-av--04.jpg",
    "is_jeffrey": true,
    "faces": [{"bbox": [x,y,w,h], "similarity": 0.71, "matched_ref": "..."}, ...],
    "error": null
  }
"""

from __future__ import annotations

import argparse
import gc
import json
import re
import sys
from datetime import datetime, timezone
from glob import glob
from pathlib import Path

import cv2
import numpy as np
from insightface.app import FaceAnalysis

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
DEFAULT_REFS_GLOB = str(REPO_ROOT / "portraits/jeffrey/corpus/shoot/jeffery-av--0[1-9].jpg")
ALSO_REF_10 = str(REPO_ROOT / "portraits/jeffrey/corpus/shoot/jeffery-av--10.jpg")

DATE_RE = re.compile(r"(\d{4}-\d{2}-\d{2})_([A-Za-z0-9_-]+?)(?:_\d+)?\.")


def parse_filename(name: str) -> tuple[str | None, str | None]:
    m = DATE_RE.search(name)
    if not m:
        return None, None
    return m.group(1), m.group(2)


def cosine_sim(a: np.ndarray, b: np.ndarray) -> float:
    return float(np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b)))


def build_references(app: FaceAnalysis, ref_paths: list[Path]) -> list[tuple[str, np.ndarray]]:
    """Return [(ref_name, embedding), ...] for every face found in references."""
    refs: list[tuple[str, np.ndarray]] = []
    for path in ref_paths:
        img = cv2.imread(str(path))
        if img is None:
            print(f"warn: could not read reference {path}", file=sys.stderr)
            continue
        faces = app.get(img)
        if not faces:
            print(f"warn: no face detected in reference {path.name}", file=sys.stderr)
            continue
        # Pick the largest face (the shoot photos are face-centered, but be safe)
        face = max(faces, key=lambda f: (f.bbox[2] - f.bbox[0]) * (f.bbox[3] - f.bbox[1]))
        refs.append((path.name, face.normed_embedding))
    if not refs:
        sys.exit("no usable reference faces — check reference paths")
    return refs


def best_match(emb: np.ndarray, refs: list[tuple[str, np.ndarray]]) -> tuple[float, str]:
    sims = [(cosine_sim(emb, ref_emb), name) for name, ref_emb in refs]
    sims.sort(reverse=True)
    return sims[0]


def main() -> int:
    p = argparse.ArgumentParser(description="Local face-match against jeffrey references.")
    p.add_argument("--input", required=True)
    p.add_argument("--output", required=True)
    p.add_argument("--references", default=None, help="glob; default: shoot/--01..--10")
    p.add_argument("--threshold", type=float, default=0.5)
    p.add_argument("--limit", type=int, default=0)
    p.add_argument("--min-face-size", type=int, default=60)
    args = p.parse_args()

    # Resolve refs
    if args.references:
        ref_paths = [Path(p) for p in sorted(glob(args.references))]
    else:
        ref_paths = [Path(p) for p in sorted(glob(DEFAULT_REFS_GLOB))]
        if Path(ALSO_REF_10).exists():
            ref_paths.append(Path(ALSO_REF_10))
    if not ref_paths:
        sys.exit("no reference photos found — run fetch-corpus.mjs --shoot first")
    print(f"references: {len(ref_paths)} ({ref_paths[0].name} … {ref_paths[-1].name})", file=sys.stderr)

    # Resolve inputs + resume
    inputs = sorted(Path(p) for p in glob(args.input))
    if not inputs:
        sys.exit(f"no inputs matched: {args.input}")
    out_path = Path(args.output).expanduser().resolve()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    already: set[str] = set()
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            try:
                already.add(json.loads(line)["path"])
            except (json.JSONDecodeError, KeyError):
                pass
    inputs = [p for p in inputs if str(p.resolve()) not in already]
    if args.limit:
        inputs = inputs[: args.limit]
    print(f"to process: {len(inputs)} images ({len(already)} already done)", file=sys.stderr)
    if not inputs:
        return 0

    # Init insightface — buffalo_l is the default 512-dim ArcFace
    print("loading insightface model (buffalo_l)…", file=sys.stderr)
    app = FaceAnalysis(name="buffalo_l", providers=["CPUExecutionProvider"])
    app.prepare(ctx_id=0, det_size=(640, 640))

    refs = build_references(app, ref_paths)
    print(f"built {len(refs)} reference embeddings", file=sys.stderr)

    n_jeffrey = 0
    n_no_face = 0
    n_other_face = 0
    n_err = 0

    with out_path.open("a") as f:
        for i, image_path in enumerate(inputs, 1):
            date, shortcode = parse_filename(image_path.name)
            record = {
                "path": str(image_path.resolve()),
                "rel_path": image_path.name,
                "date": date,
                "shortcode": shortcode,
                "tagged_at": datetime.now(timezone.utc).isoformat(),
                "error": None,
            }
            try:
                img = cv2.imread(str(image_path))
                if img is None:
                    raise ValueError("could not decode image")
                faces = app.get(img)
                # Filter tiny faces (often false positives)
                faces = [
                    fa for fa in faces
                    if (fa.bbox[2] - fa.bbox[0]) >= args.min_face_size
                    and (fa.bbox[3] - fa.bbox[1]) >= args.min_face_size
                ]
                record["n_faces"] = len(faces)
                if not faces:
                    record["best_similarity"] = 0.0
                    record["best_match_ref"] = None
                    record["is_jeffrey"] = False
                    record["faces"] = []
                    n_no_face += 1
                else:
                    face_records = []
                    for face in faces:
                        sim, ref_name = best_match(face.normed_embedding, refs)
                        x1, y1, x2, y2 = face.bbox
                        face_records.append({
                            "bbox": [int(x1), int(y1), int(x2 - x1), int(y2 - y1)],
                            "similarity": round(sim, 4),
                            "matched_ref": ref_name,
                        })
                    face_records.sort(key=lambda r: r["similarity"], reverse=True)
                    best = face_records[0]
                    record["best_similarity"] = best["similarity"]
                    record["best_match_ref"] = best["matched_ref"]
                    record["is_jeffrey"] = best["similarity"] >= args.threshold
                    record["faces"] = face_records
                    if record["is_jeffrey"]:
                        n_jeffrey += 1
                    else:
                        n_other_face += 1
            except Exception as e:
                record["error"] = f"{type(e).__name__}: {e}"
                record["is_jeffrey"] = False
                n_err += 1

            f.write(json.dumps(record) + "\n")
            f.flush()

            if i % 50 == 0:
                gc.collect()

            tag = "✓" if record.get("is_jeffrey") else (" " if record.get("n_faces") else "·")
            err = f" ERR={record['error']}" if record.get("error") else ""
            sim = record.get("best_similarity", 0.0)
            if i % 25 == 0 or i == len(inputs) or err:
                print(
                    f"[{i}/{len(inputs)}] {tag} {record['rel_path']} "
                    f"faces={record.get('n_faces', 0)} sim={sim:.3f}{err}",
                    file=sys.stderr,
                )

    print(
        f"\ndone. jeffrey: {n_jeffrey}, other-face: {n_other_face}, no-face: {n_no_face}, errors: {n_err}",
        file=sys.stderr,
    )
    print(f"output: {out_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
