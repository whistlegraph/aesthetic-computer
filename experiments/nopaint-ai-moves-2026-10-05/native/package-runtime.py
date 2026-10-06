#!/usr/bin/env python3
"""Copy the public runtime sources without paintings, credentials, or model caches."""
import argparse
from pathlib import Path
import shutil

EXPERIMENT = Path(__file__).resolve().parents[1]
REPO = EXPERIMENT.parents[1]


def package(destination):
    destination = Path(destination).resolve()
    destination.mkdir(parents=True, exist_ok=False)
    files = [p for p in EXPERIMENT.glob("*.py") if not p.name.startswith("test_")]
    files += [EXPERIMENT / name for name in (
        "play.html", "ac_account.mjs", "done.mjs", "package.json", "package-lock.json",
        "local-requirements.txt", "native/start-backend.sh")]
    for folder in ("starts", "vendor"):
        files += [p for p in (EXPERIMENT / folder).rglob("*") if p.is_file() and "__pycache__" not in p.parts]
    files += [REPO / name for name in (
        "aesel/src/ac-session.mjs", "aesel/src/account-access.mjs", "aesel/src/publish-picture.mjs",
        "aesel/src/picture-wip.mjs", "aesel/media/picture/png.mjs", "aesel/media/picture/ac-tools.mjs",
        "experiments/image-model-turntables-2026-09-23/provider_image.py")]
    for source in files:
        target = destination / source.relative_to(REPO)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
    return len(files)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    args = parser.parse_args()
    print(f"Packaged {package(args.destination)} public runtime files in {args.destination}")
