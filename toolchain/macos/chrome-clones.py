#!/usr/bin/env python3
"""Report or remove obsolete Chrome signing clones; keep newest/recent/open bundles."""
import argparse
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import time


def candidates(root, opened, now=None):
    now = time.time() if now is None else now
    clones = sorted((p for p in root.glob("code_sign_clone.*")
                     if p.is_dir() and not p.is_symlink()),
                    key=lambda p: p.stat().st_mtime, reverse=True)
    for clone in clones[1:]:  # keep the newest even when it is old
        if now - clone.stat().st_mtime < 7200 or str(clone) in opened:
            continue
        bundle = clone / "Google Chrome.app.bundle"
        if bundle.is_symlink() or list(clone.iterdir()) != [bundle]:
            continue
        try:
            with (bundle / "Contents/Info.plist").open("rb") as stream:
                identifier = plistlib.load(stream).get("CFBundleIdentifier")
        except (OSError, ValueError, plistlib.InvalidFileException):
            continue
        if identifier == "com.google.Chrome":
            yield clone


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()
    user_dir = subprocess.check_output(["getconf", "DARWIN_USER_DIR"], text=True).strip()
    root = (Path(user_dir).parent / "X/com.google.Chrome.code_sign_clone").resolve()
    if not root.is_dir() or root.is_symlink():
        return
    opened = subprocess.run(["/usr/sbin/lsof", "-nP", "-u", str(os.getuid()), "-Fn"],
                            capture_output=True, text=True, timeout=30)
    if opened.returncode or opened.stderr.strip():
        raise SystemExit("Chrome clones kept: could not verify open files")
    for path in candidates(root, opened.stdout):
        print(("Remove " if args.apply else "Candidate ") + str(path))
        if args.apply:
            shutil.rmtree(path)


if __name__ == "__main__":
    main()
