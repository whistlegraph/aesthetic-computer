#!/usr/bin/env python3
"""Unpack a Construct web export and patch known runtime bugs.

usage: patch-export.py <export.zip> <out-dir>

Construct r495.2 bug: DrawingCanvas.Instance.Draw, when a canvas that carries
an effect is drawn while another canvas pastes it (Noise, Sharpen), calls
setFromQuad3d on a Quad3D temp that has no such method. The paste command
rejects, the target canvas's draw queue jams, and it renders black. The
sibling branch uses u.copy(...); do the same here.
"""
import os, shutil, sys, zipfile

PATCHES = [
    ("scripts/c3runtime.js",
     "this._inst._IsDrawingWithEffects()?(u.setFromQuad3d(t.GetBoundingQuad()),a=u)",
     "this._inst._IsDrawingWithEffects()?(u.copy(t.GetBoundingQuad()),a=u)"),
]

zip_path, out = sys.argv[1], sys.argv[2]
shutil.rmtree(out, ignore_errors=True)
z = zipfile.ZipFile(zip_path)
for info in z.infolist():
    name = info.filename.replace("\\", "/")  # Construct zips use Windows separators
    if name.endswith("/"):
        continue
    dest = os.path.join(out, name)
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    with open(dest, "wb") as f:
        f.write(z.read(info))

for rel, old, new in PATCHES:
    path = os.path.join(out, rel)
    src = open(path, encoding="utf-8").read()
    if src.count(old) != 1:
        sys.exit(f"patch target not found exactly once in {rel} — Construct changed; re-check the bug")
    open(path, "w", encoding="utf-8").write(src.replace(old, new))
    print(f"patched {rel}")
