#!/usr/bin/env python3
"""bundle-review.py — a self-contained copy of the imab review pages.

review.html and out/melodyproof.html reach into the repo (../artist,
../../system) and the cache (~/.cache/ac/imab). This copies every
referenced file into ONE folder and rewrites the references, so the
folder can sit on a Desktop or be zipped for someone else.

  pop/.venv/bin/python pop/imab/bin/bundle-review.py [dest]   (default ~/Desktop/imab-review)
"""
import json, os, re, shutil, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
LANE = HERE.parent
CACHE = Path.home() / ".cache/ac/imab"
DEST = Path(sys.argv[1]).expanduser() if len(sys.argv) > 1 else Path.home() / "Desktop/imab-review"
DEST.mkdir(parents=True, exist_ok=True)
copied = 0


def put(src: Path, rel: str):
    global copied
    d = DEST / rel
    d.parent.mkdir(parents=True, exist_ok=True)
    if not d.exists() or d.stat().st_mtime < src.stat().st_mtime:
        shutil.copy2(src, d); copied += 1
    return rel


def rewrite_ref(ref: str, base: Path, page_dir_rel: str):
    """map one src/href to a path inside the bundle, relative to the page."""
    if ref.startswith(("http", "#", "data:")):
        return ref
    path, _, q = ref.partition("?")
    if path.startswith("file://"):
        p = Path(path[7:])
        try:
            rel = "cache/" + str(p.relative_to(CACHE))
        except ValueError:
            rel = "assets/" + p.name
    else:
        p = (base / path).resolve()
        try:
            rel = str(p.relative_to(LANE))            # out/…, samples/… keep their shape
        except ValueError:
            rel = "assets/" + p.name                  # ../artist, ../../system → assets/
    if not p.exists():
        print(f"  ! missing {p}"); return ref
    put(p, rel)
    up = "../" * (page_dir_rel.count("/") + 1) if page_dir_rel else ""
    return up + rel + (("?" + q) if q else "")


def bundle_page(page: Path, page_dir_rel: str):
    html = page.read_text()
    base = page.parent
    def sub(m):
        return f'{m.group(1)}="{rewrite_ref(m.group(2), base, page_dir_rel)}"'
    html = re.sub(r'\b(src|href)="([^"]+)"', sub, html)
    return html


# review.html at the bundle root
(DEST / "review.html").write_text(bundle_page(LANE / "review.html", ""))

# the syllawizard pages are self-contained (base64) — copied by the href walk above.
# melodyproof.html: its media paths live inside the embedded JSON
mp = (LANE / "out/melodyproof.html").read_text()
for p in set(re.findall(r'"/Users/[^"]+?\.(?:mp3|png|wav)"', mp)):
    src = Path(json.loads(p))
    try:
        rel = "cache/" + str(src.relative_to(CACHE))
    except ValueError:
        rel = "assets/" + src.name
    if src.exists():
        put(src, rel)
        mp = mp.replace(p, json.dumps("../" + rel))
(DEST / "out").mkdir(exist_ok=True)
(DEST / "out/melodyproof.html").write_text(mp)

size = sum(f.stat().st_size for f in DEST.rglob("*") if f.is_file()) / 1e6
print(f"✓ {DEST}  ({copied} files copied this run, {size:.0f} MB total)")
