"""Pull Fía's ink drawings off photographed grid/dot paper.

For each photo: divide out the paper's own lighting, keep dark *neutral*
pixels (ink) and drop blue grid lines, grey dots and the dark cutting mat;
merge nearby strokes into pieces; write each piece as a black-ink RGBA PNG
with soft alpha, plus a numbered contact sheet for naming.

usage: python extract.py <out_dir> <photo.png> [photo.png ...]
"""
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageOps
from scipy import ndimage as ndi

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
pieces_dir = out / "pieces"
pieces_dir.mkdir(exist_ok=True)
index = []

for photo in map(Path, sys.argv[2:]):
    rgb = np.asarray(ImageOps.exif_transpose(Image.open(photo)).convert("RGB")).astype(np.float32) / 255
    gray = rgb.mean(axis=2)
    # The paper: bright regions, closed over so ink inside it counts as paper.
    small = 8
    g_small = gray[::small, ::small]
    paper_small = ndi.binary_closing(g_small > 0.45, iterations=6)
    paper_small = ndi.binary_fill_holes(paper_small)
    paper_small = ndi.binary_erosion(paper_small, iterations=4)   # off the paper edge
    paper = np.kron(paper_small, np.ones((small, small), bool))[: gray.shape[0], : gray.shape[1]]
    # The paper's lighting: a wide max-then-blur of brightness (ink is thin, paper wins).
    bg_small = ndi.maximum_filter(g_small, size=9)
    bg_small = ndi.gaussian_filter(bg_small, 6)
    bg = np.kron(bg_small, np.ones((small, small)))[: gray.shape[0], : gray.shape[1]]
    norm = np.clip(gray / np.maximum(bg, 1e-3), 0, 1.2)
    sat = rgb.max(axis=2) - rgb.min(axis=2)
    # Ink: much darker than its paper and nearly colourless (grid lines are blue).
    strength = np.clip((0.78 - norm) / 0.38, 0, 1) * np.clip((0.16 - sat) / 0.08, 0, 1)
    ink = (strength > 0.35) & paper
    # Grid lines are thin (3-5 px here), the pen is thick (10-14 px): an
    # opening with a disc wider than a grid line keeps only pen strokes,
    # then a small regrow restores their edges from the candidate pixels.
    yy, xx = np.mgrid[-4:5, -4:5]
    disc = (xx * xx + yy * yy) <= 16
    core = ndi.binary_opening(ink, structure=disc)
    ink = ndi.binary_dilation(core, iterations=1) & ink
    labels, count = ndi.label(ink)
    sizes = ndi.sum(ink, labels, range(1, count + 1))
    keep = np.isin(labels, 1 + np.flatnonzero(sizes >= 140))     # specks and dots out
    # Strokes within ~2% of the image of each other belong to one piece.
    reach = max(18, int(0.012 * max(gray.shape)))
    groups, n = ndi.label(ndi.binary_dilation(keep, iterations=reach))
    for slc_i, slc in enumerate(ndi.find_objects(groups)):
        if slc is None:
            continue
        member = (groups[slc] == slc_i + 1) & keep[slc]
        area = member.sum()
        h, w = member.shape
        if area < 900 or h < 40 or w < 40:
            continue
        pad = 24
        y0, y1 = max(0, slc[0].start - pad), min(gray.shape[0], slc[0].stop + pad)
        x0, x1 = max(0, slc[1].start - pad), min(gray.shape[1], slc[1].stop + pad)
        # Soft alpha from the ink strength, limited to this piece's strokes (grown a little).
        own = np.zeros(gray.shape, bool)
        own[slc] = member
        own = ndi.binary_dilation(own[y0:y1, x0:x1], iterations=1)
        alpha = (strength[y0:y1, x0:x1] * own)
        alpha = np.clip((alpha - 0.15) / 0.7, 0, 1)
        rgba = np.zeros((y1 - y0, x1 - x0, 4), np.uint8)
        rgba[..., 3] = (alpha * 255).astype(np.uint8)
        pid = f"{photo.stem.lower()}-{len([i for i in index if i['photo'] == photo.name]):02d}"
        Image.fromarray(rgba, "RGBA").save(pieces_dir / f"{pid}.png")
        index.append({"id": pid, "photo": photo.name, "box": [int(x0), int(y0), int(x1), int(y1)],
                      "ink_px": int(area), "w": int(x1 - x0), "h": int(y1 - y0)})

(out / "pieces.json").write_text(json.dumps(index, indent=1))

# Contact sheet: every piece on white, numbered, in a grid.
cell, cols = 220, 10
rows = (len(index) + cols - 1) // cols
sheet = Image.new("RGB", (cols * cell, rows * (cell + 22)), "white")
draw = ImageDraw.Draw(sheet)
font = ImageFont.load_default(size=16)
for i, item in enumerate(index):
    piece = Image.open(pieces_dir / f"{item['id']}.png")
    piece.thumbnail((cell - 12, cell - 12))
    x, y = (i % cols) * cell, (i // cols) * (cell + 22)
    tile = Image.new("RGB", piece.size, "white")
    tile.paste((0, 0, 0), mask=piece.split()[3])
    sheet.paste(tile, (x + (cell - piece.width) // 2, y + (cell - piece.height) // 2))
    draw.text((x + 4, y + cell), f"{i} {item['id']}", fill=(200, 30, 60), font=font)
sheet.save(out / "contact-sheet.png")
print(f"{len(index)} pieces from {len(sys.argv) - 2} photos")
