#!/usr/bin/env python3
"""Build a visual index of every CultureHub material — one page-set, all art.

    python3 materials.py && chrome --headless --print-to-pdf=materials.pdf materials.html

The program brief argues a case and shows only what that case needs. This is
the other document: a plain contact sheet of everything that exists, so a
single scroll answers "what have we actually got." Sources are read from their
real homes rather than copied, so it cannot drift from the originals.
"""
import base64, pathlib, subprocess, tempfile

HERE = pathlib.Path(__file__).resolve().parent
ACC = HERE.parent
REPO = ACC.parents[2]
MN = REPO / "marketing/campaigns/macneopolitan"
PKT = ACC / "packet" / "assets"
TMP = pathlib.Path(tempfile.mkdtemp(prefix="materials-"))

# (source, title, note, group)
ITEMS = [
    (MN / "gens/program.png", "Nine laptops — the two-work image", "gens/program.png · the image the event page runs", "Program"),
    (MN / "gens/program-square.png", "Square crop", "1600×1600 · social cards", "Program"),
    (MN / "gens/program-portrait.png", "Portrait crop", "1400×1750 · Instagram, printed flyer", "Program"),

    (MN / "gens/block.png", "The Neapolitan block", "gens/block.png · all twelve semitones, TrackDrum, Menu Band UI", "MacNeoPolitan"),
    (MN / "gens/trio.png", "Inside the ring", "gens/trio.png · three machines in dialogue, chalk pulse ring", "MacNeoPolitan"),
    (MN / "gens/menubar.png", "A piano beside the clock", "gens/menubar.png · the argument in one frame", "MacNeoPolitan"),

    (MN / "refs-extra/canon-neo-citrus.png", "Canonical render — citrus", "toolchain/keyboard · deterministic, not generated", "Deterministic renders"),
    (MN / "refs-extra/canon-neo-indigo.png", "Canonical render — indigo", "render-laptop.mjs --colorway indigo", "Deterministic renders"),
    (MN / "refs-extra/canon-neo-blush.png", "Canonical render — blush", "render-laptop.mjs --colorway blush", "Deterministic renders"),
    (MN / "refs-extra/canon-overlay-check.png", "Overlay check", "overlay.mjs · layout ghosted over the product photo", "Deterministic renders"),
    (MN / "refs-extra/canon-neo-boxes.png", "Glyph box check", "render-laptop.mjs --boxes · legend ink boxes", "Deterministic renders"),

    (MN / "refs-extra/apple-neo-color-lineup.jpg", "The four colourways", "Apple press photo · © Apple · reference only", "Canon"),
    (MN / "refs-extra/trackdrum-icon.png", "TrackDrum", "rendered from Sources/TrackDrumIcon.swift", "Canon"),
    (MN / "refs-extra/notepat-keymap.png", "The twelve semitones", "notepat keymap · from labelByMidiNotepat", "Canon"),
    (MN / "refs-extra/menuband-popover.png", "Menu Band", "Mac App Store screenshot · the real interface", "Canon"),

    (PKT / "special-sign-cover.jpg", "Special Sign — release cover", "Attic Gremlin · 3000×3000", "Special Sign"),
    (PKT / "special-sign-live-spatial-3x2.jpg", "3D spatial overview", "frame @ 1:12", "Special Sign"),
    (PKT / "special-sign-graphic-score-square.jpg", "Graphic score", "thirteen named lanes", "Special Sign"),

    (ACC / "assets/jeffrey-alan-scudder-headshot-3x2-green-laptop.jpg", "Portrait — green laptop", "CHOSEN for the artist page", "Portraits"),
    (ACC / "assets/jeffrey-alan-scudder-headshot-3x2.jpg", "Portrait — plain", "alternate, if a plain headshot is wanted", "Portraits"),
    (ACC / "assets/jeffrey-alan-scudder-headshot-3x2-seated.jpg", "Portrait — seated", "alternate", "Portraits"),
    (ACC / "assets/notepat-jam-3x2.png", "Notepat Jam — showroom", "superseded · still live on the public page", "Portraits"),
]


def thumb(src: pathlib.Path, i: int) -> str:
    out = TMP / f"{i:02d}.jpg"
    subprocess.run(["sips", "-Z", "1000", "-s", "format", "jpeg", str(src), "--out", str(out)],
                   check=True, capture_output=True)
    return "data:image/jpeg;base64," + base64.b64encode(out.read_bytes()).decode()


cards, group = [], None
for i, (src, title, note, grp) in enumerate(ITEMS):
    if not src.exists():
        print(f"  missing, skipped: {src}")
        continue
    if grp != group:
        group = grp
        cards.append(f'<h2 class="grp">{grp}</h2>')
    cards.append(f"""
    <figure class="card">
      <img src="{thumb(src, i)}" alt="">
      <figcaption><b>{title}</b><span>{note}</span></figcaption>
    </figure>""")

HTML = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>CultureHub LA 2026 — All materials</title>
<style>
@page {{ size: letter; margin: 0.5in; }}
* {{ box-sizing: border-box; }}
body {{ font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; margin:0; color:#312B38; }}
.head {{ border-bottom:1.2pt solid #B44887; padding-bottom:.3em; margin-bottom:1em; }}
.head b {{ font-size:15pt; }}
.head span {{ float:right; font-size:8pt; letter-spacing:.1em; text-transform:uppercase;
  color:#66636A; padding-top:.6em; }}
.grp {{ grid-column:1 / -1; font-size:11pt; color:#B44887; margin:1.1em 0 .3em; padding-bottom:.15em;
  border-bottom:.5pt solid #ded8e2; break-after:avoid; }}
.grid {{ display:grid; grid-template-columns:1fr 1fr; gap:.7em; }}
.card {{ margin:0; border:.6pt solid #d8d2dc; border-radius:4px; overflow:hidden;
  break-inside:avoid; background:#fff; }}
.card img {{ display:block; width:100%; height:auto; max-height:3.3in; object-fit:contain;
  background:#faf9fb; }}
.card figcaption {{ padding:.35em .55em .45em; border-top:.6pt solid #ded8e2; font-size:8pt; }}
.card b {{ display:block; font-size:8.6pt; }}
.card span {{ color:#66636A; font-size:7.4pt; }}
.note {{ font-size:8.4pt; color:#66636A; margin:.6em 0 0; }}
</style></head><body>
<div class="head"><b>Whistlegraph presents — all materials</b><span>CultureHub LA · Sep 16–25 2026</span></div>
<p class="note">Every asset that exists for the residency, in one place. Apple press
photographs are reference only and must not be redistributed.</p>
<div class="grid">
{''.join(cards)}
</div>
</body></html>"""

out = HERE / "materials.html"
out.write_text(HTML)
print(f"wrote {out} ({len(HTML)/1024/1024:.2f} MB, {len([c for c in cards if 'figure' in c])} items)")
