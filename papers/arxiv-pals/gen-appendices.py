#!/usr/bin/env python3
# gen-appendices.py — emit LaTeX tile-grid fragments for the Pals appendices.
# Every tile carries its visual asset + a caption (label + creation date).
import os

HERE = os.path.dirname(os.path.abspath(__file__))

def tile(img, label, date, w):
    # one minipage tile: image (fixed width) + tiny caption
    return (
        "\\begin{minipage}[t]{%s\\textwidth}\\centering\n"
        "  \\includegraphics[width=\\linewidth,height=\\linewidth,keepaspectratio]{%s}\\\\[2pt]\n"
        "  {\\scriptsize\\textbf{%s}}\\\\[-1pt]\n"
        "  {\\scriptsize\\color{acgray}%s}\n"
        "\\end{minipage}"
    ) % (w, img, label, date)

def grid(items, per_row):
    # items: list of (img, label, date). per_row tiles per row.
    w = "%.3f" % (0.97 / per_row)
    out = []
    for i, (img, label, date) in enumerate(items):
        out.append(tile(img, label, date, w))
        if (i + 1) % per_row == 0:
            out.append("\n\n\\vspace{0.8em}\n\n")
        else:
            out.append("\\hfill")
    return "".join(out)

# ---- Appendix A: live wgr poses ----
pose_dates = {
    1: "2023-08-01", 2: "2023-08-21", 3: "2023-08-21", 4: "2023-08-01",
    5: "2023-08-21", 6: "2023-08-01", 7: "2023-08-21", 8: "2023-08-21",
    9: "2023-08-01", 10: "2023-08-01", 11: "2023-07-31", 12: "2023-07-29",
}
poses = [(f"figures/poses/small/pose-{n}.png", f"pose {n}", pose_dates[n]) for n in range(1, 13)]

# ---- Appendix B: ice-cream photos (EXIF 2023-09-08) ----
ic = [
    ("01","P9080037","18:52"),("02","P9080033","18:51"),("03","P9080032","18:51"),
    ("04","P9080041","18:53"),("05","P9080031","18:51"),("06","P9080046","18:54"),
    ("07","P9080054","18:58"),("08","P9080056","18:58"),("09","P9080047","18:55"),
    ("10","P9080049","18:55"),("11","P9080058","18:58"),("12","P9080059","18:59"),
    ("13","P9080060","19:28"),("14","P9080062","19:28"),("15","P9080063","19:28"),
    ("16","P9080064","19:29"),("17","P9080067","19:32"),("18","P9080071","19:33"),
]
icecream = [(f"figures/icecream/small/icecream-{n}.jpg", name, f"2023-09-08 {t}") for n, name, t in ic]

# ---- Appendix C: monorepo digital assets (git first-add date) ----
assets = [
    ("figures/assets/c01-purple-pals.png", "purple-pals.svg", "2024-01-31"),
    ("figures/assets/c02-vscode.png",      "vscode-extension", "2024-01-31"),
    ("figures/assets/c03-tvos.png",        "apple/tvos-tapes", "2024-01-31"),
    ("figures/assets/c04-plymouth.png",    "AC-OS boot splash", "2026-02-21"),
    ("figures/assets/c05-tray.png",        "Electron tray", "2026-05-20"),
    ("figures/assets/c14-stamp.png",       "invoice stamp", "2026-03-06"),
    ("figures/assets/c12-gray.png",        "grayscale (quotes)", "2026-05-21"),
    ("figures/assets/c13-hellsine.png",    "pop cover glyph", "2026-05-24"),
    ("figures/assets/c06-pink.png",        "keymaps pink", "2026-06-13"),
    ("figures/assets/c07-blue.png",        "keymaps blue", "2026-06-13"),
    ("figures/assets/c08-green.png",       "keymaps green", "2026-06-13"),
    ("figures/assets/c09-orange.png",      "keymaps orange", "2026-06-13"),
    ("figures/assets/c10-purple.png",      "keymaps purple", "2026-06-13"),
    ("figures/assets/c11-yellow.png",      "keymaps yellow", "2026-06-13"),
]

# ---- Appendix F: recovered iMessage sketchbook pages ----
sk = [
    ("sk-2023-06-21-a", "candidate page", "2023-06-21"),
    ("sk-2023-06-21-b", "candidate page", "2023-06-21"),
    ("sk-2023-06-21-c", "the ``circled one''", "2023-06-21"),
    ("sk-2023-06-21-d", "candidate page", "2023-06-21"),
    ("sk-2023-06-21-e", "candidate page", "2023-06-21"),
    ("sk-2023-06-24-starfish", "``looks like starfish''", "2023-06-24"),
]
sketches = [(f"figures/imessage/sketches/small/{n}.jpg", lbl, d) for n, lbl, d in sk]

# ---- Appendix D: live production captures (today) ----
live = [
    ("figures/live/d01-pals-endpoint.png", "pals.aesthetic.computer", "2026-06-29"),
    ("figures/live/d02-pals-endpoint-rotated.png", "same URL, reloaded", "2026-06-29"),
    ("figures/live/d03-raw-png.png", "/api/logo.png (raw)", "2026-06-29"),
]

frag = {
    "appendix-a.tex": grid(poses, 4),
    "appendix-b.tex": grid(icecream, 6),
    "appendix-c.tex": grid(assets, 5),
    "appendix-f.tex": grid(sketches, 3),
}
# appendix-d.tex (live UI placements) is hand-authored, not generated.
for fn, body in frag.items():
    with open(os.path.join(HERE, fn), "w") as f:
        f.write(body)
    print("wrote", fn)
