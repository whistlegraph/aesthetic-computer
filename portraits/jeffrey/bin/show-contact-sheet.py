#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Render a contact sheet for a curated show. Resolves the show via
`bin/show-resolve.mjs <slug>` (single source of truth for selectors), then tiles
post thumbnails onto a single image saved to portraits/jeffrey/shows/.

For posts with > MAX_TILES images the set is evenly sampled so the sheet stays
readable; the filename records the sample. To review every post instead of a
sample — needed for grid curation — pass --per-page=N: the show is split into
ordered pages of N posts each, one image per page (no sampling).

Usage:
  bin/show-contact-sheet.py jeffrey-only
  bin/show-contact-sheet.py no-jeffrey --max=400
  bin/show-contact-sheet.py recent-30 --account=whistlegraph
  bin/show-contact-sheet.py grid --per-page=600    # full curation review, paginated
"""

from __future__ import annotations

import argparse
import json
import math
import os
import shutil
import subprocess
import sys
import textwrap
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

REPO = Path(__file__).resolve().parents[3]
RESOLVER = REPO / "portraits/jeffrey/bin/show-resolve.mjs"
# Output lives next to the show definitions (shows/*.jpg is gitignored). NOT
# ~/Desktop — the macOS Desktop auto-cleaner wipes long renders mid-job.
OUT_DIR = REPO / "portraits/jeffrey/shows"

THUMB = 200
CAPTION_H = 60          # strip below each tile for date/likes/caption
PAD = 8
COLS = 22
HEADER_H = 120
BG = (24, 24, 24)
FG = (240, 240, 240)
DIM = (140, 140, 140)
SOFT = (200, 200, 200)


def resolve_show(slug: str, account: str) -> dict:
    out = subprocess.check_output(
        ["node", str(RESOLVER), slug, f"--account={account}"],
        cwd=REPO,
    )
    return json.loads(out)


def find_thumb_source(post: dict, archive_dir: Path) -> Path | None:
    base = f"{post['date']}_{post['shortcode']}"
    # Plain photo or carousel first frame, or video poster (instaloader writes
    # a sibling .jpg alongside every .mp4).
    for cand in (archive_dir / f"{base}.jpg", archive_dir / f"{base}_1.jpg"):
        if cand.exists():
            return cand
    return None


def load_tile(src: Path, size: int) -> Image.Image:
    im = Image.open(src)
    # JPEG fast-path: decode at the smallest size >= target before any resize.
    if im.format == "JPEG":
        im.draft("RGB", (size * 2, size * 2))
    im = im.convert("RGB")
    # Center-crop to square, then resize.
    w, h = im.size
    side = min(w, h)
    left = (w - side) // 2
    top = (h - side) // 2
    im = im.crop((left, top, left + side, top + side))
    im = im.resize((size, size), Image.LANCZOS)
    return im


def pick_font(size: int) -> ImageFont.ImageFont:
    for cand in (
        "/System/Library/Fonts/SFNSMono.ttf",
        "/System/Library/Fonts/Supplemental/Menlo.ttc",
        "/System/Library/Fonts/Helvetica.ttc",
    ):
        if os.path.exists(cand):
            try:
                return ImageFont.truetype(cand, size)
            except OSError:
                pass
    return ImageFont.load_default()


def fit_to_width(draw, text, font, max_w):
    """Truncate text to fit within max_w pixels, appending … if cut."""
    if draw.textlength(text, font=font) <= max_w:
        return text
    ell = "…"
    lo, hi = 0, len(text)
    while lo < hi:
        mid = (lo + hi) // 2
        candidate = text[: mid].rstrip() + ell
        if draw.textlength(candidate, font=font) <= max_w:
            lo = mid + 1
        else:
            hi = mid
    return (text[: max(0, lo - 1)].rstrip() + ell) if lo > 0 else ""


def wrap_to_width(draw, text, font, max_w, max_lines):
    """Greedy word-wrap; returns up to max_lines lines, last truncated with …."""
    words = text.split()
    lines: list[str] = []
    cur = ""
    for w in words:
        trial = (cur + " " + w).strip()
        if draw.textlength(trial, font=font) <= max_w:
            cur = trial
        else:
            if cur:
                lines.append(cur)
            cur = w
            if len(lines) >= max_lines:
                break
    if cur and len(lines) < max_lines:
        lines.append(cur)
    if len(lines) == max_lines:
        # We may have stopped early; check remaining text.
        consumed = " ".join(lines)
        remaining = text[len(consumed):].strip()
        if remaining:
            lines[-1] = fit_to_width(draw, lines[-1] + "…", font, max_w)
    return lines


def draw_caption_strip(draw, post, x, y, stat_font, cap_font, tile_w, rank=None):
    """Draw a 60px-tall info strip below a tile.

    Top line: #rank + date + likes♥ + comments✎ (post) or "story · date".
    Lines 2-3: caption text, wrapped + truncated to fit tile width.
    """
    kind = post.get("kind") or "other"
    likes = post.get("likes")
    comments = post.get("comments")
    date = post.get("date") or ""
    caption = (post.get("caption") or "").strip().replace("\n", " ")

    if kind == "story":
        stat_line = f"story · {date}"
    else:
        bits = []
        if rank is not None:
            bits.append(f"#{rank}")
        bits.append(date)
        if likes is not None:
            bits.append(f"{likes} like" + ("" if likes == 1 else "s"))
        if comments is not None and comments > 0:
            bits.append(f"{comments} comment" + ("" if comments == 1 else "s"))
        stat_line = " · ".join(bits)

    inner_w = tile_w - 4  # leave 2px gutter on each side
    draw.text(
        (x + 2, y),
        fit_to_width(draw, stat_line, stat_font, inner_w),
        font=stat_font,
        fill=DIM,
    )

    if caption:
        lines = wrap_to_width(draw, caption, cap_font, inner_w, max_lines=2)
        for j, line in enumerate(lines):
            draw.text(
                (x + 2, y + 16 + j * 16),
                line,
                font=cap_font,
                fill=SOFT,
            )


def sample_evenly(items: list, n: int) -> list:
    if len(items) <= n:
        return items
    step = len(items) / n
    return [items[int(i * step)] for i in range(n)]


def render_sheet(
    slug: str,
    show: dict,
    posts: list,
    account: str,
    out: Path,
    subtitle: str,
    rank_offset: int | None,
) -> Path:
    """Tile `posts` onto one contact-sheet image at `out`.

    If `rank_offset` is not None each tile is stamped #(rank_offset + i + 1) —
    the post's global position in the show order (used for paginated review).
    """
    archive_dir = REPO / "portraits/jeffrey/ig-archive" / account
    cell_h = THUMB + CAPTION_H
    rows = math.ceil(len(posts) / COLS)
    width = COLS * THUMB + (COLS + 1) * PAD
    height = HEADER_H + rows * cell_h + (rows + 1) * PAD

    canvas = Image.new("RGB", (width, height), BG)
    draw = ImageDraw.Draw(canvas)

    title_font = pick_font(38)
    sub_font = pick_font(20)
    stat_font = pick_font(12)
    cap_font = pick_font(13)
    draw.text((PAD * 2, 18), show.get("name") or slug, font=title_font, fill=FG)
    draw.text((PAD * 2, 64), subtitle, font=sub_font, fill=DIM)
    desc = show.get("description", "") or ""
    if desc:
        draw.text(
            (PAD * 2, 90),
            fit_to_width(draw, desc, sub_font, width - PAD * 4),
            font=sub_font,
            fill=DIM,
        )

    placed = 0
    missing: list[str] = []
    for i, post in enumerate(posts):
        src = find_thumb_source(post, archive_dir)
        if not src:
            missing.append(post["shortcode"])
            continue
        col = i % COLS
        row = i // COLS
        x = PAD + col * (THUMB + PAD)
        y = HEADER_H + PAD + row * cell_h
        try:
            tile = load_tile(src, THUMB)
            canvas.paste(tile, (x, y))
            tile.close()
            rank = None if rank_offset is None else rank_offset + i + 1
            draw_caption_strip(
                draw, post, x, y + THUMB + 2, stat_font, cap_font, THUMB, rank
            )
            placed += 1
        except Exception as e:  # noqa: BLE001
            missing.append(f"{post['shortcode']} ({type(e).__name__})")
        if placed % 50 == 0 and placed:
            print(f"  [{slug}] {placed}/{len(posts)}", file=sys.stderr)

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    canvas.save(out, "JPEG", quality=82, optimize=True)
    canvas.close()

    if missing:
        print(
            f"[{slug}] {placed}/{len(posts)} placed; {len(missing)} missing source",
            file=sys.stderr,
        )
    print(out)
    return out


def build_sheet(
    slug: str, account: str, max_tiles: int, per_page: int
) -> list[Path]:
    data = resolve_show(slug, account)
    show = data["show"]
    posts = data["posts"]
    total = len(posts)
    if total == 0:
        print(f"[{slug}] no posts resolved; skipping", file=sys.stderr)
        return []

    # Paginated review: every post, split into ordered pages of `per_page`.
    if per_page and per_page > 0 and total > per_page:
        n_pages = math.ceil(total / per_page)
        outs: list[Path] = []
        for pg in range(n_pages):
            lo, hi = pg * per_page, min((pg + 1) * per_page, total)
            chunk = posts[lo:hi]
            sub = (
                f"page {pg + 1}/{n_pages}  ·  ranks {lo + 1}–{hi} of {total}"
                f"  ·  {chunk[0]['date']} → {chunk[-1]['date']}  ·  @{account}"
            )
            out = OUT_DIR / f"{slug}-p{pg + 1}of{n_pages}.jpg"
            outs.append(render_sheet(slug, show, chunk, account, out, sub, lo))
        return outs

    # Single sheet: evenly sampled if it would otherwise exceed max_tiles.
    sampled = sample_evenly(posts, max_tiles)
    sampled_note = (
        f"sampled {len(sampled)} of {total}" if len(sampled) < total else f"{total}"
    )
    sub = (
        f"{sampled_note}  ·  {posts[0]['date']} → {posts[-1]['date']}"
        f"  ·  @{account}"
    )
    if len(sampled) < total:
        out = OUT_DIR / f"{slug}-sample-{len(sampled)}-of-{total}.jpg"
    else:
        out = OUT_DIR / f"{slug}-{total}.jpg"
    return [render_sheet(slug, show, sampled, account, out, sub, None)]


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("slug")
    p.add_argument("--account", default="whistlegraph")
    p.add_argument("--max", type=int, default=600, dest="max_tiles")
    p.add_argument(
        "--per-page",
        type=int,
        default=0,
        dest="per_page",
        help="render every post (no sampling) in ordered pages of N tiles",
    )
    args = p.parse_args()
    if shutil.which("node") is None:
        print("node not on PATH", file=sys.stderr)
        return 1
    build_sheet(args.slug, args.account, args.max_tiles, args.per_page)
    return 0


if __name__ == "__main__":
    sys.exit(main())
