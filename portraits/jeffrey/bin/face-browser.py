#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Build a static HTML browser for the curated jeffrey-described.jsonl.

Generates:
  - curated/thumbnails/<filename>.jpg   — 384px-max thumbnails (one per record)
  - curated/index.html                  — single-file browser, all data embedded

Open by serving the curated/ dir over HTTP:
  cd portraits/jeffrey/curated
  python3 -m http.server 8000
  open http://localhost:8000/

The browser shows: filter sidebar (year, domain, sim, confirmed), masonry-ish
grid of thumbnails with date/domain/sim overlays, click any card for a modal
with full subject/environment/photography fields and the original-resolution
image.

Usage:
  python face-browser.py \\
      --described portraits/jeffrey/curated/jeffrey-described.jsonl \\
      --match portraits/jeffrey/curated/jeffrey-match.jsonl \\
      --curated-dir portraits/jeffrey/curated
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

from PIL import Image

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
DEFAULT_CURATED = REPO_ROOT / "portraits" / "jeffrey" / "curated"

THUMB_MAX = 384


def make_thumbnail(src: Path, dst: Path) -> bool:
    if dst.exists() and dst.stat().st_size > 0:
        return False
    try:
        with Image.open(src) as img:
            img.thumbnail((THUMB_MAX, THUMB_MAX), Image.Resampling.LANCZOS)
            if img.mode != "RGB":
                img = img.convert("RGB")
            dst.parent.mkdir(parents=True, exist_ok=True)
            img.save(dst, "JPEG", quality=78, optimize=True)
        return True
    except Exception as e:
        print(f"  warn: thumbnail failed for {src.name}: {e}", file=sys.stderr)
        return False


HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Jeffrey Platter — Browser</title>
<style>
:root {
  --bg: #1a1a1f;
  --fg: #e8e8e8;
  --dim: #888;
  --card: #2a2a30;
  --accent: #4ecdc4;
  --pink: #cd5c9b;
  --gold: #d4a017;
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--bg); color: var(--fg); font: 14px/1.5 -apple-system, BlinkMacSystemFont, "Helvetica Neue", sans-serif; }
header { padding: 14px 18px; border-bottom: 1px solid #333; display: flex; gap: 14px; flex-wrap: wrap; align-items: baseline; }
h1 { margin: 0; font-size: 18px; font-weight: 600; }
.stats { color: var(--dim); font-size: 12px; }
.layout { display: grid; grid-template-columns: 220px 1fr; min-height: calc(100vh - 50px); }
aside { padding: 14px; border-right: 1px solid #333; overflow-y: auto; max-height: calc(100vh - 50px); position: sticky; top: 0; }
aside h3 { margin: 12px 0 6px; font-size: 11px; color: var(--dim); text-transform: uppercase; letter-spacing: 0.05em; }
.filter-list { list-style: none; padding: 0; margin: 0; }
.filter-list li { padding: 3px 0; cursor: pointer; user-select: none; }
.filter-list li:hover { color: var(--accent); }
.filter-list li.active { color: var(--accent); font-weight: 600; }
.filter-list li .count { color: var(--dim); font-size: 11px; margin-left: 4px; }
input[type=range] { width: 100%; }
input[type=text] { width: 100%; padding: 6px; background: var(--card); border: 1px solid #444; color: var(--fg); border-radius: 3px; }
.toggle { display: flex; align-items: center; gap: 6px; padding: 4px 0; cursor: pointer; }
main { padding: 14px; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 8px; }
.card { background: var(--card); border-radius: 4px; overflow: hidden; cursor: pointer; position: relative; transition: transform 0.1s; }
.card:hover { transform: scale(1.02); outline: 2px solid var(--accent); }
.card .thumb { width: 100%; aspect-ratio: 1; object-fit: cover; display: block; background: #111; }
.card .meta { padding: 6px 8px; font-size: 11px; }
.card .date { color: var(--dim); }
.card .domain { color: var(--accent); }
.card .sim { position: absolute; top: 4px; right: 4px; background: rgba(0,0,0,0.75); padding: 2px 5px; border-radius: 3px; font-size: 10px; font-family: ui-monospace, monospace; }
.card .badge-rejected { position: absolute; top: 4px; left: 4px; background: rgba(205,92,155,0.9); padding: 2px 5px; border-radius: 3px; font-size: 10px; font-weight: bold; }
.card .badge-video { position: absolute; bottom: 36px; left: 4px; background: rgba(0,0,0,0.75); padding: 2px 6px; border-radius: 3px; font-size: 12px; font-weight: bold; color: var(--gold); }
#modal { display: none; position: fixed; inset: 0; background: rgba(0,0,0,0.92); z-index: 100; padding: 20px; overflow-y: auto; }
#modal.open { display: block; }
.modal-grid { max-width: 1100px; margin: 0 auto; display: grid; grid-template-columns: 1fr 360px; gap: 20px; }
.modal-img { width: 100%; max-height: 80vh; object-fit: contain; background: #111; border-radius: 4px; }
.modal-info { color: var(--fg); }
.modal-info h2 { margin: 0 0 6px; font-size: 14px; color: var(--accent); }
.modal-info .field { margin-bottom: 10px; font-size: 12px; }
.modal-info .label { color: var(--dim); font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em; }
.modal-info .value { color: var(--fg); }
.modal-info .tag { display: inline-block; background: var(--card); padding: 2px 6px; border-radius: 2px; margin: 2px 4px 2px 0; font-size: 11px; }
#close { position: fixed; top: 12px; right: 16px; cursor: pointer; font-size: 24px; color: var(--fg); z-index: 101; user-select: none; }
.empty { color: var(--dim); padding: 40px; text-align: center; }
</style>
</head>
<body>
<header>
  <h1>Jeffrey Platter</h1>
  <span class="stats" id="stats">…</span>
</header>
<div class="layout">
  <aside>
    <input type="text" id="search" placeholder="Search description / tags…">

    <h3>Status</h3>
    <label class="toggle"><input type="checkbox" id="confirmedOnly" checked> Confirmed only (vision)</label>
    <label class="toggle"><input type="checkbox" id="rejectedOnly"> Rejected only (false positives)</label>

    <h3>Min similarity (face-match)</h3>
    <input type="range" id="simSlider" min="0.5" max="0.95" step="0.05" value="0.5">
    <div style="font-family: ui-monospace, monospace; color: var(--dim); font-size: 11px;" id="simValue">0.50</div>

    <h3>Year</h3>
    <ul class="filter-list" id="yearList"></ul>

    <h3>Domain</h3>
    <ul class="filter-list" id="domainList"></ul>
  </aside>
  <main>
    <div class="grid" id="grid"></div>
  </main>
</div>

<div id="modal">
  <span id="close">×</span>
  <div class="modal-grid" id="modalGrid"></div>
</div>

<script>
const RECORDS = __RECORDS__;

function bucketSim(s) {
  if (s >= 0.8) return "0.8+";
  if (s >= 0.7) return "0.7–0.8";
  if (s >= 0.6) return "0.6–0.7";
  return "0.5–0.6";
}

const state = {
  year: null,
  domain: null,
  confirmedOnly: true,
  rejectedOnly: false,
  minSim: 0.5,
  search: "",
};

function filtered() {
  return RECORDS.filter(r => {
    if (state.confirmedOnly && !r.is_jeffrey_confirmed) return false;
    if (state.rejectedOnly && r.is_jeffrey_confirmed) return false;
    if ((r.match_similarity || 0) < state.minSim) return false;
    if (state.year && r.date && !r.date.startsWith(state.year)) return false;
    if (state.domain && r.domain !== state.domain) return false;
    if (state.search) {
      const q = state.search.toLowerCase();
      const blob = JSON.stringify({
        s: r.subject, e: r.environment, p: r.photography,
        t: r.tags, c: r.caption_hint, d: r.domain, x: r.rel_path
      }).toLowerCase();
      if (!blob.includes(q)) return false;
    }
    return true;
  });
}

function renderGrid() {
  const items = filtered();
  document.getElementById("stats").textContent =
    `${items.length} of ${RECORDS.length}  ·  ${RECORDS.filter(r => r.is_jeffrey_confirmed).length} confirmed total`;
  const grid = document.getElementById("grid");
  if (items.length === 0) {
    grid.innerHTML = `<div class="empty">no matches</div>`;
    return;
  }
  grid.innerHTML = items.slice(0, 600).map((r, i) => `
    <div class="card" data-idx="${RECORDS.indexOf(r)}">
      <img class="thumb" loading="lazy" src="thumbnails/${r.rel_path}" alt="">
      <div class="sim">${(r.match_similarity || 0).toFixed(2)}</div>
      ${r.has_video ? '<div class="badge-video">▶</div>' : ''}
      ${!r.is_jeffrey_confirmed ? '<div class="badge-rejected">REJECTED</div>' : ''}
      <div class="meta">
        <div class="date">${r.date || ""}</div>
        <div class="domain">${r.domain || ""}</div>
      </div>
    </div>
  `).join("");
  if (items.length > 600) {
    grid.innerHTML += `<div class="empty">…and ${items.length - 600} more (refine filters to see)</div>`;
  }
}

function renderFilters() {
  const items = RECORDS.filter(r =>
    (!state.confirmedOnly || r.is_jeffrey_confirmed) &&
    (!state.rejectedOnly || !r.is_jeffrey_confirmed) &&
    (r.match_similarity || 0) >= state.minSim
  );

  const years = {};
  const domains = {};
  items.forEach(r => {
    if (r.date) { const y = r.date.slice(0, 4); years[y] = (years[y] || 0) + 1; }
    if (r.domain) domains[r.domain] = (domains[r.domain] || 0) + 1;
  });

  const yl = document.getElementById("yearList");
  yl.innerHTML = `<li class="${state.year === null ? 'active' : ''}" data-y="">all <span class="count">${items.length}</span></li>` +
    Object.entries(years).sort().map(([y, n]) =>
      `<li class="${state.year === y ? 'active' : ''}" data-y="${y}">${y} <span class="count">${n}</span></li>`
    ).join("");

  const dl = document.getElementById("domainList");
  dl.innerHTML = `<li class="${state.domain === null ? 'active' : ''}" data-d="">all <span class="count">${items.length}</span></li>` +
    Object.entries(domains).sort((a, b) => b[1] - a[1]).map(([d, n]) =>
      `<li class="${state.domain === d ? 'active' : ''}" data-d="${d}">${d} <span class="count">${n}</span></li>`
    ).join("");
}

function render() { renderFilters(); renderGrid(); }

document.getElementById("yearList").addEventListener("click", e => {
  const li = e.target.closest("li");
  if (!li) return;
  state.year = li.dataset.y || null;
  render();
});
document.getElementById("domainList").addEventListener("click", e => {
  const li = e.target.closest("li");
  if (!li) return;
  state.domain = li.dataset.d || null;
  render();
});
document.getElementById("confirmedOnly").addEventListener("change", e => {
  state.confirmedOnly = e.target.checked;
  if (e.target.checked) { state.rejectedOnly = false; document.getElementById("rejectedOnly").checked = false; }
  render();
});
document.getElementById("rejectedOnly").addEventListener("change", e => {
  state.rejectedOnly = e.target.checked;
  if (e.target.checked) { state.confirmedOnly = false; document.getElementById("confirmedOnly").checked = false; }
  render();
});
document.getElementById("simSlider").addEventListener("input", e => {
  state.minSim = parseFloat(e.target.value);
  document.getElementById("simValue").textContent = state.minSim.toFixed(2);
  render();
});
document.getElementById("search").addEventListener("input", e => {
  state.search = e.target.value;
  renderGrid();
});

document.getElementById("grid").addEventListener("click", e => {
  const card = e.target.closest(".card");
  if (!card) return;
  const r = RECORDS[parseInt(card.dataset.idx, 10)];
  const s = r.subject || {};
  const en = r.environment || {};
  const p = r.photography || {};
  const mediaTag = r.has_video
    ? `<video class="modal-img" src="originals/${r.video_path}" controls autoplay muted loop playsinline></video>`
    : `<img class="modal-img" src="originals/${r.rel_path}" alt="">`;
  const dim = (r.width && r.height) ? `${r.width}×${r.height}` : "?";
  document.getElementById("modalGrid").innerHTML = `
    ${mediaTag}
    <div class="modal-info">
      <h2>${r.rel_path}</h2>
      <div class="field"><span class="label">date · resolution · type</span><div class="value">${r.date || "?"} · ${dim} · ${r.has_video ? "video" : "image"}</div></div>
      <div class="field"><span class="label">domain</span><div class="value">${r.domain || "?"}</div></div>
      <div class="field"><span class="label">match similarity</span><div class="value">${(r.match_similarity || 0).toFixed(3)} → ${r.match_ref || ""}</div></div>
      <div class="field"><span class="label">vision confirmed</span><div class="value">${r.is_jeffrey_confirmed ? "yes" : "NO"} (${(r.confidence || 0).toFixed(2)})</div></div>
      ${s.description ? `<div class="field"><span class="label">subject — appearance</span><div class="value">${s.description}</div></div>` : ""}
      ${s.expression ? `<div class="field"><span class="label">expression</span><div class="value">${s.expression}</div></div>` : ""}
      ${s.pose ? `<div class="field"><span class="label">pose</span><div class="value">${s.pose}</div></div>` : ""}
      ${en.location ? `<div class="field"><span class="label">location</span><div class="value">${en.location} · ${en.time_of_day || ""}</div></div>` : ""}
      ${en.lighting ? `<div class="field"><span class="label">lighting</span><div class="value">${en.lighting}</div></div>` : ""}
      ${en.background_details ? `<div class="field"><span class="label">background</span><div class="value">${en.background_details}</div></div>` : ""}
      ${p.camera_angle ? `<div class="field"><span class="label">photography</span><div class="value">${p.camera_angle} · ${p.style} · ${p.framing}</div></div>` : ""}
      ${r.tags && r.tags.length ? `<div class="field"><span class="label">tags</span><div class="value">${r.tags.map(t => `<span class="tag">${t}</span>`).join("")}</div></div>` : ""}
      ${r.caption_hint ? `<div class="field"><span class="label">caption hint</span><div class="value"><em>${r.caption_hint}</em></div></div>` : ""}
    </div>
  `;
  document.getElementById("modal").classList.add("open");
});
document.getElementById("close").addEventListener("click", () => {
  document.getElementById("modal").classList.remove("open");
});
document.getElementById("modal").addEventListener("click", e => {
  if (e.target.id === "modal") document.getElementById("modal").classList.remove("open");
});

render();
</script>
</body>
</html>
"""


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--described", required=True)
    p.add_argument("--match", required=True)
    p.add_argument("--curated-dir", default=str(DEFAULT_CURATED))
    args = p.parse_args()

    described_path = Path(args.described)
    match_path = Path(args.match)
    curated = Path(args.curated_dir)
    thumbs_dir = curated / "thumbnails"

    # Load described records
    described: list[dict] = []
    for line in described_path.read_text().splitlines():
        try:
            described.append(json.loads(line))
        except json.JSONDecodeError:
            pass

    # Index match records by path so we can pick up sim scores even for rejects
    match_by_path: dict[str, dict] = {}
    for line in match_path.read_text().splitlines():
        try:
            r = json.loads(line)
            match_by_path[r["path"]] = r
        except (json.JSONDecodeError, KeyError):
            pass

    # Build thumbnails
    print(f"generating thumbnails to {thumbs_dir}…", file=sys.stderr)
    n_new = 0
    for r in described:
        src = Path(r["path"])
        if not src.exists():
            continue
        dst = thumbs_dir / r["rel_path"]
        if make_thumbnail(src, dst):
            n_new += 1
    print(f"thumbnails: {n_new} new, {len(described) - n_new} existed", file=sys.stderr)

    # Slim records for browser (drop heavy fields). Detect sibling video files
    # and probe original-image dimensions so the modal can show real quality.
    slim = []
    for r in described:
        m = match_by_path.get(r["path"], {})
        src = Path(r["path"])
        mp4 = src.with_suffix(".mp4")
        width = height = None
        if src.exists():
            try:
                with Image.open(src) as img:
                    width, height = img.size
            except Exception:
                pass
        slim.append({
            "rel_path": r.get("rel_path"),
            "date": r.get("date"),
            "shortcode": r.get("shortcode"),
            "match_similarity": r.get("match_similarity") or m.get("best_similarity"),
            "match_ref": r.get("match_ref") or m.get("best_match_ref"),
            "is_jeffrey_confirmed": r.get("is_jeffrey_confirmed"),
            "confidence": r.get("confidence"),
            "subject": r.get("subject"),
            "environment": r.get("environment"),
            "photography": r.get("photography"),
            "domain": r.get("domain"),
            "n_other_people": r.get("n_other_people"),
            "tags": r.get("tags"),
            "caption_hint": r.get("caption_hint"),
            "has_video": mp4.exists(),
            "video_path": mp4.name if mp4.exists() else None,
            "width": width,
            "height": height,
        })

    html = HTML_TEMPLATE.replace("__RECORDS__", json.dumps(slim, ensure_ascii=False))
    out_html = curated / "index.html"
    out_html.write_text(html)
    print(f"wrote {out_html}", file=sys.stderr)
    print("\nopen with:", file=sys.stderr)
    print(f"  cd {curated}", file=sys.stderr)
    print(f"  python3 -m http.server 8000", file=sys.stderr)
    print(f"  open http://localhost:8000/", file=sys.stderr)

    # Summary
    confirmed = [r for r in slim if r["is_jeffrey_confirmed"]]
    print(f"\nincluded: {len(slim)} records ({len(confirmed)} confirmed)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
