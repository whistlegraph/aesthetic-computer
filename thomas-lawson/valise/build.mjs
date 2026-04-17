#!/usr/bin/env node
// Builds dist/valise/ from data/artworks.json + data/collections.json.
// Surfaces every field the Valise API returns.

import { readFileSync, writeFileSync, mkdirSync, copyFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const dataDir = join(__dirname, "data");

const artworks = JSON.parse(readFileSync(join(dataDir, "artworks.json"), "utf-8"));
const collections = existsSync(join(dataDir, "collections.json"))
  ? JSON.parse(readFileSync(join(dataDir, "collections.json"), "utf-8"))
  : [];
const fetchedAt = existsSync(join(dataDir, "fetched-at.txt"))
  ? readFileSync(join(dataDir, "fetched-at.txt"), "utf-8").trim()
  : null;

// Cross-reference: artworkId -> [collection titles]
const collectionsByArtwork = {};
for (const c of collections) {
  for (const aid of c.artworkIds || []) {
    (collectionsByArtwork[aid] ||= []).push({ id: c.id, title: c.title || "(untitled collection)" });
  }
}

// Emit full fidelity — every artwork field, sorted newest→oldest by year then title.
const full = artworks
  .map((a) => ({
    id: a.id,
    uid: a.uid || "",
    title: a.title || "",
    year: a.year || "",
    medium: a.medium || "",
    dimensions: a.dimensions || "",
    tags: (a.tags || []).map((t) => ({ id: t.id, slug: t.slug, title: t.title })),
    images: (a.images || []).map((im) => ({
      id: im.id,
      url: im.url,
      filename: im.filename || "",
      w: im.width,
      h: im.height,
      type: im.type || "image",
    })),
    editions: a.editions || [],
    collections: collectionsByArtwork[a.id] || [],
    createdAt: a.createdAt,
    updatedAt: a.updatedAt,
  }))
  .sort((a, b) => {
    const ya = parseInt(a.year, 10);
    const yb = parseInt(b.year, 10);
    if (!isNaN(ya) && !isNaN(yb) && ya !== yb) return yb - ya;
    if (!isNaN(ya)) return -1;
    if (!isNaN(yb)) return 1;
    return a.title.localeCompare(b.title);
  });

const outDir = join(ROOT, "dist", "valise");
mkdirSync(outDir, { recursive: true });
writeFileSync(join(outDir, "data.json"), JSON.stringify(full));

// Stats & helpers
const withImages = full.filter((a) => a.images.length).length;
const years = [...new Set(full.map((a) => a.year).filter(Boolean))].sort();
const tagCounts = {};
for (const a of full) for (const t of a.tags) tagCounts[t.title] = (tagCounts[t.title] || 0) + 1;
const topTags = Object.entries(tagCounts)
  .sort((a, b) => b[1] - a[1])
  .map(([t, n]) => ({ title: t, count: n }));

const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Thomas Lawson — Valise archive</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,300;0,400;0,500;0,600;1,400&display=swap" rel="stylesheet">
<style>
  :root {
    --bg: #fff9ef;
    --fg: #222;
    --muted: #6f6f6f;
    --faint: #b0a99a;
    --border: #e8e2d6;
    --accent: #0170B9;
    --card-bg: #ffffff;
    --font: "Poppins", "Helvetica Neue", Helvetica, Arial, sans-serif;
    --mono: "SF Mono", "Menlo", "Monaco", monospace;
  }
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: var(--font);
    background: var(--bg);
    color: var(--fg);
    font-size: 15px;
    line-height: 1.6;
    -webkit-font-smoothing: antialiased;
  }
  a { color: var(--accent); }

  header {
    padding: 2rem 2rem 1.5rem;
    border-bottom: 1px solid var(--border);
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    align-items: center;
    gap: 1.5rem;
    flex-wrap: wrap;
  }
  header a.home { display: block; line-height: 0; }
  header .logo { height: 40px; width: auto; }
  header .page-title {
    font-size: 0.85rem;
    font-weight: 300;
    letter-spacing: 0.06em;
    text-transform: uppercase;
    color: var(--muted);
  }
  header .page-title strong { color: var(--fg); font-weight: 500; }
  header .sub {
    color: var(--muted);
    font-size: 0.78rem;
    margin-left: auto;
    font-weight: 300;
    text-align: right;
  }
  .badge {
    display: inline-flex;
    align-items: center;
    gap: 0.4rem;
    font-size: 0.7rem;
    padding: 0.2rem 0.55rem;
    background: #eaf4fb;
    color: var(--accent);
    border-radius: 2px;
    font-weight: 500;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }
  .badge::before { content: ""; width: 6px; height: 6px; background: var(--accent); border-radius: 50%; }

  .filters {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1rem 2rem;
    display: flex;
    gap: 0.75rem;
    flex-wrap: wrap;
    align-items: center;
    border-bottom: 1px solid var(--border);
  }
  .filters label {
    font-size: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--muted);
  }
  .filters select, .filters input {
    font-family: var(--font);
    font-size: 0.8rem;
    font-weight: 300;
    padding: 0.4rem 0.7rem;
    border: 1px solid var(--border);
    border-radius: 2px;
    background: var(--card-bg);
    color: var(--fg);
    outline: none;
  }
  .filters select:focus, .filters input:focus { border-color: var(--accent); }
  .filters input[type="search"] { width: 240px; }
  .filters .checkbox {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    font-size: 0.75rem;
    color: var(--muted);
    cursor: pointer;
  }
  .count {
    margin-left: auto;
    font-size: 0.8rem;
    color: var(--muted);
  }

  .grid {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1.5rem 2rem 4rem;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
    gap: 1.5rem;
  }
  .card {
    background: var(--card-bg);
    border: 1px solid var(--border);
    border-radius: 2px;
    overflow: hidden;
    transition: box-shadow 0.2s, transform 0.2s;
    cursor: pointer;
    display: flex;
    flex-direction: column;
  }
  .card:hover { box-shadow: 0 4px 20px rgba(0,0,0,0.07); transform: translateY(-2px); }
  .card .img-wrap {
    width: 100%;
    aspect-ratio: 1;
    overflow: hidden;
    background: #f5f0e6;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.75rem;
    position: relative;
  }
  .card .img-wrap img {
    max-width: 100%;
    max-height: 100%;
    object-fit: contain;
  }
  .card .img-wrap.empty::before {
    content: "No image";
    font-size: 0.7rem;
    color: var(--faint);
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }
  .card .img-count {
    position: absolute;
    bottom: 0.4rem;
    right: 0.4rem;
    font-size: 0.6rem;
    background: rgba(0,0,0,0.55);
    color: #fff;
    padding: 0.1rem 0.4rem;
    border-radius: 2px;
    font-weight: 400;
  }
  .card .info {
    padding: 0.8rem 1rem 1rem;
    border-top: 1px solid var(--border);
    flex: 1;
  }
  .card .info h3 {
    font-size: 0.82rem;
    font-weight: 500;
    line-height: 1.35;
    margin-bottom: 0.2rem;
    color: var(--fg);
  }
  .card .info h3 em { font-style: italic; }
  .card .info .year {
    font-size: 0.78rem;
    color: var(--muted);
    font-weight: 300;
  }
  .card .info .medium {
    font-size: 0.73rem;
    color: var(--muted);
    margin-top: 0.2rem;
    font-style: italic;
    font-weight: 300;
  }
  .card .info .dims {
    font-size: 0.7rem;
    color: var(--muted);
    margin-top: 0.1rem;
    font-weight: 300;
  }
  .card .info .tags {
    margin-top: 0.4rem;
    display: flex;
    gap: 0.3rem;
    flex-wrap: wrap;
  }
  .card .info .tag {
    font-size: 0.62rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.15rem 0.45rem;
    background: #f5f0e6;
    border-radius: 2px;
    color: var(--muted);
    font-weight: 400;
  }
  .card .info .serial {
    font-size: 0.78rem;
    font-family: var(--mono);
    color: var(--faint);
    margin-top: 0.4rem;
    letter-spacing: 0.05em;
  }

  .loading, .empty-state {
    max-width: 1400px;
    margin: 4rem auto;
    padding: 3rem 2rem;
    text-align: center;
    color: var(--muted);
    font-weight: 300;
  }

  footer {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem 2rem 3rem;
    border-top: 1px solid var(--border);
    font-size: 0.75rem;
    color: var(--muted);
    display: flex;
    justify-content: space-between;
    flex-wrap: wrap;
    gap: 1rem;
  }
  footer a { text-decoration: none; }
  footer a:hover { text-decoration: underline; }

  /* Detail modal */
  .modal {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,0.88);
    z-index: 100;
    align-items: center;
    justify-content: center;
    padding: 1.5rem;
  }
  .modal.open { display: flex; }
  .modal .panel {
    background: var(--bg);
    color: var(--fg);
    max-width: 1100px;
    width: 100%;
    max-height: 90vh;
    overflow: auto;
    border-radius: 3px;
    display: grid;
    grid-template-columns: 1.1fr 1fr;
    gap: 0;
  }
  .modal .close {
    position: absolute;
    top: 1rem;
    right: 1rem;
    color: #fff;
    font-size: 1.8rem;
    cursor: pointer;
    line-height: 1;
    opacity: 0.8;
  }
  .modal .close:hover { opacity: 1; }
  .modal .images {
    background: #1a1a1a;
    display: flex;
    flex-direction: column;
    min-height: 360px;
  }
  .modal .images .stage {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    position: relative;
    padding: 1rem;
    min-height: 360px;
  }
  .modal .images .stage img {
    max-width: 100%;
    max-height: 100%;
    object-fit: contain;
  }
  .modal .images .stage .empty {
    color: #888;
    font-size: 0.8rem;
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }
  .modal .images .nav {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
    color: #fff;
    font-size: 2rem;
    padding: 1rem;
    cursor: pointer;
    user-select: none;
    opacity: 0.5;
  }
  .modal .images .nav:hover { opacity: 1; }
  .modal .images .nav.prev { left: 0; }
  .modal .images .nav.next { right: 0; }
  .modal .images .thumbs {
    display: flex;
    gap: 0.3rem;
    padding: 0.5rem;
    overflow-x: auto;
    background: #0c0c0c;
    min-height: 56px;
  }
  .modal .images .thumbs .thumb {
    flex: 0 0 auto;
    width: 48px;
    height: 48px;
    background: #222;
    border: 2px solid transparent;
    cursor: pointer;
    overflow: hidden;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .modal .images .thumbs .thumb.active { border-color: var(--accent); }
  .modal .images .thumbs .thumb img { max-width: 100%; max-height: 100%; object-fit: contain; }
  .modal .images .img-meta {
    background: #000;
    color: #aaa;
    font-family: var(--mono);
    font-size: 0.65rem;
    padding: 0.4rem 0.8rem;
    display: flex;
    justify-content: space-between;
    gap: 1rem;
  }
  .modal .images .img-meta a { color: #6bb6f0; text-decoration: none; }
  .modal .images .img-meta a:hover { text-decoration: underline; }

  .modal .meta {
    padding: 1.5rem 1.75rem;
    overflow: auto;
  }
  .modal .meta h2 {
    font-size: 1.3rem;
    font-weight: 400;
    font-style: italic;
    line-height: 1.2;
    margin-bottom: 0.4rem;
  }
  .modal .meta .modal-year {
    font-size: 0.9rem;
    color: var(--muted);
    margin-bottom: 1rem;
  }
  .modal .meta dl {
    display: grid;
    grid-template-columns: max-content 1fr;
    gap: 0.35rem 1rem;
    font-size: 0.8rem;
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border);
  }
  .modal .meta dt {
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    font-size: 0.68rem;
    padding-top: 0.15rem;
  }
  .modal .meta dd { color: var(--fg); word-break: break-word; }
  .modal .meta dd code { font-family: var(--mono); font-size: 0.72rem; color: var(--muted); }
  .modal .meta .tags-full { display: flex; gap: 0.3rem; flex-wrap: wrap; }
  .modal .meta .tags-full .tag {
    font-size: 0.65rem;
    padding: 0.15rem 0.5rem;
    background: #f5f0e6;
    border-radius: 2px;
    color: var(--fg);
    text-decoration: none;
    cursor: pointer;
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }
  .modal .meta .tags-full .tag:hover { background: var(--accent); color: #fff; }
  .modal .meta details {
    margin-top: 1rem;
    font-size: 0.72rem;
  }
  .modal .meta details summary {
    cursor: pointer;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    font-size: 0.68rem;
    padding: 0.5rem 0;
    border-top: 1px solid var(--border);
  }
  .modal .meta details pre {
    font-family: var(--mono);
    font-size: 0.7rem;
    color: var(--muted);
    background: #f5f0e6;
    padding: 0.75rem;
    border-radius: 2px;
    overflow-x: auto;
    max-height: 300px;
    white-space: pre-wrap;
    word-break: break-all;
  }

  @media (max-width: 760px) {
    header { padding: 1.2rem 1rem; }
    .filters { padding: 0.8rem 1rem; }
    .grid { padding: 1rem; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); gap: 1rem; }
    .filters input[type="search"] { width: 100%; }
    .modal { padding: 0; }
    .modal .panel {
      grid-template-columns: 1fr;
      max-height: 100vh;
      border-radius: 0;
    }
    .modal .images { min-height: 280px; }
  }
</style>
</head>
<body>

<header>
  <a class="home" href="https://www.thomaslawson.com/"><img class="logo" src="https://www.thomaslawson.com/wp-content/uploads/2022/07/THOMAS-LAWSON-1-301x99.png" alt="Thomas Lawson"></a>
  <span class="page-title">Archive · <strong>Valise</strong></span>
  <span class="badge">Live</span>
  <div class="sub">
    <div id="stats">loading…</div>
    <div id="fetched" style="font-size:0.7rem;opacity:0.7"></div>
  </div>
</header>

<div class="filters" id="filters" hidden>
  <label>Tag</label>
  <select id="filter-tag"><option value="">All</option></select>
  <label>Year</label>
  <select id="filter-year"><option value="">All</option></select>
  <label>Search</label>
  <input type="search" id="filter-search" placeholder="Title, medium, tag, №…">
  <label class="checkbox"><input type="checkbox" id="filter-images"> with image only</label>
  <span class="count" id="count"></span>
</div>

<div class="loading" id="loading">Loading live archive from Valise…</div>
<div class="grid" id="grid" hidden></div>
<div class="empty-state" id="empty" hidden>No works match those filters.</div>

<footer>
  <div>
    Archive managed on <a href="https://valise.works/" target="_blank" rel="noopener">Valise</a> · API v0 · <a href="data.json">data.json</a> · <a href="gap/">website gap report</a>
  </div>
  <div id="updated"></div>
</footer>

<div class="modal" id="modal">
  <span class="close" onclick="closeModal()">&times;</span>
  <div class="panel" onclick="event.stopPropagation()">
    <div class="images">
      <div class="stage">
        <span class="nav prev" id="prev-img" onclick="navImg(-1)">&#8249;</span>
        <img id="m-img" src="" alt="" hidden>
        <span class="empty" id="m-empty" hidden>No image on file</span>
        <span class="nav next" id="next-img" onclick="navImg(1)">&#8250;</span>
      </div>
      <div class="img-meta">
        <span id="m-img-info"></span>
        <span id="m-img-link"></span>
      </div>
      <div class="thumbs" id="m-thumbs"></div>
    </div>
    <div class="meta">
      <h2 id="m-title"></h2>
      <div class="modal-year" id="m-year"></div>
      <dl id="m-dl"></dl>
      <details><summary>Raw JSON</summary><pre id="m-raw"></pre></details>
    </div>
  </div>
</div>

<script>
const TAG_OPTIONS = ${JSON.stringify(topTags)};
const FETCHED_AT = ${JSON.stringify(fetchedAt)};
let ALL = [];
let CURRENT = null;
let IMG_INDEX = 0;
const state = { tag: "", year: "", q: "", imgOnly: false };

function esc(s) {
  if (s == null) return "";
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

function fmtDate(iso) {
  if (!iso) return "";
  const d = new Date(iso);
  if (isNaN(+d)) return iso;
  return d.toISOString().slice(0, 10);
}

async function boot() {
  try {
    const res = await fetch("data.json", { cache: "no-cache" });
    if (!res.ok) throw new Error(res.status + " " + res.statusText);
    ALL = await res.json();
  } catch (err) {
    document.getElementById("loading").textContent =
      "Couldn't load archive: " + err.message;
    return;
  }
  document.getElementById("loading").hidden = true;
  document.getElementById("filters").hidden = false;
  document.getElementById("grid").hidden = false;

  const withImg = ALL.filter((a) => a.images.length).length;
  const years = [...new Set(ALL.map((a) => a.year).filter(Boolean))]
    .sort((a, b) => parseInt(b, 10) - parseInt(a, 10));
  document.getElementById("stats").textContent =
    ALL.length + " works · " + withImg + " with images · " +
    (years.length ? years.at(-1) + "–" + years[0] : "");
  if (FETCHED_AT) {
    document.getElementById("fetched").textContent = "fetched " + fmtDate(FETCHED_AT);
  }

  const tagSel = document.getElementById("filter-tag");
  for (const t of TAG_OPTIONS) {
    const opt = document.createElement("option");
    opt.value = t.title;
    opt.textContent = t.title + " (" + t.count + ")";
    tagSel.appendChild(opt);
  }
  const yearSel = document.getElementById("filter-year");
  for (const y of years) {
    const opt = document.createElement("option");
    opt.value = y; opt.textContent = y;
    yearSel.appendChild(opt);
  }

  tagSel.onchange = (e) => { state.tag = e.target.value; render(); };
  yearSel.onchange = (e) => { state.year = e.target.value; render(); };
  document.getElementById("filter-search").oninput = (e) => { state.q = e.target.value.toLowerCase(); render(); };
  document.getElementById("filter-images").onchange = (e) => { state.imgOnly = e.target.checked; render(); };

  document.getElementById("modal").onclick = closeModal;
  document.addEventListener("keydown", (e) => {
    if (!document.getElementById("modal").classList.contains("open")) return;
    if (e.key === "Escape") closeModal();
    else if (e.key === "ArrowLeft") navImg(-1);
    else if (e.key === "ArrowRight") navImg(1);
  });

  render();
}

function render() {
  let rows = ALL;
  if (state.tag) rows = rows.filter((a) => a.tags.some((t) => t.title === state.tag));
  if (state.year) rows = rows.filter((a) => a.year === state.year);
  if (state.imgOnly) rows = rows.filter((a) => a.images.length);
  if (state.q) {
    rows = rows.filter((a) =>
      a.title.toLowerCase().includes(state.q) ||
      a.medium.toLowerCase().includes(state.q) ||
      a.dimensions.toLowerCase().includes(state.q) ||
      a.uid.toLowerCase().includes(state.q) ||
      a.tags.some((t) => t.title.toLowerCase().includes(state.q))
    );
  }

  document.getElementById("count").textContent = rows.length + " works";

  const grid = document.getElementById("grid");
  const empty = document.getElementById("empty");
  if (rows.length === 0) {
    grid.hidden = true;
    empty.hidden = false;
    return;
  }
  grid.hidden = false;
  empty.hidden = true;

  const frag = document.createDocumentFragment();
  for (const a of rows) {
    const card = document.createElement("div");
    card.className = "card";
    card.dataset.id = a.id;
    card.onclick = () => openModal(a.id);

    const firstImg = a.images[0];
    const imgWrap = document.createElement("div");
    imgWrap.className = "img-wrap" + (firstImg ? "" : " empty");
    if (firstImg) {
      const img = document.createElement("img");
      img.src = firstImg.url;
      img.alt = a.title;
      img.loading = "lazy";
      imgWrap.appendChild(img);
      if (a.images.length > 1) {
        const c = document.createElement("span");
        c.className = "img-count";
        c.textContent = a.images.length;
        imgWrap.appendChild(c);
      }
    }
    card.appendChild(imgWrap);

    const info = document.createElement("div");
    info.className = "info";
    info.innerHTML =
      "<h3><em>" + esc(a.title || "Untitled") + "</em></h3>" +
      (a.year ? "<div class=\\"year\\">" + esc(a.year) + "</div>" : "") +
      (a.medium ? "<div class=\\"medium\\">" + esc(a.medium) + "</div>" : "") +
      (a.dimensions ? "<div class=\\"dims\\">" + esc(a.dimensions) + "</div>" : "") +
      (a.tags.length ? "<div class=\\"tags\\">" + a.tags.slice(0, 3).map((t) => "<span class=\\"tag\\">" + esc(t.title) + "</span>").join("") + "</div>" : "") +
      (a.uid ? "<div class=\\"serial\\">№ " + esc(a.uid) + "</div>" : "");
    card.appendChild(info);
    frag.appendChild(card);
  }
  grid.innerHTML = "";
  grid.appendChild(frag);
}

function openModal(id) {
  const a = ALL.find((x) => x.id === id);
  if (!a) return;
  CURRENT = a;
  IMG_INDEX = 0;

  document.getElementById("m-title").textContent = a.title || "Untitled";
  const yearBits = [a.year, a.uid ? "№ " + a.uid : ""].filter(Boolean);
  document.getElementById("m-year").textContent = yearBits.join(" · ");

  const dl = document.getElementById("m-dl");
  dl.innerHTML = "";
  function row(k, v) {
    if (!v) return;
    dl.innerHTML += "<dt>" + esc(k) + "</dt><dd>" + v + "</dd>";
  }
  row("Title", esc(a.title || "Untitled"));
  row("Year", esc(a.year));
  row("Medium", esc(a.medium));
  row("Dimensions", esc(a.dimensions));
  if (a.tags.length) {
    row(
      "Tags",
      "<div class=\\"tags-full\\">" +
        a.tags
          .map((t) => "<span class=\\"tag\\" onclick=\\"filterByTag('" + esc(t.title).replace(/'/g, "&#39;") + "')\\">" + esc(t.title) + "</span>")
          .join("") +
        "</div>"
    );
  }
  if (a.collections.length) {
    row("Collections", a.collections.map((c) => esc(c.title)).join(", "));
  }
  if (a.editions.length) {
    row("Editions", "<pre style=\\"font-size:0.7rem\\">" + esc(JSON.stringify(a.editions, null, 2)) + "</pre>");
  }
  row("Images", a.images.length ? a.images.length + (a.images.length === 1 ? " image" : " images") : "None on file");
  row("Catalog №", a.uid ? "<code>" + esc(a.uid) + "</code>" : "");
  row("Valise ID", "<code>" + esc(a.id) + "</code>");
  row("Created", fmtDate(a.createdAt));
  row("Updated", fmtDate(a.updatedAt));

  document.getElementById("m-raw").textContent = JSON.stringify(a, null, 2);

  renderImage();

  const thumbs = document.getElementById("m-thumbs");
  thumbs.innerHTML = "";
  if (a.images.length > 1) {
    a.images.forEach((im, i) => {
      const t = document.createElement("div");
      t.className = "thumb" + (i === 0 ? " active" : "");
      t.onclick = (ev) => { ev.stopPropagation(); IMG_INDEX = i; renderImage(); };
      const img = document.createElement("img");
      img.src = im.url;
      t.appendChild(img);
      thumbs.appendChild(t);
    });
  }

  document.getElementById("modal").classList.add("open");
}

function renderImage() {
  const a = CURRENT;
  if (!a) return;
  const img = document.getElementById("m-img");
  const empty = document.getElementById("m-empty");
  const info = document.getElementById("m-img-info");
  const link = document.getElementById("m-img-link");
  const prev = document.getElementById("prev-img");
  const next = document.getElementById("next-img");

  if (!a.images.length) {
    img.hidden = true;
    empty.hidden = false;
    info.textContent = "";
    link.textContent = "";
    prev.style.visibility = next.style.visibility = "hidden";
    return;
  }
  const im = a.images[IMG_INDEX];
  img.src = im.url;
  img.alt = a.title;
  img.hidden = false;
  empty.hidden = true;
  info.textContent =
    (IMG_INDEX + 1) + " / " + a.images.length +
    (im.filename ? "  ·  " + im.filename : "") +
    (im.w && im.h ? "  ·  " + im.w + "×" + im.h : "");
  link.innerHTML = "<a href=\\"" + esc(im.url) + "\\" target=\\"_blank\\" rel=\\"noopener\\">open ↗</a>";
  prev.style.visibility = next.style.visibility = a.images.length > 1 ? "visible" : "hidden";
  for (const [i, t] of [...document.querySelectorAll("#m-thumbs .thumb")].entries()) {
    t.classList.toggle("active", i === IMG_INDEX);
  }
}

function navImg(dir) {
  if (!CURRENT || !CURRENT.images.length) return;
  const n = CURRENT.images.length;
  IMG_INDEX = (IMG_INDEX + dir + n) % n;
  renderImage();
}

function filterByTag(tag) {
  state.tag = tag;
  document.getElementById("filter-tag").value = tag;
  closeModal();
  render();
  window.scrollTo({ top: 0, behavior: "smooth" });
}

function closeModal() {
  document.getElementById("modal").classList.remove("open");
}

boot();
</script>
</body>
</html>
`;

writeFileSync(join(outDir, "index.html"), html);
copyFileSync(join(dataDir, "artworks.json"), join(outDir, "data-raw.json"));

// ---- Gap report subpage (dist/valise/gap/index.html) ----
const gapDir = join(outDir, "gap");
mkdirSync(gapDir, { recursive: true });

const gapHtml = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Thomas Lawson — Archive gap report</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,300;0,400;0,500;0,600&display=swap" rel="stylesheet">
<style>
  :root {
    --bg: #fff9ef; --fg: #222; --muted: #6f6f6f; --faint: #b0a99a;
    --border: #e8e2d6; --accent: #0170B9; --card-bg: #fff;
    --good: #2f8b3e; --warn: #c48518; --bad: #b74125;
    --font: "Poppins", "Helvetica Neue", Helvetica, Arial, sans-serif;
    --mono: "SF Mono", "Menlo", "Monaco", monospace;
  }
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body { font-family: var(--font); background: var(--bg); color: var(--fg); font-size: 14px; line-height: 1.5; -webkit-font-smoothing: antialiased; }
  a { color: var(--accent); }
  header {
    max-width: 1400px; margin: 0 auto; padding: 2rem 2rem 1.2rem;
    border-bottom: 1px solid var(--border);
    display: flex; align-items: center; gap: 1.5rem; flex-wrap: wrap;
  }
  header a.home { display: block; line-height: 0; }
  header .logo { height: 36px; width: auto; }
  header .crumbs { font-size: 0.85rem; font-weight: 300; color: var(--muted); letter-spacing: 0.06em; text-transform: uppercase; }
  header .crumbs strong { color: var(--fg); font-weight: 500; }
  header .tabs { margin-left: auto; display: flex; gap: 0.5rem; }
  header .tab {
    font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.05em;
    padding: 0.4rem 0.9rem; border: 1px solid var(--border); border-radius: 2px;
    color: var(--muted); text-decoration: none; background: #fff;
  }
  header .tab.active { background: var(--accent); color: #fff; border-color: var(--accent); }

  main { max-width: 1400px; margin: 0 auto; padding: 1.5rem 2rem 4rem; }

  h2 { font-size: 1rem; font-weight: 500; margin: 2rem 0 0.75rem; padding-bottom: 0.4rem; border-bottom: 1px solid var(--border); }
  h2:first-child { margin-top: 0; }
  p.hint { font-size: 0.82rem; color: var(--muted); margin-bottom: 1rem; font-weight: 300; }

  .summary {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
    gap: 0.75rem; margin-bottom: 2rem;
  }
  .summary .stat {
    background: #fff; border: 1px solid var(--border); border-radius: 2px;
    padding: 0.9rem 1.1rem;
  }
  .summary .stat .label { font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.06em; color: var(--muted); font-weight: 500; }
  .summary .stat .value { font-size: 1.7rem; font-weight: 400; margin-top: 0.2rem; }
  .summary .stat.good .value { color: var(--good); }
  .summary .stat.warn .value { color: var(--warn); }
  .summary .stat.bad .value { color: var(--bad); }
  .summary .stat .sub { font-size: 0.7rem; color: var(--muted); margin-top: 0.25rem; font-weight: 300; }

  table { width: 100%; border-collapse: collapse; margin: 0.5rem 0 1.5rem; font-size: 0.82rem; }
  th, td { padding: 0.55rem 0.7rem; text-align: left; border-bottom: 1px solid var(--border); vertical-align: top; }
  th { font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.05em; color: var(--muted); font-weight: 500; background: #fcf6ea; }
  tbody tr:hover { background: #fcf6ea; }
  td.num { font-variant-numeric: tabular-nums; text-align: right; }
  td.mono { font-family: var(--mono); font-size: 0.72rem; color: var(--muted); }
  td.title-col { max-width: 320px; }
  td.title-col em { font-style: italic; }
  .bar {
    display: inline-block; width: 140px; height: 7px; background: #f0ead9;
    border-radius: 2px; overflow: hidden; vertical-align: middle; margin-right: 0.4rem;
  }
  .bar span { display: block; height: 100%; background: var(--accent); }
  .tag-pill {
    display: inline-block; font-size: 0.65rem; padding: 0.1rem 0.5rem;
    border-radius: 2px; text-transform: uppercase; letter-spacing: 0.04em; font-weight: 500;
  }
  .tag-pill.ready { background: #dff3e1; color: var(--good); }
  .tag-pill.partial { background: #fdf0d4; color: var(--warn); }
  .tag-pill.blocked { background: #f8dbd3; color: var(--bad); }
  .tag-pill.empty { background: #eee; color: var(--muted); }

  .chip {
    display: inline-block; font-size: 0.62rem; padding: 0.1rem 0.45rem;
    border-radius: 2px; text-transform: uppercase; letter-spacing: 0.04em; font-weight: 500;
  }
  .chip.strong { background: #dff3e1; color: var(--good); }
  .chip.weak { background: #fdf0d4; color: var(--warn); }
  .chip.missing { background: #f8dbd3; color: var(--bad); }
  .chip.img { background: #eaf4fb; color: var(--accent); }
  .chip.noimg { background: #f0ead9; color: var(--faint); }

  .filters { margin: 1rem 0; display: flex; gap: 0.6rem; flex-wrap: wrap; align-items: center; }
  .filters select, .filters input {
    font-family: var(--font); font-size: 0.78rem; padding: 0.35rem 0.65rem;
    border: 1px solid var(--border); border-radius: 2px; background: #fff; color: var(--fg);
  }
  .filters input[type="search"] { width: 220px; }
  .filters label { font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.05em; color: var(--muted); }
  .thumb { width: 48px; height: 48px; background: #f5f0e6; display: inline-flex; align-items: center; justify-content: center; overflow: hidden; border-radius: 2px; }
  .thumb img { max-width: 100%; max-height: 100%; object-fit: contain; }

  footer { max-width: 1400px; margin: 0 auto; padding: 2rem; border-top: 1px solid var(--border); font-size: 0.72rem; color: var(--muted); display: flex; justify-content: space-between; flex-wrap: wrap; gap: 1rem; }
  footer a { text-decoration: none; }
</style>
</head>
<body>
<header>
  <a class="home" href="https://www.thomaslawson.com/"><img class="logo" src="https://www.thomaslawson.com/wp-content/uploads/2022/07/THOMAS-LAWSON-1-301x99.png" alt="Thomas Lawson"></a>
  <span class="crumbs">Archive · <strong>Website gap report</strong></span>
  <div class="tabs">
    <a href="../" class="tab">Valise archive</a>
    <a href="./" class="tab active">Gap report</a>
  </div>
</header>

<main>
  <p class="hint" id="generated">Loading…</p>

  <h2>Summary</h2>
  <p class="hint">The site (<a href="https://www.thomaslawson.com/">thomaslawson.com</a>) shows a curated selection. Valise holds the full archive. This report compares them so you can: upload missing works to Valise, or rebuild site pages from live Valise data.</p>
  <div class="summary" id="summary"></div>

  <h2>Page readiness — which site pages can be rebuilt from Valise?</h2>
  <p class="hint">A page is <strong>ready</strong> if ≥80% of its artworks are matched to Valise with an image. <strong>Partial</strong> means ≥40%. <strong>Blocked</strong> means too many works are still missing from Valise.</p>
  <div style="overflow-x: auto;">
    <table id="pages-table">
      <thead>
        <tr>
          <th>Page</th>
          <th class="num">Works</th>
          <th>Coverage</th>
          <th class="num">Strong</th>
          <th class="num">Weak</th>
          <th class="num">Missing</th>
          <th class="num">Strong w/image</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
  </div>

  <h2>Site works <em>not</em> in Valise — upload candidates</h2>
  <p class="hint">These artworks appear on thomaslawson.com but have no confident match in the Valise archive. Uploading them to Valise would let the site pull them live.</p>
  <div class="filters">
    <label>Period</label>
    <select id="missing-period"><option value="">All</option></select>
    <label>Tier</label>
    <select id="missing-tier"><option value="missing">Missing</option><option value="weak">Weak match (review)</option><option value="all">All unmatched</option></select>
    <label>Search</label>
    <input type="search" id="missing-search" placeholder="Title…">
    <span id="missing-count" style="color: var(--muted); font-size: 0.8rem; margin-left: auto;"></span>
  </div>
  <div style="overflow-x: auto;">
    <table id="missing-table">
      <thead>
        <tr>
          <th></th>
          <th>Site title</th>
          <th>Year</th>
          <th>Medium</th>
          <th>Period</th>
          <th>Best Valise candidate</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
  </div>

  <h2>Valise works not yet on the site</h2>
  <p class="hint">These are in Valise but don't match any artwork currently shown on thomaslawson.com. Candidates for additions to existing pages or new archive pages.</p>
  <div class="filters">
    <label>Year</label>
    <select id="only-year"><option value="">All</option></select>
    <label>Has image</label>
    <select id="only-image">
      <option value="">Either</option>
      <option value="yes">With image</option>
      <option value="no">Without image</option>
    </select>
    <label>Search</label>
    <input type="search" id="only-search" placeholder="Title, medium…">
    <span id="only-count" style="color: var(--muted); font-size: 0.8rem; margin-left: auto;"></span>
  </div>
  <div style="overflow-x: auto;">
    <table id="only-table">
      <thead>
        <tr>
          <th></th>
          <th>Title</th>
          <th>Year</th>
          <th>Medium</th>
          <th>Dimensions</th>
          <th>Tags</th>
          <th>№</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
  </div>
</main>

<footer>
  <div>Generated from <a href="gap.json">gap.json</a> · <a href="../">back to archive</a></div>
  <div id="source-links"></div>
</footer>

<script>
function esc(s) {
  if (s == null) return "";
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}
function fmtDate(iso) {
  if (!iso) return "";
  const d = new Date(iso);
  return isNaN(+d) ? iso : d.toISOString().slice(0, 10);
}

let GAP = null;

async function boot() {
  try {
    const res = await fetch("gap.json", { cache: "no-cache" });
    GAP = await res.json();
  } catch (err) {
    document.getElementById("generated").textContent = "Failed to load gap.json: " + err.message;
    return;
  }
  document.getElementById("generated").textContent = "Generated " + fmtDate(GAP.generatedAt) + " · site: " + GAP.siteTotal + " works, Valise: " + GAP.valiseTotal + " works.";
  renderSummary();
  renderPages();
  buildFilters();
  renderMissing();
  renderOnly();
}

function renderSummary() {
  const s = GAP;
  const stats = [
    { label: "Works on site", value: s.siteTotal, sub: "scraped from thomaslawson.com" },
    { label: "Works in Valise", value: s.valiseTotal, sub: "api.valise.works/v0" },
    { label: "Strong matches", value: s.strong, sub: "confident cross-reference", cls: "good" },
    { label: "Weak matches", value: s.weak, sub: "probable but review needed", cls: "warn" },
    { label: "Missing from Valise", value: s.missing, sub: "upload candidates", cls: "bad" },
    { label: "Valise-only", value: s.valiseOnly, sub: "not shown on site" },
  ];
  document.getElementById("summary").innerHTML = stats
    .map(
      (st) =>
        '<div class="stat' + (st.cls ? " " + st.cls : "") + '">' +
          '<div class="label">' + esc(st.label) + '</div>' +
          '<div class="value">' + st.value + '</div>' +
          '<div class="sub">' + esc(st.sub) + '</div>' +
        '</div>',
    )
    .join("");
}

function renderPages() {
  const tbody = document.querySelector("#pages-table tbody");
  tbody.innerHTML = GAP.pages.map((p) => {
    const pct = p.total ? Math.round((p.strongWithImage / p.total) * 100) : 0;
    const link = p.pageUrl ? '<a href="' + esc(p.pageUrl) + '" target="_blank" rel="noopener">' + esc(p.key) + '</a>' : esc(p.key);
    return (
      "<tr>" +
        "<td>" + link + "</td>" +
        "<td class=\\"num\\">" + p.total + "</td>" +
        "<td><span class=\\"bar\\"><span style=\\"width:" + pct + "%\\"></span></span> " + pct + "%</td>" +
        "<td class=\\"num\\">" + p.strong + "</td>" +
        "<td class=\\"num\\">" + p.weak + "</td>" +
        "<td class=\\"num\\">" + p.missing + "</td>" +
        "<td class=\\"num\\">" + p.strongWithImage + "</td>" +
        "<td><span class=\\"tag-pill " + p.replaceableTier + "\\">" + p.replaceableTier + "</span></td>" +
      "</tr>"
    );
  }).join("");
}

function buildFilters() {
  const periodSel = document.getElementById("missing-period");
  const periods = [...new Set(GAP.siteArtworks.map((r) => r.site.period || r.site.section).filter(Boolean))].sort();
  for (const p of periods) {
    const o = document.createElement("option");
    o.value = p; o.textContent = p; periodSel.appendChild(o);
  }
  const yearSel = document.getElementById("only-year");
  const years = [...new Set(GAP.valiseOnlyArtworks.map((v) => v.year).filter(Boolean))]
    .sort((a, b) => parseInt(b, 10) - parseInt(a, 10));
  for (const y of years) {
    const o = document.createElement("option");
    o.value = y; o.textContent = y; yearSel.appendChild(o);
  }
  document.getElementById("missing-period").onchange = renderMissing;
  document.getElementById("missing-tier").onchange = renderMissing;
  document.getElementById("missing-search").oninput = renderMissing;
  document.getElementById("only-year").onchange = renderOnly;
  document.getElementById("only-image").onchange = renderOnly;
  document.getElementById("only-search").oninput = renderOnly;
}

function renderMissing() {
  const period = document.getElementById("missing-period").value;
  const tier = document.getElementById("missing-tier").value;
  const q = document.getElementById("missing-search").value.toLowerCase();
  let rows = GAP.siteArtworks.filter((r) => r.match.tier !== "strong");
  if (tier === "missing") rows = rows.filter((r) => r.match.tier === "missing");
  else if (tier === "weak") rows = rows.filter((r) => r.match.tier === "weak");
  if (period) rows = rows.filter((r) => (r.site.period || r.site.section) === period);
  if (q) rows = rows.filter((r) => (r.site.title || "").toLowerCase().includes(q));
  document.getElementById("missing-count").textContent = rows.length + " works";
  document.querySelector("#missing-table tbody").innerHTML = rows.slice(0, 400).map((r) => {
    const s = r.site;
    const tierChip = '<span class="chip ' + r.match.tier + '">' + r.match.tier + '</span>';
    const best = r.match.valise
      ? '<em>' + esc(r.match.valise.title) + '</em>' +
        (r.match.valise.year ? ' · ' + esc(r.match.valise.year) : '') +
        ' <span class="chip ' + (r.match.valise.hasImage ? "img" : "noimg") + '">' +
        (r.match.valise.hasImage ? "img" : "no img") + '</span>' +
        ' <span style="color:var(--muted); font-size:0.72rem">' + esc(r.match.how) + ' ' + r.match.score + '</span>'
      : '<span style="color: var(--faint)">— no candidate —</span>';
    const thumb = s.image
      ? '<div class="thumb"><img src="' + esc(s.image) + '" alt="" loading="lazy"></div>'
      : '<div class="thumb"></div>';
    return (
      "<tr>" +
        "<td>" + thumb + "</td>" +
        "<td class=\\"title-col\\">" + tierChip + " <em>" + esc(s.title || "Untitled") + "</em></td>" +
        "<td>" + esc(s.year || "") + "</td>" +
        "<td>" + esc(s.medium || "") + "</td>" +
        "<td>" + esc(s.period || s.section || "") + "</td>" +
        "<td>" + best + "</td>" +
      "</tr>"
    );
  }).join("") + (rows.length > 400 ? '<tr><td colspan="6" style="text-align:center; color: var(--muted); font-style: italic">Showing first 400 of ' + rows.length + '</td></tr>' : "");
}

function renderOnly() {
  const y = document.getElementById("only-year").value;
  const imgFlag = document.getElementById("only-image").value;
  const q = document.getElementById("only-search").value.toLowerCase();
  let rows = GAP.valiseOnlyArtworks;
  if (y) rows = rows.filter((v) => v.year === y);
  if (imgFlag === "yes") rows = rows.filter((v) => v.hasImage);
  if (imgFlag === "no") rows = rows.filter((v) => !v.hasImage);
  if (q) rows = rows.filter((v) =>
    (v.title || "").toLowerCase().includes(q) ||
    (v.medium || "").toLowerCase().includes(q),
  );
  document.getElementById("only-count").textContent = rows.length + " works";
  document.querySelector("#only-table tbody").innerHTML = rows.slice(0, 500).map((v) => {
    const thumb = v.imageUrl
      ? '<div class="thumb"><img src="' + esc(v.imageUrl) + '" alt="" loading="lazy"></div>'
      : '<div class="thumb"></div>';
    return (
      "<tr>" +
        "<td>" + thumb + "</td>" +
        "<td class=\\"title-col\\"><em>" + esc(v.title || "Untitled") + "</em></td>" +
        "<td>" + esc(v.year || "") + "</td>" +
        "<td>" + esc(v.medium || "") + "</td>" +
        "<td>" + esc(v.dimensions || "") + "</td>" +
        "<td>" + (v.tags || []).slice(0, 3).map((t) => '<span class="chip" style="background:#f5f0e6; color:var(--muted)">' + esc(t) + '</span>').join(" ") + "</td>" +
        "<td class=\\"mono\\">" + esc(v.uid || "") + "</td>" +
      "</tr>"
    );
  }).join("") + (rows.length > 500 ? '<tr><td colspan="7" style="text-align:center; color: var(--muted); font-style: italic">Showing first 500 of ' + rows.length + '</td></tr>' : "");
}

boot();
</script>
</body>
</html>
`;

writeFileSync(join(gapDir, "index.html"), gapHtml);

// ---- Add top-level nav link from main index.html to gap subpage ----
// Inject a link in the footer-left area of the main page for discoverability.

const dataBytes = JSON.stringify(full).length;
console.log(
  `Built dist/valise/\n` +
    `  index.html    ${(html.length / 1024).toFixed(0)} KB\n` +
    `  data.json     ${(dataBytes / 1024).toFixed(0)} KB  (${full.length} works, ${withImages} with images)\n` +
    `  data-raw.json ${(readFileSync(join(outDir, "data-raw.json"), "utf-8").length / 1024).toFixed(0)} KB  (full API payload)\n` +
    `  tags:         ${topTags.length} distinct\n` +
    `  years:        ${years[0]}–${years.at(-1)}\n` +
    `  collections:  ${collections.length}`,
);
