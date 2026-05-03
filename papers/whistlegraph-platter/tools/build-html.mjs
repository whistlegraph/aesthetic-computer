#!/usr/bin/env node
// build-html.mjs
// Bundles dropbox-manifest.json + dropbox-files.json into a self-contained
// HTML browser at ~/Desktop/whistlegraph-archive.html.

import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MANIFEST = path.resolve(__dirname, "../dropbox-manifest.json");
const FILES = path.resolve(__dirname, "../dropbox-files.json");
const OUT = path.resolve(os.homedir(), "Desktop/whistlegraph-archive.html");

const manifest = JSON.parse(fs.readFileSync(MANIFEST, "utf8"));
const files = JSON.parse(fs.readFileSync(FILES, "utf8"));

const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>whistlegraph archive — ${manifest.totalFiles} files · ${(manifest.totalBytes / 1e9).toFixed(2)} GB</title>
<style>
  :root {
    --bg: #000;
    --fg: #6cffa3;
    --fg-bright: #b8ffce;
    --muted: #2a6b3f;
    --dim: #1a3322;
    --accent: #ffd166;
    --hi: #00ff7f;
    --row-hover: #061a0c;
    --row-stripe: #030a06;
  }
  * { box-sizing: border-box; }
  html, body {
    margin: 0; padding: 0;
    background: var(--bg);
    color: var(--fg);
    font-family: "SF Mono", Menlo, Monaco, "Courier New", monospace;
    font-size: 12px;
    line-height: 1.45;
  }
  body { padding: 16px 20px 80px; }
  h1 { font-size: 14px; font-weight: 600; margin: 0 0 4px; color: var(--fg-bright); letter-spacing: 0.04em; }
  h2 { font-size: 12px; font-weight: 600; margin: 22px 0 8px; color: var(--accent); text-transform: uppercase; letter-spacing: 0.1em; }
  .stats { color: var(--muted); margin-bottom: 18px; }
  .stats b { color: var(--fg); font-weight: 600; }
  table { border-collapse: collapse; width: 100%; }
  th, td { text-align: left; padding: 4px 10px 4px 0; border-bottom: 1px solid var(--dim); white-space: nowrap; vertical-align: top; }
  th { color: var(--accent); cursor: pointer; user-select: none; font-weight: 600; position: sticky; top: 0; background: var(--bg); }
  th:hover { color: var(--hi); }
  th.sorted::after { content: " ▾"; color: var(--hi); }
  th.sorted.asc::after { content: " ▴"; }
  td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
  td.path { white-space: normal; word-break: break-all; max-width: 720px; }
  tr:hover td { background: var(--row-hover); color: var(--fg-bright); }
  tr.stripe td { background: var(--row-stripe); }
  tr.stripe:hover td { background: var(--row-hover); }
  .ext { color: var(--muted); }
  .top { color: var(--accent); }
  .controls {
    position: sticky; top: 0; z-index: 5;
    background: var(--bg);
    padding: 10px 0 8px;
    border-bottom: 1px solid var(--dim);
    margin-bottom: 12px;
    display: flex; gap: 12px; align-items: center; flex-wrap: wrap;
  }
  input[type="text"], select {
    background: var(--bg);
    color: var(--fg-bright);
    border: 1px solid var(--muted);
    padding: 6px 10px;
    font: inherit;
    border-radius: 0;
    outline: none;
  }
  input[type="text"]:focus, select:focus { border-color: var(--hi); }
  input[type="text"] { min-width: 320px; }
  .pill {
    display: inline-block;
    padding: 2px 8px;
    border: 1px solid var(--muted);
    color: var(--muted);
    font-size: 11px;
    cursor: pointer;
  }
  .pill:hover { color: var(--fg-bright); border-color: var(--fg); }
  .pill.active { color: var(--bg); background: var(--hi); border-color: var(--hi); }
  .meta { color: var(--muted); font-size: 11px; }
  details > summary { cursor: pointer; color: var(--accent); padding: 4px 0; }
  details[open] > summary { color: var(--hi); }
  .pager { display: flex; gap: 8px; align-items: center; margin: 12px 0 0; color: var(--muted); }
  .pager button {
    background: var(--bg); color: var(--fg);
    border: 1px solid var(--muted);
    padding: 4px 12px; cursor: pointer; font: inherit;
  }
  .pager button:hover:not(:disabled) { color: var(--hi); border-color: var(--hi); }
  .pager button:disabled { opacity: 0.4; cursor: default; }
  ::selection { background: var(--hi); color: var(--bg); }
</style>
</head>
<body>
  <h1>whistlegraph archive · /Whistlegraph/</h1>
  <div class="stats">
    <b>${manifest.totalFiles.toLocaleString()}</b> files ·
    <b>${(manifest.totalBytes / 1e9).toFixed(2)} GB</b> ·
    <b>${Object.keys(manifest.byTop).length}</b> top-level subfolders ·
    <span class="meta">manifest pulled ${manifest.generatedAt.slice(0, 19).replace("T", " ")}Z</span>
  </div>

  <details open>
    <summary>subfolders (click header to sort) · ${Object.keys(manifest.byTop).length}</summary>
    <table id="folders">
      <thead>
        <tr>
          <th data-key="name">name</th>
          <th data-key="files" class="num">files</th>
          <th data-key="subdirs" class="num">subdirs</th>
          <th data-key="bytes" class="num sorted">size</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
  </details>

  <h2>files</h2>
  <div class="controls">
    <input type="text" id="search" placeholder="filter (substring match on path; case-insensitive)" autofocus>
    <select id="topFilter">
      <option value="">all subfolders</option>
    </select>
    <select id="extFilter">
      <option value="">all types</option>
    </select>
    <span class="pill" data-preset="">all</span>
    <span class="pill" data-preset="video">video</span>
    <span class="pill" data-preset="audio">audio</span>
    <span class="pill" data-preset="image">image</span>
    <span class="pill" data-preset="score">scores</span>
    <span class="pill" data-preset="procreate">procreate</span>
    <span class="meta" id="resultCount"></span>
  </div>

  <table id="files">
    <thead>
      <tr>
        <th data-key="p">path</th>
        <th data-key="s" class="num sorted">size</th>
        <th data-key="m" class="num">modified</th>
      </tr>
    </thead>
    <tbody></tbody>
  </table>

  <div class="pager">
    <button id="prev">‹ prev</button>
    <span id="pageInfo">page 1 of 1</span>
    <button id="next">next ›</button>
    <span class="meta">· ${(files.length).toLocaleString()} files indexed · 200 per page</span>
  </div>

<script>
const MANIFEST = ${JSON.stringify(manifest)};
const FILES = ${JSON.stringify(files)};

const PRESETS = {
  video:    /\\.(mp4|mov|webm|m4v|mpeg|avi|mxf)$/i,
  audio:    /\\.(wav|aif|m4a|mp3|aac)$/i,
  image:    /\\.(png|jpg|jpeg|heic|webp|gif|tiff?)$/i,
  score:    /\\b(score|manuscript|whistlegraph|drawing)/i,
  procreate:/\\.(procreate|pixaki)$/i,
};

const fmtSize = (b) => {
  if (b >= 1e9) return (b / 1e9).toFixed(2) + " GB";
  if (b >= 1e6) return (b / 1e6).toFixed(1) + " MB";
  if (b >= 1e3) return (b / 1e3).toFixed(1) + " KB";
  return (b || 0) + " B";
};
const fmtDate = (d) => d ? d.slice(0, 10) : "—";
const ext = (p) => {
  const i = p.lastIndexOf("."); const s = p.lastIndexOf("/");
  return (i > s && i !== -1) ? p.slice(i).toLowerCase() : "";
};
const top = (p) => p.split("/")[0];

// Pre-compute decorated entries
for (const f of FILES) { f.t = top(f.p); f.x = ext(f.p); }

// === Folder summary table ===
function renderFolders() {
  const rows = Object.entries(MANIFEST.byTop).map(([name, s]) => ({ name, ...s }));
  const tbody = document.querySelector("#folders tbody");
  let sortKey = "bytes", sortAsc = false;
  function sort() {
    rows.sort((a, b) => {
      const av = a[sortKey], bv = b[sortKey];
      if (typeof av === "string") return sortAsc ? av.localeCompare(bv) : bv.localeCompare(av);
      return sortAsc ? av - bv : bv - av;
    });
    render();
  }
  function render() {
    tbody.innerHTML = rows.map((r, i) => {
      const klass = i % 2 ? "stripe" : "";
      return \`<tr class="\${klass}"><td><span class="top">\${esc(r.name)}</span></td><td class="num">\${r.files.toLocaleString()}</td><td class="num">\${r.subdirs.toLocaleString()}</td><td class="num">\${fmtSize(r.bytes)}</td></tr>\`;
    }).join("");
  }
  document.querySelectorAll("#folders th").forEach((th) => {
    th.addEventListener("click", () => {
      const key = th.dataset.key;
      if (sortKey === key) sortAsc = !sortAsc; else { sortKey = key; sortAsc = (key === "name"); }
      document.querySelectorAll("#folders th").forEach((t) => t.classList.remove("sorted", "asc"));
      th.classList.add("sorted");
      if (sortAsc) th.classList.add("asc");
      sort();
    });
  });
  sort();
}

// === File browser ===
const PAGE_SIZE = 200;
let filtered = FILES.slice();
let page = 0;
let sortKey = "s", sortAsc = false;
let preset = "";

const tbody = document.querySelector("#files tbody");
const search = document.getElementById("search");
const topFilter = document.getElementById("topFilter");
const extFilter = document.getElementById("extFilter");
const resultCount = document.getElementById("resultCount");
const pageInfo = document.getElementById("pageInfo");
const prev = document.getElementById("prev");
const next = document.getElementById("next");

// Populate top filter from manifest
{
  const opts = Object.keys(MANIFEST.byTop).sort();
  for (const t of opts) {
    const o = document.createElement("option");
    o.value = t; o.textContent = t;
    topFilter.appendChild(o);
  }
}
// Populate ext filter
{
  const opts = Object.entries(MANIFEST.extCounts).sort((a, b) => b[1] - a[1]).slice(0, 40);
  for (const [x, n] of opts) {
    const o = document.createElement("option");
    o.value = x; o.textContent = \`\${x || "(none)"} (\${n})\`;
    extFilter.appendChild(o);
  }
}

function esc(s) {
  return String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
}

function applyFilter() {
  const q = search.value.trim().toLowerCase();
  const t = topFilter.value;
  const x = extFilter.value;
  const re = preset && PRESETS[preset];
  filtered = FILES.filter((f) => {
    if (q && !f.p.toLowerCase().includes(q)) return false;
    if (t && f.t !== t) return false;
    if (x && f.x !== x) return false;
    if (re && !re.test(f.p)) return false;
    return true;
  });
  applySort();
  page = 0;
  render();
}

function applySort() {
  filtered.sort((a, b) => {
    const av = a[sortKey], bv = b[sortKey];
    if (sortKey === "p") return sortAsc ? av.localeCompare(bv) : bv.localeCompare(av);
    if (av == null) return 1;
    if (bv == null) return -1;
    return sortAsc ? (av > bv ? 1 : av < bv ? -1 : 0) : (av < bv ? 1 : av > bv ? -1 : 0);
  });
}

function render() {
  const totalPages = Math.max(1, Math.ceil(filtered.length / PAGE_SIZE));
  if (page >= totalPages) page = totalPages - 1;
  const start = page * PAGE_SIZE;
  const slice = filtered.slice(start, start + PAGE_SIZE);
  tbody.innerHTML = slice.map((f, i) => {
    const klass = i % 2 ? "stripe" : "";
    const parts = f.p.split("/");
    const name = parts.pop();
    const dir = parts.join("/");
    return \`<tr class="\${klass}"><td class="path"><span class="top">\${esc(parts[0] || "")}</span>\${dir.includes("/") ? "<span class=\\"ext\\">/" + esc(dir.split("/").slice(1).join("/")) + "</span>" : ""}<span>/\${esc(name)}</span></td><td class="num">\${fmtSize(f.s)}</td><td class="num">\${fmtDate(f.m)}</td></tr>\`;
  }).join("");
  resultCount.textContent = \`\${filtered.length.toLocaleString()} match\${filtered.length === 1 ? "" : "es"}\`;
  pageInfo.textContent = \`page \${page + 1} of \${totalPages}\`;
  prev.disabled = page === 0;
  next.disabled = page >= totalPages - 1;
  window.scrollTo(0, 0);
}

document.querySelectorAll("#files th").forEach((th) => {
  th.addEventListener("click", () => {
    const key = th.dataset.key;
    if (sortKey === key) sortAsc = !sortAsc; else { sortKey = key; sortAsc = (key === "p"); }
    document.querySelectorAll("#files th").forEach((t) => t.classList.remove("sorted", "asc"));
    th.classList.add("sorted");
    if (sortAsc) th.classList.add("asc");
    applySort();
    render();
  });
});

document.querySelectorAll(".pill").forEach((pill) => {
  pill.addEventListener("click", () => {
    document.querySelectorAll(".pill").forEach((p) => p.classList.remove("active"));
    pill.classList.add("active");
    preset = pill.dataset.preset;
    applyFilter();
  });
});

let searchT;
search.addEventListener("input", () => {
  clearTimeout(searchT);
  searchT = setTimeout(applyFilter, 120);
});
topFilter.addEventListener("change", applyFilter);
extFilter.addEventListener("change", applyFilter);
prev.addEventListener("click", () => { if (page > 0) { page--; render(); } });
next.addEventListener("click", () => { page++; render(); });

renderFolders();
applyFilter();
</script>
</body>
</html>
`;

fs.writeFileSync(OUT, html);
const stat = fs.statSync(OUT);
console.error(`wrote ${OUT} (${(stat.size / 1e6).toFixed(2)} MB)`);
