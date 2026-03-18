#!/usr/bin/env node
// Builds a self-contained HTML page with the catalog JSON baked in.
// Output: dist/index.html

import { readFileSync, writeFileSync, mkdirSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const catalog = readFileSync(join(__dirname, "data", "catalog.json"), "utf-8");
const outDir = join(__dirname, "dist");
mkdirSync(outDir, { recursive: true });

const html = `<?php
// Bootstrap WordPress to check current user
define('SHORTINIT', false);
$wp_load = dirname(__DIR__) . '/wp-load.php';
if (!file_exists($wp_load)) {
    // Try absolute path as fallback
    $wp_load = $_SERVER['DOCUMENT_ROOT'] . '/wp-load.php';
}
require_once $wp_load;

$wp_user = null;
if (function_exists('is_user_logged_in') && is_user_logged_in()) {
    $current = wp_get_current_user();
    $wp_user = [
        'id'       => $current->ID,
        'login'    => $current->user_login,
        'name'     => $current->display_name,
        'email'    => $current->user_email,
        'isAdmin'  => current_user_can('manage_options'),
        'isEditor' => current_user_can('edit_posts'),
        'roles'    => $current->roles,
    ];
}
?>
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Thomas Lawson — Artwork Index</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,300;0,400;0,500;0,600;1,400&display=swap" rel="stylesheet">
<style>
  :root {
    --bg: #fff9ef;
    --fg: #222222;
    --muted: #6f6f6f;
    --border: #e8e2d6;
    --accent: #0170B9;
    --card-bg: #ffffff;
    --font: "Poppins", "Helvetica Neue", Helvetica, Arial, sans-serif;
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

  /* Header */
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
  header a {
    display: block;
    line-height: 0;
  }
  header .logo {
    height: 40px;
    width: auto;
  }
  header .page-title {
    font-size: 0.85rem;
    font-weight: 300;
    letter-spacing: 0.06em;
    text-transform: uppercase;
    color: var(--muted);
  }
  header .sub {
    color: var(--muted);
    font-size: 0.78rem;
    margin-left: auto;
    font-weight: 300;
  }

  /* User badge */
  .user-badge {
    font-size: 0.72rem;
    font-weight: 400;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .user-badge .user-name {
    color: var(--fg);
  }
  .user-badge .user-role {
    font-size: 0.62rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.15rem 0.5rem;
    border-radius: 2px;
    font-weight: 500;
  }
  .user-badge .role-admin {
    background: #0170B9;
    color: #fff;
  }
  .user-badge .role-editor {
    background: #e8e2d6;
    color: var(--muted);
  }
  .user-badge .role-viewer {
    background: #f5f0e6;
    color: var(--muted);
  }
  .user-badge a {
    color: var(--accent);
    text-decoration: none;
    font-size: 0.72rem;
  }
  .user-badge a:hover { text-decoration: underline; }

  /* Filters */
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
  .filters select:focus, .filters input:focus {
    border-color: var(--accent);
  }
  .filters input[type="search"] {
    width: 220px;
  }
  .count {
    margin-left: auto;
    font-size: 0.8rem;
    color: var(--muted);
  }

  /* Tabs */
  .tabs {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
    display: flex;
    gap: 0;
    border-bottom: 1px solid var(--border);
  }
  .tabs button {
    font-family: var(--font);
    font-size: 0.78rem;
    font-weight: 400;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    padding: 0.8rem 1.4rem;
    border: none;
    background: none;
    color: var(--muted);
    cursor: pointer;
    border-bottom: 2px solid transparent;
    transition: all 0.2s;
  }
  .tabs button.active {
    color: var(--fg);
    border-bottom-color: var(--accent);
    font-weight: 500;
  }
  .tabs button:hover { color: var(--fg); }

  /* Card */
  .card {
    background: var(--card-bg);
    border: 1px solid var(--border);
    border-radius: 2px;
    overflow: hidden;
    transition: box-shadow 0.2s, transform 0.2s;
    cursor: pointer;
  }
  .card:hover {
    box-shadow: 0 4px 20px rgba(0,0,0,0.07);
    transform: translateY(-2px);
  }
  .card .img-wrap {
    width: 100%;
    aspect-ratio: 1;
    overflow: hidden;
    background: #f5f0e6;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.75rem;
  }
  .card .img-wrap img {
    max-width: 100%;
    max-height: 100%;
    object-fit: contain;
  }
  .card .info {
    padding: 0.8rem 1rem 1rem;
    border-top: 1px solid var(--border);
  }
  .card .info h3 {
    font-size: 0.82rem;
    font-weight: 500;
    line-height: 1.35;
    margin-bottom: 0.2rem;
    color: var(--fg);
  }
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
    font-family: "SF Mono", "Menlo", "Monaco", monospace;
    color: #b0a99a;
    margin-top: 0.4rem;
    letter-spacing: 0.05em;
  }
  .card .img-wrap {
    position: relative;
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

  /* Section headers */
  .section-header {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2.5rem 2rem 0.5rem;
  }
  .section-header:first-child {
    padding-top: 1.5rem;
  }
  .section-header h2 {
    font-size: 1rem;
    font-weight: 500;
    letter-spacing: 0.04em;
    color: var(--fg);
    margin-bottom: 0.4rem;
  }
  .section-header .section-line {
    width: 40px;
    height: 2px;
    background: var(--accent);
    margin-bottom: 0.8rem;
  }
  .section-intros {
    max-width: 700px;
  }
  .section-intros .intro-block {
    margin-bottom: 1rem;
  }
  .section-intros .intro-heading {
    font-size: 0.82rem;
    font-weight: 500;
    font-style: italic;
    color: var(--fg);
    margin-bottom: 0.25rem;
  }
  .section-intros .intro-text {
    font-size: 0.8rem;
    font-weight: 300;
    color: var(--muted);
    line-height: 1.65;
  }
  .section-grid {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1rem 2rem 1rem;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
    gap: 1.8rem;
  }

  /* Writings list */
  .writings-list {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1.5rem 2rem 4rem;
  }
  .writing-row {
    display: grid;
    grid-template-columns: 1fr auto auto;
    gap: 1rem;
    padding: 0.6rem 0;
    border-bottom: 1px solid var(--border);
    align-items: baseline;
  }
  .writing-row .w-title {
    font-size: 0.85rem;
    font-weight: 400;
  }
  .writing-row .w-pub {
    font-size: 0.8rem;
    color: var(--muted);
    font-style: italic;
    font-weight: 300;
  }
  .writing-row .w-year {
    font-size: 0.8rem;
    color: var(--muted);
    min-width: 3rem;
    text-align: right;
    font-weight: 300;
  }

  /* Exhibitions */
  .exhibitions-grid {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1.5rem 2rem 4rem;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
    gap: 1.5rem;
  }
  .exhibition-card {
    background: var(--card-bg);
    border: 1px solid var(--border);
    border-radius: 2px;
    padding: 1.2rem;
  }
  .exhibition-card h3 {
    font-size: 0.88rem;
    font-weight: 500;
    margin-bottom: 0.3rem;
  }
  .exhibition-card .e-year {
    font-size: 0.8rem;
    color: var(--muted);
    font-weight: 300;
  }
  .exhibition-card .e-images {
    display: flex;
    gap: 0.4rem;
    margin-top: 0.6rem;
    overflow-x: auto;
  }
  .exhibition-card .e-images img {
    height: 60px;
    border-radius: 2px;
    object-fit: cover;
  }

  /* Lightbox */
  .lightbox {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,0.85);
    z-index: 100;
    align-items: center;
    justify-content: center;
    cursor: pointer;
  }
  .lightbox.open { display: flex; }
  .lightbox img {
    max-width: 90vw;
    max-height: 90vh;
    object-fit: contain;
  }
  .lightbox .lb-info {
    position: absolute;
    bottom: 2rem;
    left: 50%;
    transform: translateX(-50%);
    color: #ccc;
    text-align: center;
    font-size: 0.85rem;
    max-width: 600px;
  }
  .lightbox .lb-info h3 {
    color: #fff;
    font-size: 1rem;
    margin-bottom: 0.3rem;
  }

  @media (max-width: 640px) {
    header { padding: 1.2rem 1rem; }
    .filters { padding: 0.8rem 1rem; }
    .tabs { padding: 0 1rem; }
    .grid { padding: 1rem; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); gap: 1rem; }
    .writings-list { padding: 1rem; }
    .writing-row { grid-template-columns: 1fr auto; }
    .writing-row .w-pub { display: none; }
  }
</style>
</head>
<body>

<header>
  <a href="https://www.thomaslawson.com/"><img class="logo" src="https://www.thomaslawson.com/wp-content/uploads/2022/07/THOMAS-LAWSON-1-301x99.png" alt="Thomas Lawson"></a>
  <span class="page-title">Artwork Index</span>
  <div class="sub" id="stats"></div>
  <div class="user-badge" id="user-badge"></div>
</header>

<div class="tabs" id="tabs">
  <button class="active" data-tab="artworks">Artworks</button>
  <button data-tab="writings">Writings</button>
  <button data-tab="exhibitions">Exhibitions</button>
</div>

<div id="artworks-view">
  <div class="filters" id="artwork-filters">
    <label>Section</label>
    <select id="filter-section"><option value="">All</option></select>
    <label>Series</label>
    <select id="filter-series"><option value="">All</option></select>
    <label>Search</label>
    <input type="search" id="filter-search" placeholder="Title...">
    <span class="count" id="artwork-count"></span>
  </div>
  <div id="artwork-sections"></div>
</div>

<div id="writings-view" style="display:none">
  <div class="filters">
    <label>Category</label>
    <select id="filter-w-cat"><option value="">All</option></select>
    <label>Search</label>
    <input type="search" id="filter-w-search" placeholder="Title...">
    <span class="count" id="writing-count"></span>
  </div>
  <div class="writings-list" id="writings-list"></div>
</div>

<div id="exhibitions-view" style="display:none">
  <div class="exhibitions-grid" id="exhibitions-grid"></div>
</div>

<div class="lightbox" id="lightbox">
  <img id="lb-img" src="" alt="">
  <div class="lb-info">
    <h3 id="lb-title"></h3>
    <div id="lb-detail"></div>
  </div>
</div>

<script>
const WP_USER = <?php echo $wp_user ? json_encode($wp_user) : 'null'; ?>;
const CATALOG = ${catalog};

// --- State ---
let currentTab = "artworks";
let filters = { section: "", series: "", search: "" };
let wFilters = { category: "", search: "" };

// --- Init ---
function init() {
  const c = CATALOG;
  document.getElementById("stats").textContent =
    c.artworks.length + " artworks · " +
    c.writings.length + " writings · " +
    c.exhibitions.length + " exhibitions · " +
    "b. " + c.artist.born + ", " + c.artist.birthplace;

  // Populate filter dropdowns
  const sectionLabels = (c.sections || []).map(s => s.label);
  const seriesList = [...new Set(c.artworks.map(a => a.series).filter(Boolean))].sort();
  const wCats = [...new Set(c.writings.map(w => w.category))].sort();

  populateSelect("filter-section", sectionLabels);
  populateSelect("filter-series", seriesList);
  populateSelect("filter-w-cat", wCats);

  // Event listeners
  document.getElementById("filter-section").onchange = e => { filters.section = e.target.value; renderArtworks(); };
  document.getElementById("filter-series").onchange = e => { filters.series = e.target.value; renderArtworks(); };
  document.getElementById("filter-search").oninput = e => { filters.search = e.target.value.toLowerCase(); renderArtworks(); };
  document.getElementById("filter-w-cat").onchange = e => { wFilters.category = e.target.value; renderWritings(); };
  document.getElementById("filter-w-search").oninput = e => { wFilters.search = e.target.value.toLowerCase(); renderWritings(); };

  // Tabs
  document.getElementById("tabs").onclick = e => {
    const btn = e.target.closest("button");
    if (!btn) return;
    currentTab = btn.dataset.tab;
    document.querySelectorAll(".tabs button").forEach(b => b.classList.toggle("active", b === btn));
    document.getElementById("artworks-view").style.display = currentTab === "artworks" ? "" : "none";
    document.getElementById("writings-view").style.display = currentTab === "writings" ? "" : "none";
    document.getElementById("exhibitions-view").style.display = currentTab === "exhibitions" ? "" : "none";
  };

  // Lightbox
  const lb = document.getElementById("lightbox");
  lb.onclick = () => lb.classList.remove("open");

  // User badge
  const badge = document.getElementById("user-badge");
  if (WP_USER) {
    const role = WP_USER.isAdmin ? "admin" : WP_USER.isEditor ? "editor" : "viewer";
    const roleClass = WP_USER.isAdmin ? "role-admin" : WP_USER.isEditor ? "role-editor" : "role-viewer";
    badge.innerHTML =
      '<span class="user-name">' + esc(WP_USER.name) + '</span>' +
      '<span class="user-role ' + roleClass + '">' + role + '</span>' +
      '<a href="/wp-admin/">Dashboard</a>';
  } else {
    badge.innerHTML = '<a href="/wp-login.php?redirect_to=' + encodeURIComponent(location.href) + '">Log in</a>';
  }

  renderArtworks();
  renderWritings();
  renderExhibitions();
}

function populateSelect(id, values) {
  const sel = document.getElementById(id);
  for (const v of values) {
    const opt = document.createElement("option");
    opt.value = v;
    opt.textContent = v;
    sel.appendChild(opt);
  }
}

// --- Render Artworks (grouped by section with intros) ---
function renderArtworks() {
  const container = document.getElementById("artwork-sections");
  const sections = CATALOG.sections || [];
  let allArtworks = CATALOG.artworks;
  let totalShown = 0;

  if (filters.series) allArtworks = allArtworks.filter(a => a.series === filters.series);
  if (filters.search) allArtworks = allArtworks.filter(a => a.title.toLowerCase().includes(filters.search));

  let html = "";

  for (const sec of sections) {
    let artworks = allArtworks.filter(a => {
      if (sec.period) return a.period === sec.period;
      if (sec.tag) return a.tags && a.tags.includes(sec.tag);
      return false;
    });

    if (filters.section && filters.section !== sec.label) continue;
    if (artworks.length === 0 && !filters.section) continue;

    totalShown += artworks.length;

    // Section header
    html += '<div class="section-header">';
    html += '<h2>' + esc(sec.label) + '</h2>';
    html += '<div class="section-line"></div>';

    if (sec.intros && sec.intros.length > 0) {
      html += '<div class="section-intros">';
      for (const intro of sec.intros) {
        html += '<div class="intro-block">';
        if (intro.heading) {
          html += '<div class="intro-heading">' + esc(intro.heading) + '</div>';
        }
        const paras = intro.text.split("\\n\\n");
        for (const p of paras) {
          html += '<div class="intro-text">' + esc(p) + '</div>';
        }
        html += '</div>';
      }
      html += '</div>';
    }

    html += '</div>';

    // Artwork grid
    html += '<div class="section-grid">';
    html += artworks.map(a => renderCard(a)).join("");
    html += '</div>';
  }

  container.innerHTML = html;
  document.getElementById("artwork-count").textContent = totalShown + " works";
}

function renderCard(a) {
  const img = a.images[0];
  const thumbUrl = makeThumb(img.url);
  const yearStr = a.year ? String(a.year) + (a.yearEnd ? "\\u2013" + a.yearEnd : "") : "";
  const mediumStr = a.medium ? '<div class="medium">' + esc(a.medium) + '</div>' : "";
  const dimsStr = a.dimensions ? '<div class="dims">' + formatDims(a.dimensions) + '</div>' : "";
  const tagsHtml = a.series ? '<div class="tags"><span class="tag">' + esc(a.series) + '</span></div>' : "";
  const serialStr = a.serial ? '<div class="serial">' + esc(a.serial) + '</div>' : "";
  const imgCount = a.images.length > 1 ? '<span class="img-count">' + a.images.length + ' views</span>' : "";

  return '<div class="card" data-slug="' + a.slug + '" onclick="openLightbox(this)"' +
    ' data-full="' + esc(img.url) + '"' +
    ' data-title="' + esc(a.title) + '"' +
    ' data-serial="' + esc(a.serial || "") + '"' +
    ' data-detail="' + esc([yearStr, a.medium, a.dimensions ? formatDims(a.dimensions) : ""].filter(Boolean).join(" \\u00b7 ")) + '">' +
    '<div class="img-wrap"><img src="' + esc(thumbUrl) + '" alt="' + esc(a.title) + '" loading="lazy">' + imgCount + '</div>' +
    '<div class="info">' +
      '<h3><em>' + esc(a.title) + '</em></h3>' +
      (yearStr ? '<div class="year">' + yearStr + '</div>' : '') +
      mediumStr + dimsStr + tagsHtml + serialStr +
    '</div></div>';
}

function makeThumb(url) {
  // Use full-res URL directly — WP doesn't generate square thumbs
  return url;
}

function formatDims(d) {
  if (!d) return "";
  if (d.text) return d.text;
  let s = d.width + " × " + d.height;
  if (d.depth) s += " × " + d.depth;
  s += " " + d.unit + ".";
  return s;
}

function esc(s) {
  if (!s) return "";
  return String(s).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}

function openLightbox(card) {
  const lb = document.getElementById("lightbox");
  document.getElementById("lb-img").src = card.dataset.full;
  document.getElementById("lb-title").innerHTML = "<em>" + esc(card.dataset.title) + "</em>";
  const serial = card.dataset.serial ? ' <span style="font-size:0.75rem;color:#999;font-family:monospace">' + esc(card.dataset.serial) + '</span>' : '';
  document.getElementById("lb-detail").innerHTML = esc(card.dataset.detail) + serial;
  lb.classList.add("open");
}

// --- Render Writings ---
function renderWritings() {
  const list = document.getElementById("writings-list");
  let writings = CATALOG.writings;

  if (wFilters.category) writings = writings.filter(w => w.category === wFilters.category);
  if (wFilters.search) writings = writings.filter(w => w.title.toLowerCase().includes(wFilters.search));

  document.getElementById("writing-count").textContent = writings.length + " entries";

  list.innerHTML = writings.map(w => {
    const pub = [w.publication, w.issue].filter(Boolean).join(", ");
    return '<div class="writing-row">' +
      '<div class="w-title">' + esc(w.title) + '</div>' +
      '<div class="w-pub">' + esc(pub) + '</div>' +
      '<div class="w-year">' + (w.year || "") + '</div>' +
    '</div>';
  }).join("");
}

// --- Render Exhibitions ---
function renderExhibitions() {
  const grid = document.getElementById("exhibitions-grid");
  grid.innerHTML = CATALOG.exhibitions.map(e => {
    const imgs = e.images.slice(0, 6).map(i =>
      '<img src="' + esc(makeThumb(i.url)) + '" alt="" loading="lazy">'
    ).join("");
    return '<div class="exhibition-card">' +
      '<h3>' + esc(e.title) + '</h3>' +
      '<div class="e-year">' + (e.year || "") + '</div>' +
      (imgs ? '<div class="e-images">' + imgs + '</div>' : '') +
    '</div>';
  }).join("");
}

init();
</script>
</body>
</html>`;

writeFileSync(join(outDir, "index.php"), html);
console.log(`Built dist/index.php (${(html.length / 1024).toFixed(0)} KB)`);
