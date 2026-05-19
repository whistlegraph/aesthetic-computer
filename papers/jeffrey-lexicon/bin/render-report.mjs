#!/usr/bin/env node
// render-report.mjs — render the jeffrey-lexicon HTML dashboard from
// dictionary.json. Writes to the lith public dir (canonical URL) and
// optionally copies to ~/Desktop.

import { readFile, writeFile, mkdir, copyFile, stat } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { homedir } from "node:os";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const REPO = join(ROOT, "..", "..");

const PUBLIC_DIR = join(REPO, "system/public/papers.aesthetic.computer/platter/jeffrey-lexicon");
const DESKTOP = join(homedir(), "Desktop");

const NEOLOGISMS = [
  "kidlisp", "whistlegraph", "notepat", "platter", "platterization",
  "aestheticants", "aesthetic", "computer", "oven", "menuband", "slab",
  "nopaint", "recap", "sotce", "compush", "honeydo", "bios", "disk",
  "disks", "piece", "pieces", "prompt", "fedac",
];

const ESCAPE = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" };
const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) => ESCAPE[c]);

function fmt(n) {
  return n.toLocaleString("en-US");
}

function pickTop(dict, n, predicate) {
  const out = [];
  for (const [k, v] of Object.entries(dict)) {
    if (predicate && !predicate(k, v)) continue;
    out.push([k, v]);
    if (out.length > n * 4) {
      out.sort((a, b) => b[1].count - a[1].count);
      out.length = n;
    }
  }
  out.sort((a, b) => b[1].count - a[1].count);
  return out.slice(0, n);
}

function isWritten(v) {
  return (v.by_source.commits ?? 0) + (v.by_source.comments ?? 0) > 0;
}
function isSpoken(v) {
  return (v.by_source.lectures ?? 0) + (v.by_source.recap_audio ?? 0) > 0;
}

function row(rank, [k, v]) {
  const src = v.by_source;
  const c = src.commits ?? 0;
  const co = src.comments ?? 0;
  const l = src.lectures ?? 0;
  const r = src.recap_audio ?? 0;
  const total = v.count;
  const bar = (n, klass) =>
    n > 0
      ? `<span class="bar ${klass}" style="width:${(n / total) * 100}%" title="${klass}: ${n}"></span>`
      : "";
  const firstRef = v.first_seen?.ref ?? "";
  const firstDate = (v.first_seen?.iso ?? "").slice(0, 10);
  return `<tr>
    <td class="rank">${rank}</td>
    <td class="word">${esc(k)}</td>
    <td class="count">${fmt(total)}</td>
    <td class="srcbar">${bar(c, "commits")}${bar(co, "comments")}${bar(l, "lectures")}${bar(r, "recap_audio")}</td>
    <td class="src">${c ? `c·${fmt(c)}` : ""} ${co ? `co·${fmt(co)}` : ""} ${l ? `l·${fmt(l)}` : ""} ${r ? `r·${fmt(r)}` : ""}</td>
    <td class="first" title="${esc(firstRef)}">${firstDate}</td>
  </tr>`;
}

function table(rows) {
  return `<table>
    <thead><tr><th>#</th><th>word</th><th>count</th><th>by source</th><th></th><th>first seen</th></tr></thead>
    <tbody>${rows.map((r, i) => row(i + 1, r)).join("")}</tbody>
  </table>`;
}

async function main() {
  const dict = JSON.parse(await readFile(join(ROOT, "dictionary.json"), "utf8"));
  const tokens = Object.entries(dict);
  const total = tokens.reduce((a, [, v]) => a + v.count, 0);

  const sourceTotals = { commits: 0, comments: 0, lectures: 0, recap_audio: 0 };
  for (const [, v] of tokens) {
    for (const [s, n] of Object.entries(v.by_source)) {
      sourceTotals[s] = (sourceTotals[s] ?? 0) + n;
    }
  }

  const top200 = pickTop(dict, 200);
  const neologisms = NEOLOGISMS.map((n) => [n, dict[n]]).filter(([, v]) => v).sort((a, b) => b[1].count - a[1].count);
  const spokenOnly = pickTop(dict, 100, (_k, v) => isSpoken(v) && !isWritten(v));
  const writtenOnly = pickTop(dict, 100, (_k, v) => isWritten(v) && !isSpoken(v) && v.count >= 50);
  const oldest = tokens
    .filter(([, v]) => v.first_seen?.iso)
    .sort((a, b) => a[1].first_seen.iso.localeCompare(b[1].first_seen.iso))
    .slice(0, 50);

  const buildIso = new Date().toISOString();
  const buildDate = buildIso.slice(0, 10);

  const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>jeffrey-lexicon · platter</title>
<meta name="description" content="A frequency-attributed dictionary of words used by Jeffrey Alan Scudder, sourced only from first-hand textual + transcribed material across Aesthetic Computer.">
<link rel="icon" href="https://aesthetic.computer/icon/128x128/prompt.png" type="image/png">
<link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
<style>
:root {
  color-scheme: dark light;
  --bg:#111; --bg2:#1a1a1a; --fg:#ddd; --fg2:#888; --fg3:#555;
  --accent:#ff6b9d; --accent2:#4ecdc4; --gold:#ffd93d;
  --border:#333; --hover:#222;
  --src-commits:#4ecdc4; --src-comments:#ffd93d; --src-lectures:#ff6b9d; --src-recap_audio:#a78bfa;
}
@media (prefers-color-scheme: light) {
  :root {
    --bg:#fafafa; --bg2:#fff; --fg:#222; --fg2:#666; --fg3:#999;
    --accent:#d63384; --accent2:#0a8a82; --gold:#b8860b;
    --border:#ddd; --hover:#f0f0f0;
    --src-commits:#0a8a82; --src-comments:#b8860b; --src-lectures:#d63384; --src-recap_audio:#7c3aed;
  }
}
*{margin:0;padding:0;box-sizing:border-box}
html,body{background:var(--bg);color:var(--fg)}
body{font-family:'Berkeley Mono Variable',ui-monospace,monospace;font-size:13px;line-height:1.45}
a{color:var(--accent2);text-decoration:none}
a:hover{color:var(--accent);text-decoration:underline}
header{padding:18px 16px 14px;border-bottom:1px solid var(--border)}
h1{font-size:20px;font-weight:normal;letter-spacing:3px;margin-bottom:6px}
h1 .accent{color:var(--accent)}
.sub{font-size:11px;color:var(--fg2);margin-bottom:8px;max-width:780px}
.crumbs{font-size:11px;color:var(--fg3);margin-bottom:10px}
.crumbs a{color:var(--fg2)}
.stats{display:flex;gap:14px;flex-wrap:wrap;font-size:11px;color:var(--fg2);font-variant-numeric:tabular-nums}
.stats b{color:var(--fg);font-weight:normal}
.stats .pill{padding:2px 6px;border:1px solid var(--border);border-radius:0}
main{padding:14px 16px 60px;max-width:1200px}
section{margin:28px 0}
section h2{font-size:14px;font-weight:normal;letter-spacing:2px;color:var(--accent);margin-bottom:6px}
section p.note{font-size:11px;color:var(--fg2);margin-bottom:10px;max-width:780px}
table{border-collapse:collapse;width:100%;font-size:12px;font-variant-numeric:tabular-nums}
th{text-align:left;padding:4px 8px;border-bottom:1px solid var(--border);color:var(--fg2);font-weight:normal;font-size:10px;letter-spacing:1px;text-transform:uppercase}
td{padding:3px 8px;border-bottom:1px solid var(--bg2);vertical-align:middle}
tr:hover td{background:var(--hover)}
.rank{color:var(--fg3);width:36px}
.word{color:var(--fg);font-weight:bold}
.count{color:var(--fg);width:60px;text-align:right}
.src{color:var(--fg2);font-size:10px;white-space:nowrap}
.first{color:var(--fg3);font-size:10px;width:90px}
.srcbar{width:160px;height:8px;background:var(--bg2);position:relative}
.srcbar .bar{display:inline-block;height:8px;vertical-align:top}
.bar.commits{background:var(--src-commits)}
.bar.comments{background:var(--src-comments)}
.bar.lectures{background:var(--src-lectures)}
.bar.recap_audio{background:var(--src-recap_audio)}
.legend{display:flex;gap:14px;font-size:10px;color:var(--fg2);margin:6px 0 10px}
.legend span{display:inline-flex;align-items:center;gap:5px}
.legend i{display:inline-block;width:10px;height:10px;font-style:normal}
.legend .commits i{background:var(--src-commits)}
.legend .comments i{background:var(--src-comments)}
.legend .lectures i{background:var(--src-lectures)}
.legend .recap_audio i{background:var(--src-recap_audio)}
.search{margin:14px 0;display:flex;gap:8px;align-items:center}
.search input{font-family:inherit;font-size:13px;padding:6px 10px;background:var(--bg2);color:var(--fg);border:1px solid var(--border);outline:none;flex:1;max-width:360px}
.search input:focus{border-color:var(--accent2)}
.search .hint{font-size:10px;color:var(--fg3)}
#search-results{margin-top:10px}
.col2{display:grid;grid-template-columns:1fr 1fr;gap:24px}
@media(max-width:780px){.col2{grid-template-columns:1fr}}
footer{padding:24px 16px;border-top:1px solid var(--border);font-size:11px;color:var(--fg2);margin-top:32px}
footer p{margin-bottom:6px;max-width:780px}
.tag{display:inline-block;padding:1px 5px;font-size:10px;color:var(--fg2);border:1px solid var(--border);margin-right:4px}
</style>
</head>
<body>
<header>
  <div class="crumbs"><a href="/">papers</a> &middot; <a href="/platter/">platter</a> &middot; jeffrey-lexicon</div>
  <h1>jeffrey-<span class="accent">lexicon</span></h1>
  <p class="sub">A frequency-attributed dictionary of words used by Jeffrey Alan Scudder, sourced only from first-hand textual and transcribed material. Textual analogue of the <a href="/platter/jeffrey/">jeffrey-platter</a> photo index and the <code>jeffrey-pvc</code> voice clone.</p>
  <div class="stats">
    <span class="pill"><b>${fmt(tokens.length)}</b> unique tokens</span>
    <span class="pill"><b>${fmt(total)}</b> total occurrences</span>
    <span class="pill">commits <b>${fmt(sourceTotals.commits)}</b></span>
    <span class="pill">comments <b>${fmt(sourceTotals.comments)}</b></span>
    <span class="pill">lectures <b>${fmt(sourceTotals.lectures)}</b></span>
    <span class="pill">recap_audio <b>${fmt(sourceTotals.recap_audio)}</b></span>
    <span class="pill">built <b>${buildDate}</b></span>
    <span class="pill"><a href="dictionary.json">dictionary.json</a></span>
  </div>
</header>

<main>

<div class="legend">
  <span class="commits"><i></i>commits</span>
  <span class="comments"><i></i>comments</span>
  <span class="lectures"><i></i>lectures</span>
  <span class="recap_audio"><i></i>recap_audio</span>
</div>

<section>
  <h2>search</h2>
  <p class="note">Substring search across all ${fmt(tokens.length)} tokens. Loads <code>dictionary.json</code> on first keystroke.</p>
  <div class="search">
    <input id="q" placeholder="type a word fragment…" autocomplete="off">
    <span class="hint" id="hint">type to search</span>
  </div>
  <div id="search-results"></div>
</section>

<section>
  <h2>AC-coined / neologism candidates</h2>
  <p class="note">Words first introduced inside the AC project — names, subsystems, conventions. Counts and provenance below.</p>
  ${table(neologisms)}
</section>

<div class="col2">
  <section>
    <h2>top 200 overall</h2>
    <p class="note">Most frequent tokens across all four sources. Stop-words intact — voice is in the texture, not just the long tail.</p>
    ${table(top200)}
  </section>
  <section>
    <h2>spoken-only — top 100</h2>
    <p class="note">Tokens that appear in lectures or recap audio but never in commits or code comments. Jeffrey's vocabulary that doesn't make it onto the keyboard.</p>
    ${table(spokenOnly)}
  </section>
</div>

<div class="col2">
  <section>
    <h2>written-only — top 100 (count ≥ 50)</h2>
    <p class="note">Tokens with significant frequency in commits/comments but never spoken in lectures or recap. Code-and-commit register.</p>
    ${table(writtenOnly)}
  </section>
  <section>
    <h2>earliest 50 — by first commit</h2>
    <p class="note">Tokens whose first appearance in the corpus has the oldest timestamp. The vocabulary jeffrey started with.</p>
    ${table(oldest)}
  </section>
</div>

</main>

<footer>
  <p><b>Provenance.</b> Every token in this lexicon traces to a first-hand jeffrey source: a commit message authored by jeffrey, a code comment authored by jeffrey (verified via <code>git blame</code>), a lecture VTT, or a recap audio segment whose script came from jeffrey's commits. AI-generated text is excluded by source filter — see <a href="https://github.com/whistlegraph/aesthetic-computer/tree/main/papers/jeffrey-lexicon">papers/jeffrey-lexicon/README.md</a>.</p>
  <p><b>Source breakdown.</b> ${fmt(tokens.length)} unique tokens across <b>${fmt(sourceTotals.commits)}</b> commit-message words, <b>${fmt(sourceTotals.comments)}</b> code-comment words, <b>${fmt(sourceTotals.lectures)}</b> lecture-transcript words, <b>${fmt(sourceTotals.recap_audio)}</b> recap-audio words. Built ${buildIso}.</p>
  <p><b>Phases pending.</b> arXiv papers (LaTeX-stripped), CV body, repo planning files (TODO/schedule/honeydo), IG captions from <code>@whistlegraph</code>, vault email (sent-only), and a handwriting-OCR pass — see the README roadmap.</p>
  <p>Sibling indices: <a href="/platter/jeffrey/">jeffrey-platter</a> (photos) &middot; <a href="/platter/">platter root</a> &middot; <a href="https://github.com/whistlegraph/aesthetic-computer/tree/main/papers/jeffrey-lexicon">source on github</a></p>
</footer>

<script>
const q = document.getElementById('q');
const out = document.getElementById('search-results');
const hint = document.getElementById('hint');
let DICT = null;
let loadingPromise = null;
async function loadDict() {
  if (DICT) return DICT;
  if (loadingPromise) return loadingPromise;
  hint.textContent = 'loading dictionary.json…';
  loadingPromise = fetch('dictionary.json').then(r => r.json()).then(d => { DICT = d; hint.textContent = 'ready'; return d; });
  return loadingPromise;
}
function fmt(n){return n.toLocaleString('en-US')}
function escape(s){return String(s).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'})[c])}
function rowHTML(rank, k, v){
  const s = v.by_source||{};
  const c=s.commits||0,co=s.comments||0,l=s.lectures||0,r=s.recap_audio||0;
  const t=v.count;
  const bar = (n,k) => n>0 ? '<span class="bar '+k+'" style="width:'+((n/t)*100)+'%"></span>' : '';
  const fr = (v.first_seen||{}).ref || '';
  const fd = ((v.first_seen||{}).iso || '').slice(0,10);
  return '<tr><td class="rank">'+rank+'</td><td class="word">'+escape(k)+'</td><td class="count">'+fmt(t)+'</td><td class="srcbar">'+bar(c,'commits')+bar(co,'comments')+bar(l,'lectures')+bar(r,'recap_audio')+'</td><td class="src">'+(c?'c·'+fmt(c):'')+' '+(co?'co·'+fmt(co):'')+' '+(l?'l·'+fmt(l):'')+' '+(r?'r·'+fmt(r):'')+'</td><td class="first" title="'+escape(fr)+'">'+fd+'</td></tr>';
}
let timer=null;
q.addEventListener('input', () => {
  clearTimeout(timer);
  timer = setTimeout(async () => {
    const term = q.value.trim().toLowerCase();
    if (!term) { out.innerHTML=''; return; }
    const dict = await loadDict();
    const matches = [];
    for (const k of Object.keys(dict)) {
      if (k.includes(term)) matches.push([k, dict[k]]);
      if (matches.length > 500) break;
    }
    matches.sort((a,b) => b[1].count - a[1].count);
    const top = matches.slice(0, 200);
    if (!top.length) { out.innerHTML = '<p class="note">no matches</p>'; return; }
    hint.textContent = matches.length > 500 ? '500+ matches (showing top 200)' : fmt(matches.length)+' matches';
    out.innerHTML = '<table><thead><tr><th>#</th><th>word</th><th>count</th><th>by source</th><th></th><th>first seen</th></tr></thead><tbody>'+top.map((m,i)=>rowHTML(i+1,m[0],m[1])).join('')+'</tbody></table>';
  }, 120);
});
</script>
</body>
</html>`;

  await mkdir(PUBLIC_DIR, { recursive: true });
  await writeFile(join(PUBLIC_DIR, "index.html"), html);
  await copyFile(join(ROOT, "dictionary.json"), join(PUBLIC_DIR, "dictionary.json"));
  const desktopPath = join(DESKTOP, "jeffrey-lexicon-report.html");
  await writeFile(desktopPath, html);

  const idxSize = (await stat(join(PUBLIC_DIR, "index.html"))).size;
  const dictSize = (await stat(join(PUBLIC_DIR, "dictionary.json"))).size;
  console.log(`[done] wrote:`);
  console.log(`       ${join(PUBLIC_DIR, "index.html")} (${(idxSize / 1024).toFixed(1)} KB)`);
  console.log(`       ${join(PUBLIC_DIR, "dictionary.json")} (${(dictSize / 1024 / 1024).toFixed(1)} MB)`);
  console.log(`       ${desktopPath}`);
  console.log(`[url]  https://papers.aesthetic.computer/platter/jeffrey-lexicon/`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
