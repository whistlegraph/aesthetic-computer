#!/usr/bin/env node
// build catalog.json — every utterance + variation, with duration + relative path.
// also emits audition.html (no deps; pure static page) for browsing/auditioning
// and prototyping syllable strings.

import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { basename, dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = dirname(here);

function durSeconds(file) {
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", file], { encoding: "utf8" });
  if (r.status !== 0) return null;
  return +r.stdout.trim();
}

function walk(dir) {
  const out = [];
  if (!existsSync(dir)) return out;
  for (const f of readdirSync(dir)) {
    const p = join(dir, f);
    const s = statSync(p);
    if (s.isDirectory()) out.push(...walk(p));
    else if (f.endsWith(".wav")) out.push(p);
  }
  return out;
}

function classify(absPath) {
  const rel = relative(root, absPath);
  const parts = rel.split("/");
  if (parts[0] === "utterances") {
    return { kind: "utterance", group: parts[1], variation: null, name: basename(parts[2], ".wav") };
  }
  if (parts[0] === "variations") {
    return { kind: "variation", group: parts[1], name: parts[2], variation: basename(parts[3], ".wav") };
  }
  return null;
}

function build() {
  const files = [
    ...walk(join(root, "utterances")),
    ...walk(join(root, "variations")),
  ];
  const entries = [];
  for (const abs of files) {
    const meta = classify(abs);
    if (!meta) continue;
    const dur = durSeconds(abs);
    entries.push({
      path: relative(root, abs),
      group: meta.group,
      name: meta.name,
      variation: meta.variation,
      kind: meta.kind,
      dur,
    });
  }
  entries.sort((a, b) => a.path.localeCompare(b.path));
  return entries;
}

function emit(entries) {
  writeFileSync(join(root, "catalog.json"), JSON.stringify(entries, null, 2));
  console.log(`catalog: ${entries.length} entries → catalog.json`);
}

function emitHtml(entries) {
  const html = `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>americomputadora — audition</title>
<style>
  :root { --bg:#fff3f9; --ink:#1a1a1a; --accent:#ff3e7f; --soft:#fce4ee; --code:#e91e63; }
  * { box-sizing: border-box; }
  html, body { margin: 0; background: var(--bg); color: var(--ink); font-family: -apple-system, "Helvetica Neue", sans-serif; }
  header { padding: 18px 24px; border-bottom: 2px solid var(--accent); display: flex; align-items: baseline; gap: 24px; flex-wrap: wrap; }
  h1 { margin: 0; font-size: 28px; color: var(--accent); letter-spacing: -0.5px; }
  h1 span.small { color: var(--ink); font-weight: 400; font-size: 14px; margin-left: 12px; opacity: 0.6; }
  .filters { display: flex; gap: 8px; align-items: center; flex-wrap: wrap; }
  .filters button { background: white; border: 1px solid var(--accent); color: var(--accent); padding: 6px 14px; border-radius: 999px; font-family: inherit; font-size: 14px; cursor: pointer; }
  .filters button.active { background: var(--accent); color: white; }
  main { display: grid; grid-template-columns: 1fr 380px; gap: 0; min-height: calc(100vh - 70px); }
  .pool { padding: 16px 24px; overflow-y: auto; max-height: calc(100vh - 70px); }
  .arrangement { background: var(--soft); padding: 16px 24px; border-left: 2px solid var(--accent); overflow-y: auto; max-height: calc(100vh - 70px); }
  .arrangement h2 { margin: 0 0 12px 0; font-size: 16px; color: var(--accent); }
  .slot { background: white; border: 1px solid #eee; border-radius: 8px; padding: 10px; margin-bottom: 8px; display: flex; gap: 8px; align-items: center; }
  .slot button { background: none; border: none; cursor: pointer; font-size: 16px; }
  .slot .lbl { flex: 1; font-family: ui-monospace, Menlo, monospace; font-size: 12px; }
  .slot .dur { color: #999; font-size: 11px; }
  .group { margin-bottom: 24px; }
  .group h3 { color: var(--accent); margin: 12px 0 8px 0; font-size: 14px; text-transform: uppercase; letter-spacing: 1px; }
  .row { display: grid; grid-template-columns: repeat(auto-fill, minmax(170px, 1fr)); gap: 6px; }
  .clip { background: white; border: 1px solid #eee; border-radius: 6px; padding: 6px 8px; font-size: 12px; font-family: ui-monospace, Menlo, monospace; display: flex; gap: 6px; align-items: center; cursor: pointer; transition: background 0.1s; }
  .clip:hover { background: var(--soft); border-color: var(--accent); }
  .clip .play { font-size: 14px; color: var(--accent); }
  .clip .name { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .clip .dur { color: #aaa; font-size: 10px; }
  .clip.queued { background: var(--soft); border-color: var(--accent); }
  .tx { font-family: ui-monospace, Menlo, monospace; font-size: 12px; padding: 6px 10px; border: 1px solid var(--accent); border-radius: 6px; background: white; color: var(--code); cursor: pointer; }
  .tx:hover { background: var(--accent); color: white; }
  .arrangement-controls { display: flex; gap: 8px; margin-bottom: 12px; flex-wrap: wrap; }
  .empty { color: #999; font-style: italic; }
</style>
</head>
<body>
<header>
  <h1>americomputadora <span class="small">audition pool · ${entries.length} clips</span></h1>
  <div class="filters" id="filters">
    <button data-group="all" class="active">all</button>
    <button data-group="america">america</button>
    <button data-group="computer">computer</button>
    <button data-group="dora">dora</button>
    <label style="font-size:13px; color:#888; margin-left: 12px;"><input type="checkbox" id="hide-variations"> hide variations</label>
  </div>
</header>
<main>
  <section class="pool" id="pool"></section>
  <aside class="arrangement">
    <h2>arrangement</h2>
    <div class="arrangement-controls">
      <button class="tx" id="play-all">▶ play in sequence</button>
      <button class="tx" id="clear">clear</button>
      <button class="tx" id="export">export json</button>
    </div>
    <div id="slots"></div>
    <p style="font-size:11px; color:#999; margin-top: 20px;">click a clip below to add to arrangement. click the arrangement entry to play / drag the ✕ to remove.</p>
  </aside>
</main>
<script>
const ENTRIES = ${JSON.stringify(entries)};
const pool = document.getElementById("pool");
const slotsEl = document.getElementById("slots");
let filter = "all";
let hideVar = false;
let arrangement = [];

function fmtDur(d) { return d ? d.toFixed(2) + "s" : "—"; }
function audioFor(path) { const a = new Audio(path); a.preload = "none"; return a; }

function renderPool() {
  const groups = ["america", "computer", "dora"];
  pool.innerHTML = "";
  for (const g of groups) {
    if (filter !== "all" && filter !== g) continue;
    let items = ENTRIES.filter((e) => e.group === g);
    if (hideVar) items = items.filter((e) => e.kind === "utterance");
    if (!items.length) continue;
    const section = document.createElement("div");
    section.className = "group";
    section.innerHTML = "<h3>" + g + " · " + items.length + "</h3>";
    const row = document.createElement("div");
    row.className = "row";
    for (const e of items) {
      const c = document.createElement("div");
      c.className = "clip";
      const label = e.kind === "variation" ? e.name + "/" + e.variation : e.name;
      c.innerHTML = '<span class="play">▶</span><span class="name" title="' + label + '">' + label + '</span><span class="dur">' + fmtDur(e.dur) + '</span>';
      c.addEventListener("click", (ev) => {
        if (ev.shiftKey) {
          arrangement.push(e);
          renderArrangement();
          c.classList.add("queued");
        } else {
          new Audio(e.path).play();
        }
      });
      c.addEventListener("dblclick", () => { arrangement.push(e); renderArrangement(); c.classList.add("queued"); });
      row.appendChild(c);
    }
    section.appendChild(row);
    pool.appendChild(section);
  }
}

function renderArrangement() {
  slotsEl.innerHTML = "";
  if (!arrangement.length) { slotsEl.innerHTML = '<p class="empty">empty — double-click a clip or shift-click to add</p>'; return; }
  arrangement.forEach((e, i) => {
    const s = document.createElement("div");
    s.className = "slot";
    const label = e.kind === "variation" ? e.group + "/" + e.name + "/" + e.variation : e.group + "/" + e.name;
    s.innerHTML = '<button title="play">▶</button><span class="lbl">' + (i + 1) + ". " + label + '</span><span class="dur">' + fmtDur(e.dur) + '</span><button title="remove">✕</button>';
    const [play, , rm] = s.querySelectorAll("button");
    s.querySelector("span.lbl").addEventListener("click", () => new Audio(e.path).play());
    play.addEventListener("click", () => new Audio(e.path).play());
    rm.addEventListener("click", () => { arrangement.splice(i, 1); renderArrangement(); });
    slotsEl.appendChild(s);
  });
}

async function playAll() {
  for (const e of arrangement) {
    await new Promise((res) => {
      const a = new Audio(e.path);
      a.addEventListener("ended", res);
      a.addEventListener("error", res);
      a.play();
    });
  }
}

document.getElementById("filters").addEventListener("click", (e) => {
  if (e.target.tagName !== "BUTTON") return;
  document.querySelectorAll("#filters button").forEach((b) => b.classList.remove("active"));
  e.target.classList.add("active");
  filter = e.target.dataset.group;
  renderPool();
});
document.getElementById("hide-variations").addEventListener("change", (e) => { hideVar = e.target.checked; renderPool(); });
document.getElementById("play-all").addEventListener("click", playAll);
document.getElementById("clear").addEventListener("click", () => { arrangement = []; renderArrangement(); document.querySelectorAll(".clip.queued").forEach((c) => c.classList.remove("queued")); });
document.getElementById("export").addEventListener("click", () => {
  const blob = new Blob([JSON.stringify(arrangement.map((e, i) => ({ idx: i, group: e.group, name: e.name, variation: e.variation, path: e.path })), null, 2)], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url; a.download = "arrangement.json"; a.click();
});

renderPool();
renderArrangement();
</script>
</body>
</html>`;
  writeFileSync(join(root, "audition.html"), html);
  console.log(`audition: → audition.html`);
}

function main() {
  const entries = build();
  emit(entries);
  emitHtml(entries);
  // summary
  const counts = {};
  for (const e of entries) {
    counts[e.group] = (counts[e.group] || 0) + 1;
  }
  console.log("by group:", counts);
}

main();
