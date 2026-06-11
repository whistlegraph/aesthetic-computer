#!/usr/bin/env node
// pop/bin/audition-motion.mjs — the motion TAKES BOARD: a local page to
// audition every generated take of every section (current + archived),
// pick the keepers, re-roll duds, and assemble the final cut — all
// without re-spending on shots you already own.
//
// Generic across /pop lanes via the motion-pipeline conventions:
//   shots   pop/<lane>/out/motion/<slug>-shot-<i>-<name>.mp4
//   archive pop/<lane>/out/motion/archive/*.vN.mp4
//   picks   pop/<lane>/out/motion/takes.json
//   struct  pop/<lane>/out/<slug>.struct.json
//   audio   pop/<lane>/out/<slug>.mp3
//   driver  pop/<lane>/bin/gen-motion-<slug>.mjs
//
// usage:  node pop/bin/audition-motion.mjs --lane marimba --slug marimbaba
//         [--port 7878] [--no-open]

import { createServer } from "node:http";
import { readFileSync, writeFileSync, existsSync, readdirSync, statSync, createReadStream, mkdirSync } from "node:fs";
import { resolve, dirname, normalize } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const LANE = flags.lane, SLUG = flags.slug;
if (!LANE || !SLUG) { console.error("usage: audition-motion.mjs --lane <lane> --slug <slug>"); process.exit(1); }
const LANE_DIR = `${POP}/${LANE}`;
const OUT = `${LANE_DIR}/out`;
const MOTION = `${OUT}/motion`;
const TAKES_PATH = `${MOTION}/takes.json`;
const STRUCT = `${OUT}/${SLUG}.struct.json`;
const AUDIO = `${OUT}/${SLUG}.mp3`;
const DRIVER = `${LANE_DIR}/bin/gen-motion-${SLUG}.mjs`;
const FINAL = `${OUT}/${SLUG}-motion-yt.mp4`;
const PORT = Number(flags.port ?? 7878);
mkdirSync(`${MOTION}/archive`, { recursive: true });

const jobs = {}; // name|"assemble" → { startedAt, log }

function state() {
  const struct = JSON.parse(readFileSync(STRUCT, "utf8"));
  const takes = existsSync(TAKES_PATH) ? JSON.parse(readFileSync(TAKES_PATH, "utf8")) : {};
  const archived = existsSync(`${MOTION}/archive`) ? readdirSync(`${MOTION}/archive`) : [];
  const sections = struct.sections.map((s, i) => {
    const cur = `${SLUG}-shot-${i}-${s.name}.mp4`;
    const list = [];
    if (existsSync(`${MOTION}/${cur}`)) list.push({ file: cur, label: "current" });
    for (const f of archived.filter((f) => f.startsWith(`${SLUG}-shot-${i}-${s.name}.v`)).sort()) {
      list.push({ file: `archive/${f}`, label: f.match(/\.v(\d+)\.mp4$/)?.[1] ? `v${f.match(/\.v(\d+)\.mp4$/)[1]}` : f });
    }
    return {
      i, name: s.name,
      start: s.startSec, end: s.endSec, exact: s.endSec - s.startSec,
      takes: list, picked: takes[s.name] || (list[0]?.file ?? null),
      job: jobs[s.name] ? { running: jobs[s.name].running, log: jobs[s.name].log.slice(-600) } : null,
    };
  });
  return {
    slug: SLUG, lane: LANE, sections,
    final: existsSync(FINAL) ? { mtime: statSync(FINAL).mtimeMs } : null,
    assembling: jobs.assemble ? { running: jobs.assemble.running, log: jobs.assemble.log.slice(-600) } : null,
  };
}

function runJob(key, args) {
  if (jobs[key]?.running) return false;
  const job = { running: true, log: "" };
  jobs[key] = job;
  const child = spawn("node", [DRIVER, ...args], { cwd: POP + "/.." });
  const sink = (d) => { job.log += d.toString(); };
  child.stdout.on("data", sink); child.stderr.on("data", sink);
  child.on("exit", (code) => { job.running = false; job.log += `\n[exit ${code}]`; });
  return true;
}

function streamFile(req, res, path, type) {
  if (!existsSync(path)) { res.writeHead(404); res.end("not found"); return; }
  const size = statSync(path).size;
  const range = req.headers.range;
  if (range) {
    const m = range.match(/bytes=(\d+)-(\d*)/);
    const start = Number(m[1]);
    const end = m[2] ? Number(m[2]) : size - 1;
    res.writeHead(206, {
      "Content-Range": `bytes ${start}-${end}/${size}`,
      "Accept-Ranges": "bytes",
      "Content-Length": end - start + 1,
      "Content-Type": type,
    });
    createReadStream(path, { start, end }).pipe(res);
  } else {
    res.writeHead(200, { "Content-Length": size, "Content-Type": type, "Accept-Ranges": "bytes" });
    createReadStream(path).pipe(res);
  }
}

const PAGE = /* html */ `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>${SLUG} — motion takes</title>
<style>
  :root { --bg:#101418; --ink:#e8eef2; --accent:#ffd24a; --soft:#1a2128; --ok:#7ce38b; --bad:#ff6a6a; }
  * { box-sizing: border-box; }
  html, body { margin:0; background:var(--bg); color:var(--ink); font-family:-apple-system,"Helvetica Neue",sans-serif; }
  header { padding:14px 22px; border-bottom:2px solid var(--accent); display:flex; align-items:center; gap:18px; flex-wrap:wrap; }
  h1 { margin:0; font-size:22px; color:var(--accent); }
  h1 .small { font-size:13px; color:var(--ink); opacity:.55; font-weight:400; margin-left:10px; }
  button { background:var(--soft); border:1px solid var(--accent); color:var(--accent); padding:6px 14px; border-radius:999px; font:inherit; font-size:13px; cursor:pointer; }
  button:hover { background:var(--accent); color:#10100a; }
  button:disabled { opacity:.4; cursor:default; }
  main { padding:14px 22px 60px; }
  .section { margin-bottom:26px; background:var(--soft); border-radius:10px; padding:12px 14px; }
  .section h2 { margin:0 0 8px; font-size:15px; display:flex; align-items:baseline; gap:12px; }
  .section h2 .t { font-family:ui-monospace,Menlo,monospace; font-size:11px; opacity:.55; }
  .takes { display:flex; gap:10px; overflow-x:auto; padding-bottom:4px; }
  .take { flex:0 0 300px; background:#0b0f13; border:2px solid transparent; border-radius:8px; padding:6px; }
  .take.picked { border-color:var(--ok); }
  .take video { width:100%; border-radius:4px; display:block; background:#000; }
  .take .bar { display:flex; align-items:center; gap:8px; margin-top:5px; font-family:ui-monospace,Menlo,monospace; font-size:11px; }
  .take .bar .lbl { flex:1; opacity:.75; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
  .take .pick { font-size:11px; padding:3px 10px; }
  .joblog { font-family:ui-monospace,Menlo,monospace; font-size:10px; white-space:pre-wrap; color:#9ab; max-height:90px; overflow-y:auto; margin-top:6px; }
  .music { font-size:11px; padding:3px 10px; }
  #assemblebar { position:fixed; bottom:0; left:0; right:0; background:var(--soft); border-top:2px solid var(--accent); padding:10px 22px; display:flex; gap:14px; align-items:center; }
  #assemblebar .status { font-family:ui-monospace,Menlo,monospace; font-size:11px; opacity:.7; flex:1; }
</style>
</head>
<body>
<header>
  <h1>${SLUG} <span class="small">motion takes board — pick · re-roll · assemble (re-rolls cost money; picking is free)</span></h1>
  <audio id="ref" src="/audio" preload="auto"></audio>
</header>
<main id="board"></main>
<div id="assemblebar">
  <button id="assemble">⛭ assemble final cut from picks</button>
  <button id="watch">▶ open final in QuickTime</button>
  <span class="status" id="astatus"></span>
</div>
<script>
const ref = document.getElementById("ref");
let musicTimer = null;
function playSection(start, end) {
  clearTimeout(musicTimer);
  ref.currentTime = start; ref.play();
  musicTimer = setTimeout(() => ref.pause(), (end - start) * 1000);
}
async function api(path, body) {
  const r = await fetch(path, body ? { method:"POST", headers:{"Content-Type":"application/json"}, body: JSON.stringify(body) } : undefined);
  return r.json();
}
let prev = "";
async function render() {
  const st = await api("/state");
  const sig = JSON.stringify(st);
  if (sig === prev) return; prev = sig;
  const board = document.getElementById("board");
  board.innerHTML = "";
  for (const s of st.sections) {
    const div = document.createElement("div");
    div.className = "section";
    div.innerHTML = \`<h2>§\${s.i} \${s.name} <span class="t">\${s.start.toFixed(2)}–\${s.end.toFixed(2)}s (\${s.exact.toFixed(2)}s)</span>
      <button class="music" onclick="playSection(\${s.start},\${s.end})">♪ music</button>
      <button class="music reroll" data-name="\${s.name}" \${s.job?.running ? "disabled" : ""}>\${s.job?.running ? "⏳ rolling…" : "↻ re-roll ($)"}</button></h2>\`;
    const takes = document.createElement("div");
    takes.className = "takes";
    for (const t of s.takes) {
      const el = document.createElement("div");
      el.className = "take" + (t.file === s.picked ? " picked" : "");
      el.innerHTML = \`<video src="/v/\${encodeURIComponent(t.file)}" controls loop muted preload="metadata"></video>
        <div class="bar"><span class="lbl">\${t.label}</span>
        <button class="pick" data-name="\${s.name}" data-file="\${t.file}">\${t.file === s.picked ? "✓ picked" : "pick"}</button></div>\`;
      takes.appendChild(el);
    }
    if (!s.takes.length) takes.innerHTML = '<em style="opacity:.5;font-size:12px">no takes yet — re-roll to generate</em>';
    div.appendChild(takes);
    if (s.job?.log) { const lg = document.createElement("div"); lg.className = "joblog"; lg.textContent = s.job.log; div.appendChild(lg); }
    board.appendChild(div);
  }
  const a = document.getElementById("astatus");
  a.textContent = st.assembling?.running ? "assembling… " + st.assembling.log.split("\\n").pop()
    : st.final ? "final cut: " + new Date(st.final.mtime).toLocaleTimeString() : "no final cut yet";
  document.getElementById("assemble").disabled = !!st.assembling?.running;
}
document.addEventListener("click", async (e) => {
  if (e.target.classList.contains("pick")) { await api("/pick", { name: e.target.dataset.name, file: e.target.dataset.file }); prev=""; render(); }
  if (e.target.classList.contains("reroll")) {
    if (!confirm("Re-roll " + e.target.dataset.name + "? This costs money (current take is archived, never lost).")) return;
    await api("/reroll", { name: e.target.dataset.name }); prev=""; render();
  }
});
document.getElementById("assemble").onclick = async () => { await api("/assemble", {}); prev=""; render(); };
document.getElementById("watch").onclick = () => api("/open", {});
render(); setInterval(render, 2500);
</script>
</body>
</html>`;

const server = createServer(async (req, res) => {
  const url = new URL(req.url, `http://localhost:${PORT}`);
  const json = (obj) => { res.writeHead(200, { "Content-Type": "application/json" }); res.end(JSON.stringify(obj)); };
  const body = async () => JSON.parse(await new Promise((r) => { let b = ""; req.on("data", (d) => b += d); req.on("end", () => r(b || "{}")); }));

  try {
    if (url.pathname === "/") { res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" }); res.end(PAGE); }
    else if (url.pathname === "/state") json(state());
    else if (url.pathname === "/audio") streamFile(req, res, AUDIO, "audio/mpeg");
    else if (url.pathname.startsWith("/v/")) {
      const rel = normalize(decodeURIComponent(url.pathname.slice(3)));
      if (rel.includes("..")) { res.writeHead(403); res.end(); return; }
      streamFile(req, res, `${MOTION}/${rel}`, "video/mp4");
    }
    else if (url.pathname === "/pick" && req.method === "POST") {
      const { name, file } = await body();
      const takes = existsSync(TAKES_PATH) ? JSON.parse(readFileSync(TAKES_PATH, "utf8")) : {};
      takes[name] = file;
      writeFileSync(TAKES_PATH, JSON.stringify(takes, null, 2));
      json({ ok: true });
    }
    else if (url.pathname === "/reroll" && req.method === "POST") {
      const { name } = await body();
      json({ ok: runJob(name, ["--only", name, "--force"]) });
    }
    else if (url.pathname === "/assemble" && req.method === "POST") {
      json({ ok: runJob("assemble", ["--assemble"]) });
    }
    else if (url.pathname === "/open" && req.method === "POST") {
      spawn("open", ["-a", "QuickTime Player", FINAL]);
      json({ ok: true });
    }
    else { res.writeHead(404); res.end("not found"); }
  } catch (e) {
    res.writeHead(500); res.end(String(e?.message || e));
  }
});

server.listen(PORT, () => {
  const addr = `http://localhost:${PORT}`;
  console.log(`▸ ${SLUG} motion takes board → ${addr}`);
  if (!flags["no-open"]) spawn("open", [addr]);
});
