#!/usr/bin/env node
// analysis-overlay.mjs — a realtime "analysis layer" viewer for slab.
//
// PROTOTYPE (uncommitted). It is the visual companion to `frame` (OBSERVE:
// pixels + OCR + AX) and `puppet` (ACT: trusted stroke/gesture/key): it draws,
// live over the captured screen, WHERE the machine "sees" text (OCR boxes),
// WHERE it can act (AX click targets), and WHERE it just clicked (hit-scan
// pulses) — the same analysis a driving agent reasons over, made glanceable.
//
// It serves an SVG overlay composited on top of the existing JPEG frame
// streams, so it touches NEITHER the puppet daemon NOR the frame server —
// it is a pure reader of files those tools already write:
//
//   ~/.local/share/slab/frames/<machine>.jpg         (frame --preview / loop)
//   ~/.local/share/slab/state/frame.out.json         (latest local envelope)
//   ~/.local/share/slab/frames/<machine>.frame.json  (optional per-machine envelope, see --emit)
//   ~/.local/share/slab/analysis/<machine>.hits.jsonl (click pulses; see hit logger below)
//
// Routes:
//   GET /                      dark grid, one analysis pane per machine
//   GET /m/<machine>           full-screen analysis pane for one machine
//   GET /frame/<machine>.jpg   the raw JPEG (no-store)
//   GET /data/<machine>.json   { screen, ocr[], ax[], hits[] } for the overlay
//
// The overlay (OCR boxes / AX targets / hit pulses) toggles per-layer in the
// page with the keys O / A / H, or the on-page checkboxes. Boxes are drawn in
// an <svg> scaled from screen-points to the displayed JPEG, so they track the
// image at any window size.
//
//   node slab/bin/analysis-overlay.mjs [port]     # default 7778 (puppet-view owns 7777)
//   open http://localhost:7778
//
// PUBLIC repo: no machine names here — they come from the frames on disk.

import { readFileSync, readdirSync, existsSync, statSync } from "node:fs";
import http from "node:http";
import { homedir } from "node:os";
import { join } from "node:path";

const HOME = homedir();
const SLAB = process.env.SLAB_HOME || join(HOME, ".local", "share", "slab");
const FRAMES_DIR = join(SLAB, "frames");
const ANALYSIS_DIR = join(SLAB, "analysis");
const LOCAL_ENVELOPE = join(SLAB, "state", "frame.out.json");
const PORT = Number(process.argv[2] || 7778);

// Machine names = the <machine>.jpg files frame --preview leaves on disk.
function machineNames() {
  if (!existsSync(FRAMES_DIR)) return [];
  return readdirSync(FRAMES_DIR)
    .filter((f) => f.endsWith(".jpg") && !f.endsWith(".overlay.jpg"))
    .map((f) => f.slice(0, -4));
}

function readJSON(path) {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

// Find the freshest envelope for a machine. A per-machine sidecar
// (<machine>.frame.json) wins if present (write it with `frame <m> --json >
// …`, or via the --emit helper); otherwise fall back to the single local
// state envelope (only meaningful for the controller capturing itself).
function envelopeFor(machine) {
  const sidecar = join(FRAMES_DIR, `${machine}.frame.json`);
  if (existsSync(sidecar)) return readJSON(sidecar);
  if (existsSync(LOCAL_ENVELOPE)) return readJSON(LOCAL_ENVELOPE);
  return null;
}

// Hit pulses: a JSON-lines log, one {x,y,kind,label,t} per click/stroke point.
// Tail the last N within a recency window so the overlay shows recent activity.
function hitsFor(machine, { windowMs = 8000, max = 200 } = {}) {
  const path = join(ANALYSIS_DIR, `${machine}.hits.jsonl`);
  if (!existsSync(path)) return [];
  let lines;
  try {
    lines = readFileSync(path, "utf8").trim().split("\n").slice(-max);
  } catch {
    return [];
  }
  const now = Date.now();
  const out = [];
  for (const ln of lines) {
    if (!ln.trim()) continue;
    try {
      const h = JSON.parse(ln);
      if (typeof h.t === "number" && now - h.t > windowMs) continue;
      out.push(h);
    } catch {}
  }
  return out;
}

// The overlay payload: screen geometry + the boxes the agent reasons over.
// OCR + AX rects are [x,y,w,h] in LOGICAL screen points (top-left origin) —
// exactly what FrameCapture.swift emits. The page scales them to the image.
function dataFor(machine) {
  const env = envelopeFor(machine) || {};
  const screen = env.meta?.screen || { w: 0, h: 0, scale: 1 };
  const ocr = (env.ocr || []).map((o) => ({ r: o.r, t: o.t, cx: o.cx, cy: o.cy }));
  const ax = (env.ax?.elements || []).map((e) => ({
    r: e.r,
    role: e.role,
    title: e.title,
    cx: e.cx,
    cy: e.cy,
  }));
  let jpgMtime = 0;
  const jpg = join(FRAMES_DIR, `${machine}.jpg`);
  try {
    jpgMtime = statSync(jpg).mtimeMs;
  } catch {}
  return {
    machine,
    screen,
    ocr,
    ax,
    hits: hitsFor(machine),
    jpgMtime,
    frontmost: env.meta?.frontmost?.app || null,
    capture: env.capture || null,
  };
}

// ─── page ──────────────────────────────────────────────────────────────────

// The client polls /data/<m>.json and rebuilds the SVG; the JPEG is reloaded
// via cache-busting on mtime change. (MJPEG would be smoother but couples to
// puppet-view's stream; this prototype stays decoupled — a poll loop is fine
// for a few-Hz frame.) Layer toggles: O ocr, A ax, H hits.
function panePage(machine) {
  return `<!doctype html><meta charset=utf8><title>analysis · ${machine}</title>
<style>
  html,body{margin:0;height:100%;background:#0b0f10;color:#9ab;font:12px ui-monospace,monospace}
  #wrap{position:relative;display:inline-block;max-width:100%}
  #shot{display:block;max-width:100%;height:auto;border:1px solid #233}
  svg{position:absolute;inset:0;width:100%;height:100%;pointer-events:none}
  #hud{position:fixed;top:8px;left:8px;background:#0e1417cc;border:1px solid #243;border-radius:8px;padding:6px 10px;backdrop-filter:blur(6px);z-index:9}
  #hud label{margin-right:10px;cursor:pointer;user-select:none}
  .sw{display:inline-block;width:9px;height:9px;border-radius:2px;vertical-align:middle;margin-right:4px}
  b{color:#cde}
</style>
<div id=hud>
  <b>${machine}</b> &nbsp;
  <label><span class=sw style="background:#39d98a"></span><input type=checkbox id=cO checked> OCR (<span id=nO>0</span>)</label>
  <label><span class=sw style="background:#5ca8ff"></span><input type=checkbox id=cA checked> AX (<span id=nA>0</span>)</label>
  <label><span class=sw style="background:#ff4081"></span><input type=checkbox id=cH checked> hits (<span id=nH>0</span>)</label>
  <span id=meta style="color:#677"></span>
</div>
<div id=wrap><img id=shot src="/frame/${machine}.jpg"><svg id=ov preserveAspectRatio=none></svg></div>
<script>
const M=${JSON.stringify(machine)};
const shot=document.getElementById('shot'), ov=document.getElementById('ov'), wrap=document.getElementById('wrap');
const cO=document.getElementById('cO'),cA=document.getElementById('cA'),cH=document.getElementById('cH');
let last={jpgMtime:0};
addEventListener('keydown',e=>{
  if(e.key==='o'||e.key==='O'){cO.checked=!cO.checked;render();}
  if(e.key==='a'||e.key==='A'){cA.checked=!cA.checked;render();}
  if(e.key==='h'||e.key==='H'){cH.checked=!cH.checked;render();}
});
[cO,cA,cH].forEach(c=>c.onchange=render);
let DATA=null;
function render(){
  if(!DATA)return;
  const S=DATA.screen; if(!S.w||!S.h){ov.innerHTML='';return;}
  ov.setAttribute('viewBox','0 0 '+S.w+' '+S.h); // overlay coords = screen points
  const esc=s=>(s||'').replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
  let g='';
  if(cO.checked) for(const o of DATA.ocr){ if(!o.r)continue; const[x,y,w,h]=o.r;
    g+='<rect x='+x+' y='+y+' width='+w+' height='+h+' fill="none" stroke="#39d98a" stroke-width="1.5"/>'
      +'<text x='+x+' y='+(y-2)+' fill="#39d98a" font-size="11" font-family="monospace">'+esc(o.t).slice(0,40)+'</text>';
  }
  if(cA.checked) for(const e of DATA.ax){ if(!e.r)continue; const[x,y,w,h]=e.r;
    g+='<rect x='+x+' y='+y+' width='+w+' height='+h+' fill="rgba(92,168,255,.08)" stroke="#5ca8ff" stroke-width="1.5" stroke-dasharray="4 3"/>'
      +'<text x='+x+' y='+(y+12)+' fill="#5ca8ff" font-size="11" font-family="monospace">'+esc(e.role)+' '+esc(e.title).slice(0,30)+'</text>';
  }
  if(cH.checked){ const now=Date.now();
    for(const hit of DATA.hits){ const age=now-(hit.t||now); const k=Math.max(0,1-age/8000);
      const r=10+22*(1-k); // expanding ring as it ages
      g+='<circle cx='+hit.x+' cy='+hit.y+' r='+r.toFixed(1)+' fill="none" stroke="#ff4081" stroke-width="2" opacity="'+(0.15+0.85*k).toFixed(2)+'"/>'
        +'<circle cx='+hit.x+' cy='+hit.y+' r="3" fill="#ff4081" opacity="'+(0.3+0.7*k).toFixed(2)+'"/>';
      if(hit.label) g+='<text x='+(hit.x+8)+' y='+(hit.y-8)+' fill="#ff4081" font-size="11" font-family="monospace">'+esc(hit.label)+'</text>';
    }
  }
  ov.innerHTML=g;
  document.getElementById('nO').textContent=DATA.ocr.length;
  document.getElementById('nA').textContent=DATA.ax.length;
  document.getElementById('nH').textContent=DATA.hits.length;
  document.getElementById('meta').textContent=' · '+S.w+'×'+S.h+(DATA.frontmost?' · '+DATA.frontmost:'')+(DATA.capture==='permission_needed'?' · NO PIXELS (grant Screen Recording)':'');
}
async function poll(){
  try{
    const d=await fetch('/data/'+M+'.json',{cache:'no-store'}).then(r=>r.json());
    DATA=d;
    if(d.jpgMtime && d.jpgMtime!==last.jpgMtime){ shot.src='/frame/'+M+'.jpg?t='+d.jpgMtime; last.jpgMtime=d.jpgMtime; }
    render();
  }catch(e){}
  setTimeout(poll, 500); // a few Hz — overlay tracks the latest pulled frame
}
poll();
</script>`;
}

function gridPage() {
  const names = machineNames();
  const tiles = names
    .map(
      (n) =>
        `<a href="/m/${n}" style="text-decoration:none;color:#9ab"><figure style="margin:0">
        <figcaption style="padding:3px 6px;color:#9a9">${n}</figcaption>
        <div style="position:relative"><img src="/frame/${n}.jpg" style="width:100%;display:block;border:1px solid #233"></div>
        </figure></a>`,
    )
    .join("");
  return `<!doctype html><meta charset=utf8><title>slab analysis layer</title>
<body style="background:#0b0f10;margin:0;padding:8px;font:12px ui-monospace,monospace;color:#9ab">
<div style="padding:4px 6px;color:#677">slab analysis layer — click a machine for the live OCR / AX / hit overlay</div>
<div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:8px">
${tiles || '<p style="color:#677">no frames yet — pull one with: frame &lt;machine&gt; --preview</p>'}
</div>`;
}

http
  .createServer((req, res) => {
    const url = req.url.split("?")[0];
    if (url === "/") {
      res.writeHead(200, { "Content-Type": "text/html" });
      return res.end(gridPage());
    }
    let m;
    if ((m = url.match(/^\/m\/([\w.-]+)$/))) {
      res.writeHead(200, { "Content-Type": "text/html" });
      return res.end(panePage(m[1]));
    }
    if ((m = url.match(/^\/data\/([\w.-]+)\.json$/))) {
      res.writeHead(200, { "Content-Type": "application/json", "Cache-Control": "no-store" });
      return res.end(JSON.stringify(dataFor(m[1])));
    }
    if ((m = url.match(/^\/frame\/([\w.-]+)\.jpg$/))) {
      const file = join(FRAMES_DIR, `${m[1]}.jpg`);
      if (!existsSync(file)) {
        res.writeHead(404);
        return res.end("no frame");
      }
      res.writeHead(200, { "Content-Type": "image/jpeg", "Cache-Control": "no-store" });
      return res.end(readFileSync(file));
    }
    res.writeHead(404);
    res.end("not found");
  })
  .listen(PORT, () =>
    console.log(`analysis-overlay: http://localhost:${PORT} (frames: ${FRAMES_DIR})`),
  );
