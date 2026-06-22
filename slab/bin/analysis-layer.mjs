#!/usr/bin/env node
// analysis-layer.mjs — the shared building blocks for slab's realtime
// "analysis layer": (1) a page-side overlay snippet that draws OCR boxes + hit
// pulses INSIDE a browser target (the __puppet_cursor pattern, generalized),
// and (2) a hit-logger that records click/stroke points so the out-of-page
// viewer (analysis-overlay.mjs) and the Swift FramePreview can render pulses.
//
// PROTOTYPE (uncommitted). Importable as a module; also runnable for a self-
// test (`node analysis-layer.mjs selftest`) that exercises the JS generators
// against jsdom-free assumptions (string output only — no browser needed).
//
// Two rendering surfaces, one shared idea (boxes-in-screen-space + pulses):
//   • PAGE-SIDE (browser targets): inject an SVG overlay via CDP
//     Runtime.evaluate, exactly like cursorEval injects #__puppet_cursor.
//     Zero new infra, rides the existing puppet session. Sees only the DOM
//     viewport — fine for browser-driven rigs, blind to native chrome.
//   • OUT-OF-PAGE (any native app): the JPEG-overlay viewer in
//     analysis-overlay.mjs draws the SAME boxes over a captured frame. Works
//     for every app because `frame` already OCRs the whole screen.
//
// Coordinate space note: OCR/AX rects from FrameCapture.swift are LOGICAL
// SCREEN POINTS (top-left origin). CDP Input.dispatchMouseEvent + the page-side
// overlay use CSS pixels in the page VIEWPORT. For a maximized browser window
// they line up closely; for precise native overlays use the out-of-page viewer.

import { appendFileSync, mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

const SLAB = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const ANALYSIS_DIR = join(SLAB, "analysis");

// ─── hit logger ──────────────────────────────────────────────────────────
// Append one {x,y,kind,label,t} per interaction. The viewer tails this file
// and pulses recent points. Call from a stroke/click wrapper (see the puppet
// hook sketch at the bottom). Pure file I/O — never touches the daemon socket.
export function logHit(machine, { x, y, kind = "click", label = "" } = {}) {
  mkdirSync(ANALYSIS_DIR, { recursive: true });
  const line = JSON.stringify({ x: Math.round(x), y: Math.round(y), kind, label, t: Date.now() });
  appendFileSync(join(ANALYSIS_DIR, `${machine}.hits.jsonl`), line + "\n");
}

export function logStroke(machine, points, label = "stroke") {
  for (const [x, y] of points) logHit(machine, { x, y, kind: "stroke", label });
}

// ─── page-side overlay snippet (CDP Runtime.evaluate) ──────────────────────
// Returns a JS expression string to feed to Runtime.evaluate on a browser
// target. It ensures a single #__analysis_overlay <svg> pinned over the page
// and (re)draws the given rects/hits. Mirrors cursorEval: idempotent install,
// pointer-events:none, max z-index so it never intercepts the trusted input
// stream the cursor overlay rides.
//
//   rects: [{ r:[x,y,w,h], kind:"ocr"|"ax", t?:string }]   (screen→viewport px)
//   hits:  [{ x, y, label? }]
// Self-clearing by design: rects live in a single replaceable layer (redraw
// each call), and every hit is a transient <g> that fades out and removes
// itself ~0.9s after it lands. The container <svg> deletes itself once empty,
// so nothing lingers on the page after driving stops.
export function overlayDrawExpr(rects = [], hits = []) {
  const payload = JSON.stringify({ rects, hits });
  return `(()=>{
const D=${payload};
const NS="http://www.w3.org/2000/svg";
let s=document.getElementById("__analysis_overlay");
if(!s){
  s=document.createElementNS(NS,"svg");
  s.id="__analysis_overlay";
  s.style.cssText="position:fixed;inset:0;width:100vw;height:100vh;z-index:2147483646;pointer-events:none";
  document.documentElement.appendChild(s);
}
const mk=(t,a)=>{const e=document.createElementNS(NS,t);for(const k in a)e.setAttribute(k,a[k]);return e;};
const reap=()=>{if(s&&!s.childNodes.length&&s.isConnected)s.remove();};
// rects layer — replaced wholesale each call (OCR/AX boxes)
const old=s.querySelector("#__ao_rects");if(old)old.remove();
if(D.rects.length){
  const rg=mk("g",{id:"__ao_rects"});
  for(const o of D.rects){if(!o.r)continue;const[x,y,w,h]=o.r;const ocr=o.kind!=="ax";
    rg.appendChild(mk("rect",{x,y,width:w,height:h,fill:ocr?"none":"rgba(92,168,255,.08)",
      stroke:ocr?"#39d98a":"#5ca8ff","stroke-width":1.5,"stroke-dasharray":ocr?"":"4 3"}));
    if(o.t){const txt=mk("text",{x,y:y-2,fill:ocr?"#39d98a":"#5ca8ff","font-size":11,"font-family":"monospace"});
      txt.textContent=String(o.t).slice(0,40);rg.appendChild(txt);}
  }
  s.appendChild(rg);
}
// hits layer — each hit is a transient group that fades + self-removes
for(const hit of D.hits){
  const g=mk("g",{});
  g.appendChild(mk("circle",{cx:hit.x,cy:hit.y,r:14,fill:"none",stroke:"#ff4081","stroke-width":2}));
  g.appendChild(mk("circle",{cx:hit.x,cy:hit.y,r:3,fill:"#ff4081"}));
  const ring=mk("circle",{cx:hit.x,cy:hit.y,r:6,fill:"none",stroke:"#ff4081","stroke-width":2});
  g.appendChild(ring);
  ring.appendChild(mk("animate",{attributeName:"r",from:"6",to:"34",dur:"0.6s",fill:"freeze"}));
  // fade the WHOLE group out after a brief hold, then remove it
  g.appendChild(mk("animate",{attributeName:"opacity",from:"1",to:"0",begin:"0.2s",dur:"0.7s",fill:"freeze"}));
  if(hit.label){const t=mk("text",{x:hit.x+8,y:hit.y-8,fill:"#ff4081","font-size":11,"font-family":"monospace"});
    t.textContent=String(hit.label);g.appendChild(t);}
  s.appendChild(g);
  setTimeout(()=>{g.remove();reap();},900);
}
reap();
return D.rects.length+"/"+D.hits.length;
})()`;
}

// Returns a JS expression string that removes the page-side overlay (toggle off).
export function overlayClearExpr() {
  return `(()=>{const s=document.getElementById("__analysis_overlay");if(s)s.remove();return true;})()`;
}

// ─── CLI: emit a per-machine sidecar envelope for the JPEG viewer ──────────
// `frame <m> --json` prints the envelope to stdout; pipe it here to drop the
// sidecar the viewer reads:  frame panda --json | analysis-layer.mjs emit panda
async function emitSidecar(machine) {
  const chunks = [];
  for await (const c of process.stdin) chunks.push(c);
  const json = Buffer.concat(chunks).toString("utf8").trim();
  if (!json) {
    console.error("no envelope on stdin (pipe `frame <m> --json`)");
    process.exit(1);
  }
  const out = join(SLAB, "frames", `${machine}.frame.json`);
  mkdirSync(dirname(out), { recursive: true });
  const { writeFileSync } = await import("node:fs");
  writeFileSync(out, json);
  console.log(`wrote ${out}`);
}

// ─── self-test (no browser, no daemon) ─────────────────────────────────────
function selftest() {
  const expr = overlayDrawExpr(
    [{ r: [100, 200, 80, 20], kind: "ocr", t: "Submit" }, { r: [300, 50, 120, 30], kind: "ax", t: "button" }],
    [{ x: 140, y: 210, label: "stroke" }],
  );
  console.assert(expr.includes("__analysis_overlay"), "overlay id present");
  console.assert(expr.includes("Submit"), "ocr label present");
  console.assert(expr.includes("#ff4081"), "hit color present");
  console.assert(overlayClearExpr().includes("remove"), "clear removes node");
  // logHit round-trip
  logHit("__selftest", { x: 12, y: 34, label: "t" });
  console.log("selftest OK — overlay expr", expr.length, "chars; hit logged to", ANALYSIS_DIR);
}

// CLI dispatch — only when run directly (importing must have no side effects).
if (import.meta.url === `file://${process.argv[1]}`) {
  const cmd = process.argv[2];
  if (cmd === "selftest") selftest();
  else if (cmd === "emit") await emitSidecar(process.argv[3] || "local");
  else if (cmd === "draw-expr") process.stdout.write(overlayDrawExpr()); // for eyeballing the snippet
  else {
    console.log(
      "analysis-layer.mjs — slab analysis-layer building blocks\n" +
        "  module exports: logHit, logStroke, overlayDrawExpr, overlayClearExpr\n" +
        "  node analysis-layer.mjs selftest         exercise the generators\n" +
        "  frame <m> --json | analysis-layer.mjs emit <m>   write the viewer sidecar\n",
    );
  }
}

// ─── puppet hook sketch (NOT applied — design note for production wiring) ──
// To make `stroke`/`gesture`/`cursor` flash their points in BOTH surfaces,
// add, inside puppet.mjs Machine.stroke(), AFTER computing `points`:
//
//   import { logHit, overlayDrawExpr } from "./analysis-layer.mjs";
//   // out-of-page surface (JPEG viewer / FramePreview):
//   for (const [x, y] of points) logHit(this.name, { x, y, kind: "stroke", label: "stroke" });
//   // page-side surface (browser target), riding the same session:
//   if (this.analysisOn) this.callNoWait("Runtime.evaluate",
//     { expression: overlayDrawExpr([], points.map(([x,y]) => ({ x, y }))) }, sessionId);
//
// and gate it behind a per-machine `analysisOn` flag toggled by a new
// `analysis on|off <machine>` daemon command — so it is OFF by default and
// adds zero cost to normal driving.
