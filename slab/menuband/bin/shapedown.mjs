#!/usr/bin/env node
// shapedown.mjs — transparent, over-everything score visuals for a Menu Band
// fleet performance. No backdrop: the desktop stays visible.
//
// The plate is THE REAL STRIP: captured Menu Band menu-bar frames
// (pop/menuband/out/menubar-frames/mb-idle.png + mb-<midi>.png pressed
// states — the same assets the waltz reel used). The overlay never redraws
// the design; it scales the actual pixels. The strip flies OUT of the menu
// bar before the downbeat, note bars float down past the real keys (the
// pressed frame swaps in as each note sounds), and when the music ends the
// strip flies home. Subtitles are ONLY what each machine speaks (conduct's
// say schedule via --captions), in prox bubble letters (Comic Sans, white
// fill, dark outline, hard status-colour shadow, per-char jitter + wiggle —
// ported from slab/menubar-swift PromptSigilOverlay.rebuildName).
//
//   node bin/shapedown.mjs <score> [--epoch <unix>] [--skews s0,s1]
//                          [--captions <file.json>] [--debug <t>]

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "shapedown");
const FRAMES = resolve(HERE, "..", "..", "..", "pop", "menuband", "out", "menubar-frames");

const [slug, ...rest] = process.argv.slice(2);
if (!slug) {
  console.log("usage: shapedown.mjs <score> [--epoch <unix>] [--skews s0,s1] [--captions file] [--debug <t>]");
  process.exit(1);
}
const arg = (flag) => {
  const i = rest.indexOf(flag);
  return i >= 0 ? rest[i + 1] : null;
};
const epoch = Number(arg("--epoch")) || null;
const debugT = arg("--debug");
const skews = (arg("--skews") || "").split(",").map(Number);
const captionsFile = arg("--captions");
const allCaptions = captionsFile
  ? JSON.parse(readFileSync(captionsFile, "utf8"))
  : [];

const score = JSON.parse(
  readFileSync(resolve(HERE, "..", "scores", `${slug}.mbscore`), "utf8"),
);

function parseNotes(csv) {
  let at = 0;
  const notes = [];
  for (const tok of csv.split(",")) {
    const [p, b] = tok.split(":");
    const beats = Number(b);
    if (p !== "r") notes.push({ midi: Number(p), atBeat: at, beats });
    at += beats;
  }
  return { notes, total: at };
}
function parsePerc(csv) {
  if (!csv) return [];
  let at = 0;
  return csv.split(",").map((tok) => {
    const [hit, b] = tok.split(":");
    const e = { hit, atBeat: at };
    at += Number(b);
    return e;
  });
}

const bpm = score.bpm;
const spb = 60 / bpm;
const voices = score.voices.map((v, i) => {
  const { notes, total } = parseNotes(v.notes);
  return { index: i, name: v.name, notes, totalBeats: total, perc: parsePerc(v.notes2) };
});
const durSec = Math.max(...voices.map((v) => v.totalBeats)) * spb;

// ── the real strip frames ─────────────────────────────────────────────────
// mb-idle.png is the plate; mb-<midi>.png is the strip with that key pressed.
// The captured strip is two octaves C4–B5 (white keys 60..83) — that IS the
// layout, identical everywhere, never re-derived.
const b64 = (p) => `data:image/png;base64,${readFileSync(p).toString("base64")}`;
const frames = { idle: b64(resolve(FRAMES, "mb-idle.png")) };
const usedMidis = [...new Set(voices.flatMap((v) => v.notes.map((n) => n.midi)))];
for (const m of usedMidis) {
  const p = resolve(FRAMES, `mb-${m}.png`);
  if (existsSync(p)) frames[m] = b64(p);
}
// White-key slot for a midi within the captured strip (C4=slot 0 … B5=13).
// Sharps snap to their lower white neighbour (the capture set is diatonic).
const WHITE_SLOT = { 0:0, 2:1, 4:2, 5:3, 7:4, 9:5, 11:6 };

const STATUS = ["#ff64b4", "#78dcff", "#ffd166", "#95f2a6"];

function html(voice) {
  const cfg = {
    bpm,
    epoch: epoch ? epoch + (skews[voice.index] || 0) : null,
    debugT: debugT ? Number(debugT) : null,
    voice: voice.name,
    notes: voice.notes,
    perc: voice.perc,
    durSec,
    status: STATUS[voice.index % STATUS.length],
    captions: allCaptions.filter((c) => c.v === voice.index)
      .map(({ t, s, text }) => ({ t, s, text })),
  };
  if (cfg.debugT != null && !cfg.captions.length)
    cfg.captions = [{ t: cfg.debugT - 1, s: 4, text: "Hi neo, this is blueberry. I have the caller." }];
  return `<!doctype html>
<meta charset="utf-8"><title>shapedown · ${score.title} · ${voice.name}</title>
<style>
  html,body{margin:0;height:100%;background:transparent;overflow:hidden;cursor:none}
  canvas{display:block;width:100vw;height:100vh}
</style>
<canvas id="c"></canvas>
<script>
const CFG = ${JSON.stringify(cfg)};
const FRAMES = ${JSON.stringify(frames)};
const cv = document.getElementById("c"), cx = cv.getContext("2d");
let W, H, DPR;
function size(){ DPR = devicePixelRatio||1; W = innerWidth; H = innerHeight;
  cv.width = W*DPR; cv.height = H*DPR; cx.setTransform(DPR,0,0,DPR,0,0); }
addEventListener("resize", () => { size(); if (typeof draw === "function") draw(); });
size();

const IMG = {};
let loaded = 0, want = 0;
for (const k in FRAMES){ want++;
  const im = new Image();
  im.onload = () => { if (++loaded === want && CFG.debugT!=null) draw(); };
  im.src = FRAMES[k]; IMG[k] = im;
}

const SPB = 60/CFG.bpm;
const FALL = 2.6;
const t0 = CFG.epoch ? CFG.epoch*1000 : Date.now()+1000;
const now = () => CFG.debugT!=null ? CFG.debugT : (Date.now()-t0)/1000;

// ── plate flight: menu bar (top right) → center stage → home ─────────────
const ASPECT = ${(2584 / 176).toFixed(4)};      // the captured strip's true shape
const WHITE_SLOT = ${JSON.stringify(WHITE_SLOT)};
const SLOTS = 14;                                // two captured octaves, C4..B5
function slotOf(m){ const oct = Math.floor((m-60)/12);
  const st = ((m-60)%12+12)%12;
  const w = WHITE_SLOT[st] ?? WHITE_SLOT[st-1] ?? 0;
  return oct*7 + w; }
function flightE(t){
  const outK = Math.max(0, Math.min(1, (t+4.2)/3.6));
  const homeT = CFG.durSec + 1.0;
  const homeK = Math.max(0, Math.min(1, (t-homeT)/2.4));
  const s = k => k*k*(3-2*k);
  return Math.min(s(outK), 1-s(homeK));
}
function plateNow(t){
  const e = flightE(t);
  const wSmall = 210, wBig = Math.min(W*0.58, 980);
  const pw = wSmall + (wBig-wSmall)*e;
  const ph = pw/ASPECT;
  const xSmall = W-wSmall-140, xBig = (W-pw)/2;
  const ySmall = 3, yBig = H*0.60;
  return { x: xSmall+(xBig-xSmall)*e, y: ySmall+(yBig-ySmall)*e, w: pw, h: ph, e };
}
function laneOf(P, m){
  const kw = P.w/SLOTS;
  return { x: P.x + slotOf(m)*kw, w: kw };
}
const hue = m => (m%12)/12*360;
const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
function comic(px){ return "700 "+px+"px 'Comic Sans MS','Chalkboard SE',sans-serif"; }
function rr(x,y,w,h,r){ cx.beginPath(); cx.roundRect(x,y,w,h,r); }

// prox bubble-letter subtitles (PromptSigilOverlay.rebuildName port)
function hash(s){ let h=2166136261>>>0;
  for (const b of s) { h = (h ^ b.charCodeAt(0)) >>> 0; h = Math.imul(h, 16777619) >>> 0; }
  return h; }
function kf(p, vals){
  const times=[0,0.25,0.75,1];
  for (let i=0;i<3;i++) if (p<=times[i+1]){
    const u=(p-times[i])/(times[i+1]-times[i]);
    const e=(1-Math.cos(u*Math.PI))/2;
    return vals[i]+(vals[i+1]-vals[i])*e;
  }
  return vals[3];
}
function subtitle(text, t, alpha){
  const px = Math.min(44, W*0.034);
  cx.save(); cx.font = comic(px); cx.textAlign="left"; cx.textBaseline="alphabetic";
  const chars = [...text];
  const widths = chars.map(ch => cx.measureText(ch).width);
  const total = widths.reduce((a,b)=>a+b,0);
  let x = (W-total)/2;
  const y = H-56;
  for (let i=0;i<chars.length;i++){
    const h = hash("rock"+i+text);
    const dy = (h%5)/2 - 1;
    const rotS = (((h>>8)%9)-4)*0.9*Math.PI/180;
    const p = (((t - i*0.12)%1.8)+1.8)%1.8/1.8;
    const wig = kf(p,[0,1.2,-0.8,0]);
    const rotW = kf(p,[0,1.2,-0.8,0])*Math.PI/180;
    cx.save(); cx.globalAlpha = alpha;
    cx.translate(x+widths[i]/2, y+dy+wig);
    cx.rotate(rotS+rotW);
    cx.shadowColor = CFG.status; cx.shadowBlur = 0;
    cx.shadowOffsetX = 2.2; cx.shadowOffsetY = 2.2;
    cx.lineWidth = px*0.10; cx.strokeStyle = "rgba(20,20,20,1)";
    cx.strokeText(chars[i], -widths[i]/2, 0);
    cx.shadowColor = "transparent";
    cx.fillStyle = "#ffffff";
    cx.fillText(chars[i], -widths[i]/2, 0);
    cx.restore();
    x += widths[i];
  }
  cx.restore();
}

function draw(){
  const t = now();
  cx.clearRect(0,0,W,H);

  // gentle kick/hat washes over the desktop
  for (const p of CFG.perc){
    const dt = t - p.atBeat*SPB;
    if (dt>=0 && dt<0.28){
      const a = (1-dt/0.28);
      if (p.hit==="k"){ cx.fillStyle = "rgba(255,80,160,"+(0.10*a)+")"; cx.fillRect(0,0,W,H); }
      else { cx.fillStyle = "rgba(120,220,255,"+(0.4*a)+")";
             cx.fillRect(0,0,8,H); cx.fillRect(W-8,0,8,H); }
    }
  }

  const P = plateNow(t);

  // note bars — float down into the real keys and on past them
  if (P.e > 0.95) for (const n of CFG.notes){
    const tOn = n.atBeat*SPB;
    const enter = tOn - FALL;
    if (t < enter || t > tOn + 2.2) continue;
    const g = laneOf(P, n.midi);
    const len = Math.max(20, n.beats*SPB*95);
    const speed = (P.y+len)/FALL;
    const yHead = -len + speed*(t-enter);
    const above = Math.min(yHead, P.y+3);
    const below = yHead - (P.y+P.h);
    cx.save();
    cx.fillStyle = "hsl("+hue(n.midi)+" 90% 62%)";
    cx.shadowColor="rgba(0,0,0,0.55)"; cx.shadowBlur=0; cx.shadowOffsetX=3; cx.shadowOffsetY=3;
    if (above > -len) {
      cx.globalAlpha = 0.95;
      rr(g.x+3, Math.max(-len, above-len), g.w-6, Math.min(len, above+len), 7); cx.fill();
    }
    if (below > 0) {
      cx.globalAlpha = Math.max(0, 0.8 - below/(H*0.3));
      rr(g.x+3, P.y+P.h+Math.max(0,below-len), g.w-6, Math.min(len, below), 7); cx.fill();
    }
    cx.restore();
  }

  // ── the REAL strip — captured pixels, pressed frame while sounding ──────
  if (P.e > 0.01 && IMG.idle.complete){
    // most recent sounding note picks the pressed frame
    let frame = IMG.idle, best = -1;
    for (const n of CFG.notes){
      const tOn = n.atBeat*SPB, tOff = tOn + n.beats*SPB;
      if (t>=tOn && t<=tOff && tOn>best && IMG[n.midi]){ best = tOn; frame = IMG[n.midi]; }
    }
    cx.save();
    cx.shadowColor="rgba(0,0,0,0.45)"; cx.shadowBlur=0;
    cx.shadowOffsetX=2+4*P.e; cx.shadowOffsetY=2+4*P.e;
    cx.imageSmoothingQuality = "high";
    cx.drawImage(frame, P.x, P.y, P.w, P.h);
    cx.restore();

    // impact ring + note letter at the real key while on stage
    if (P.e > 0.95) for (const n of CFG.notes){
      const tOn = n.atBeat*SPB, tOff = tOn + n.beats*SPB;
      if (t<tOn || t>tOff+0.6) continue;
      const g = laneOf(P, n.midi);
      const xC = g.x+g.w/2;
      if (t-tOn<0.55){
        cx.save(); cx.globalAlpha = 1-(t-tOn)/0.55;
        cx.strokeStyle = "hsl("+hue(n.midi)+" 90% 62%)"; cx.lineWidth=4;
        cx.beginPath(); cx.arc(xC, P.y, 10+(t-tOn)*180, 0, 7); cx.stroke(); cx.restore();
      }
      const a = Math.max(0, 1-(t-tOn)/(tOff-tOn+0.6));
      cx.save(); cx.font = comic(30); cx.textAlign="center"; cx.globalAlpha=a;
      cx.fillStyle="rgba(0,0,0,0.9)"; cx.fillText(NAMES[n.midi%12], xC+2, P.y+P.h+38);
      cx.fillStyle="hsl("+hue(n.midi)+" 90% 68%)"; cx.fillText(NAMES[n.midi%12], xC, P.y+P.h+36);
      cx.restore();
    }
  }

  // subtitles — only the machine's own spoken lines
  for (const c of CFG.captions){
    const dt = t - c.t;
    if (dt<0 || dt>c.s+0.6) continue;
    const a = Math.max(0, Math.min(1, dt*4, (c.s+0.6-dt)*2.5));
    subtitle(c.text, t, a);
  }
  if (CFG.debugT==null) requestAnimationFrame(draw);
}
draw();
if (CFG.debugT!=null){ setTimeout(draw,300); setTimeout(draw,1200); }
</script>`;
}

mkdirSync(OUT, { recursive: true });
const files = voices.map((v) => {
  const p = resolve(OUT, `${slug}-v${v.index}.html`);
  writeFileSync(p, html(v));
  return p;
});
console.log(`shapedown → ${files.join("\n            ")} (${Object.keys(frames).length - 1} pressed frames embedded)`);
console.log(epoch ? `synced to epoch ${epoch}` : "no --epoch: free-runs 1s after open");
