#!/usr/bin/env node
// syllawizard.mjs — the syllable wizard: a self-contained HTML annotator
// where @jeffrey draws rectangles around spectrogram bands per syllable.
// His judgment becomes boundaries-drawn-<take>.json — ground truth the
// whole chain (holyvox, shapebound training, the vocal model) proceeds
// from. Parametric by take, so N takes can be annotated over time.
//
//   node pop/imab/bin/syllawizard.mjs [take]   (assets must exist — see below)
//   → pop/imab/out/syllawizard-<take>.html     (open in a browser)
//
// Assets per take (built by the run that calls this):
//   ~/.cache/ac/imab/wizard-spec.png   spectrogram at 260 px/s
//   ~/.cache/ac/imab/wizard-audio.mp3  the vocal stem
//   ~/.cache/ac/imab/bounds-<take>.json  starting guesses (optional)

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
const TAKE = process.argv[2] || "7311159624588070175";
const PXS = 260;

const spec = readFileSync(`${WORK}/wizard-spec.png`).toString("base64");
const audio = readFileSync(`${WORK}/wizard-audio.mp3`).toString("base64");
const SYLS = [
  ["i'm", 0], ["a", 1], ["but", 2], ["ter", 2], ["fly", 2], ["flap", 3], ["ping", 3],
  ["for", 4], ["you", 5], ["guys", 6], ["just", 7], ["a", 8], ["cos", 9], ["tume", 9],
  ["i", 10], ["put", 11], ["on", 12], ["in", 13], ["my", 14], ["room", 15]];
let seed = [];
const bp = `${WORK}/bounds-${TAKE}.json`;
if (existsSync(bp)) {
  const B = JSON.parse(readFileSync(bp, "utf8"));
  const flat = B.words.flatMap((w) => w.sylls || []);
  seed = SYLS.map(([lab], i) => {
    const s = flat[i];
    return s ? { fromMs: s.fromMs, toMs: s.toMs, fLo: 0.15, fHi: 0.9 } : null;
  });
}

const html = `<!doctype html><meta charset="utf-8"><title>syllawizard · ${TAKE}</title>
<style>
body{margin:0;background:#0e0d14;color:#eee;font:15px Helvetica,Arial}
#bar{padding:10px 16px;display:flex;gap:14px;align-items:center;flex-wrap:wrap}
#sylls{display:flex;gap:6px;flex-wrap:wrap}
.syl{padding:4px 10px;border:1px solid #555;border-radius:6px;cursor:pointer;user-select:none}
.syl.done{border-color:#6c8;color:#6c8}.syl.cur{background:#ff5c9e;color:#000;border-color:#ff5c9e}
#wrap{overflow-x:scroll;position:relative}
#stage{position:relative;height:560px}
#stage img{position:absolute;top:40px;left:0;height:520px;image-rendering:auto}
canvas{position:absolute;top:0;left:0;cursor:crosshair}
button{background:#222;color:#eee;border:1px solid #666;border-radius:6px;padding:6px 14px;cursor:pointer}
#json{width:98%;height:90px;background:#111;color:#9d9;border:1px solid #444;margin:8px}
b{color:#ff5c9e}
</style>
<div id="bar">
  <b>syllawizard · take ${TAKE}</b>
  <button id="play">play/pause (space)</button>
  <span>click spectrogram = seek · drag = draw rect for the <b>highlighted</b> syllable · click a chip to reselect · export when done</span>
  <button id="export">⇩ export boundaries-drawn-${TAKE}.json</button>
  <button id="copy">copy json</button>
</div>
<div id="bar"><div id="sylls"></div></div>
<div id="wrap"><div id="stage">
  <img id="spec" src="data:image/png;base64,${spec}">
</div></div>
<textarea id="json" readonly></textarea>
<audio id="au" src="data:audio/mpeg;base64,${audio}"></audio>
<script>
const PXS=${PXS}, SYLS=${JSON.stringify(SYLS)}, TAKE="${TAKE}";
let rects=${JSON.stringify(seed)};
if(!rects.length) rects=SYLS.map(()=>null);
let cur=rects.findIndex(r=>!r); if(cur<0) cur=0;
const au=document.getElementById("au"), img=document.getElementById("spec");
const stage=document.getElementById("stage"), wrap=document.getElementById("wrap");
const cv=document.createElement("canvas");
img.onload=()=>{cv.width=img.naturalWidth;cv.height=560;stage.style.width=img.naturalWidth+"px";stage.appendChild(cv);draw();};
const ctx=cv.getContext("2d");
const chips=document.getElementById("sylls");
SYLS.forEach(([lab],i)=>{const c=document.createElement("span");c.className="syl";c.textContent=lab;
  c.onclick=()=>{cur=i;refresh();};chips.appendChild(c);});
function refresh(){[...chips.children].forEach((c,i)=>{c.classList.toggle("cur",i===cur);
  c.classList.toggle("done",!!rects[i]&&i!==cur);});draw();emit();}
function tX(ms){return ms/1000*PXS;}
function draw(){ctx.clearRect(0,0,cv.width,cv.height);
  ctx.font="22px Helvetica";
  rects.forEach((r,i)=>{if(!r)return;
    const x=tX(r.fromMs),w=tX(r.toMs)-x;
    const y=40+(1-r.fHi)*520,h=(r.fHi-r.fLo)*520;
    ctx.fillStyle=i===cur?"rgba(255,92,158,.25)":"rgba(120,200,255,.16)";
    ctx.strokeStyle=i===cur?"#ff5c9e":"#7ac8ff";ctx.lineWidth=2;
    ctx.fillRect(x,y,w,h);ctx.strokeRect(x,y,w,h);
    ctx.fillStyle=i===cur?"#ff5c9e":"#7ac8ff";ctx.fillText(SYLS[i][0],x+4,y-6);});
  ctx.fillStyle="#ffd655";ctx.fillRect(au.currentTime*PXS-1,0,2,560);}
let dragging=null;
cv.onmousedown=e=>{const r=cv.getBoundingClientRect();
  dragging={x0:e.clientX-r.left,y0:e.clientY-r.top,moved:false};};
cv.onmousemove=e=>{if(!dragging)return;const r=cv.getBoundingClientRect();
  const x=e.clientX-r.left,y=e.clientY-r.top;
  if(Math.abs(x-dragging.x0)>4)dragging.moved=true;
  if(dragging.moved){const a=Math.min(dragging.x0,x),b=Math.max(dragging.x0,x);
    const ya=Math.min(dragging.y0,y),yb=Math.max(dragging.y0,y);
    rects[cur]={fromMs:Math.round(a/PXS*1000),toMs:Math.round(b/PXS*1000),
      fHi:Math.max(0,Math.min(1,1-(ya-40)/520)),fLo:Math.max(0,Math.min(1,1-(yb-40)/520))};draw();}};
cv.onmouseup=e=>{if(dragging&&!dragging.moved){au.currentTime=dragging.x0/PXS;au.play();}
  else if(dragging&&dragging.moved){if(cur<SYLS.length-1)cur=rects.findIndex((r,i)=>i>cur&&!r)>=0?rects.findIndex((r,i)=>i>cur&&!r):cur+1;}
  dragging=null;refresh();};
document.body.onkeydown=e=>{if(e.code==="Space"){e.preventDefault();au.paused?au.play():au.pause();}
  if(e.code==="Backspace"){rects[cur]=null;refresh();}};
document.getElementById("play").onclick=()=>au.paused?au.play():au.pause();
function payload(){return JSON.stringify({take:TAKE,drawn:new Date().toISOString(),
  sylls:SYLS.map(([lab,wi],i)=>rects[i]?{label:lab,wi,...rects[i]}:null).filter(Boolean)},null,1);}
function emit(){document.getElementById("json").value=payload();}
document.getElementById("export").onclick=()=>{const b=new Blob([payload()],{type:"application/json"});
  const a=document.createElement("a");a.href=URL.createObjectURL(b);
  a.download="boundaries-drawn-"+TAKE+".json";a.click();};
document.getElementById("copy").onclick=()=>navigator.clipboard.writeText(payload());
(function loop(){draw();requestAnimationFrame(loop);})();
refresh();
</script>`;
writeFileSync(`${OUT}/syllawizard-${TAKE}.html`, html);
console.log(`✓ ${OUT}/syllawizard-${TAKE}.html`);
