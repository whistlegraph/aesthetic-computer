// Generate SVG specimens (currentColor, viewBox-only so CSS sizes them).
//  font_1  → vector line strokes from disks/drawings/font_1
//  mc8/uni → BDF pixels from /tmp/bdf
// Specimen text spells AC piece names — contextual, not lorem.
// Run from this dir: node gen-specimens.mjs
import fs from "node:fs";

// ── font_1 (line strokes on a 6×10 grid) ──
const F1 = "../../aesthetic.computer/disks/drawings/font_1";
function loadF1(){
  const map = {};
  for (const sub of ["lowercase","uppercase","numbers","symbols"]) {
    let dir; try { dir = fs.readdirSync(`${F1}/${sub}`); } catch { continue; }
    for (const f of dir) { if (f.endsWith(".json")) try { map[f[0]] = JSON.parse(fs.readFileSync(`${F1}/${sub}/${f}`,"utf8")); } catch {} }
  }
  return map;
}
const f1 = loadF1();
const F1_ADV = 7, F1_H = 10;
function f1Row(str){
  let ox = 0, body = "";
  for (const ch of str) {
    const g = f1[ch];
    if (g && g.commands) for (const c of g.commands) {
      const a = c.args;
      if (c.name === "line") body += `<line x1="${ox+a[0]}" y1="${a[1]}" x2="${ox+a[2]}" y2="${a[3]}"/>`;
      else if (c.name === "point" || c.name === "plot") body += `<rect x="${ox+a[0]}" y="${a[1]}" width="1" height="1" stroke="none" fill="currentColor"/>`;
    }
    ox += F1_ADV;
  }
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="-1 -1 ${ox+1} ${F1_H+2}" fill="none" stroke="currentColor" stroke-width="1" stroke-linecap="square" stroke-linejoin="miter">${body}</svg>`;
}

// ── BDF (pixels) ──
function parseBDF(path, wanted){
  const lines = fs.readFileSync(path,"utf8").split("\n");
  const glyphs = new Map(); let g=null, bm=false;
  for (const line of lines){
    if (line.startsWith("STARTCHAR")){ g={rows:[]}; bm=false; }
    else if (g && line.startsWith("ENCODING")) g.enc=parseInt(line.split(/\s+/)[1]);
    else if (g && line.startsWith("DWIDTH")) g.dx=parseInt(line.split(/\s+/)[1]);
    else if (g && line.startsWith("BBX")){ const p=line.split(/\s+/); g.w=+p[1]; g.h=+p[2]; g.xoff=+p[3]; g.yoff=+p[4]; }
    else if (g && line.startsWith("BITMAP")) bm=true;
    else if (g && line.startsWith("ENDCHAR")){ bm=false; if(!wanted||wanted.has(g.enc)) glyphs.set(g.enc,g); g=null; }
    else if (bm && g) g.rows.push(line.trim());
  }
  return glyphs;
}
function bdfRow(str, glyphs){
  const cps=[...str].map(c=>c.codePointAt(0));
  const present=cps.map(cp=>glyphs.get(cp)).filter(Boolean);
  if(!present.length) return "";
  const ascent=Math.max(...present.map(g=>g.yoff+g.h)), descent=Math.min(...present.map(g=>g.yoff));
  const height=ascent-descent; let penX=0, rects="";
  for(const cp of cps){
    const g=glyphs.get(cp); if(!g){ penX+=6; continue; }
    const top=ascent-(g.yoff+g.h), bytes=Math.ceil(g.w/8);
    for(let r=0;r<g.h;r++){ const hex=(g.rows[r]||"").padEnd(bytes*2,"0");
      for(let c=0;c<g.w;c++){ const v=parseInt(hex.substr((c>>3)*2,2),16); if((v>>(7-(c&7)))&1) rects+=`<rect x="${penX+g.xoff+c}" y="${top+r}" width="1" height="1"/>`; } }
    penX += g.dx!=null?g.dx:g.w+1;
  }
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${penX} ${height}" fill="currentColor" shape-rendering="crispEdges">${rects}</svg>`;
}

// ── write specimens (AC piece names) ──
const W = (n,s)=>fs.writeFileSync(`specimens/${n}.svg`, s);
W("font_1-1", f1Row("aesthetic computer"));
W("font_1-2", f1Row("prompt notepat melody"));
W("font_1-3", f1Row("painting freehand wand"));

const mc8 = parseBDF("/tmp/bdf/mc8.bdf", null);
W("mc8-1", bdfRow("chat kidlisp tape", mc8));
W("mc8-2", bdfRow("bleep stample spray", mc8));
W("mc8-3", bdfRow("line box circle balls", mc8));

const latin="aesthetic computer", multi="日本語 中文 한글 Ωμ →★";
const uni = parseBDF("/tmp/bdf/unifont.bdf", new Set([...(latin+multi)].map(c=>c.codePointAt(0))));
W("unifont-latin", bdfRow(latin, uni));
W("unifont-multi", bdfRow(multi, uni));

console.log("font_1 glyphs:", Object.keys(f1).length, "· mc8:", mc8.size, "· unifont sample:", uni.size);
