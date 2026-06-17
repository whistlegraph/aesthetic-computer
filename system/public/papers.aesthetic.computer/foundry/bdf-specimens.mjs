// Parse BDF bitmap fonts and emit theme-aware SVG specimens (1 rect per pixel,
// fill=currentColor). Sources fetched to /tmp/bdf (MatrixChunky8, unifont).
// Run: node bdf-specimens.mjs  (from this dir)
import fs from "node:fs";

function parseBDF(path, wanted) {
  const lines = fs.readFileSync(path, "utf8").split("\n");
  const glyphs = new Map();
  let g = null, inBitmap = false;
  for (const line of lines) {
    if (line.startsWith("STARTCHAR")) { g = { rows: [] }; inBitmap = false; }
    else if (g && line.startsWith("ENCODING")) g.enc = parseInt(line.split(/\s+/)[1]);
    else if (g && line.startsWith("DWIDTH")) g.dx = parseInt(line.split(/\s+/)[1]);
    else if (g && line.startsWith("BBX")) { const p = line.split(/\s+/); g.w = +p[1]; g.h = +p[2]; g.xoff = +p[3]; g.yoff = +p[4]; }
    else if (g && line.startsWith("BITMAP")) inBitmap = true;
    else if (g && line.startsWith("ENDCHAR")) { inBitmap = false; if (!wanted || wanted.has(g.enc)) glyphs.set(g.enc, g); g = null; }
    else if (inBitmap && g) g.rows.push(line.trim());
  }
  return glyphs;
}

function rowSVG(str, glyphs) {
  const cps = [...str].map((c) => c.codePointAt(0));
  const present = cps.map((cp) => glyphs.get(cp)).filter(Boolean);
  if (!present.length) return "";
  const ascent = Math.max(...present.map((g) => g.yoff + g.h));
  const descent = Math.min(...present.map((g) => g.yoff));
  const height = ascent - descent;
  let penX = 0, rects = "";
  for (const cp of cps) {
    const g = glyphs.get(cp);
    if (!g) { penX += 6; continue; }
    const top = ascent - (g.yoff + g.h);
    const bytes = Math.ceil(g.w / 8);
    for (let r = 0; r < g.h; r++) {
      const hex = (g.rows[r] || "").padEnd(bytes * 2, "0");
      for (let c = 0; c < g.w; c++) {
        const val = parseInt(hex.substr((c >> 3) * 2, 2), 16);
        if ((val >> (7 - (c & 7))) & 1) rects += `<rect x="${penX + g.xoff + c}" y="${top + r}" width="1" height="1"/>`;
      }
    }
    penX += g.dx != null ? g.dx : g.w + 1;
  }
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${penX} ${height}" fill="currentColor" shape-rendering="crispEdges">${rects}</svg>`;
}

const mc8 = parseBDF("/tmp/bdf/mc8.bdf", null);
fs.writeFileSync("specimens/matrixchunky8-upper.svg", rowSVG("ABCDEFGHIJKLMNOPQRSTUVWXYZ", mc8));
fs.writeFileSync("specimens/matrixchunky8-lower.svg", rowSVG("abcdefghijklmnopqrstuvwxyz", mc8));
fs.writeFileSync("specimens/matrixchunky8-num.svg", rowSVG("0123456789 .,:!?@#", mc8));

const sample = "aesthetic computer";
const sample2 = "日本語 中文 한글 Ωμ →★";
const wanted = new Set([...(sample + sample2)].map((c) => c.codePointAt(0)));
const uni = parseBDF("/tmp/bdf/unifont.bdf", wanted);
fs.writeFileSync("specimens/unifont-latin.svg", rowSVG(sample, uni));
fs.writeFileSync("specimens/unifont-multi.svg", rowSVG(sample2, uni));

console.log("mc8 glyphs:", mc8.size, "· unifont sample glyphs:", uni.size);
