#!/usr/bin/env node
// build-text.mjs — per-letter animated invite text.
// each glyph: chunky Rockwell slab, its own party color, a sharp (un-blurred)
// drop shadow. letters bounce in one-by-one (damped cosine), then hold.
// emits out/.text/compose.sh — an ffmpeg command that overlays the animated
// letters onto the motion clip and muxes the flutterbap soundtrack.

import { execSync } from "node:child_process";
import { writeFileSync, mkdirSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dir = dirname(fileURLToPath(import.meta.url));
const REPO = join(__dir, "..", "..");
const OUT = join(__dir, "out");
const TXT = join(OUT, ".text");
mkdirSync(TXT, { recursive: true });

const FONT = "/System/Library/Fonts/Supplemental/Rockwell.ttc";
const SHADOW = "#2a1020"; // deep plum, sharp
const DX = 6, DY = 7;     // sharp drop-shadow offset
const W = 720, H = 1280;

// fun party palette, cycled per glyph
const PAL = ["#FF3B6B", "#FF8A1E", "#FFD23F", "#36D399", "#34C3FF", "#9B6BFF", "#FF5FA2", "#7BE04A"];

// lines: text, pointsize, extra tracking (px between glyphs), top-y, entrance fx
//   fx ∈ type | bounce | slideL | slideR | rise — one distinct effect per line
const LINES = [
  { t: "YOU'RE INVITED TO", size: 34, track: 10, y: 792,  fx: "type"   },
  { t: "FIA'S",            size: 118, track: 6,  y: 826,  fx: "bounce" },
  { t: "BIRTHDAY",         size: 92,  track: 4,  y: 952,  fx: "slideL" },
  { t: "EDENDALE",         size: 50,  track: 12, y: 1060, fx: "slideR" },
  { t: "JULY 3RD · 8–11PM", size: 40, track: 8, y: 1126, fx: "rise"   },
];

const sh = (c) => execSync(c, { encoding: "utf8" }).trim();

// render one glyph (color fill + sharp shadow), return {png, w, h}
let gid = 0;
function glyph(ch, size, color) {
  const fg = join(TXT, `.fg.png`);
  const shp = join(TXT, `.sh.png`);
  const chFile = join(TXT, `.ch.txt`);
  writeFileSync(chFile, ch); // feed glyph via file → no shell-quoting hazards
  const common = `-background none -font "${FONT}" -pointsize ${size}`;
  sh(`magick ${common} -fill "${color}" label:@"${chFile}" -trim +repage "${fg}"`);
  sh(`magick ${common} -fill "${SHADOW}" label:@"${chFile}" -trim +repage "${shp}"`);
  const [w, h] = sh(`magick identify -format "%w %h" "${fg}"`).split(" ").map(Number);
  const cw = w + DX, chh = h + DY;
  const out = join(TXT, `g${String(gid).padStart(3, "0")}.png`);
  sh(`magick -size ${cw}x${chh} xc:none "${shp}" -geometry +${DX}+${DY} -composite "${fg}" -geometry +0+0 -composite "${out}"`);
  gid++;
  return { png: out, w, h }; // advance by fg width w
}

// scrim — gentle dark gradient at the bottom for legibility
const scrim = join(TXT, "scrim.png");
sh(`magick -size 720x650 gradient:none-black -channel A -evaluate multiply 0.66 +channel "${scrim}"`);
sh(`magick -size ${W}x${H} xc:none "${scrim}" -gravity South -composite "${scrim}"`);

// layout + timing
const placements = [];
let colorIdx = 0;
const T0 = 0.45;      // first letter start
const STEP = 0.045;   // stagger between letters
let order = 0;
for (const line of LINES) {
  // measure advances first
  const chars = [...line.t];
  const rendered = chars.map((ch) => {
    if (ch === " ") return { space: true, adv: Math.round(line.size * 0.34) };
    const color = PAL[colorIdx++ % PAL.length];
    const g = glyph(ch, line.size, color);
    return { ...g, adv: g.w + line.track };
  });
  const lineW = rendered.reduce((s, r) => s + r.adv, 0) - line.track;
  let x = Math.round((W - lineW) / 2);
  for (const r of rendered) {
    if (!r.space) {
      placements.push({ png: r.png, x, y: line.y, t: (T0 + order * STEP).toFixed(3), fx: line.fx });
      order++;
    }
    x += r.adv;
  }
}

console.log(`rendered ${gid} glyphs, ${placements.length} placed`);

// static layout preview over the still (landed state) for quick QA
{
  const still = join(OUT, "fia-felt-A-v2.png");
  const prev = join(TXT, "layout-preview.png");
  let c = `magick "${still}" -resize ${W}x${H}^ -gravity center -extent ${W}x${H} -gravity NorthWest "${scrim}" -geometry +0+0 -composite`;
  for (const p of placements) c += ` "${p.png}" -geometry +${p.x}+${p.y} -composite`;
  c += ` "${prev}"`;
  sh(c);
  console.log("preview →", prev);
}

// build ffmpeg compose.sh
const DUR = 14; // two 7s scenes
const MOTION = join(OUT, "fia-felt-combined.mp4"); // clip-A + clip-B concatenated
const AUDIO = join(REPO, "pop/marimba/out/flutterbap.mp3");
const FINAL = join(OUT, "fia-birthday-invite.mp4");

// per-line entrance fx → overlay x/y expressions (dt = t - start). no commas
// allowed in ffmpeg expressions, so these use only exp/cos.
function fxExpr(p) {
  const dt = `(t-${p.t})`;
  switch (p.fx) {
    case "type":   return { x: `${p.x}`, y: `${p.y}`, fade: 0.10 };
    case "slideL": return { x: `${p.x} - 360*exp(-11*${dt})`, y: `${p.y}`, fade: 0.12 };
    case "slideR": return { x: `${p.x} + 360*exp(-11*${dt})`, y: `${p.y}`, fade: 0.12 };
    case "rise":   return { x: `${p.x}`, y: `${p.y} + 80*exp(-11*${dt})`, fade: 0.14 };
    case "bounce":
    default:       return { x: `${p.x}`, y: `${p.y} - 55*exp(-9*${dt})*cos(14*${dt})`, fade: 0.16 };
  }
}

const inputs = [`-i "${MOTION}"`, `-loop 1 -i "${scrim}"`];
placements.forEach((p) => inputs.push(`-loop 1 -i "${p.png}"`));
inputs.push(`-ss 6 -t ${DUR} -i "${AUDIO}"`);

// filter graph
const fc = [];
// scrim is input 1, static full-frame, hold all 12s
fc.push(`[0:v][1:v]overlay=0:0:eof_action=repeat[b0]`);
placements.forEach((p, i) => {
  const inIdx = i + 2;            // glyph inputs start at 2
  const prev = `b${i}`;
  const next = `b${i + 1}`;
  const fade = `f${i}`;
  const e = fxExpr(p);
  fc.push(`[${inIdx}:v]format=rgba,fade=t=in:st=${p.t}:d=${e.fade}:alpha=1[${fade}]`);
  fc.push(`[${prev}][${fade}]overlay=x=${e.x}:y=${e.y}:eof_action=repeat[${next}]`);
});
const vlabel = `b${placements.length}`;
const aIdx = placements.length + 2;
fc.push(`[${aIdx}:a]afade=t=in:st=0:d=1.2,afade=t=out:st=${DUR - 1.4}:d=1.4,volume=0.95[a]`);

const cmd = [
  `ffmpeg -y -v error`,
  inputs.join(" "),
  `-filter_complex "${fc.join(";")}"`,
  `-map "[${vlabel}]" -map "[a]" -t ${DUR}`,
  `-c:v libx264 -pix_fmt yuv420p -crf 19 -preset medium -movflags +faststart`,
  `-c:a aac -b:a 192k`,
  `"${FINAL}"`,
].join(" ");

const composeSh = join(TXT, "compose.sh");
writeFileSync(composeSh, "#!/bin/bash\nset -e\n" + cmd + "\necho \"✓ " + FINAL + "\"\n");
console.log("wrote", composeSh);
