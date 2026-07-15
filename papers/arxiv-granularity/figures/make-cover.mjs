// Generates the cover for "Gaps in Granularity".
// The thesis, drawn: one expressive continuous gesture (the scratch) versus
// the coarse staircase a speed menu quantizes it to — and the gap between
// them, measured. AC palette, print-crisp, wordless (the .tex sets the title).
import { writeFileSync } from "node:fs";

const W = 1600, H = 1600;

// --- palette ---
const pink = "#b44887", purple = "#7850b4", dark = "#40384a";
const gray = "#8a8390", green = "#46c85a", paper = "#fdfdfb";

// --- rate axis: screen y for each playback rate (higher rate = higher up) ---
const X0 = 200, X1 = 1360;         // horizontal working span (time)
const levels = [                    // the itemized menu, top to bottom
  { r: "2×",   y: 560 },
  { r: "1.5×", y: 720 },
  { r: "1×",   y: 880 },
  { r: "0.5×", y: 1040 },
];
const ys = levels.map((l) => l.y);
const nearestLevelY = (y) => ys.reduce((a, b) => (Math.abs(b - y) < Math.abs(a - y) ? b : a));

// --- the continuous gesture: an expressive wander a hand actually makes ---
// smooth, between roughly 2x and 0.5x, deliberately not landing on the presets.
const gesture = (t) => {
  // t in [0,1] across the span. Combine a few sines for an organic scratch.
  const base = 800
    - 300 * Math.sin(t * Math.PI * 1.15 + 0.3)
    - 120 * Math.sin(t * Math.PI * 3.1 + 1.0)
    - 45  * Math.sin(t * Math.PI * 6.7 + 0.2);
  return Math.max(500, Math.min(1090, base));
};

const N = 260;
const pts = [];
for (let i = 0; i <= N; i++) {
  const t = i / N;
  pts.push({ x: X0 + t * (X1 - X0), y: gesture(t), t });
}

// smooth curve path (the gesture)
const curvePath = "M " + pts.map((p) => `${p.x.toFixed(1)} ${p.y.toFixed(1)}`).join(" L ");

// staircase (the menu quantizes the gesture to the nearest preset tread)
let stair = "";
let prevY = null;
for (const p of pts) {
  const qy = nearestLevelY(p.y);
  if (prevY === null) stair += `M ${p.x.toFixed(1)} ${qy}`;
  else if (qy !== prevY) stair += ` L ${p.x.toFixed(1)} ${prevY} L ${p.x.toFixed(1)} ${qy}`;
  else stair += ` L ${p.x.toFixed(1)} ${qy}`;
  prevY = qy;
}

// gap connectors: the error between intent and menu, sampled and measured
let gaps = "";
const dots = [];
for (let i = 8; i < N; i += 12) {
  const p = pts[i];
  const qy = nearestLevelY(p.y);
  if (Math.abs(qy - p.y) < 14) continue;
  gaps += `<line x1="${p.x.toFixed(1)}" y1="${p.y.toFixed(1)}" x2="${p.x.toFixed(1)}" y2="${qy}" stroke="${pink}" stroke-width="3" stroke-opacity="0.55" stroke-dasharray="2 7" stroke-linecap="round"/>`;
  dots.push({ x: p.x, y: p.y });
}

// background record grooves (faint texture, upper-left origin of the gesture)
let grooves = "";
const cx = 250, cy = 300;
for (let r = 40; r < 1500; r += 34) {
  grooves += `<circle cx="${cx}" cy="${cy}" r="${r}" fill="none" stroke="${dark}" stroke-width="1.4" stroke-opacity="0.045"/>`;
}

// level labels + faint tread guides
let axis = "";
for (const l of levels) {
  axis += `<line x1="${X0}" y1="${l.y}" x2="${X1}" y2="${l.y}" stroke="${gray}" stroke-width="1.2" stroke-opacity="0.28" stroke-dasharray="1 9"/>`;
  axis += `<text x="${X0 - 26}" y="${l.y + 12}" font-family="'Latin Modern Mono','Courier New',monospace" font-size="34" fill="${gray}" text-anchor="end">${l.r}</text>`;
}

// leading finger/needle dot on the gesture
const head = pts[Math.round(N * 0.5)];

// the menu, echoed as a glyph, lower-right
const menuX = 1090, menuY = 1180, menuW = 300, rowH = 58;
const menuRows = ["0.5×", "1×", "1.25×", "1.5×", "2×"];
let menu = `<rect x="${menuX}" y="${menuY}" width="${menuW}" height="${rowH * menuRows.length + 16}" rx="14" fill="#ffffff" stroke="${gray}" stroke-width="2.4"/>`;
menuRows.forEach((r, i) => {
  const ry = menuY + 8 + i * rowH;
  const sel = r === "1.5×";
  if (sel) menu += `<rect x="${menuX + 6}" y="${ry}" width="${menuW - 12}" height="${rowH}" rx="8" fill="${purple}" fill-opacity="0.14"/>`;
  menu += `<text x="${menuX + 28}" y="${ry + 39}" font-family="'Latin Modern Mono','Courier New',monospace" font-size="34" fill="${sel ? purple : dark}">${r}</text>`;
  if (sel) menu += `<circle cx="${menuX + menuW - 34}" cy="${ry + rowH / 2}" r="7" fill="${purple}"/>`;
});

const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">
  <defs>
    <linearGradient id="ges" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0" stop-color="${pink}"/>
      <stop offset="0.55" stop-color="${purple}"/>
      <stop offset="1" stop-color="${green}"/>
    </linearGradient>
  </defs>
  <rect width="${W}" height="${H}" fill="${paper}"/>
  ${grooves}
  ${axis}
  <path d="${stair}" fill="none" stroke="${gray}" stroke-width="9" stroke-linejoin="round" stroke-linecap="round" stroke-opacity="0.9"/>
  ${gaps}
  <path d="${curvePath}" fill="none" stroke="url(#ges)" stroke-width="12" stroke-linejoin="round" stroke-linecap="round"/>
  ${dots.map((d) => `<circle cx="${d.x.toFixed(1)}" cy="${d.y.toFixed(1)}" r="4.5" fill="${pink}"/>`).join("")}
  <circle cx="${head.x.toFixed(1)}" cy="${head.y.toFixed(1)}" r="26" fill="none" stroke="${pink}" stroke-width="5"/>
  <circle cx="${head.x.toFixed(1)}" cy="${head.y.toFixed(1)}" r="10" fill="#ffffff" stroke="${purple}" stroke-width="4"/>
  ${menu}
</svg>`;

writeFileSync(new URL("./cover.svg", import.meta.url), svg);
console.log("wrote cover.svg");
