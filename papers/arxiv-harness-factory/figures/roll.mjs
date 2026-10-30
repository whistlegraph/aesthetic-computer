import { readFileSync, writeFileSync } from "node:fs";
const [scoreFile, out] = process.argv.slice(2);
const s = JSON.parse(readFileSync(scoreFile, "utf8"));
const STEPS = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
const midi = (p) => { const m = /^([a-g])([#b]?)(-?\d)$/i.exec(p); return 12 * (+m[3] + 1) + STEPS[m[1].toLowerCase()] + (m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0); };
const tones = s.notes.filter((n) => n.pitch !== "noise");
const hits = s.notes.filter((n) => n.pitch === "noise");
const lo = Math.min(...tones.map((n) => midi(n.pitch))) - 1, hi = Math.max(...tones.map((n) => midi(n.pitch))) + 1;
const end = Math.max(...s.notes.map((n) => n.at + n.beats));
const W = 1600, L = 140, R = 30, T = 30, rowH = 13, H0 = (hi - lo + 1) * rowH, laneGap = 26, laneH = 34, B = 92;
const H = T + H0 + laneGap + laneH + B;
const X = (b) => L + (b / end) * (W - L - R);
const Y = (m) => T + (hi - m) * rowH;
const COL = { triangle: "#b44887", sine: "#7850b4", saw: "#2a8a82", noise: "#40384a" };
const names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
let g = `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" font-family="Helvetica, Arial, sans-serif"><rect width="${W}" height="${H}" fill="#fff"/>`;
for (let m = lo; m <= hi; m++) {
  const black = names[m % 12].includes("#");
  g += `<rect x="${L}" y="${Y(m)}" width="${W - L - R}" height="${rowH}" fill="${black ? "#f2eff5" : "#fff"}"/>`;
  if (m % 12 === 0 || m % 12 === 9) g += `<text x="${L - 10}" y="${Y(m) + rowH - 2}" font-size="27" text-anchor="end" fill="#555">${names[m % 12]}${Math.floor(m / 12) - 1}</text>`;
}
for (let b = 0; b <= end; b += 4) { const last = b === end;
  g += `<line x1="${X(b)}" y1="${T}" x2="${X(b)}" y2="${T + H0 + laneGap + laneH}" stroke="${b % 16 ? "#ddd" : "#999"}" stroke-width="${b % 16 ? 1 : 2}"/>`;
  if (!last) g += `<text x="${X(b)}" y="${T + H0 + laneGap + laneH + 36}" font-size="28" text-anchor="middle" fill="#444">${b / 4 + 1}</text>`;
}
for (const n of tones) g += `<rect x="${X(n.at) + 1}" y="${Y(midi(n.pitch)) + 1}" width="${Math.max(3, X(n.at + n.beats) - X(n.at) - 2)}" height="${rowH - 2}" rx="2" fill="${COL[n.wave || "sine"]}"/>`;
const ly = T + H0 + laneGap;
g += `<rect x="${L}" y="${ly}" width="${W - L - R}" height="${laneH}" fill="#f7f7f7"/><text x="${L - 10}" y="${ly + 24}" font-size="27" text-anchor="end" fill="#555">noise</text>`;
for (const n of hits) g += `<rect x="${X(n.at) + 1}" y="${ly + 5}" width="${Math.max(4, X(n.at + n.beats) - X(n.at) - 2)}" height="${laneH - 10}" rx="2" fill="${COL.noise}"/>`;
g += `<text x="${(L + W - R) / 2}" y="${H - 8}" font-size="28" text-anchor="middle" fill="#444">bar (4 beats each, ${s.bpm} bpm)</text>`;
const leg = [["triangle", "melody · triangle"], ["sine", "pad · sine"], ["saw", "bass · saw"]];
let lx = W - R - 720;
for (const [w, label] of leg) { g += `<rect x="${lx}" y="${T + 6}" width="22" height="20" rx="2" fill="${COL[w]}"/><text x="${lx + 30}" y="${T + 24}" font-size="27" fill="#333">${label}</text>`; lx += 30 + label.length * 15 + 40; }
g += `</svg>`;
writeFileSync(out, g);
console.log({ lo, hi, end, tones: tones.length, hits: hits.length, W, H });
