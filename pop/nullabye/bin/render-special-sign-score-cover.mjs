#!/usr/bin/env node
// Render Special Sign's complete scorodeon data as one radial graphic score.
// Time wraps clockwise; each colored orbit is one physical sound body, with
// the Jeffrey vowel choir as the thirteenth ring. The cover and moving score
// therefore share one authored event dataset instead of merely sharing style.

import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, registerFont } from "canvas";
import { magickRenderText } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? resolve(process.argv[i + 1]) : fallback;
};
const dataPath = arg("--score", resolve(LANE, "release/special-sign/special-sign.scorodeon.json"));
const outPath = arg("--out", resolve(LANE, "cover/special-sign-radial-score-v1-3000.jpg"));
const S = JSON.parse(readFileSync(dataPath, "utf8"));
const W = 3000, H = 3000, CX = W / 2, CY = 1605;
const CREAM = "#f7f4eb", INK = "#202127", MUTED = "#797a80", RED = "#b3402e";
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
try { registerFont("/System/Library/Fonts/Menlo.ttc", { family: "MenloScore" }); } catch {}
const mono = (px) => `${px}px MenloScore, monospace`;
const TAU = Math.PI * 2;
const angle = (t) => -Math.PI / 2 + (t / S.dur) * TAU;
const polar = (r, a) => [CX + Math.cos(a) * r, CY + Math.sin(a) * r];

ctx.fillStyle = CREAM;
ctx.fillRect(0, 0, W, H);

// Quiet score-paper field: registration lines and sparse print flecks.
ctx.strokeStyle = "rgba(32,33,39,.055)";
ctx.lineWidth = 2;
for (let x = 120; x < W; x += 120) {
  ctx.beginPath(); ctx.moveTo(x, 440); ctx.lineTo(x, 2740); ctx.stroke();
}
for (let y = 480; y < 2740; y += 120) {
  ctx.beginPath(); ctx.moveTo(110, y); ctx.lineTo(W - 110, y); ctx.stroke();
}
let seed = 0x51a1c0de;
const rand = () => { seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; return (seed >>> 0) / 0xffffffff; };
ctx.fillStyle = "rgba(70,61,50,.08)";
for (let i = 0; i < 1500; i++) {
  const x = rand() * W, y = rand() * H, r = 0.35 + rand() * 1.2;
  ctx.beginPath(); ctx.arc(x, y, r, 0, TAU); ctx.fill();
}

const work = mkdtempSync(join(tmpdir(), "special-sign-cover-"));
try {
  const title = await magickRenderText("Special Sign", {
    ptSize: 196, fill: INK, outPath: resolve(work, "title.png"),
  });
  const scale = Math.min(1, 1320 / title.width);
  const tw = title.width * scale, th = title.height * scale;
  ctx.drawImage(title, CX - tw / 2, 88, tw, th);
} finally {
  rmSync(work, { recursive: true, force: true });
}
ctx.textAlign = "center";
ctx.fillStyle = MUTED;
ctx.font = mono(46);
ctx.fillText("AESTHETIC DOT COMPUTER · PIXSIES", CX, 382);

// Movement and spin halos outside the authored lanes.
S.movements.forEach((movement, index) => {
  ctx.strokeStyle = index === 2 ? "rgba(179,64,46,.60)" : `rgba(62,68,112,${0.13 + movement.level * 0.22})`;
  ctx.lineWidth = index === 2 ? 56 : 34;
  ctx.beginPath();
  ctx.arc(CX, CY, 1105, angle(movement.t0) + 0.006, angle(movement.t1) - 0.006);
  ctx.stroke();
  const mid = angle((movement.t0 + movement.t1) / 2);
  const [lx, ly] = polar(1175, mid);
  ctx.save();
  ctx.translate(lx, ly);
  let rot = mid + Math.PI / 2;
  if (rot > Math.PI / 2 && rot < Math.PI * 1.5) rot += Math.PI;
  ctx.rotate(rot);
  ctx.fillStyle = "rgba(32,33,39,.66)";
  ctx.font = mono(25);
  ctx.fillText(movement.name.replace(/^[IVX]+ · /, ""), 0, 0);
  ctx.restore();
});

// Chord rail: one segmented orbit immediately outside the sound bodies.
ctx.font = mono(26);
S.chords.forEach((chord, index) => {
  const a0 = angle(chord.t) + 0.005, a1 = angle(chord.t + chord.dur) - 0.005;
  ctx.strokeStyle = index >= S.chords.length - 2 ? "rgba(179,64,46,.78)" : "rgba(62,68,112,.30)";
  ctx.lineWidth = 28;
  ctx.beginPath(); ctx.arc(CX, CY, 1017, a0, a1); ctx.stroke();
  if (index % 4 === 0 || index >= S.chords.length - 2) {
    const [x, y] = polar(1018, (a0 + a1) / 2);
    ctx.fillStyle = CREAM;
    ctx.fillText(chord.name, x, y + 9);
  }
});

// Thirteen literal score orbits. Pitch moves within a lane; gain thickens the
// stroke; duration is angular length. Tiny clock/percussion events remain dots.
const inner = 344, outer = 945;
const laneGap = (outer - inner) / Math.max(1, S.lanes.length - 1);
S.lanes.forEach((lane, laneIndex) => {
  const pitches = lane.events.map((event) => event.pitch).filter(Number.isFinite);
  const pLo = pitches.length ? Math.min(...pitches) : 0;
  const pHi = pitches.length ? Math.max(...pitches) : 1;
  const base = inner + laneIndex * laneGap;
  ctx.strokeStyle = lane.color;
  ctx.globalAlpha = 0.12;
  ctx.lineWidth = 2;
  ctx.beginPath(); ctx.arc(CX, CY, base, 0, TAU); ctx.stroke();
  for (const event of lane.events) {
    const pitchMix = Number.isFinite(event.pitch) ? (event.pitch - pLo) / Math.max(1, pHi - pLo) - 0.5 : 0;
    const radius = base + pitchMix * laneGap * 0.72;
    const a0 = angle(event.t);
    const a1 = Math.max(a0 + 0.004, angle(Math.min(S.dur, event.t + event.dur)));
    ctx.globalAlpha = 0.27 + Math.min(0.60, (event.g ?? 0.2) * 0.85);
    ctx.lineWidth = 3.5 + (event.g ?? 0.2) * 16;
    ctx.lineCap = event.dur < 0.25 ? "round" : "butt";
    ctx.beginPath(); ctx.arc(CX, CY, radius, a0, a1); ctx.stroke();
  }
  ctx.globalAlpha = 1;
});

// Actual locked-master dynamic arc, wrapped around the complete notation.
ctx.strokeStyle = "rgba(62,68,112,.82)";
ctx.lineWidth = 6;
ctx.beginPath();
for (let i = 0; i < S.arc.length; i++) {
  const a = angle((i / (S.arc.length - 1)) * S.dur);
  const r = 1060 + S.arc[i] * 42;
  const [x, y] = polar(r, a);
  i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y);
}
ctx.closePath(); ctx.stroke();

// A sign at the middle: the score's scale and the central super-spin axis.
ctx.fillStyle = "rgba(247,244,235,.94)";
ctx.beginPath(); ctx.arc(CX, CY, 252, 0, TAU); ctx.fill();
ctx.strokeStyle = RED; ctx.lineWidth = 7;
ctx.beginPath(); ctx.arc(CX, CY, 232, 0, TAU); ctx.stroke();
ctx.strokeStyle = "rgba(179,64,46,.25)";
for (const r of [184, 144, 104]) { ctx.beginPath(); ctx.arc(CX, CY, r, 0, TAU); ctx.stroke(); }
ctx.fillStyle = INK;
ctx.font = mono(54);
const eventCount = S.lanes.reduce((n, lane) => n + lane.events.length, 0);
ctx.fillText(`${eventCount.toLocaleString("en-US")} EVENTS`, CX, CY - 38);
ctx.font = mono(31);
ctx.fillStyle = MUTED;
ctx.fillText("12 SOUND BODIES + HUMAN CHOIR", CX, CY + 20);
ctx.fillText("76 BPM · C MAJOR · 1:41.375", CX, CY + 68);

// Compact lane key at the foot; the cover can be read back into its score.
const cols = 7, colW = 385, keyX = CX - (cols * colW) / 2 + 30;
ctx.font = mono(27);
ctx.textAlign = "left";
S.lanes.forEach((lane, index) => {
  const row = Math.floor(index / cols), col = index % cols;
  const x = keyX + col * colW, y = 2800 + row * 58;
  ctx.fillStyle = lane.color;
  ctx.fillRect(x, y - 19, 34, 18);
  ctx.fillStyle = INK;
  ctx.fillText(lane.name.toUpperCase(), x + 48, y);
});
ctx.textAlign = "center";
ctx.fillStyle = "rgba(32,33,39,.55)";
ctx.font = mono(24);
ctx.fillText("COMPLETE RADIAL SCORE · CLOCKWISE FROM TWELVE", CX, 2960);

mkdirSync(dirname(outPath), { recursive: true });
const ext = extname(outPath).toLowerCase();
writeFileSync(outPath, ext === ".jpg" || ext === ".jpeg"
  ? canvas.toBuffer("image/jpeg", { quality: 0.96, progressive: true })
  : canvas.toBuffer("image/png"));
console.log(`✓ ${outPath} · ${W}×${H} · ${S.lanes.reduce((n, lane) => n + lane.events.length, 0)} events`);
