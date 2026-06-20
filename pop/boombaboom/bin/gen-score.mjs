#!/usr/bin/env node
// gen-score.mjs — graphic score for boombaboom. A timeline of the arrangement
// (sections, every voice's active region + intensity, and the key events) so
// the structure reads at a glance. Constants MIRROR c/boombaboom.c — if the
// engine changes, change them here too.
//
// Output: pop/boombaboom/out/boombaboom-score.png (3200×1800) + a copy on the
//         Neo ~/Desktop.
//
// Usage: node pop/boombaboom/bin/gen-score.mjs

import { writeFileSync, mkdirSync, copyFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { createCanvas } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

// ── engine constants (must match c/boombaboom.c) ──────────────────────
const BPM = 140, BEAT = 60 / BPM, BAR = BEAT * 4;
const PRELUDE = 8, INTRO = 8, PASS = 20, BR = 8, OUTRO = 10;
const DROP = PRELUDE + INTRO;
const passBar = [DROP, DROP + PASS + BR, DROP + PASS + BR + PASS + BR];
const TB = passBar[2] + PASS + OUTRO;
const KICKIN = DROP;                            // kick hits strong on the first BOOM
const TOTAL = TB * BAR;                         // seconds
const fmt = (s) => `${Math.floor(s / 60)}:${String(Math.round(s % 60)).padStart(2, "0")}`;

const sections = [
  { name: "prelude",    a: 0,          b: PRELUDE,           note: "lost-vocal haze" },
  { name: "build",      a: PRELUDE,    b: DROP,              note: "sweep rising" },
  { name: "pass 1",     a: passBar[0], b: passBar[0] + PASS, note: "DROP · strong kick on 1st boom" },
  { name: "bridge",     a: passBar[0] + PASS, b: passBar[1], note: "breakdown · osc" },
  { name: "pass 2",     a: passBar[1], b: passBar[1] + PASS, note: "harder · power sines build" },
  { name: "bridge",     a: passBar[1] + PASS, b: passBar[2], note: "breakdown · osc" },
  { name: "pass 3",     a: passBar[2], b: passBar[2] + PASS, note: "hardest · octave LOWER ending" },
  { name: "outro",      a: passBar[2] + PASS, b: TB,         note: "tail" },
];

// lanes: each is a set of {a,b (bars), level 0..1}. Vocal/harmony levels mirror
// the per-pass gains; bed levels mirror the intensity ramp.
const P = passBar.map((pb, i) => ({ a: pb, b: pb + PASS, p: i }));
const lanes = [
  { key: "lead",    label: "VOCAL lead",      color: "#B23A48", segs: P.map(s => ({ ...s, level: 1.0 })) },
  { key: "oct",     label: "harm · octave",   color: "#C9748A", segs: P.map(s => ({ ...s, level: [0.70,0.90,0.28][s.p] })) },
  { key: "fifth",   label: "harm · fifth",    color: "#C9748A", segs: P.map(s => ({ ...s, level: [0.50,0.85,0.85][s.p] })) },
  { key: "third",   label: "harm · third",    color: "#C9748A", segs: P.map(s => ({ ...s, level: [0.0,0.55,0.80][s.p] })).filter(s => s.level > 0) },
  { key: "octdn",   label: "harm · OCT DOWN", color: "#7A4FA3", segs: [{ a: passBar[2], b: passBar[2] + PASS, level: 1.0 }] },
  { key: "swarm",   label: "healing swarm (432Hz, voice-energy)", color: "#2A8C82", segs: [{ a: 0, b: TB, level: 0.6 }] },
  { key: "power",   label: "power sines (build)", color: "#3A6EA5", segs: [{ a: passBar[1], b: TB, level: 0.0, ramp: true }] },
  { key: "osc",     label: "osc wobble (lost-vocal/bridges)", color: "#C08A2D", segs: [{ a: 0, b: PRELUDE, level: 0.55 }, { a: passBar[0]+PASS, b: passBar[0]+PASS+3, level: 0.8 }, { a: passBar[1]+PASS, b: passBar[1]+PASS+3, level: 0.8 }] },
  { key: "kick",    label: "sine kick (fade-in)", color: "#1E1E1E", segs: P.map(s => ({ a: Math.max(s.a, s.p===0?KICKIN:s.a), b: s.b, level: [0.85,1.16,1.32][s.p] })) },
  { key: "hats",    label: "hats (offbeat)",  color: "#6B6B6B", segs: P.map(s => ({ a: Math.max(s.a, s.p===0?KICKIN:s.a), b: s.b, level: [0.40,0.54,0.68][s.p] })) },
  { key: "bells",   label: "sine bells (arp)",color: "#6E5A8E", segs: [{ a: 0, b: TB, level: 0.6 }] },
  { key: "chord",   label: "chord pad (Dm·Bb·F·C)", color: "#8E7CC3", segs: [{ a: 0, b: TB, level: 0.5 }] },
  { key: "sub",     label: "sub-bell (root)", color: "#4A4A4A", segs: [{ a: 0, b: TB, level: 0.5 }] },
];

const events = [
  { bar: KICKIN, label: "kick in + sweep pops" },
  { bar: passBar[1], label: "go harder" },
  { bar: passBar[1] + 8, label: "power sines" },
  { bar: passBar[2], label: "octave-lower ending" },
];

// ── canvas ─────────────────────────────────────────────────────────────
const W = 3200, H = 1800, ML = 540, MR = 70, MT = 230, MB = 120;
const PW = W - ML - MR, PH = H - MT - MB;
const cv = createCanvas(W, H); const g = cv.getContext("2d");
const CREAM = "#FBFAF6", INK = "#1E1E1E";
const x = (bar) => ML + (bar / TB) * PW;
const laneH = PH / lanes.length;
const y = (i) => MT + i * laneH;

g.fillStyle = CREAM; g.fillRect(0, 0, W, H);

// title block
g.fillStyle = INK;
g.font = "700 78px Georgia, serif"; g.fillText("boombaboom", ML, 118);
g.font = "400 34px Georgia, serif";
g.fillStyle = "#5A5A5A";
g.fillText(`graphic score · D minor · ${BPM} BPM · ${fmt(TOTAL)} · ${TB} bars · sine bells + sine kick + the voice as a healing swarm`, ML, 168);

// section bands
const secColors = ["#EDE6F2", "#F3E2E4", "#E6EFE9", "#F1E2E4", "#E6EFE9", "#EAE0F0", "#ECECE8"];
for (let i = 0; i < sections.length; i++) {
  const s = sections[i];
  const xa = x(s.a), xb = x(s.b);
  g.fillStyle = secColors[i % secColors.length];
  g.fillRect(xa, MT, xb - xa, PH);
  g.strokeStyle = "rgba(30,30,30,0.18)"; g.lineWidth = 1.5;
  g.beginPath(); g.moveTo(xa, MT); g.lineTo(xa, MT + PH); g.stroke();
  g.fillStyle = INK; g.font = "700 30px Georgia, serif";
  g.fillText(s.name, xa + 12, MT - 64);
  g.fillStyle = "#6A6A6A"; g.font = "400 21px Georgia, serif";
  g.fillText(s.note, xa + 12, MT - 36);
  g.fillStyle = "#9A9A9A"; g.font = "400 19px monospace";
  g.fillText(fmt(s.a * BAR), xa + 12, MT - 10);
}

// lane rows
for (let i = 0; i < lanes.length; i++) {
  const L = lanes[i], yy = y(i);
  g.strokeStyle = "rgba(30,30,30,0.08)"; g.lineWidth = 1;
  g.beginPath(); g.moveTo(ML, yy + laneH); g.lineTo(ML + PW, yy + laneH); g.stroke();
  g.fillStyle = INK; g.font = "500 27px Georgia, serif";
  g.textAlign = "right"; g.fillText(L.label, ML - 22, yy + laneH / 2 + 9); g.textAlign = "left";
  for (const seg of L.segs) {
    const xa = x(seg.a), xb = x(seg.b), barH = laneH - 26;
    if (seg.ramp) {                         // a triangle ramp (build)
      g.fillStyle = L.color + "DD";
      g.beginPath(); g.moveTo(xa, yy + laneH - 13);
      g.lineTo(xb, yy + 13); g.lineTo(xb, yy + laneH - 13); g.closePath(); g.fill();
    } else {
      const lvl = seg.level ?? 1;
      const h = Math.max(8, barH * lvl);
      g.globalAlpha = 0.30 + 0.6 * lvl;
      g.fillStyle = L.color;
      g.fillRect(xa + 2, yy + (laneH - h) / 2, xb - xa - 4, h);
      g.globalAlpha = 1;
    }
  }
}

// event markers (dashed verticals + labels)
g.setLineDash([8, 8]);
for (const e of events) {
  const xe = x(e.bar);
  g.strokeStyle = "rgba(178,58,72,0.7)"; g.lineWidth = 2;
  g.beginPath(); g.moveTo(xe, MT); g.lineTo(xe, MT + PH); g.stroke();
  g.fillStyle = "#B23A48"; g.font = "600 20px monospace";
  g.save(); g.translate(xe + 6, MT + PH - 12); g.rotate(-Math.PI / 2);
  g.fillText(`▸ ${e.label}`, 0, 0); g.restore();
}
g.setLineDash([]);

// time axis (every 16 bars ≈ 27.4s)
g.fillStyle = "#8A8A8A"; g.font = "400 19px monospace"; g.textAlign = "center";
for (let bar = 0; bar <= TB; bar += 8) {
  g.fillText(fmt(bar * BAR), x(bar), MT + PH + 36);
}
g.textAlign = "left";
g.fillStyle = "#9A9A9A"; g.font = "400 20px Georgia, serif";
g.fillText("kick fades in after the voice · booms→beats, 'ma'→bars · harmonies build then drop an octave for the ending · swarm tracks vocal energy as a deep echo",
  ML, H - 44);

const png = resolve(OUT, "boombaboom-score.png");
writeFileSync(png, cv.toBuffer("image/png"));
const desk = resolve(homedir(), "Desktop", "boombaboom-score.png");
try { copyFileSync(png, desk); } catch {}
console.log(`✓ ${png}`);
console.log(`✓ ${desk}`);
