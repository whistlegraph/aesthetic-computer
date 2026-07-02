#!/usr/bin/env node
// render-cutezip.mjs — cutezip · cutezen's zippy sibling: a 2:00 bouncy
// carved-noise groove (132 BPM · 66 bars exactly · C major pentatonic),
// all DSP through pop/nullabye/c/nullnoise.c, techno master.
//
// Same law as cutezen: two phase-cancelled pink-noise copies, notes carved
// by EQ bells — the kick is a 115→46 Hz pitch-drop bell, the clap is a
// wide 2.3 kHz bell (the clap IS the noise), hats are narrow 8.2 kHz
// ticks, the bass is one monophonic EQ point with octave glides, and the
// hook is the Q-80 noise-whistle up top. Pads ride a parallel group.
//
//   node pop/cutezen/bin/render-cutezip.mjs          → out/cutezip.score.txt
//   node pop/nullabye/c/run-c.mjs pop/cutezen/out/cutezip.score.txt \
//     --out pop/cutezen/out/cutezip.mp3 --master techno
import { mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

const BPM = 160, BARS = 80;
const BEAT = 60 / BPM, BAR = BEAT * 4;
const DUR = BARS * BAR; // 120.000 s
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// C major pentatonic
const N = { C2: 65.41, G2: 98, C3: 130.81, C4: 261.63, C5: 523.25, D5: 587.33, E5: 659.26, G5: 783.99, A5: 880, C6: 1046.5, D6: 1174.7, E6: 1318.5, G6: 1568, A6: 1760, C7: 2093 };

// form (bar ranges, inclusive start / exclusive end)
const GROOVE = [8, 28], HOOK = [28, 48], BREAK = [48, 56], DROP = [56, 76];
const beatOn = (bar) => (bar >= GROOVE[0] && bar < BREAK[0]) || (bar >= DROP[0] && bar < DROP[1]);

function band(events) {
  const s = [...events].sort((a, b) => a.t0 - b.t0);
  return s.map((e, i) => {
    const cut = e.ring ? -1 : (s[i + 1] ? s[i + 1].t0 : -1);
    return `${e.t0.toFixed(4)} ${e.f} ${e.fEnd ?? 0} ${e.db} ${e.q} ${e.atk} ${e.hold} ${e.rel} ${cut}`;
  });
}

// ── kick — pitch-drop bell, four on the floor (≤ +31 dB house rule) ───
const kick = [];
for (let b = 0; b < BARS; b++) {
  if (!beatOn(b)) continue;
  const db = b < GROOVE[0] + 2 ? 26 : 30; // ease in
  for (let k = 0; k < 4; k++)
    kick.push({ t0: t(b, k), f: 115, fEnd: 46, db, q: 4.5, atk: 0.004, hold: 0.035, rel: 0.14 });
}

// ── sub — offbeat C2 "oontz" ──────────────────────────────────────────
const sub = [];
for (let b = 0; b < BARS; b++) {
  if (!beatOn(b)) continue;
  for (let k = 0; k < 4; k++)
    sub.push({ t0: t(b, k + 0.5), f: N.C2, db: 24, q: 14, atk: 0.01, hold: 0.14, rel: 0.12 });
}

// ── clap — the noise itself, wide 2.3 kHz on 2 + 4 ────────────────────
const clap = [];
for (let b = GROOVE[0]; b < DROP[1]; b++) {
  if (!beatOn(b)) continue;
  for (const k of [1, 3])
    clap.push({ t0: t(b, k), f: 2300, db: 13, q: 2.2, atk: 0.003, hold: 0.04, rel: 0.09 });
}

// ── hats — narrow 8.2 kHz ticks: driving straight 8ths, offbeat accent
// rides longer ("open"), 16ths in the drop ────────────────────────────
const hat = [];
for (let b = 2; b < DROP[1]; b++) {
  if (b >= BREAK[0] && b < BREAK[1]) continue;
  const sixteenths = b >= DROP[0];
  for (let k = 0; k < (sixteenths ? 16 : 8); k++) {
    const off = sixteenths ? k / 4 : k / 2;
    const accent = sixteenths ? k % 4 === 2 : k % 2 === 1;
    hat.push({ t0: t(b, off), f: 8200, db: accent ? 13 : 9, q: 6, atk: 0.002, hold: 0.008, rel: accent ? 0.09 : 0.045 });
  }
}

// ── bass — one mono EQ point, driving 8th-note octave pump ────────────
const bass = [];
for (let b = 4; b < DROP[1]; b++) {
  if (b >= BREAK[0] && b < BREAK[1]) continue;
  for (let k = 0; k < 8; k++) {
    let f = k % 2 === 0 ? N.C2 : N.C3; // low on the beat, octave pop off it
    if (b % 4 === 3 && k >= 6) f = N.G2; // fifth walk into the next bar
    const glide = b % 8 === 7 && k === 7; // slide up into the next phrase
    bass.push({ t0: t(b, k * 0.5), f, fEnd: glide ? N.C3 : 0, db: 26, q: 18, atk: 0.006, hold: 0.1, rel: 0.07 });
  }
}

// ── whistle hook — 2-bar cute bounce, 8va sparkle answers in the drop ─
const W = { db: 26, q: 110, atk: 0.02, rel: 0.12 };
const hookPhrase = [
  [0, N.E6, 0.28], [0.5, N.G6, 0.28], [1, N.A6, 0.28], [1.5, N.G6, 0.45],
  [2.5, N.E6, 0.28], [3, N.D6, 0.28], [3.5, N.C6, 0.65],
  [5, N.C6, 0.28], [5.5, N.D6, 0.28], [6, N.E6, 0.45], [7, N.G6, 0.85],
];
const whistle = [];
for (const start of [HOOK[0], HOOK[0] + 4, HOOK[0] + 8, HOOK[0] + 12, DROP[0], DROP[0] + 4, DROP[0] + 8, DROP[0] + 12]) {
  for (const [beat, f, hold] of hookPhrase)
    whistle.push({ t0: t(start, beat), f, hold, ...W });
}
// break — one long exhale note + bend back in
whistle.push({ t0: t(BREAK[0], 1), f: N.A5, hold: 5.5, db: 24, q: 110, atk: 1.2, rel: 1.8 });
whistle.push({ t0: t(BREAK[1] - 2, 0), f: N.D5, fEnd: N.E5, hold: 3.0, db: 24, q: 110, atk: 0.8, rel: 0.8 });

// ── sparkles — answers an octave up, drop only ────────────────────────
const sparkle = [];
for (const start of [DROP[0] + 2, DROP[0] + 6, DROP[0] + 10, DROP[0] + 14])
  for (const [beat, f] of [[0, N.C7], [1, N.C7 * 1.5], [2.5, N.C7]])
    sparkle.push({ t0: t(start, beat), f, db: 15, q: 90, atk: 0.01, hold: 0.06, rel: 0.4 });

// ── breath riser — the cutezen breath as a build (break + intro) ──────
const breath = [
  { t0: t(0), f: 250, fEnd: 430, db: 8, q: 1.9, atk: 3, hold: 2, rel: 2.5 },
  { t0: t(4), f: 250, fEnd: 430, db: 8, q: 1.9, atk: 3, hold: 2, rel: 2.5 },
  { t0: t(BREAK[0] + 2), f: 240, fEnd: 900, db: 9, q: 1.6, atk: BAR * 4.5, hold: 0.4, rel: 1.2 }, // the riser
];

// ── group B — pads (parallel pair, no serial dB stacking) ─────────────
// C · Am-ish (A) · F-ish (C/A over F is pentatonic-safe: use A + C) · G
const pads = [];
const padProg = [[N.C4, N.G5 / 2], [N.A5 / 2, N.C4], [N.C4, N.A5 / 2], [N.G2 * 4, N.D5]];
for (let b = GROOVE[0]; b < DROP[1]; b += 2) {
  const [f1, f2] = padProg[(b / 2) % 4 | 0];
  const db = b >= BREAK[0] && b < BREAK[1] ? 20 : 17;
  pads.push({ t0: t(b), f: f1, db, q: 60, atk: 0.9, hold: BAR * 2 - 1.6, rel: 0.9 });
  pads.push({ t0: t(b) + 0.02, f: f2 === f1 ? f2 * 2 : f2, db: db - 2, q: 60, atk: 0.9, hold: BAR * 2 - 1.6, rel: 0.9 });
}

const L = [
  "sr 48000",
  `dur ${DUR}`,
  "detune 1.0012 0.9988",
  "seed 48879 12648430",
  "normpeak 0.88",
  "fadein 0.004",
  "fadeout 1.5",
];
for (const ev of [kick, sub, clap, hat, bass, whistle, sparkle, breath]) {
  const lines = band(ev);
  L.push(`band ${lines.length}`, ...lines);
}
L.push("group 3735928559 195894762");
// pads split into two mono lanes (low voice / high voice)
const low = pads.filter((_, i) => i % 2 === 0), high = pads.filter((_, i) => i % 2 === 1);
for (const ev of [low, high]) {
  const lines = band(ev);
  L.push(`band ${lines.length}`, ...lines);
}

const scorePath = resolve(OUT, "cutezip.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
const n = kick.length + sub.length + clap.length + hat.length + bass.length + whistle.length + sparkle.length + breath.length + pads.length;
console.log(`✓ cutezip · ${BPM} bpm · ${BARS} bars · ${DUR.toFixed(3)}s · ${n} carved events → ${scorePath}`);
