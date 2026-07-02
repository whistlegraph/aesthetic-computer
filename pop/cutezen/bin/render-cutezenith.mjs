#!/usr/bin/env node
// render-cutezenith.mjs — cutezenith · the layered peak of the cutezen
// lane: 160 BPM · 96 bars · exactly 2:24 · C major pentatonic · every
// voice family carved from its OWN noise material, summed in parallel:
//
//   pink   — whistle lead + counter-whistle + breath + drone (the singers)
//   brown  — kick, sub, bass, toms, subdrop boom (dark lows, hiss-free)
//   velvet — chord pads + hums (smooth, static-free sustain)
//   white  — hats, clap, arp, sparkles, risers (crisp tops that WANT hiss)
//
// Same law as ever: each pair is noise + its inverted twin — flat EQ =
// bit-exact silence (--proof passes for all four materials). Short intro:
// the hook sings over bass + hats at bar 0, kick lands at bar 4.
//
//   node pop/cutezen/bin/render-cutezenith.mjs      → out/cutezenith.score.txt
//   node pop/nullabye/c/run-c.mjs pop/cutezen/out/cutezenith.score.txt \
//     --out pop/cutezen/out/cutezenith.mp3 --master techno
import { mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

const BPM = 160, BARS = 96;
const BEAT = 60 / BPM, BAR = BEAT * 4;
const DUR = BARS * BAR; // 144.000 s
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

const N = { C2: 65.41, G2: 98, C3: 130.81, C4: 261.63, E4: 329.63, G4: 392, A4: 440, C5: 523.25, D5: 587.33, E5: 659.26, G5: 783.99, A5: 880, C6: 1046.5, D6: 1174.7, E6: 1318.5, G6: 1568, A6: 1760, C7: 2093, E7: 2637, G7: 3136 };
const PENT = [N.C5, N.D5, N.E5, N.G5, N.A5];

// form — SHORT intro (4 bars, hook sings immediately over bass + hats)
const INTRO = [0, 4], GROOVE = [4, 16], HOOK = [16, 32], BREAK = [32, 38],
  DROP = [38, 56], STILL = [56, 60], FINALE = [60, 88], OUTRO = [88, 96];
const beatOn = (b) =>
  (b >= GROOVE[0] && b < BREAK[0]) || (b >= DROP[0] && b < STILL[0]) ||
  (b >= FINALE[0] && b < OUTRO[0] + 4);

function band(events) {
  const s = [...events].sort((a, b) => a.t0 - b.t0);
  return s.map((e, i) => {
    const cut = e.ring ? -1 : (s[i + 1] ? s[i + 1].t0 : -1);
    return `${e.t0.toFixed(4)} ${e.f} ${e.fEnd ?? 0} ${e.db} ${e.q} ${e.atk} ${e.hold} ${e.rel} ${cut}`;
  });
}

// ════ PINK — the singers ═══════════════════════════════════════════════
const W = { db: 26, q: 110, atk: 0.02, rel: 0.12 };
const hookA = [
  [0, N.E6, 0.28], [0.5, N.G6, 0.28], [1, N.A6, 0.28], [1.5, N.G6, 0.45],
  [2.5, N.E6, 0.28], [3, N.D6, 0.28], [3.5, N.C6, 0.65],
  [5, N.C6, 0.28], [5.5, N.D6, 0.28], [6, N.E6, 0.45], [7, N.G6, 0.85],
];
const hookB = [ // the answer phrase — falls where A rises
  [0, N.A6, 0.28], [0.5, N.G6, 0.28], [1, N.E6, 0.45], [2, N.D6, 0.28],
  [2.5, N.E6, 0.28], [3, N.D6, 0.28], [3.5, N.C6, 0.9],
  [5.5, N.G5, 0.28], [6, N.A5, 0.45], [7, N.C6, 0.85],
];
const whistle = [];
// short intro — the hook sings at bar 0, once, naked
for (const [beat, f, hold] of hookA) whistle.push({ t0: t(INTRO[0], beat), f, hold, ...W });
for (const start of [HOOK[0], HOOK[0] + 8, DROP[0], DROP[0] + 8, FINALE[0], FINALE[0] + 8, FINALE[0] + 16])
  for (const [beat, f, hold] of hookA) whistle.push({ t0: t(start, beat), f, hold, ...W });
for (const start of [HOOK[0] + 4, HOOK[0] + 12, DROP[0] + 4, DROP[0] + 12, FINALE[0] + 4, FINALE[0] + 12, FINALE[0] + 20])
  for (const [beat, f, hold] of hookB) whistle.push({ t0: t(start, beat), f, hold, ...W });
// break — one long exhale + the bend back in; still — a lone slow phrase
whistle.push({ t0: t(BREAK[0], 1), f: N.A5, hold: 4.5, db: 24, q: 110, atk: 1.2, rel: 1.8 });
whistle.push({ t0: t(BREAK[1] - 2, 0), f: N.D5, fEnd: N.E5, hold: 2.6, db: 24, q: 110, atk: 0.8, rel: 0.8 });
whistle.push({ t0: t(STILL[0], 1), f: N.E5, hold: 2.2, db: 23, q: 110, atk: 0.7, rel: 1.2 });
whistle.push({ t0: t(STILL[0] + 2, 1), f: N.C5, hold: 2.6, db: 23, q: 110, atk: 0.7, rel: 1.6 });
// outro — the hook's tail, slowing, home to C
whistle.push({ t0: t(OUTRO[0] + 2, 0), f: N.E6, hold: 0.5, ...W });
whistle.push({ t0: t(OUTRO[0] + 2, 1.5), f: N.D6, hold: 0.6, ...W });
whistle.push({ t0: t(OUTRO[0] + 3, 0), f: N.C6, hold: 2.8, db: 24, q: 110, atk: 0.4, rel: 2.5 });

// counter-whistle — an octave below, answering in the gaps (finale only
// gets thirds-ish pentatonic parallels for the lift)
const counter = [];
const answer = [[2, N.G5, 0.4], [2.5, N.A5, 0.4], [3, N.G5, 0.6], [6.5, N.E5, 0.4], [7, N.D5, 0.7]];
for (const start of [HOOK[0] + 8, DROP[0], DROP[0] + 8])
  for (const [beat, f, hold] of answer) counter.push({ t0: t(start, beat), f, hold, db: 22, q: 110, atk: 0.03, rel: 0.15 });
// parallel line under hookA — 4 pentatonic degrees down (stays in scale)
const PENT_LADDER = [N.C5, N.D5, N.E5, N.G5, N.A5, N.C6, N.D6, N.E6, N.G6, N.A6, N.C7];
const pentDown = (f, steps) => {
  const i = PENT_LADDER.findIndex((p) => Math.abs(p - f) < 1);
  return i >= steps ? PENT_LADDER[i - steps] : f / 2;
};
for (const start of [FINALE[0], FINALE[0] + 8, FINALE[0] + 16])
  for (const [beat, f, hold] of hookA) counter.push({ t0: t(start, beat), f: pentDown(f, 4), hold, db: 21, q: 110, atk: 0.03, rel: 0.15 });

const breath = [
  { t0: t(0), f: 250, fEnd: 430, db: 8, q: 1.9, atk: 2.2, hold: 1.2, rel: 2 },
  { t0: t(BREAK[0] + 1), f: 240, fEnd: 900, db: 9, q: 1.6, atk: BAR * 4, hold: 0.3, rel: 1 },
  { t0: t(STILL[0]), f: 430, fEnd: 240, db: 8, q: 1.9, atk: 1.8, hold: 1, rel: 2.6 }, // exhale into the still
  { t0: t(FINALE[0] + 24), f: 240, fEnd: 700, db: 8, q: 1.6, atk: BAR * 3, hold: 0.3, rel: 1.5 },
];
const drone = [
  { t0: t(STILL[0]), f: N.C3, db: 20, q: 30, atk: 1.5, hold: BAR * 4 - 3, rel: 2 },
];

// ════ BROWN — the lows ═════════════════════════════════════════════════
const kick = [];
for (let b = 0; b < BARS; b++) {
  if (!beatOn(b)) continue;
  const db = b < GROOVE[0] + 2 ? 25 : 28;
  for (let k = 0; k < 4; k++)
    kick.push({ t0: t(b, k), f: 115, fEnd: 46, db, q: 4.5, atk: 0.004, hold: 0.035, rel: 0.14 });
}
const sub = [];
for (let b = 0; b < BARS; b++) {
  if (!beatOn(b)) continue;
  for (let k = 0; k < 4; k++)
    sub.push({ t0: t(b, k + 0.5), f: N.C2, db: 22, q: 14, atk: 0.01, hold: 0.14, rel: 0.12 });
}
const bass = [];
for (let b = INTRO[0]; b < OUTRO[1] - 2; b++) {
  if (b >= BREAK[0] + 2 && b < BREAK[1]) continue;
  if (b >= STILL[0] && b < STILL[1]) continue;
  for (let k = 0; k < 8; k++) {
    let f = k % 2 === 0 ? N.C2 : N.C3;
    if (b % 4 === 3 && k >= 6) f = N.G2;
    const glide = b % 8 === 7 && k === 7;
    bass.push({ t0: t(b, k * 0.5), f, fEnd: glide ? N.C3 : 0, db: 26, q: 18, atk: 0.006, hold: 0.1, rel: 0.07 });
  }
}
const toms = []; // pitch-drop fills into every section seam
for (const seam of [GROOVE[0], HOOK[0], BREAK[0], DROP[0], FINALE[0], OUTRO[0]]) {
  for (let k = 0; k < 6; k++) {
    const f = 220 - k * 22;
    toms.push({ t0: t(seam - 1, 2 + k * (2 / 6)), f, fEnd: f * 0.55, db: 21, q: 6, atk: 0.004, hold: 0.03, rel: 0.11 });
  }
}
const boom = [ // subdrop at the two big landings
  { t0: t(DROP[0]), f: 65, fEnd: 28, db: 26, q: 8, atk: 0.005, hold: 0.25, rel: 1.4, ring: true },
  { t0: t(FINALE[0]), f: 65, fEnd: 28, db: 26, q: 8, atk: 0.005, hold: 0.25, rel: 1.4, ring: true },
];

// ════ VELVET — the sustain ═════════════════════════════════════════════
const padProg = [[N.C4, N.G4], [N.A4 / 2 * 2, N.C4 * 2], [N.C4, N.A4], [N.G4 / 2 * 2, N.D5]];
const padLow = [], padHigh = [], padFifth = [];
for (let b = GROOVE[0]; b < OUTRO[0]; b += 2) {
  if (b >= STILL[0] && b < STILL[1]) continue;
  const [f1, f2] = padProg[(b / 2) % 4 | 0];
  const db = b >= BREAK[0] && b < BREAK[1] ? 20 : 17;
  padLow.push({ t0: t(b), f: f1, db, q: 60, atk: 0.9, hold: BAR * 2 - 1.6, rel: 0.9 });
  padHigh.push({ t0: t(b) + 0.02, f: f2, db: db - 2, q: 60, atk: 0.9, hold: BAR * 2 - 1.6, rel: 0.9 });
  if (b >= FINALE[0]) // third pad voice lifts the finale
    padFifth.push({ t0: t(b) + 0.04, f: f2 * 1.5, db: db - 4, q: 60, atk: 0.9, hold: BAR * 2 - 1.6, rel: 0.9 });
}
padLow.push({ t0: t(STILL[0]), f: N.E4, db: 18, q: 60, atk: 1.4, hold: BAR * 4 - 3, rel: 1.6 });
padHigh.push({ t0: t(STILL[0]) + 0.02, f: N.G4, db: 16, q: 60, atk: 1.4, hold: BAR * 4 - 3, rel: 1.6 });

// ════ WHITE — the tops ═════════════════════════════════════════════════
const hat = [];
for (let b = INTRO[0]; b < OUTRO[1] - 2; b++) {
  if (b >= BREAK[0] + 2 && b < BREAK[1]) continue;
  if (b >= STILL[0] && b < STILL[1]) continue;
  const sixteenths = (b >= DROP[0] && b < STILL[0]) || b >= FINALE[0] + 8;
  for (let k = 0; k < (sixteenths ? 16 : 8); k++) {
    const off = sixteenths ? k / 4 : k / 2;
    const accent = sixteenths ? k % 4 === 2 : k % 2 === 1;
    hat.push({ t0: t(b, off), f: 8200, db: accent ? 11 : 8, q: 6, atk: 0.002, hold: 0.008, rel: accent ? 0.09 : 0.045 });
  }
}
const clap = [];
for (let b = GROOVE[0]; b < OUTRO[0]; b++) {
  if (!beatOn(b)) continue;
  for (const k of [1, 3])
    clap.push({ t0: t(b, k), f: 2300, db: 12, q: 2.2, atk: 0.003, hold: 0.04, rel: 0.09 });
}
const arp = []; // glassy 16th pentatonic up-down, drops only
for (let b = DROP[0]; b < OUTRO[0]; b++) {
  if (b >= STILL[0] && b < STILL[1]) continue;
  if (b < FINALE[0] && b >= DROP[0] + 8 && b < DROP[0] + 12) continue; // breathe
  for (let k = 0; k < 16; k++) {
    const step = k < 8 ? k : 15 - k; // up then down
    const f = PENT[step % 5] * (step >= 5 ? 2 : 1) * 2; // C6 register
    arp.push({ t0: t(b, k / 4), f, db: 13, q: 90, atk: 0.004, hold: 0.03, rel: 0.06 });
  }
}
const sparkle = [];
for (const start of [DROP[0] + 2, DROP[0] + 6, FINALE[0] + 2, FINALE[0] + 6, FINALE[0] + 10, FINALE[0] + 14])
  for (const [beat, f] of [[0, N.C7], [1, N.G7], [2.5, N.E7]])
    sparkle.push({ t0: t(start, beat), f, db: 13, q: 90, atk: 0.01, hold: 0.06, rel: 0.4 });
sparkle.push({ t0: t(OUTRO[1] - 3), f: N.C7, db: 12, q: 90, atk: 0.01, hold: 0.06, rel: 3, ring: true }); // last chime
const riser = [ // real white-noise risers into the landings
  { t0: t(DROP[0] - 2), f: 400, fEnd: 3500, db: 11, q: 3, atk: BAR * 1.7, hold: 0.2, rel: 0.3 },
  { t0: t(FINALE[0] - 2), f: 400, fEnd: 4200, db: 11, q: 3, atk: BAR * 1.7, hold: 0.2, rel: 0.3 },
];

// ════ score ════════════════════════════════════════════════════════════
const L = [
  "sr 48000",
  `dur ${DUR}`,
  "detune 1.0012 0.9988",
  "seed 48879 12648430",
  "normpeak 0.88",
  "fadein 0.004",
  "fadeout 2",
];
const push = (bandsArr) => {
  for (const ev of bandsArr) {
    const lines = band(ev);
    if (lines.length) L.push(`band ${lines.length}`, ...lines);
  }
};
push([whistle, counter, breath, drone]); // group 0 — pink (default)
L.push("group 3735928559 195894762", "noisetype brown");
push([kick, sub, bass, toms, boom]);
L.push("group 271828182 314159265", "noisetype velvet");
push([padLow, padHigh, padFifth]);
L.push("group 161803398 141421356", "noisetype white");
push([hat, clap, arp, sparkle, riser]);

const scorePath = resolve(OUT, "cutezenith.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
const n = [whistle, counter, breath, drone, kick, sub, bass, toms, boom, padLow, padHigh, padFifth, hat, clap, arp, sparkle, riser].reduce((s, a) => s + a.length, 0);
console.log(`✓ cutezenith · ${BPM} bpm · ${BARS} bars · ${DUR.toFixed(3)}s · ${n} events · 17 bands / 4 noise materials → ${scorePath}`);
