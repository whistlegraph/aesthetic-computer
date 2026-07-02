#!/usr/bin/env node
// render-cutezen.mjs — cutezen · a 2:30 meditation carved from cancelled
// noise (the nullabye technique — pop/nullabye/c/nullnoise.c does all DSP).
//
// Two copies of pink noise, one inverted: flat EQ = perfect silence.
// Everything you hear is the cancellation breaking. The piece breathes in
// 10-second cycles (6 breaths/min, calm-breath pace): a glide-swell band IS
// the breath (up = inhale, down = exhale), a Q-65 noise-whistle sings a
// C-major-pentatonic shakuhachi line on the exhales, temple-bell dings mark
// the bloom, and at the end every bell releases — the noise cancels back to
// literal digital silence. Form is emptiness.
//
//   node pop/cutezen/bin/render-cutezen.mjs          → out/cutezen.score.txt
//   node pop/nullabye/c/run-c.mjs pop/cutezen/out/cutezen.score.txt \
//     --out pop/cutezen/out/cutezen.mp3 --master lullaby
//
// House rules (learned on nuellaby/teknull): the EQ chain is SERIAL — bands
// at the same frequency ADD dB, so every voice owns a disjoint register;
// harmony pads run on a parallel `group` (second noise pair). Mono lanes
// need time-sorted events; each event's cut = the next event's t0.
import { mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

const DUR = 150; // 2:30
const CYCLE = 10; // one calm breath — 6/min
const t = (cycle, off = 0) => cycle * CYCLE + off;

// C major pentatonic — no leading tone, nothing to resolve.
const P = { C4: 261.63, C5: 523.25, D5: 587.33, E5: 659.26, G5: 783.99, A5: 880, C6: 1046.5, E7: 2637, G7: 3136 };

function band(events) {
  // time-sort + chain mono-cuts
  const s = [...events].sort((a, b) => a.t0 - b.t0);
  return s.map((e, i) => {
    const cut = e.ring ? -1 : (s[i + 1] ? s[i + 1].t0 : -1);
    return `${e.t0} ${e.f} ${e.fEnd ?? 0} ${e.db} ${e.q} ${e.atk} ${e.hold} ${e.rel} ${cut}`;
  });
}

// ── group A (main pair) — drones, breath, whistle, chimes ─────────────
// drone root — C2, one long tone under the whole sit
const droneRoot = [{ t0: 2, f: 65.41, db: 24, q: 30, atk: 8, hold: 122, rel: 10 }];
// drone fifth — G2, arrives with the whistle, leaves before the dissolve
const droneFifth = [{ t0: 28, f: 98, db: 20, q: 30, atk: 10, hold: 84, rel: 10 }];

// the breath — a gentle wide-ish bell gliding up (inhale) and down (exhale)
// every cycle; it fades as the sit deepens and stops before the silence
const breath = [];
for (let c = 0; c < 14; c++) {
  const db = c < 3 ? 9 : c < 11 ? 8 : 8 - (c - 10) * 1.5; // recede at the end
  breath.push({ t0: t(c, 0.0), f: 250, fEnd: 430, db, q: 1.4, atk: 2.4, hold: 0.5, rel: 1.4 }); // in
  breath.push({ t0: t(c, 4.4), f: 430, fEnd: 240, db, q: 1.4, atk: 1.8, hold: 0.8, rel: 2.6 }); // out
}

// the whistle — sparse pentatonic phrases on the exhales, long tones,
// one shakuhachi bend; descends home to C and lets go
const W = { db: 26, q: 65, atk: 0.5, rel: 1.5 };
const whistle = [
  { t0: t(3, 4.4), f: P.E5, hold: 3.0, ...W },
  { t0: t(3, 8.6), f: P.G5, hold: 3.6, ...W },
  { t0: t(5, 4.4), f: P.A5, hold: 2.8, ...W },
  { t0: t(5, 8.4), f: P.G5, hold: 4.0, ...W },
  { t0: t(7, 4.4), f: P.C6, hold: 2.6, ...W },
  { t0: t(7, 8.0), f: P.A5, hold: 2.6, ...W },
  { t0: t(8, 2.0), f: P.G5, hold: 4.6, ...W },
  { t0: t(9, 4.4), f: P.D5, fEnd: P.E5, hold: 4.4, ...W, atk: 0.9 }, // the bend
  { t0: t(11, 4.4), f: P.E5, hold: 2.8, ...W },
  { t0: t(11, 8.4), f: P.D5, hold: 3.4, ...W },
  { t0: t(12, 4.4), f: P.C5, hold: 5.6, ...W, rel: 3.0 }, // home
];

// temple bells — tiny dings at bloom cycle-boundaries, long ring (no cut)
const chime = [
  { t0: t(7), f: P.E7, db: 16, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(9), f: P.G7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(11), f: P.E7, db: 14, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(13), f: P.C6 * 2, db: 12, q: 80, atk: 0.012, hold: 0.05, rel: 4.0, ring: true }, // last, quietest
];

// ── group B (parallel pair) — harmony hums + sub warmth ───────────────
const hum = [
  { t0: t(7, 4.0), f: 329.63, db: 20, q: 50, atk: 2.5, hold: 8, rel: 3 }, // E4
  { t0: t(9, 4.0), f: 392.0, db: 19, q: 50, atk: 2.5, hold: 7, rel: 3 }, // G4
  { t0: t(11, 4.0), f: P.C4, db: 19, q: 50, atk: 2.5, hold: 10, rel: 4 }, // C4 home
];
const warmth = [];
for (let c = 6; c < 12; c += 2)
  warmth.push({ t0: t(c, 1), f: 130.81, db: 16, q: 20, atk: 3.5, hold: 2.5, rel: 3.5 }); // C3 swells

const L = [
  "sr 48000",
  `dur ${DUR}`,
  "detune 1.0012 0.9988",
  "seed 48879 12648430",
  "normpeak 0.88",
  "fadein 0.004",
  "fadeout 5",
];
for (const ev of [droneRoot, droneFifth, breath, whistle, chime]) {
  const lines = band(ev);
  L.push(`band ${lines.length}`, ...lines);
}
L.push("group 3735928559 195894762");
for (const ev of [hum, warmth]) {
  const lines = band(ev);
  L.push(`band ${lines.length}`, ...lines);
}

const scorePath = resolve(OUT, "cutezen.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
const n = droneRoot.length + droneFifth.length + breath.length + whistle.length + chime.length + hum.length + warmth.length;
console.log(`✓ cutezen · ${DUR}s · ${n} carved events (7 bands, 2 noise pairs) → ${scorePath}`);
