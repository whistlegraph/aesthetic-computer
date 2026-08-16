#!/usr/bin/env node
// render-cutezen2.mjs — cutezen2 · the cutezen sit, restarted where it
// gets good. Same nullabye law (pop/nullabye/c/nullnoise.c does all DSP:
// noise + inverted twin, everything heard is cancellation breaking), but:
//
//   · opens AT the whistle — both drones + first phrase inside cycle 0
//     (what was 0:37 of cutezen is now the front door)
//   · no oscillating weirdness — the gliding breath-siren band is gone,
//     stereo detune eased 1.0012→1.0004 (kills the ~1.5 Hz beat-tremolo),
//     whistle Q raised 65→110 (steady tone, no narrowband flutter)
//   · faster breath — 8.8 s cycles (~6.8/min) instead of 10 s
//   · more layers — velvet chord pads (C4/E4/G4·A4, staggered entries),
//     C3 warmth swells, white octave-echo of the whistle + high sparkles,
//     all thickening through the back half
//   · ever-present harmony (2026-08-13) — every chord tone is one quiet
//     whole-piece band with a SUPER long attack (10–26 s) and long decay,
//     entering one by one and never leaving; new halo tones stacked on
//     top (G3 fifth, D5/G5/C6 velvet halos, D6 white sheen) so the sit
//     harmonizes continuously through the full 2:30
//
//   node pop/cutezen/bin/render-cutezen2.mjs         → out/cutezen2.score.txt
//   node pop/nullabye/c/run-c.mjs pop/cutezen/out/cutezen2.score.txt \
//     --out pop/cutezen/out/cutezen2.mp3 --master lullaby
//
// House rules: the EQ chain is SERIAL per group — bands at the same
// frequency ADD dB, so every voice owns a disjoint register; parallel
// groups are separate noise pairs. Mono lanes need time-sorted events;
// each event's cut = the next event's t0.
import { mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

const DUR = 150; // 2:30
const CYCLE = 8.8; // one breath — ~6.8/min (cutezen was 10 s)
const t = (cycle, off = 0) => cycle * CYCLE + off;
const EX = 3.9; // exhale point within a cycle

// C major pentatonic — no leading tone, nothing to resolve.
const P = { C3: 130.81, C4: 261.63, E4: 329.63, G4: 392, A4: 440, C5: 523.25, D5: 587.33, E5: 659.26, G5: 783.99, A5: 880, C6: 1046.5, C7: 2093, E7: 2637, G7: 3136 };

function band(events) {
  // time-sort + chain mono-cuts
  const s = [...events].sort((a, b) => a.t0 - b.t0);
  return s.map((e, i) => {
    const cut = e.ring ? -1 : (s[i + 1] ? s[i + 1].t0 : -1);
    return `${e.t0.toFixed(4)} ${e.f} ${e.fEnd ?? 0} ${e.db} ${e.q} ${e.atk} ${e.hold} ${e.rel} ${cut}`;
  });
}

// ── group A (pink) — drones, whistle, temple bells ────────────────────
// both drones from the top, held through the ENTIRE sit — no re-swells
const droneRoot = [{ t0: 0.3, f: 65.41, db: 24, q: 30, atk: 6, hold: 134, rel: 9 }];
const droneFifth = [{ t0: 2, f: 98, db: 20, q: 30, atk: 7, hold: 130, rel: 10 }];

// the whistle — pentatonic phrases on the exhales, first phrase in
// cycle 0 (cutezen's 0:37 material is the opening); back half doubles
// its phrase rate, one shakuhachi bend each half, home to C at the end
const W = { db: 26, q: 110, atk: 0.5, rel: 1.5 };
const whistle = [
  { t0: t(0, EX), f: P.E5, hold: 2.6, ...W }, // front door
  { t0: t(0, 7.6), f: P.G5, hold: 3.2, ...W },
  { t0: t(2, EX), f: P.A5, hold: 2.6, ...W },
  { t0: t(2, 7.4), f: P.G5, hold: 3.4, ...W },
  { t0: t(4, EX), f: P.C6, hold: 2.4, ...W },
  { t0: t(4, 7.0), f: P.A5, hold: 2.4, ...W },
  { t0: t(5, 1.8), f: P.G5, hold: 4.0, ...W },
  { t0: t(6, EX), f: P.D5, fEnd: P.E5, hold: 3.8, ...W, atk: 0.9 }, // the bend
  // ── back half: phrases every cycle ──
  { t0: t(8, EX), f: P.E5, hold: 2.4, ...W },
  { t0: t(8, 7.4), f: P.C6, hold: 2.6, ...W },
  { t0: t(9, EX), f: P.A5, hold: 2.4, ...W },
  { t0: t(9, 7.2), f: P.G5, hold: 3.0, ...W },
  { t0: t(11, 2.0), f: P.C6, hold: 2.2, ...W },
  { t0: t(11, 5.2), f: P.A5, hold: 2.2, ...W },
  { t0: t(12, EX), f: P.D5, fEnd: P.E5, hold: 3.4, ...W, atk: 0.9 }, // bend, answered
  { t0: t(13, 2.0), f: P.E5, hold: 2.2, ...W },
  { t0: t(13, 5.0), f: P.G5, hold: 2.2, ...W },
  { t0: t(13, 7.6), f: P.A5, hold: 2.8, ...W },
  { t0: t(14, EX), f: P.G5, hold: 3.0, ...W },
  { t0: t(15, EX), f: P.C5, hold: 5.4, ...W, rel: 3.0 }, // home
];

// temple bells — dings at cycle boundaries, long ring (no cut)
const chime = [
  { t0: t(1), f: P.E7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(3), f: P.G7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(5), f: P.E7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(7), f: P.G7, db: 16, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(9), f: P.E7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(11), f: P.G7, db: 15, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(13), f: P.E7, db: 14, q: 80, atk: 0.012, hold: 0.05, rel: 3.0, ring: true },
  { t0: t(15), f: P.C6 * 2, db: 12, q: 80, atk: 0.012, hold: 0.05, rel: 4.0, ring: true }, // last, quietest
];

// air — three long fixed-frequency swells (no glide: the old up/down
// breath-siren is what oscillated; this just sighs)
const air = [
  { t0: t(1, 0), f: 300, db: 6, q: 1.6, atk: 4, hold: 2, rel: 4 },
  { t0: t(7, 0), f: 300, db: 6, q: 1.6, atk: 4, hold: 2, rel: 4 },
  { t0: t(13, 0), f: 300, db: 5, q: 1.6, atk: 4, hold: 2, rel: 5 },
];

// ── group B (velvet) — the ever-present chord ─────────────────────────
// One quiet whole-piece band per chord tone. SUPER long attacks (10–26 s)
// stagger the entries; nothing ever leaves — the harmony just deepens.
const padLow = [{ t0: 1, f: P.C4, db: 16, q: 55, atk: 12, hold: 117, rel: 10 }];
const padMid = [{ t0: 6, f: P.E4, db: 15, q: 55, atk: 14, hold: 118, rel: 12 }];
const padTopG = [{ t0: 12, f: P.G4, db: 14, q: 55, atk: 16, hold: 108, rel: 14 }];
const padTopA = [{ t0: 20, f: P.A4, db: 13, q: 55, atk: 18, hold: 98, rel: 14 }];
// warmth + a low fifth, also permanent
const warmth = [{ t0: 0.5, f: P.C3, db: 15, q: 20, atk: 10, hold: 128, rel: 11 }];
const lowFifth = [{ t0: 4, f: 196, db: 12, q: 40, atk: 12, hold: 122, rel: 12 }];
// new halo tones on top — very quiet, glacial fades, stacked pentatonic
const haloD5 = [{ t0: 26, f: P.D5, db: 10, q: 80, atk: 20, hold: 92, rel: 12 }];
const haloG5 = [{ t0: 34, f: P.G5, db: 9, q: 80, atk: 22, hold: 80, rel: 14 }];
const haloC6 = [{ t0: 44, f: P.C6, db: 8, q: 90, atk: 24, hold: 66, rel: 16 }];

// ── group C (white) — octave-echo of the whistle + high sparkles ──────
// echo: back-half whistle notes shadowed an octave up, 0.6 s behind
const echo = [];
for (const e of whistle) {
  if (e.t0 < t(8) || e.fEnd || e.f * 2 > 2200) continue;
  echo.push({ t0: e.t0 + 0.6, f: e.f * 2, db: 14, q: 120, atk: 0.4, hold: Math.min(e.hold, 1.8), rel: 1.2 });
}
const sparkle = [];
for (const [c, f] of [[9, P.C7], [10, P.G7], [11, P.E7], [12, P.C7], [13, P.G7], [14, P.E7]])
  sparkle.push({ t0: t(c, 6.2), f, db: 12, q: 90, atk: 0.01, hold: 0.06, rel: 1.6, ring: true });
// D6 sheen — the quietest permanent tone, arriving last (D6 is free of the
// octave-echo register: the echo skips bend notes, so D5×2 never sounds there)
const sheen = [{ t0: 56, f: P.D5 * 2, db: 7, q: 100, atk: 26, hold: 52, rel: 16 }];

const L = [
  "sr 48000",
  `dur ${DUR}`,
  "detune 1.0004 0.9996", // was 1.0012/0.9988 — enough width, no beat-wobble
  "seed 48879 12648430",
  "normpeak 0.88",
  "fadein 0.004",
  "fadeout 5",
];
const push = (bandsArr) => {
  for (const ev of bandsArr) {
    const lines = band(ev);
    if (lines.length) L.push(`band ${lines.length}`, ...lines);
  }
};
push([droneRoot, droneFifth, whistle, chime, air]); // group 0 — pink
L.push("group 3735928559 195894762", "noisetype velvet");
push([padLow, padMid, padTopG, padTopA, warmth, lowFifth, haloD5, haloG5, haloC6]);
L.push("group 161803398 141421356", "noisetype white");
push([echo, sparkle, sheen]);

const scorePath = resolve(OUT, "cutezen2.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
const bands = [droneRoot, droneFifth, whistle, chime, air, padLow, padMid, padTopG, padTopA, warmth, lowFifth, haloD5, haloG5, haloC6, echo, sparkle, sheen];
const n = bands.reduce((s, a) => s + a.length, 0);
console.log(`✓ cutezen2 · ${DUR}s · ${CYCLE}s breath · ${n} carved events (${bands.length} bands, 3 noise materials, ever-present harmony) → ${scorePath}`);
