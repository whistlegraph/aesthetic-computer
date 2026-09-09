#!/usr/bin/env node
// wildfield-gen — natural sound in the spatial field, physically modelled.
// Nothing here is a recording: every strike is a mode bank derived from
// material constants (see modesFor in nsscore-bake-audio.mjs, which
// follows pop/nullabye/c/ac_mesh_acoustics.h), and every texture is
// filtered noise with an envelope. Wood knocks because wood is stiff and
// lossy; glass rings because glass is stiff and isn't.
//
// The narrative is a night that arrives and passes:
//
//   I   Before dawn  — wind alone, the field wide and far
//   II  First drops  — water underfoot, one at a time
//   III Waking       — birds enter above, the field draws in close
//   IV  Full canopy  — everything, and the whole scene turns
//   V   Rain         — density peaks, the field tilts overhead
//   VI  Passing      — it moves off; the field scales away
//   VII Still        — one last drop, far off
//
// Global gestures ride under it: fieldShift swings the entire room,
// fieldTilt lifts or drops it, fieldScale pushes it near and far.
//
//   node wildfield-gen.mjs [out.nsscore] [--minutes 2.5]

import { writeFileSync } from "node:fs";

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? parseFloat(args[i + 1]) : d; };
const MINUTES = opt("minutes", 2.5);

let seed = 20260908;
const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 32;
const pick = (a) => a[Math.floor(rnd() * a.length)];
const range = (a, b) => a + rnd() * (b - a);

const wind = [], drops = [], birds = [], wood = [], stones = [], leaves = [], thunder = [];

// ── the arc ──────────────────────────────────────────────────────────
const SECTIONS = [
  { name: "I · Before dawn", sub: "wind alone, the field far off", secs: 26,
    wind: 1, drop: 0, bird: 0, wood: 0, leaf: 0.2, scale: 2.6, tilt: 0.1 },
  { name: "II · First drops", sub: "water arrives underfoot", secs: 24,
    wind: 0.8, drop: 0.5, bird: 0, wood: 0.2, leaf: 0.3, scale: 1.8, tilt: 0 },
  { name: "III · Waking", sub: "birds above, the field draws close", secs: 26,
    wind: 0.6, drop: 0.8, bird: 0.5, wood: 0.5, leaf: 0.5, scale: 1.1, tilt: 0.15 },
  { name: "IV · Full canopy", sub: "everything, and the room turns", secs: 30,
    wind: 0.7, drop: 1, bird: 1, wood: 0.8, leaf: 0.8, scale: 0.85, tilt: 0.2 },
  { name: "V · Rain", sub: "density peaks, the sky tips over", secs: 26,
    wind: 1, drop: 1.6, bird: 0.4, wood: 0.3, leaf: 1, scale: 0.7, tilt: 0.55 },
  { name: "VI · Passing", sub: "it moves off", secs: 24,
    wind: 0.8, drop: 0.7, bird: 0.6, wood: 0.2, leaf: 0.6, scale: 1.9, tilt: 0.1 },
  { name: "VII · Still", sub: "one drop, far away", secs: 14,
    wind: 0.35, drop: 0.12, bird: 0.15, wood: 0, leaf: 0.15, scale: 3.4, tilt: 0 },
];
const total = SECTIONS.reduce((a, s) => a + s.secs, 0);
const SCALE_T = (MINUTES * 60) / total;

const movements = [];
let t = 0;
for (const sec of SECTIONS) {
  const t0 = t, t1 = t + sec.secs * SCALE_T;

  // WIND — long bandpassed noise swells, overlapping so it never gaps
  for (let x = t0; x < t1; x += range(2.4, 4.2)) {
    const dur = range(4.5, 8);
    wind.push({ t: +x.toFixed(3), dur: +dur.toFixed(3), hz: 1, wave: "noise",
                bp: range(280, 900), q: range(0.7, 1.6), g: 0.30 * sec.wind });
  }

  // LEAVES — dense short rustles, a bright bandpass
  const leafEvery = 0.55 / Math.max(0.05, sec.leaf);
  for (let x = t0; x < t1; x += range(leafEvery * 0.5, leafEvery * 1.5))
    leaves.push({ t: +x.toFixed(3), dur: range(0.10, 0.26), hz: 1, wave: "noise",
                  bp: range(2200, 5200), q: 1.1, decay: 0.05, g: 0.16 * sec.leaf });

  // DROPS — a small water cavity: resonant ping with the pitch rising as
  // the bubble closes. The classic droplet, and it is honest physics.
  const dropEvery = 0.9 / Math.max(0.03, sec.drop);
  for (let x = t0; sec.drop > 0 && x < t1; x += range(dropEvery * 0.3, dropEvery * 1.7)) {
    const f = range(680, 2100);
    drops.push({ t: +x.toFixed(3), dur: range(0.09, 0.2), hz: f, wave: "sine",
                 sweep: range(0.45, 0.7), sweepMs: range(9, 22),
                 decay: range(0.02, 0.05), g: range(0.28, 0.5) });
  }

  // BIRDS — swept, vibrato'd whistles through a resonant bandpass
  const birdEvery = 2.2 / Math.max(0.03, sec.bird);
  for (let x = t0; sec.bird > 0 && x < t1; x += range(birdEvery * 0.4, birdEvery * 1.8)) {
    const n = Math.floor(range(2, 5));
    const base = range(1900, 3600);
    for (let k = 0; k < n; k++)
      birds.push({ t: +(x + k * range(0.07, 0.15)).toFixed(3), dur: range(0.07, 0.14),
                   hz: base * range(0.9, 1.25), wave: "sine",
                   sweep: pick([1.5, 0.65, 1.25]), sweepMs: range(18, 45),
                   vib: range(9, 22), vibDepth: range(0.02, 0.06),
                   bp: base * 1.1, q: 2.2, decay: 0.05, g: range(0.2, 0.36) * sec.bird });
  }

  // WOOD + STONE — struck bodies, modal. Wood is lossy and knocks; stone
  // is stiff and clacks. Both are derived, not sampled.
  const woodEvery = 3.0 / Math.max(0.03, sec.wood);
  for (let x = t0; sec.wood > 0 && x < t1; x += range(woodEvery * 0.5, woodEvery * 1.6)) {
    wood.push({ t: +x.toFixed(3), dur: range(0.35, 0.7), wave: "modal",
                material: "wood", size: range(0.10, 0.30), ring: range(0.7, 1.3), g: 0.42 });
    if (rnd() < 0.4)
      stones.push({ t: +(x + range(0.1, 0.5)).toFixed(3), dur: range(0.2, 0.45), wave: "modal",
                    material: "stone", size: range(0.05, 0.14), ring: 0.8, g: 0.3 });
  }

  // THUNDER — one deep modal roll at the height of the rain
  if (sec.name.startsWith("V ·"))
    thunder.push({ t: +(t0 + sec.secs * SCALE_T * 0.42).toFixed(3), dur: 4.5, wave: "modal",
                   material: "stone", size: 3.2, ring: 3.5, drive: 1.4, g: 0.75 });

  movements.push({ name: sec.name, sub: sec.sub, t0: +t0.toFixed(3), t1: +t1.toFixed(3),
                   level: 0.25 + sec.drop * 0.3 + sec.bird * 0.2, scale: sec.scale, tilt: sec.tilt });
  t = t1;
}
const dur = +t.toFixed(3);

// ── the global gestures ──────────────────────────────────────────────
const N = 512;
const secAt = (τ) => movements.find(m => τ >= m.t0 && τ < m.t1) || movements[movements.length - 1];
const lerpSec = (τ, key) => { // smooth across section seams, never step
  const i = Math.max(0, movements.findIndex(m => τ >= m.t0 && τ < m.t1));
  const m = movements[i], nx = movements[Math.min(movements.length - 1, i + 1)];
  const u = Math.min(1, Math.max(0, (τ - m.t0) / (m.t1 - m.t0)));
  const e = u > 0.75 ? (u - 0.75) / 0.25 : 0;                 // ease into the next
  return m[key] * (1 - e) + nx[key] * e;
};
const ramp = (fn) => Array.from({ length: N }, (_, i) => +fn((i / (N - 1)) * dur).toFixed(4));

const score = {
  name: "wildfield",
  dur,
  masterDrive: 1.0,                         // no glue: this piece lives on its
                                            // dynamics, not on loudness
  room: { mix: 0.36 },                      // early reflections: it externalizes
  movements,
  // per-voice orbit stays slow — this field turns as a whole instead
  rotation: ramp(() => 0.12),
  elevation: ramp((τ) => Math.sin(τ * 0.06) * 0.25),
  distance: ramp((τ) => 0.25 + 0.1 * Math.sin(τ * 0.11)),
  // GLOBAL: the whole room swings, tips and breathes near/far
  fieldShift: ramp((τ) => Math.sin(τ * 0.021) * 0.8 + τ / dur * 0.5),
  fieldTilt: ramp((τ) => lerpSec(τ, "tilt")),
  fieldScale: ramp((τ) => lerpSec(τ, "scale")),
  lanes: [
    { name: "wind", color: [150, 165, 175], events: wind, az: -2.6, el: 0.35, dist: 2.2, mix: 0.9 },
    { name: "leaves", color: [120, 160, 110], events: leaves, az: 2.2, el: 0.25, dist: 1.5, mix: 0.85 },
    { name: "drops", color: [80, 140, 190], events: drops, az: 0.4, el: -0.55, dist: 0.8 },
    { name: "birds", color: [235, 190, 70], events: birds, azOffset: 0 },      // orbiting, overhead
    { name: "wood", color: [150, 105, 60], events: wood, az: -1.2, el: -0.1, dist: 1.1 },
    { name: "stones", color: [130, 130, 140], events: stones, az: 1.4, el: -0.25, dist: 1.0 },
    { name: "thunder", color: [90, 80, 110], events: thunder, az: 3.0, el: 0.5, dist: 3.2, mix: 1.2 },
  ],
};

const dest = args.find(a => !a.startsWith("--") && a.endsWith(".nsscore")) || "wildfield.nsscore";
writeFileSync(dest, JSON.stringify(score) + "\n");
const n = score.lanes.reduce((a, l) => a + l.events.length, 0);
console.log(`${dest} — ${Math.floor(dur / 60)}m${Math.round(dur % 60)}s, ${n} events (wind ${wind.length}, drops ${drops.length}, birds ${birds.length}, leaves ${leaves.length}, modal ${wood.length + stones.length + thunder.length})`);
