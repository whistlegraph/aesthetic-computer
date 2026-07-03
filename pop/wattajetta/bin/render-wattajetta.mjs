#!/usr/bin/env node
// render-wattajetta.mjs — the composition. A fighter jet made entirely
// of water, ~3 minutes. Starts ON the drop. The bell runs are our
// physically-modeled FEM bells and over the flight the water hardens:
// glass → bronze → steel → stone (a granite lithophone material added
// to the engine for this track), decays tightening as it goes. A
// turntablist rides the whole thing — scratch gestures scrub slices of
// the track itself, a global warp pass drags the platter and wobbles
// the record out.
//
// Novel voices this cut:
//   chimes     — tubular-geometry bells in quick clusters (wind chimes)
//   bloops     — rising sine chirps, actual water-drip percussion
//   water choir— formant-shaped sine stacks breathing "ooh" in the breaths
//   underwater — bells resampled through a slow LFO, heard through water
//   church     — one huge church-geometry toll at the coda
//
//   node pop/wattajetta/bin/render-wattajetta.mjs          → out/wattajetta.mp3
//   node pop/wattajetta/bin/render-wattajetta.mjs --score  → print the engine score
//
// Arc (bars):
//   0–11   drop A   glass bells · silver bowls
//   12–15  breath 1 chimes · flyby · platter drag · baby-scratch at 11.5
//   16–27  drop B   glass + bloops · super scratch at 15
//   28–31  breath 2 water choir enters · bronze toll · slow scrub
//   32–43  drop C   bronze, tighter decays · brass bowls · bloops denser
//   44–47  breath 3 platter drag · steel toll · choir swells
//   48–59  drop D   steel, tight runs · super scratch 2 at 47 · flyby
//   60–63  breath 4 underwater bells · stone toll · choir
//   64–75  drop E   STONE — granite runs + stone bowls, everything in
//   76–79  coda     church bell tolls · kick thins to downbeats
//   80–95  mist     stone tolls · vinyl wobble · underwater scrub ·
//                   one last glass bell closing the circle

import { writeFileSync, readFileSync, mkdirSync, unlinkSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { renderBell } from "../../lib/bell.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
mkdirSync(OUT, { recursive: true });

const SR = 48000;
const TAU = 2 * Math.PI;
const BPM = 138;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const BARS = 96;
const DUR = BARS * BAR + 5.5; // room for the last toll to settle

// deterministic sprinkle — same track every bake
let _s = 0xa7757e77;
const rnd = () => ((_s = (_s * 1664525 + 1013904223) >>> 0) / 4294967296);

// e minor pentatonic — the bell runs live here. One octave down from
// the first cuts: E5–E6 read piercing-tangy on laptop speakers
const DROP_NOTES = ["E4", "G4", "A4", "B4", "D5", "E5"];
const CHIME_NOTES = ["E5", "G5", "A5", "B5", "D6"];
const BLOOP_HZ = [164.81, 196.0, 220.0, 246.94, 293.66]; // e3 pentatonic
// sub roots: e1 g1 a1 d2 under each 2-bar phrase
const ROOTS = [41.203, 48.999, 55.0, 73.416];
const ROOT_BELLS = ["E4", "G4", "A4", "D5"];

const kicks = [];  // t0 f0 f1 sweep ampDb hole decay
const sines = [];  // t0 dur f0 f1 ampDb atk rel pan0 pan1 vibHz vibCents
const noises = []; // t0 dur f0 f1 q peakDb atk rel pan
const bells = [];  // { t, note, vel, pan, gain, material, geometry, dur, warp? }

const bar = (n) => n * BAR;

// the flight plan: each drop hardens the water a little more
const DROPS = [
  { a: 0,  z: 12, mat: "glass",  bowl: "silver", durs: [3.2, 3.2, 1.4], density: 0.85, gainDb: -14, bloops: false },
  { a: 16, z: 28, mat: "glass",  bowl: "silver", durs: [3.2, 1.4, 0.9], density: 0.9,  gainDb: -13, bloops: 0.4 },
  { a: 32, z: 44, mat: "bronze", bowl: "brass",  durs: [1.8, 1.8, 1.0], density: 0.9,  gainDb: -13, bloops: 0.6 },
  { a: 48, z: 60, mat: "steel",  bowl: "gold",   durs: [1.2, 1.2, 0.7], density: 0.95, gainDb: -12.5, bloops: 0.6 },
  { a: 64, z: 76, mat: "stone",  bowl: "stone",  durs: [1.6, 1.0, 0.7], density: 0.95, gainDb: -11.5, bloops: 0.75 },
];
const BREATHS = [12, 28, 44, 60]; // 4-bar rests between drops

// ── kicks: halftime 1+3 early (the part we loved), then the flight
//    ramps — drop C flips to four-on-the-floor halfway, and the steel
//    and stone drops drive full trance 4/4 ────────────────────────────
const kickAt = (t) => kicks.push([t, 118, 41, 0.075, -2, 0.012, 0.28]);
for (const d of DROPS)
  for (let b = d.a; b < d.z; b++) {
    const fourFloor = d.a >= 48 || (d.a === 32 && b >= 38);
    if (fourFloor) for (let k = 0; k < 4; k++) kickAt(bar(b) + k * BEAT);
    else { kickAt(bar(b)); kickAt(bar(b) + 2 * BEAT); }
  }
// coda: the downbeat only, letting the stone ring between hits
for (let b = 76; b < 80; b++) kickAt(bar(b));

// ── sub bass: the fuselage — root + quiet octave, one note per 2 bars ─
function subNote(t, dur, freq, db) {
  sines.push([t, dur, freq, freq, db, 0.06, 0.25, 0, 0, 0, 0]);
  sines.push([t, dur, freq * 2, freq * 2, db - 7, 0.06, 0.25, 0, 0, 4.5, 6]);
}
for (const d of DROPS)
  for (let b = d.a; b < d.z; b += 2) {
    const root = ROOTS[((b - d.a) / 2) % 4];
    if (d.a >= 48) {
      // trance gallop: offbeat eighth stabs — the sidechain makes the pump
      for (let bb = b; bb < b + 2; bb++)
        for (let k = 0; k < 4; k++)
          sines.push([bar(bb) + (k + 0.5) * BEAT, 0.16, root * 2, root * 2, -8, 0.005, 0.08, 0, 0, 0, 0]);
      sines.push([bar(b), 2 * BAR - 0.08, root, root, -9, 0.06, 0.25, 0, 0, 0, 0]); // sub floor stays
    } else {
      subNote(bar(b), 2 * BAR - 0.08, root, -7.5);
    }
  }
subNote(bar(76), 4 * BAR - 0.1, ROOTS[0], -9); // coda holds the root
// each breath: the sub lets go and rises an octave into vapor
for (const b of BREATHS)
  sines.push([bar(b), 4 * BAR, 41.203, 82.407, -11, 1.0, 2.2, 0, 0, 0, 0]);

// ── bell runs: eighth-note pentatonic random walks; decay varies per
//    strike (a few land choked), material follows the flight plan ──────
function bellRun(b, d) {
  let idx = Math.floor(rnd() * DROP_NOTES.length);
  for (let e = 0; e < 8; e++) {
    idx = Math.max(0, Math.min(DROP_NOTES.length - 1, idx + (rnd() < 0.5 ? -1 : 1)));
    if (rnd() < d.density)
      bells.push({
        t: bar(b) + e * 0.5 * BEAT,
        note: DROP_NOTES[idx],
        vel: 0.55 + rnd() * 0.3,
        pan: (rnd() * 2 - 1) * 0.7,
        gain: Math.pow(10, d.gainDb / 20),
        material: d.mat, geometry: "glass",
        dur: d.durs[Math.floor(rnd() * d.durs.length)],
      });
  }
}
for (const d of DROPS) for (let b = d.a; b < d.z; b++) bellRun(b, d);

// ── bloops: actual water-drip percussion — a sine chirping UP into its
//    note the way a drip rings a pool, on the swung offbeats ────────────
function bloop(t, hz, db) {
  sines.push([t, 0.11, hz * 0.55, hz, db, 0.006, 0.07, (rnd() * 2 - 1) * 0.5, 0, 0, 0]);
}
for (const d of DROPS) {
  if (!d.bloops) continue;
  for (let b = d.a; b < d.z; b++)
    for (const slot of [1.75, 3.25, 3.75])
      if (rnd() < d.bloops) bloop(bar(b) + slot * BEAT, BLOOP_HZ[Math.floor(rnd() * BLOOP_HZ.length)], -17);
}

// ── water choir: formant-shaped sine stacks — an "ooh" breathed out of
//    pure sines. Harmonic amplitudes follow /u/ vowel resonances;
//    detuned pairs beat slowly like the surface of held water ──────────
const FORMANTS = [[300, 170], [870, 260], [2250, 430]]; // [center, bandwidth] — wide = breathy
function choirNote(t, dur, f0, db, pan) {
  for (let h = 1; h <= 8; h++) {
    const fh = f0 * h;
    let a = 0;
    for (const [fc, bw] of FORMANTS) a += Math.exp(-(((fh - fc) / bw) ** 2));
    a = Math.max(a, 0.04) / h; // spectral tilt keeps it breathy, not brassy
    const hdb = db + 20 * Math.log10(a);
    if (hdb < -60) continue;
    for (const det of [-5, 0, 5]) // detuned trio — thick slow beating
      sines.push([t, dur, fh * Math.pow(2, det / 1200), fh * Math.pow(2, det / 1200),
                  hdb - 9, dur * 0.35, dur * 0.4, pan, pan, 4.8, 7]);
  }
}
function choirChord(t, dur, db) {
  choirNote(t, dur, 82.407, db - 2, 0);      // e2
  choirNote(t, dur, 123.47, db - 3, -0.35);  // b2
  choirNote(t, dur, 164.81, db, 0.35);       // e3
  choirNote(t, dur, 196.0, db - 4, -0.15);   // g3 — the minor color
}
choirChord(bar(28), 4 * BAR, -19);
choirChord(bar(44), 4 * BAR, -17);
choirChord(bar(60), 4 * BAR, -17);
choirChord(bar(80), 8 * BAR, -20); // the mist hums it one last time

// ── chimes: tubular bells in quick clusters — wind chimes on the
//    canopy rail, breath sections and the mist ─────────────────────────
function chimeCluster(t, count, db) {
  let tt = t;
  for (let i = 0; i < count; i++) {
    bells.push({ t: tt, note: CHIME_NOTES[Math.floor(rnd() * CHIME_NOTES.length)],
                 vel: 0.35 + rnd() * 0.25, pan: (rnd() * 2 - 1) * 0.8,
                 gain: Math.pow(10, db / 20),
                 material: "aluminum", geometry: "tubular", dur: 2.2 });
    tt += 0.03 + rnd() * 0.09;
  }
}
for (const b of BREATHS) { chimeCluster(bar(b) + BEAT, 5, -20); chimeCluster(bar(b + 2) + 2 * BEAT, 4, -22); }
for (const b of [82, 86, 91]) chimeCluster(bar(b), 3, -23);

// ── bowls: a low anchor each 2-bar downbeat, material morphing too ────
for (const d of DROPS)
  for (let b = d.a; b < d.z; b += 2)
    bells.push({ t: bar(b), note: ROOT_BELLS[((b - d.a) / 2) % 4],
                 vel: 0.7, pan: (b / 2) % 2 === 0 ? -0.2 : 0.2,
                 gain: Math.pow(10, -16 / 20),
                 material: d.bowl, geometry: "bowl", dur: 4.5 });

// ── breaths, coda, mist: long bowls + tolls; the next material always
//    tolls once before its drop arrives; breath 4 bells come through
//    water (warp = slow LFO resample) ──────────────────────────────────
const longBowl = (t, note, mat, db, dur = 7, extra = {}) =>
  bells.push({ t, note, vel: 0.8, pan: 0, gain: Math.pow(10, db / 20),
               material: mat, geometry: "bowl", dur, ...extra });
longBowl(bar(12), "E4", "silver", -15);
longBowl(bar(44), "E4", "brass", -15);
longBowl(bar(60), "E4", "gold", -15, 7, { warp: { depth: 0.03, hz: 0.7 } });
longBowl(bar(76), "E3", "stone", -14, 8);
longBowl(bar(80), "E4", "silver", -17, 9, { warp: { depth: 0.035, hz: 0.55 } });
// foreshadow tolls — the ear learns each material before its drop
const foreshadow = [[30, "E5", "bronze"], [46, "B5", "steel"], [62, "G5", "stone"]];
for (const [b, n, m] of foreshadow)
  bells.push({ t: bar(b), note: n, vel: 0.6, pan: 0.3, gain: Math.pow(10, -16 / 20),
               material: m, geometry: "glass", dur: 2.5 });
// breath sparkles — breath 4's come through water
for (const b of [13.5, 14.5, 29, 45.5, 61, 62.5])
  bells.push({ t: bar(b), note: DROP_NOTES[Math.floor(rnd() * DROP_NOTES.length)],
               vel: 0.5, pan: (rnd() * 2 - 1) * 0.5, gain: Math.pow(10, -17 / 20),
               material: "glass", geometry: "glass", dur: 3.2,
               ...(b >= 60 ? { warp: { depth: 0.04, hz: 0.8 } } : {}) });
// coda: the church bell — one huge toll, then its echo
bells.push({ t: bar(76), note: "E3", vel: 0.9, pan: 0, gain: Math.pow(10, -13 / 20),
             material: "bronze", geometry: "church", dur: 9 });
bells.push({ t: bar(78), note: "E3", vel: 0.55, pan: 0.15, gain: Math.pow(10, -17 / 20),
             material: "bronze", geometry: "church", dur: 7 });
// mist: stone tolls slowing, underwater, one last glass bell full circle
for (const [b, n] of [[80, "E5"], [81, "G5"], [82.5, "A5"], [84, "E5"], [86, "D6"], [88.5, "G5"], [91, "A5"]])
  bells.push({ t: bar(b), note: n, vel: 0.55, pan: (rnd() * 2 - 1) * 0.4,
               gain: Math.pow(10, (-16 - (b - 80) * 0.4) / 20),
               material: "stone", geometry: "glass", dur: 2.0,
               warp: { depth: 0.03, hz: 0.6 } });
bells.push({ t: bar(93), note: "E6", vel: 0.45, pan: 0, gain: Math.pow(10, -19 / 20),
             material: "glass", geometry: "glass", dur: 3.2, warp: { depth: 0.05, hz: 0.5 } });

// ── flybys: sine dopplers at the seams, quiet ─────────────────────────
function flyby(t, dir) {
  const d = 2 * BAR;
  sines.push([t, d, 760, 185, -17, d * 0.4, d * 0.45, -dir, dir, 0, 0]);
}
flyby(bar(13), 1);
flyby(bar(29), -1);
flyby(bar(53), 1);
flyby(bar(70), -1);

// ── spray, whisper level: a steam breath at each drop, an exhale out ──
for (const d of DROPS) noises.push([bar(d.a), 2.0, 2000, 300, 0.8, -23, 0.015, 1.8, 0]);
noises.push([bar(80), 16 * BAR + 4, 3000, 500, 0.8, -27, 2.0, 14 * BAR, 0]);
noises.push([0, DUR, 1100, 1100, 0.6, -41, 4, 4, 0]);

// ── bake + render the engine part ─────────────────────────────────────
const fmt = (rows) => rows.map((r) => "  " + r.map((v) => +v.toFixed(5)).join(" ")).join("\n");
const score = [
  `sr ${SR}`,
  `dur ${DUR.toFixed(3)}`,
  `normpeak 0.82`,
  `fadein 0.004`,
  `fadeout 4.0`,
  `sidechain 0.015 0.2 -6`,
  `kick ${kicks.length}`, fmt(kicks),
  `sine ${sines.length}`, fmt(sines),
  `noise ${noises.length}`, fmt(noises),
].join("\n") + "\n";

const scorePath = resolve(OUT, "wattajetta.score.txt");
writeFileSync(scorePath, score);
if (process.argv.includes("--score")) { console.log(score); process.exit(0); }
console.log(`baked ${kicks.length} kicks, ${sines.length} sines, ${noises.length} sprays, ${bells.length} bells`);

const rawPath = resolve(OUT, "wattajetta.f32.raw");
const kickPath = resolve(OUT, "wattajetta.kick.f32.raw");
const r = spawnSync("node", [resolve(HERE, "../c/run-c.mjs"), scorePath, "--raw", rawPath, "--kickraw", kickPath], { stdio: "inherit" });
if (r.status !== 0) process.exit(1);

// ── mix the bells over the engine render ──────────────────────────────
// The kick rides its own bus: scratches and ducks only ever touch the
// musical bus, so the instinctual rhythm is never destroyed — we layer,
// we don't overwrite. One FEM render per unique voicing (cached),
// copies mixed per strike with equal-power pan. A strike with `warp` is
// resampled through a slow sine LFO first — a bell heard through water.
const raw = readFileSync(rawPath);
const mix = new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4);
const kraw = readFileSync(kickPath);
const kickBus = new Float32Array(kraw.buffer, kraw.byteOffset, kraw.length / 4);
const ns = mix.length / 2;
const smooth = (p) => p * p * (3 - 2 * p);

// ── crunch: the water hardens sonically too — a tanh waveshaper whose
//    blend ramps in across drops C→E, eases off for the coda and mist.
//    It runs HERE, on the engine bus and the kick only, BEFORE the
//    bells and vocals layer in — distorted sines and kicks get teeth,
//    the bells stay pure (crunched bells read tangy on small speakers) ─
{
  const DRIVE = 2.4;
  const norm = Math.tanh(DRIVE);
  const blendAt = (t) => {
    if (t < bar(24)) return 0;
    if (t < bar(64)) return 0.85 * smooth((t - bar(24)) / (bar(64) - bar(24)));
    if (t < bar(76)) return 0.85;
    if (t < bar(80)) return 0.85 - 0.6 * smooth((t - bar(76)) / (4 * BAR));
    return 0.15;
  };
  for (let f = 0; f < ns; f++) {
    const m = blendAt(f / SR);
    if (m <= 0) continue;
    for (let ch = 0; ch < 2; ch++) {
      const x = mix[2 * f + ch];
      mix[2 * f + ch] = x * (1 - m) + (Math.tanh(x * DRIVE) / norm) * m;
      const k = kickBus[2 * f + ch];
      kickBus[2 * f + ch] = k * (1 - m) + (Math.tanh(k * DRIVE) / norm) * m;
    }
  }
}

const bank = new Map();
const bellFor = ({ note, material, geometry, dur }) => {
  const k = `${note}/${material}/${geometry}/${dur}`;
  if (!bank.has(k)) {
    console.log(`  bell ${note} (${material}/${geometry} ×${dur}s)…`);
    bank.set(k, renderBell({ note, material, geometry, dur }));
  }
  return bank.get(k);
};

function underwater(buf, depth, hz) {
  const out = new Float32Array(buf.length);
  let pos = 0;
  for (let i = 0; i < out.length; i++) {
    const j = Math.floor(pos);
    if (j >= buf.length - 1) break;
    const fr = pos - j;
    out[i] = buf[j] * (1 - fr) + buf[j + 1] * fr;
    pos += 1 + depth * Math.sin(TAU * hz * (i / SR));
  }
  return out;
}

for (const s of bells) {
  let { L, R } = bellFor(s);
  if (s.warp) { L = underwater(L, s.warp.depth, s.warp.hz); R = underwater(R, s.warp.depth, s.warp.hz); }
  const at = Math.floor(s.t * SR);
  const a = (s.pan + 1) * 0.25 * Math.PI;
  const gl = Math.cos(a) * s.gain * s.vel;
  const gr = Math.sin(a) * s.gain * s.vel;
  const n = Math.min(L.length, ns - at);
  const fade = Math.min(Math.floor(0.08 * SR), Math.floor(n * 0.2)); // choked strikes land soft, never click
  for (let i = 0; i < n; i++) {
    const env = i > n - fade ? (n - i) / fade : 1;
    mix[2 * (at + i)] += L[i] * gl * env;
    mix[2 * (at + i) + 1] += R[i] * gr * env;
  }
}

// ── super scratching: a hand scrubs the record — the playhead scrubs
//    a slice of the track itself. Position follows smoothstep gestures
//    (zero velocity at the turnarounds, like a real wrist), a
//    transformer gate chops, and the mix ducks under the hand ──────────
function scratchAt(tOut, srcT, srcDur, gestures, gainDb) {
  const src0 = Math.floor(srcT * SR);
  const srcN = Math.floor(srcDur * SR);
  const gain = Math.pow(10, gainDb / 20);
  let t = tOut;
  for (const g of gestures) {
    const n = Math.floor(g.dur * SR);
    const at = Math.floor(t * SR);
    for (let i = 0; i < n; i++) {
      const p = smooth(i / n);
      const pos = (g.from + (g.to - g.from) * p) * srcN;
      const j = src0 + Math.floor(pos);
      if (j < 0 || j >= ns - 1 || at + i >= ns) continue;
      const fr = pos - Math.floor(pos);
      // 5ms gate ramps so the chop never clicks
      const edge = Math.min(i, n - i) / (0.005 * SR);
      const gate = (g.gate ?? 1) * Math.min(1, edge);
      const duck = 1 - 0.65 * gate;
      for (let ch = 0; ch < 2; ch++) {
        const v = mix[2 * j + ch] * (1 - fr) + mix[2 * (j + 1) + ch] * fr;
        mix[2 * (at + i) + ch] = mix[2 * (at + i) + ch] * duck + v * gate * gain;
      }
    }
    t += g.dur;
  }
}
const E = BEAT / 2; // an eighth
const SUPER = [
  { dur: E * 2, from: 0, to: 0.8 }, { dur: E * 2, from: 0.8, to: 0.05 },
  { dur: E, from: 0.05, to: 0.7 }, { dur: E, from: 0.7, to: 0.1, gate: 0.7 },
  { dur: E * 0.5, from: 0.1, to: 0.5 }, { dur: E * 0.5, from: 0.5, to: 0.15 },
  { dur: E * 0.5, from: 0.15, to: 0.6, gate: 0.8 }, { dur: E * 0.5, from: 0.6, to: 0.05 },
  { dur: E * 0.25, from: 0.05, to: 0.3 }, { dur: E * 0.25, from: 0.3, to: 0.1 },
  { dur: E * 0.25, from: 0.1, to: 0.35 }, { dur: E * 0.25, from: 0.35, to: 0 },
];
// end of drop A: quick baby-scratch fill on the opening slice
scratchAt(bar(11) + 2 * BEAT, bar(0), BEAT, [
  { dur: E, from: 0, to: 0.6 }, { dur: E, from: 0.6, to: 0.1 },
  { dur: E * 0.5, from: 0.1, to: 0.5 }, { dur: E * 0.5, from: 0.5, to: 0 },
  { dur: E, from: 0, to: 1 },
], -4);
// the SUPER scratches: each one scrubs the NEXT drop's downbeat in
scratchAt(bar(15), bar(16), BEAT, SUPER, -3);
scratchAt(bar(47), bar(48), BEAT, SUPER, -3);
// breath 2: slow half-speed scrub, barely gated
scratchAt(bar(29.5), bar(16), 2 * BEAT, [
  { dur: BEAT, from: 0, to: 0.5, gate: 0.5 }, { dur: BEAT, from: 0.5, to: 0.15, gate: 0.4 },
], -10);
// mist: one last underwater scrub of the stone drop
scratchAt(bar(84.5), bar(64), 2 * BEAT, [
  { dur: BEAT * 1.5, from: 0, to: 0.4, gate: 0.45 }, { dur: BEAT, from: 0.4, to: 0.1, gate: 0.35 },
], -11);

// ── vocals: jeffrey-pvc drops (pop/bin/say.mjs → assets/). Mixed after
//    the scratches so the hand never chews them, before crunch + warp so
//    they harden and accelerate with the record. `rate` re-pitches
//    (1 = as recorded); `warp` sends one underwater ────────────────────
function loadVocal(file) {
  const p = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", resolve(HERE, "../assets", file),
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"],
    { maxBuffer: 64 * 1024 * 1024 });
  if (p.status !== 0) { console.error(`✗ vocal decode failed: ${file}`); process.exit(1); }
  const b = p.stdout; // copy into an aligned buffer — stdout offset isn't guaranteed %4
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + b.length - (b.length % 4)));
}
const VOCALS = [
  { t: bar(0), file: "wattajetta.mp3", db: -8, rate: 1 },              // title drop over the steam
  { t: bar(15) - 0.15, file: "wayer.mp3", db: -6, rate: 1 },           // riding super scratch 1
  { t: bar(47) - 0.15, file: "wayer.mp3", db: -6, rate: 0.94 },        // into steel, a shade deeper
  { t: bar(63.5), file: "wayer.mp3", db: -8, rate: 0.8,
    warp: { depth: 0.05, hz: 0.9 } },                                  // underwater, into stone
  { t: bar(76) + 2 * BEAT, file: "wattajetta.mp3", db: -12, rate: 0.9 }, // coda echo under the church bell
];
const vocalCache = new Map();
for (const v of VOCALS) {
  if (!vocalCache.has(v.file)) vocalCache.set(v.file, loadVocal(v.file));
  let buf = vocalCache.get(v.file);
  if (v.rate !== 1 || v.warp) {
    const out = new Float32Array(Math.floor(buf.length / v.rate / 2) * 2);
    let pos = 0;
    for (let f = 0; f * 2 + 3 < out.length; f++) {
      const wob = v.warp ? 1 + v.warp.depth * Math.sin(TAU * v.warp.hz * (f / SR)) : 1;
      const j = Math.floor(pos) * 2;
      if (j + 3 >= buf.length) break;
      const fr = pos - Math.floor(pos);
      out[2 * f] = buf[j] * (1 - fr) + buf[j + 2] * fr;
      out[2 * f + 1] = buf[j + 1] * (1 - fr) + buf[j + 3] * fr;
      pos += v.rate * wob;
    }
    buf = out;
  }
  const g = Math.pow(10, v.db / 20);
  const at = Math.floor(v.t * SR);
  const n = Math.min(buf.length / 2, ns - at);
  for (let i = 0; i < n; i++) {
    mix[2 * (at + i)] += buf[2 * i] * g;
    mix[2 * (at + i) + 1] += buf[2 * i + 1] * g;
  }
}

// ── the kick returns: layered back on top, untouched by every hand ────
for (let i = 0; i < mix.length; i++) mix[i] += kickBus[i];

// ── warping: the whole record is wet — a global variable-rate pass.
//    The platter drags mid-breath (a hand on the vinyl), the tempo
//    RAMPS ~6% faster through the back half (138 → ~146 effective BPM,
//    kick and all — the rhythm stays locked because everything warps
//    together), and the mist rides a warped-record wobble out ──────────
{
  const rate = (t) => {
    let r = 1;
    for (const c of [bar(13.5), bar(45.5)]) {
      const d = (t - c) / 1.4;
      if (d > -1 && d < 1) r *= 1 - 0.38 * (0.5 + 0.5 * Math.cos(Math.PI * d)) ** 2;
    }
    // the trance accelerando: ramp in across drop C, hold through stone,
    // let go across the coda
    if (t >= bar(28) && t < bar(48)) r *= 1 + 0.06 * smooth((t - bar(28)) / (bar(48) - bar(28)));
    else if (t >= bar(48) && t < bar(76)) r *= 1.06;
    else if (t >= bar(76) && t < bar(80)) r *= 1 + 0.06 * (1 - smooth((t - bar(76)) / (4 * BAR)));
    if (t > bar(80)) {
      const w = Math.min(1, (t - bar(80)) / (2 * BAR));
      r *= 1 + 0.012 * w * Math.sin(TAU * 0.8 * t);
    }
    return r;
  };
  const warped = new Float32Array(mix.length);
  let pos = 0;
  for (let i = 0; i < ns; i++) {
    const j = Math.floor(pos);
    if (j >= ns - 1) break;
    const fr = pos - j;
    warped[2 * i] = mix[2 * j] * (1 - fr) + mix[2 * (j + 1)] * fr;
    warped[2 * i + 1] = mix[2 * j + 1] * (1 - fr) + mix[2 * (j + 1) + 1] * fr;
    pos += rate(i / SR);
  }
  mix.set(warped);
}

// re-peak after the layers so the master sees a sane level
let peak = 0;
for (let i = 0; i < mix.length; i++) { const v = Math.abs(mix[i]); if (v > peak) peak = v; }
if (peak > 0.9) { const g = 0.9 / peak; for (let i = 0; i < mix.length; i++) mix[i] *= g; }

const mixedPath = resolve(OUT, "wattajetta.mixed.f32.raw");
writeFileSync(mixedPath, Buffer.from(mix.buffer, mix.byteOffset, mix.length * 4));

// ── master: firmer than the pure cut — the crunch ramp wants a spine,
//    but the opening still breathes ─────────────────────────────────────
const MASTER = [
  "highpass=f=24",
  "acompressor=threshold=-19dB:ratio=2.8:attack=10:release=150:makeup=2.2:knee=6",
  "equalizer=f=50:t=q:w=1.2:g=2.5", // the boom under the kick — no treble boost; laptop speakers read it as tang
  "alimiter=limit=0.96:attack=4:release=70",
  // the accelerando consumes source faster than the clock — trim the
  // dead air it leaves at the tail
  "areverse", "silenceremove=start_periods=1:start_threshold=-70dB", "areverse",
];
const mp3 = resolve(OUT, "wattajetta.mp3");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mixedPath,
  "-af", MASTER.join(","), "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", "title=wattajetta", "-metadata", "album=pixsies",
  mp3], { stdio: "inherit" });
for (const p of [rawPath, kickPath, mixedPath]) { try { unlinkSync(p); } catch {} }
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${mp3} (glass→bronze→steel→stone · chimes/bloops/choir/underwater · scratched + warped)`);
