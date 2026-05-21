#!/usr/bin/env node
// compose.mjs — `gradus`, a /pop track. A classical two-voice piece
// composed entirely from the new pop compositional tools:
//
//   • lib/meter.mjs   — Fibonacci numbers set the FORM: phrase lengths
//     are consecutive Fibonacci values laid out as an arch, and
//     fibPartition places an internal rhythmic stress inside each phrase.
//   • lib/counterpoint.mjs — generateFirstSpecies writes the upper voice
//     above an algorithmically-grown cantus firmus, in strict Fux
//     first species; checkFirstSpecies verifies every phrase.
//   • dance/synths/sinepower.mjs — both voices are rendered with the
//     stacked-sine "pad" voice, an organ-ish sustain.
//
// Key: D dorian (Fux's home key). Output: gradus.mp3 + the two voices
// as .np scores. Deterministic — fixed RNG seed, same track every run.
//
// Usage:  node pop/gradus/bin/compose.mjs

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { fibSequence, fibPartition } from "../../lib/meter.mjs";
import { generateFirstSpecies, checkFirstSpecies } from "../../lib/counterpoint.mjs";
import { mixEventSinePower } from "../../dance/synths/sinepower.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SR = 48_000;
const PRESET = "pad";
const TARGET_SEC = 84;          // aim for ~1:24
const TAIL = 2.6;               // decay tail past the last note
let BPM, BEAT;                  // tempo is solved to the target length

// ── D dorian ──────────────────────────────────────────────────────────
const TONIC = 2;                              // pitch class D
const DORIAN = [0, 2, 3, 5, 7, 9, 10];        // dorian degrees
const inKey = (m) => DORIAN.includes(((m - TONIC) % 12 + 12) % 12);
const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
const midiName = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);

// scale pool of D-dorian notes spanning the cantus-firmus register
function poolBetween(lo, hi) {
  const p = [];
  for (let m = lo; m <= hi; m++) if (inKey(m)) p.push(m);
  return p;
}

// deterministic RNG (mulberry32)
function rng(seed) {
  return () => {
    seed = (seed + 0x6D2B79F5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// ── cantus firmus ─────────────────────────────────────────────────────
// A stepwise walk through the scale pool: starts and ends on the tonic,
// mostly steps with the odd third, one shaped climax. Lower voice.
function makeCantus(len, pool, rand) {
  const tonicIdxs = pool.map((m, i) => (inKey(m + 0) && ((m - TONIC) % 12 + 12) % 12 === 0 ? i : -1)).filter((i) => i >= 0);
  const startIdx = tonicIdxs[0];                       // lowest tonic
  const cf = [startIdx];
  for (let i = 1; i < len - 1; i++) {
    const prev = cf[i - 1];
    const climax = i / len < 0.62;                     // climb early, fall late
    let step;
    const r = rand();
    if (r < 0.62) step = climax ? 1 : -1;              // a scale step
    else if (r < 0.82) step = climax ? -1 : 1;         // a step the other way
    else step = (climax ? 1 : -1) * 2;                 // a third
    let idx = prev + step;
    idx = Math.max(0, Math.min(pool.length - 1, idx));
    if (idx === prev) idx = prev + (climax ? 1 : -1);
    idx = Math.max(0, Math.min(pool.length - 1, idx));
    cf.push(idx);
  }
  // resolve to the tonic by step
  const finalIdx = tonicIdxs[0];
  cf[len - 1] = finalIdx;
  if (Math.abs(cf[len - 2] - finalIdx) !== 1)
    cf[len - 2] = finalIdx + 1;                        // penultimate steps in
  return cf.map((i) => pool[Math.max(0, Math.min(pool.length - 1, i))]);
}

// ── form: Fibonacci arch of phrases ───────────────────────────────────
// Phrase lengths are consecutive Fibonacci numbers mirrored into an arch.
const fib = fibSequence(6);                     // 1,2,3,5,8,13
const climb = [fib[3], fib[4], fib[5]];         // 5, 8, 13
const PHRASES = [...climb, ...climb.slice().reverse()]; // 5,8,13,13,8,5 = 52 cols
const rand = rng(0x6772 /* "gr" */);

// the two highest (8-note) phrases sit an octave up — registral climax
const phrases = [];
let totalCols = 0;
for (let p = 0; p < PHRASES.length; p++) {
  const len = PHRASES[p];
  const high = len === 13;                      // climax phrases
  const pool = high ? poolBetween(57, 79) : poolBetween(50, 69);

  let cantus = null, counter = null;
  for (let tries = 0; tries < 80 && !counter; tries++) {
    cantus = makeCantus(len, pool, rand);
    counter = generateFirstSpecies(cantus, { tonic: TONIC, scale: DORIAN });
  }
  if (!counter) { console.error(`✗ phrase ${p + 1}: no counterpoint found`); process.exit(1); }

  const check = checkFirstSpecies(cantus, counter);
  phrases.push({ len, high, cantus, counter, check });
  totalCols += len;
}

// ── rhythm: fibPartition stresses + cadence holds ─────────────────────
// Most columns are 2 beats. fibPartition(len) marks an internal stress
// (3 beats). The last column of each phrase is a 4-beat cadence; the
// very last column of the piece is held 8 beats.
const columns = []; // { cantus, counter, beats }
for (let p = 0; p < phrases.length; p++) {
  const ph = phrases[p];
  const stressAt = fibPartition(ph.len)[0];     // first golden-split point
  for (let i = 0; i < ph.len; i++) {
    let beats = 2;
    if (i === stressAt && i !== ph.len - 1) beats = 3;
    if (i === ph.len - 1) beats = (p === phrases.length - 1) ? 8 : 4;
    columns.push({ cantus: ph.cantus[i], counter: ph.counter[i], beats });
  }
}

// ── tempo solved to the target length ─────────────────────────────────
const totalBeats = columns.reduce((s, c) => s + c.beats, 0);
BEAT = (TARGET_SEC - TAIL) / totalBeats;
BPM = Math.round(60 / BEAT);

// ── render — two voices, sinepower pad ────────────────────────────────
const totalSec = totalBeats * BEAT + TAIL;
const low  = new Float32Array(Math.ceil(totalSec * SR));
const high = new Float32Array(low.length);

let t = 0;
for (const col of columns) {
  const dur = col.beats * BEAT;
  mixEventSinePower({ startSec: t, midi: col.cantus,  durSec: dur, gain: 0.80 },
    low,  { sampleRate: SR, preset: PRESET });
  mixEventSinePower({ startSec: t, midi: col.counter, durSec: dur, gain: 0.72 },
    high, { sampleRate: SR, preset: PRESET });
  t += dur;
}

// gentle stereo spread — cantus left-leaning, counterpoint right-leaning
const stereo = new Float32Array(low.length * 2);
let peak = 0;
for (let i = 0; i < low.length; i++) {
  const L = low[i] * 0.92 + high[i] * 0.32;
  const R = high[i] * 0.92 + low[i] * 0.32;
  stereo[i * 2] = L; stereo[i * 2 + 1] = R;
  peak = Math.max(peak, Math.abs(L), Math.abs(R));
}
if (peak > 0) { const n = 0.92 / peak; for (let i = 0; i < stereo.length; i++) stereo[i] *= n; }

// ── write .np scores + mp3 ────────────────────────────────────────────
function toNp(title, pick) {
  let s = `# ${title}\n# gradus — D dorian · ${BPM} bpm · Fibonacci-arch first-species counterpoint\n\n`;
  let col = 0;
  phrases.forEach((ph, p) => {
    s += `# phrase ${p + 1} — ${ph.len} notes${ph.high ? " (octave climax)" : ""}\n`;
    const cells = [];
    for (let i = 0; i < ph.len; i++) {
      const c = columns[col++];
      cells.push(`${midiName(pick(c))}:_*${c.beats}`);
    }
    s += cells.join(" ") + "\n\n";
  });
  return s;
}
mkdirSync(LANE, { recursive: true });
writeFileSync(resolve(LANE, "gradus.cantus.np"),  toNp("cantus voice (lower)",      (c) => c.cantus));
writeFileSync(resolve(LANE, "gradus.counter.np"), toNp("counterpoint voice (upper)", (c) => c.counter));

const mp3 = resolve(LANE, "gradus.mp3");
const raw = resolve(LANE, ".gradus.f32.raw");
const b = Buffer.alloc(stereo.length * 4);
for (let i = 0; i < stereo.length; i++) b.writeFloatLE(stereo[i], i * 4);
writeFileSync(raw, b);
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", raw,
  "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", "title=gradus", "-metadata", "album=pixsies",
  "-metadata", "genre=classical", mp3], { stdio: "inherit" });
try { unlinkSync(raw); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

// ── report ────────────────────────────────────────────────────────────
const mm = Math.floor(totalSec / 60), ss = Math.round(totalSec % 60);
console.log(`\ngradus — ${phrases.length} phrases, ${totalCols} columns, ${totalBeats} beats`);
phrases.forEach((ph, p) => {
  const flags = [...ph.check.violations.map((v) => "✗" + v.rule),
                 ...ph.check.warnings.map((w) => "·" + w.rule)];
  console.log(`  phrase ${p + 1}  ${String(ph.len).padStart(2)} notes  ` +
    `${ph.check.ok ? "valid first species" : "RULE BREAK"}` +
    (flags.length ? `  [${flags.join(" ")}]` : ""));
});
console.log(`\n✓ ${mp3}`);
console.log(`✓ gradus.cantus.np · gradus.counter.np`);
console.log(`  duration ≈ ${mm}:${String(ss).padStart(2, "0")}`);
