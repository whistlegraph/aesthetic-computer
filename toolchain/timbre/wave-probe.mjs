#!/usr/bin/env node
// wave-probe.mjs — measure notepat's wave types into the same timbre space
// the GM catalogue was measured into.
//
// Renders every playable AC wave through the REAL `lib/sound/synth.mjs`
// (the AudioWorklet class, driven sample-by-sample under Node) using
// notepat's own note settings, then measures brightness and bite with
// `brightness.mjs` — the same math as `slab/menuband/bin/gm-timbre-probe.c`.
//
// Wessel's equalization (§B) applies: one pitch for every wave, one
// amplitude contour for every wave, RMS-matched before analysis. What is
// left is timbre.
//
// Run:   node toolchain/timbre/wave-probe.mjs
// Write: node toolchain/timbre/wave-probe.mjs --write
//
// `--write` regenerates system/public/aesthetic.computer/lib/sound/wave-timbre.mjs,
// which is what the runtime actually reads. Nothing measures at boot.

import { writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { measure, normalizeBite, barkToHz } from "./brightness.mjs";

const SAMPLE_RATE = 48000;
// synth.mjs reads `sampleRate` as a bare AudioWorklet global. Set it before
// the module is imported so every wavelength it precomputes is right.
globalThis.sampleRate = SAMPLE_RATE;

// `harp` pre-fills its Karplus-Strong delay line from Math.random, and
// `noise-white` draws from it every sample, so an un-pinned probe reports a
// different brightness every run (harp moved 7.2 → 8.1 between two). Swap in
// a seeded generator and average several draws, so the number describes the
// VOICE rather than one lucky pluck.
const TRIALS = 8;
let rngState = 0;
Math.random = () => {
  rngState ^= rngState << 13; rngState ^= rngState >>> 17; rngState ^= rngState << 5;
  return (rngState >>> 0) / 4294967296;
};
function seedRandom(seed) { rngState = seed >>> 0 || 0x9e3779b9; }

const { default: Synth } = await import(
  "../../system/public/aesthetic.computer/lib/sound/synth.mjs"
);

const root = join(dirname(fileURLToPath(import.meta.url)), "../..");

const TONE = 261.6255653;      // C4 — the same reference pitch the GM probe used
const SECONDS = 1.0;
const FRAMES = Math.round(SAMPLE_RATE * SECONDS);
const ATTACK = 0.0005;         // notepat's `attack` const, a fraction of duration

let nextId = 1;
function voice(type, { tone = TONE, attack = ATTACK, decay = 0, volume = 1 } = {}) {
  return new Synth({
    type,
    id: nextId++,
    options: { tone },
    duration: FRAMES,
    attack: Math.round(FRAMES * attack),
    decay: Math.round(FRAMES * decay),
    volume,
    pan: 0,
  });
}

function renderVoices(voices) {
  const out = new Float32Array(FRAMES);
  for (let i = 0; i < FRAMES; i += 1) {
    let s = 0;
    for (const v of voices) s += v.next(0);
    out[i] = Number.isFinite(s) ? s : 0;
  }
  return out;
}

// notepat's `composite` is hand-authored additive synthesis — five
// oscillators, each with its own attack, detune and level. Reproduced here
// exactly (minus the per-note randInt jitter, pinned to 0 so the measurement
// is reproducible) because it is a wave you can pick, not a synth primitive.
function renderComposite() {
  const v = 1;
  return renderVoices([
    voice("sine",     { tone: TONE,     attack: 0.0025, decay: 0.9, volume: v }),
    voice("sine",     { tone: TONE + 9, attack: 0.0025,             volume: v / 3 }),
    voice("sawtooth", { tone: TONE,     attack: ATTACK, decay: 0.9, volume: v / 48 }),
    voice("triangle", { tone: TONE + 8, attack: 0.999,              volume: v / 32 }),
    voice("square",   { tone: TONE,     attack: 0.05,               volume: v / 64 }),
  ]);
}

// The waves that ARE waves. `gm`, `stample` and `drum` are modes — a patch
// picker, a sample loader, a 12-drum kit keyed by pitch class — so they have
// no single timbre to measure and are excluded from the ordering.
const WAVES = ["sine", "triangle", "sawtooth", "square", "harp", "whistle", "composite"];
const MODES = ["gm", "stample", "drum"];

// Each wave is measured `TRIALS` times from different seeds and averaged.
// Deterministic waves land on the same number every trial; stochastic ones
// (harp) settle instead of reporting whichever draw came up.
const measured = WAVES.map((w) => {
  const runs = [];
  for (let t = 0; t < TRIALS; t += 1) {
    seedRandom(0x5EED + t * 2654435761);
    nextId = 1 + t * 16;
    runs.push(measure(w === "composite" ? renderComposite() : renderVoices([voice(w)]),
                      SAMPLE_RATE));
  }
  const mean = (pick) => runs.reduce((a, r) => a + pick(r), 0) / runs.length;
  return {
    brightness: mean((r) => r.brightness),
    riseMs: mean((r) => r.riseMs),
    asyncMs: mean((r) => r.asyncMs),
    rms: mean((r) => r.rms),
    silent: runs.every((r) => r.silent),
    spread: Math.max(...runs.map((r) => r.brightness)) -
            Math.min(...runs.map((r) => r.brightness)),
  };
});
const bites = normalizeBite(measured);

const rows = WAVES.map((wave, i) => ({
  wave,
  brightness: measured[i].brightness,
  bite: bites[i],
  riseMs: measured[i].riseMs,
  asyncMs: measured[i].asyncMs,
  silent: measured[i].silent,
  spread: measured[i].spread,
}));

const sorted = [...rows].sort((a, b) => a.brightness - b.brightness);

process.stdout.write(`wave        bark   ~Hz    bite   rise_ms  async_ms  spread\n`);
for (const r of sorted) {
  process.stdout.write(
    `${r.wave.padEnd(11)}${r.brightness.toFixed(2).padStart(5)} ` +
    `${Math.round(barkToHz(r.brightness)).toString().padStart(6)} ` +
    `${r.bite.toFixed(3).padStart(7)} ${r.riseMs.toFixed(2).padStart(8)} ` +
    `${r.asyncMs.toFixed(2).padStart(9)} ${r.spread.toFixed(3).padStart(7)}` +
    `${r.silent ? "  SILENT" : ""}\n`,
  );
}
process.stdout.write(
  `\n(spread = brightness range across ${TRIALS} seeded trials; only stochastic voices move)\n`,
);
process.stdout.write(`\ndarkest → brightest: ${sorted.map((r) => r.wave).join(" → ")}\n`);

if (!process.argv.includes("--write")) {
  process.stdout.write("\n(run with --write to regenerate lib/sound/wave-timbre.mjs)\n");
  process.exit(0);
}

const entries = sorted
  .map((r) => `  ${r.wave}: { brightness: ${r.brightness.toFixed(4)}, ` +
              `bite: ${r.bite.toFixed(4)}, riseMs: ${r.riseMs.toFixed(3)}, ` +
              `asyncMs: ${r.asyncMs.toFixed(3)} },`)
  .join("\n");

const out = `// wave-timbre.mjs — GENERATED by toolchain/timbre/wave-probe.mjs. Do not edit.
//
// Measured coordinates for AC's wave types, rendered through lib/sound/synth.mjs
// at C4 (${TONE.toFixed(2)} Hz) with one shared amplitude contour and RMS-matched
// before analysis, then measured with toolchain/timbre/brightness.mjs.
//
// After David Wessel, "Timbre Space as a Musical Control Structure", Computer
// Music Journal 3(2), 1979. \`brightness\` is the Bark centroid of a Zwicker-style
// excitation pattern — the acoustic correlate Wessel used to interpret his
// measured vertical axis. \`bite\` combines onset rate with cross-band onset
// spread, his horizontal one. NO listeners were asked and NO multidimensional
// scaling was run, so this is a proxy for a timbre space rather than one.
//
// The space is stimulus-set-relative: these numbers describe THESE waves at
// THIS pitch. Add a wave and re-run the probe; do not hand-edit.

/** Per-wave coordinates, darkest first. */
export const WAVE_TIMBRE = {
${entries}
};

/**
 * Wave types ordered darkest → brightest. This is the order notepat's Tab
 * cycle walks, so Tab is a brightness ramp instead of an authoring accident.
 */
export const WAVES_BY_BRIGHTNESS = [
${sorted.map((r) => `  "${r.wave}",`).join("\n")}
];

/**
 * Distance between two waves in the measured space, or \`null\` when either
 * side has no measured timbre (the \`gm\` / \`stample\` / \`drum\` modes, which
 * are patch pickers rather than single voices).
 *
 * Brightness is rescaled to the 0…1 range \`bite\` already uses so neither axis
 * dominates the metric purely through its units.
 */
export function waveDistance(a, b) {
  const p = WAVE_TIMBRE[a], q = WAVE_TIMBRE[b];
  if (!p || !q) return null;
  const db = (p.brightness - q.brightness) / BRIGHTNESS_SPAN;
  const dt = p.bite - q.bite;
  return Math.sqrt(db * db + dt * dt);
}

/** Brightness range across the measured set — the normalizer for distances. */
export const BRIGHTNESS_SPAN = ${(
  Math.max(...rows.map((r) => r.brightness)) - Math.min(...rows.map((r) => r.brightness))
).toFixed(4)};

/** Wave names that are modes, not measurable single timbres. */
export const UNMEASURED_WAVES = [${MODES.map((m) => `"${m}"`).join(", ")}];
`;

const dest = join(root, "system/public/aesthetic.computer/lib/sound/wave-timbre.mjs");
writeFileSync(dest, out);
process.stdout.write(`\nwrote ${dest.replace(root + "/", "")}\n`);
