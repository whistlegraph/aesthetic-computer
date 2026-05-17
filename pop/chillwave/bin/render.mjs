#!/usr/bin/env node
// chillwave/bin/render.mjs — render a chillwave .np score to mp3.
//
// Layers (signal flow, all summed mono):
//   1. waves   — pink noise → slow LFO low-pass (~0.12 Hz, 8s wave period)
//                + slow tremolo → stereo doubling via tiny offset
//   2. sweeps  — white noise → very slow LFO low-pass (~0.05 Hz, 20s sweep)
//                + 1/8 amp duck on the troughs
//   3. bubbles — short descending sine blips (~80–150ms, 1500→400Hz),
//                sparse Poisson-distributed, random pan
//   4. pad     — sinepower pad chord under each .np note (Am pentatonic)
//   5. bells   — sinebells from the .np score, sparse, far back
//
// Section flags in the .np score (e.g. "# drift 1 16 [waves, sweep-slow,
// bubbles-sparse]") gate which layers fire in each section.
//
// Usage:
//   node bin/render.mjs --slug explobeach
//   node bin/render.mjs --slug explobeach --bpm 70 --transpose 0
//   node bin/render.mjs --slug explobeach --layer waves   (solo a layer)
//   node bin/render.mjs --slug explobeach --no-bubbles
//
// Output: pop/chillwave/out/<slug>.mp3

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";
import { applyBitcrush, applyFlange } from "../../dance/synths/fx.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

// ── AC native sample loader (headerless float32 mono @ 48 kHz) ──────
// Used to drop real recorded animal/piano samples from fedac/native/
// samples/ into the track. Bird/whale/owl ground the beach scene with
// actual recordings rather than only synth chirps.
const ZOO_DIR = `${REPO}/fedac/native/samples/zoo`;
function loadRawSample(path) {
  try {
    const buf = readFileSync(path);
    const ab = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength);
    return new Float32Array(ab);
  } catch (_e) {
    return null;
  }
}
function paintSample(sample, startSec, gain, pan, pitchSteps = 0) {
  if (!sample || sample.length === 0) return;
  const startIdx = Math.floor(startSec * SAMPLE_RATE);
  const step = Math.pow(2, pitchSteps / 12);
  const nOut = Math.floor(sample.length / step);
  const lG = Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
  const rG = Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;
  for (let i = 0; i < nOut; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= LEN_SAMP) break;
    const srcF = i * step;
    const s0 = Math.floor(srcF);
    const s1 = Math.min(sample.length - 1, s0 + 1);
    const frac = srcF - s0;
    const s = sample[s0] * (1 - frac) + sample[s1] * frac;
    outL[dst] += s * gain * lG;
    outR[dst] += s * gain * rG;
  }
}

// ── args ─────────────────────────────────────────────────────────────
const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG       = flags.slug || "undabeach";
const SCORE_PATH = `${LANE}/${SLUG}.np`;
const BPM        = Number(flags.bpm ?? 70);
const TRANSPOSE  = Number(flags.transpose ?? 0);
const SAMPLE_RATE = 48_000;
const OUT_PATH   = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${LANE}/out/${SLUG}.mp3`;

const SOLO  = flags.layer ? String(flags.layer).toLowerCase() : null;
const ONLY  = (name) => !SOLO || SOLO === name;
const SKIP  = {
  waves:   flags["no-waves"]   === true,
  sweeps:  flags["no-sweeps"]  === true,
  bubbles: flags["no-bubbles"] === true,
  shakers: flags["no-shakers"] === true,
  birds:   flags["no-birds"]   === true,
  kick:    flags["no-kick"]    === true,
  pad:     flags["no-pad"]     === true,
  bells:   flags["no-bells"]   === true,
};

// ── note utils ───────────────────────────────────────────────────────
const NOTE_BASE = { C:0,"C#":1,DB:1,D:2,"D#":3,EB:3,E:4,F:5,
                    "F#":6,GB:6,G:7,"G#":8,AB:8,A:9,"A#":10,BB:10,B:11 };
function noteToMidi(s) {
  s = s.toUpperCase();
  const oct = parseInt(s.slice(-1), 10);
  return 12 * (oct + 1) + NOTE_BASE[s.slice(0, -1)];
}
function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// ── parse .np score with section flags ───────────────────────────────
// Returns { events: [{startSec, midi, durSec, section}],
//           sections: [{name, startSec, endSec, flags: Set}],
//           totalSec }
function parseScore(path, bpm) {
  const lines = readFileSync(path, "utf8").split("\n");
  const beat_s = 60.0 / bpm;
  let beat_pos = 0;
  const events = [];
  const sections = [];
  let curSection = null;

  const HEADER_RE = /^([a-z][a-z0-9-]*(?:\s+\d+)?)\s+(\d+)(?:\s+\[([^\]]+)\])?\s*$/i;

  for (const raw of lines) {
    const t = raw.trim();
    if (!t) continue;
    if (t.startsWith("#")) {
      // try parsing a section header out of a comment line:
      //   "# drift 1 16 [waves, sweep-slow]"
      //   "# tide-in 8"
      const body = t.replace(/^#\s*/, "");
      const m = body.match(HEADER_RE);
      if (m) {
        // close prior section
        if (curSection) {
          curSection.endSec = beat_pos * beat_s;
          sections.push(curSection);
        }
        const name = m[1].trim();
        const bars = Number(m[2]);
        const flagStr = m[3] || "";
        const flags = new Set(
          flagStr.split(",").map((s) => s.trim()).filter(Boolean)
        );
        curSection = {
          name, bars,
          startSec: beat_pos * beat_s,
          endSec:   null,            // filled when next header or eof
          flags,
        };
      }
      continue;
    }
    // melody token line
    for (const tok of t.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      const note = m[1];
      const weight = Number(m[3] ?? 1);
      events.push({
        startSec: beat_pos * beat_s,
        midi: noteToMidi(note) + TRANSPOSE,
        durSec: weight * beat_s,
        section: curSection?.name ?? null,
      });
      beat_pos += weight;
    }
  }
  if (curSection) {
    curSection.endSec = beat_pos * beat_s;
    sections.push(curSection);
  }
  const totalSec = beat_pos * beat_s;
  return { events, sections, totalSec };
}

const score = parseScore(SCORE_PATH, BPM);
const TAIL_SEC = 2.5;
const LEN_SEC  = score.totalSec + TAIL_SEC;
const LEN_SAMP = Math.ceil(LEN_SEC * SAMPLE_RATE);

console.log(`→ score: ${score.events.length} notes · ${score.totalSec.toFixed(1)}s · ${score.sections.length} sections`);

// ── event log for struct.json sidecar ────────────────────────────────
// Each render-function pushes its triggered events here so the preview-
// score visualizer can flash notes on the timeline. Keys mirror the
// lane names the visualizer expects.
const eventLog = {
  bells: [], waves: [], kick: [], sub: [],
  hat: [], bubbles: [], birds: [], fx: [], sfx: [],
};

// ── stereo output buffer (interleaved) ───────────────────────────────
// Each layer accumulates into outL/outR. To capture per-lane mix
// contributions for the preview-score visualizer, we swap outL/outR
// with fresh per-lane buffers around each renderer call (renderLane),
// then sum the layer back into the global output AND save a
// downsampled mono copy as the lane's audio buffer for display.
let outL = new Float32Array(LEN_SAMP);
let outR = new Float32Array(LEN_SAMP);

// per-lane downsampled mono buffers (LANE_DISPLAY_SR Hz) keyed by lane
const LANE_DISPLAY_SR = 4000;
const laneBuffers = {};                  // key → Float32Array

function downsampleMono(L, R, sr, outSr) {
  const factor = sr / outSr;
  const outLen = Math.ceil(L.length / factor);
  const out = new Float32Array(outLen);
  for (let i = 0; i < outLen; i++) {
    const s0 = Math.floor(i * factor);
    const s1 = Math.min(L.length, Math.floor((i + 1) * factor));
    let pk = 0;
    for (let s = s0; s < s1; s++) {
      const a = Math.abs((L[s] + R[s]) * 0.5);
      if (a > pk) pk = a;
    }
    out[i] = pk;     // peak-of-mono per output sample → cheap waveform display
  }
  return out;
}

function renderLane(key, fn) {
  const savedL = outL, savedR = outR;
  const layerL = new Float32Array(LEN_SAMP);
  const layerR = new Float32Array(LEN_SAMP);
  outL = layerL;
  outR = layerR;
  fn();
  laneBuffers[key] = downsampleMono(layerL, layerR, SAMPLE_RATE, LANE_DISPLAY_SR);
  for (let i = 0; i < LEN_SAMP; i++) {
    savedL[i] += layerL[i];
    savedR[i] += layerR[i];
  }
  outL = savedL;
  outR = savedR;
}

// ── deterministic RNG (mulberry32, seeded from slug + bpm) ───────────
function makeRng(seedStr) {
  let h = 2166136261;
  for (let i = 0; i < seedStr.length; i++) {
    h ^= seedStr.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  let a = h >>> 0;
  return function () {
    a = (a + 0x6D2B79F5) | 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rng = makeRng(`${SLUG}:${BPM}:${TRANSPOSE}`);

// ── section helpers ──────────────────────────────────────────────────
function sectionAt(timeSec) {
  for (const s of score.sections) {
    if (timeSec >= s.startSec && timeSec < s.endSec) return s;
  }
  return null;
}
function bellGainAt(timeSec) {
  // fade bells in over the first 4s of drift-1, out over the last
  // 4s of tide-out.
  const sec = sectionAt(timeSec);
  if (!sec) return 0.0;
  if (sec.flags.has("no-bells")) return 0.0;
  if (sec.name.startsWith("tide-out") && sec.flags.has("no-bells-last-4")) {
    // fade out across the FIRST half of tide-out, silent after
    const half = sec.startSec + (sec.endSec - sec.startSec) * 0.5;
    if (timeSec >= half) return 0.0;
    const r = (timeSec - sec.startSec) / (half - sec.startSec);
    return 1 - Math.max(0, Math.min(1, r));
  }
  return 1.0;
}
function bubbleDensityAt(timeSec) {
  const sec = sectionAt(timeSec);
  if (!sec) return 0.0;
  if (sec.flags.has("bubbles-dense"))  return 1.0;
  if (sec.flags.has("bubbles-sparse")) return 0.35;
  return 0.15; // background trickle
}
function waveAmpAt(timeSec) {
  const sec = sectionAt(timeSec);
  if (!sec) return 0.85;
  // tide-in / tide-out get fades
  const dur = sec.endSec - sec.startSec;
  const intoSec = timeSec - sec.startSec;
  if (sec.flags.has("waves-fade-in")) {
    return Math.max(0, Math.min(1, intoSec / dur));
  }
  if (sec.flags.has("waves-fade-out")) {
    return Math.max(0, Math.min(1, 1 - intoSec / dur));
  }
  return 0.85;
}

// ── pink noise generator (Paul Kellet's economy filter) ──────────────
function pinkSample(state) {
  // state: array of 7 numbers
  const w = rng() * 2 - 1;
  state[0] = 0.99886 * state[0] + w * 0.0555179;
  state[1] = 0.99332 * state[1] + w * 0.0750759;
  state[2] = 0.96900 * state[2] + w * 0.1538520;
  state[3] = 0.86650 * state[3] + w * 0.3104856;
  state[4] = 0.55000 * state[4] + w * 0.5329522;
  state[5] = -0.7616 * state[5] - w * 0.0168980;
  const pink = state[0]+state[1]+state[2]+state[3]+state[4]+state[5]+state[6]+w*0.5362;
  state[6] = w * 0.115926;
  return pink * 0.11;
}

// ── 1. waves layer (event-composed) ──────────────────────────────────
// Discrete wave events of five types, overlapping in time. Each event
// has its own envelope, noise color, resonant filter, and harmonic
// pitch (tied to A minor pentatonic so the ocean "speaks in the key").
//
// Types:
//   crash       — short bright break (1–2s), peaky envelope, high harmonics
//   roller      — long deep swell (5–8s), slow rise+fall, low harmonics
//   wash        — medium gentle (3–5s), no peak, mid harmonics
//   boom        — deep sub-felt (4–6s), low LP + sine sub-pulse, lowest harmonics
//   hiss        — bright pebble sizzle (0.7–1.5s), fast envelope, very high
//
// Density + type mix vary per section (tide-in: washes + hisses fade in;
// drift: balanced; swell: crashes + booms; tide-out: tapering tail).
function renderWaves() {
  if (SKIP.waves) return;
  if (SOLO && !ONLY("waves")) return;
  console.log("  waves …");

  // Harmonic series for A — wave events resonate at one of these so
  // the ocean stays in the key of the song.
  const HARMONICS = {
    low:  [55, 73, 82, 110, 130, 147],            // A1 D2 E2 A2 C3 D3
    mid:  [165, 196, 220, 247, 262, 294, 330],     // E3 G3 A3 B3 C4 D4 E4
    high: [392, 440, 523, 587, 659, 784, 880],     // G4 A4 C5 D5 E5 G5 A5
    vhigh:[988, 1047, 1175, 1318, 1568, 1760, 2093] // B5 C6 D6 E6 G6 A6 C7
  };
  const pick = (arr) => arr[Math.floor(rng() * arr.length)];

  // Event renderer — paints one wave event into outL/outR.
  // Params:
  //   start         — start time in seconds
  //   durSec        — total event length (incl. tail)
  //   noise         — "white" | "pink"
  //   env           — { attack, peak, release } fractions of durSec
  //                   (attack rises 0→1, peak holds, release falls 1→0)
  //   cutoffArc     — [openHz, peakHz, closeHz] — LP cutoff moves
  //                   through these at attack-end, peak-end, release-end
  //   resHz         — resonant peak frequency (harmonic) — SVF center
  //   resQ          — Q factor (higher = more singing)
  //   gain          — peak amplitude (after envelope)
  //   pan           — -1..1
  //   subHz         — optional sub-bass sine added (for booms)
  function paintWave({ start, durSec, noise, env, cutoffArc, resHz, resQ, gain, pan, subHz }) {
    const startIdx = Math.floor(start * SAMPLE_RATE);
    const nSamp    = Math.floor(durSec * SAMPLE_RATE);
    const attackS  = Math.max(1, Math.floor(env.attack  * nSamp));
    const peakS    = Math.max(1, Math.floor(env.peak    * nSamp));
    const releaseS = Math.max(1, nSamp - attackS - peakS);
    const cutA = cutoffArc[0], cutB = cutoffArc[1], cutC = cutoffArc[2];
    const lGain = Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
    const rGain = Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;

    // SVF state (one-pole-ish bandpass + LP blend for body)
    let low = 0, band = 0;
    // damp = 1/Q
    const damp = 1 / Math.max(0.5, resQ);
    // Resonant frequency coefficient (kept constant — the resonance is
    // the harmonic pitch; the LP cutoff is the brightness envelope).
    const fRes = 2 * Math.sin(Math.PI * Math.min(resHz, SAMPLE_RATE / 3) / SAMPLE_RATE);

    // Second filter: a one-pole LP that gates overall brightness via
    // the cutoff envelope (this is what gives "the wave is opening up
    // and crashing then dying back into the deep").
    let lpOut = 0;
    const dt = 1 / SAMPLE_RATE;

    // Pink noise state (per event) — only used for "pink" mode.
    const pState = [0,0,0,0,0,0,0];

    // Sub-bass phase (only for boom)
    const subOmega = subHz ? 2 * Math.PI * subHz / SAMPLE_RATE : 0;
    let subPhase = 0;

    for (let i = 0; i < nSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) continue;

      // envelope (0..1 over the three regions)
      let envVal;
      let cutHz;
      if (i < attackS) {
        const u = i / attackS;
        envVal = 0.5 - 0.5 * Math.cos(Math.PI * u);
        cutHz = cutA + (cutB - cutA) * u;
      } else if (i < attackS + peakS) {
        envVal = 1.0;
        cutHz = cutB;
      } else {
        const u = (i - attackS - peakS) / releaseS;
        envVal = 0.5 + 0.5 * Math.cos(Math.PI * u);
        cutHz = cutB + (cutC - cutB) * u;
      }

      // raw noise source
      const x = noise === "pink" ? pinkSample(pState) : (rng() * 2 - 1);

      // resonant SVF (gives the harmonic ring at resHz)
      const high = x - low - damp * band;
      band += fRes * high;
      low  += fRes * band;
      // blend bandpass (resonant ring) with the raw LP path for body
      const ring = band * 0.9;

      // moving LP for brightness gating
      const rc = 1 / (2 * Math.PI * Math.max(40, cutHz));
      const alpha = dt / (rc + dt);
      lpOut += alpha * (x + ring * 0.45 - lpOut);

      // optional sub-bass sine (for boom events)
      let sub = 0;
      if (subHz) {
        subPhase += subOmega;
        sub = Math.sin(subPhase) * 0.5;
      }

      const s = (lpOut + sub) * envVal * gain;
      outL[dst] += s * lGain;
      outR[dst] += s * rGain;
    }
  }

  // Type recipes. Each returns a fully-parameterized event when called.
  const RECIPES = {
    crash: () => ({
      durSec: 1.1 + rng() * 0.9,
      noise: "white",
      env: { attack: 0.05, peak: 0.10, release: 0.85 },
      cutoffArc: [900, 6500 + rng() * 1500, 800],
      resHz: pick(HARMONICS.high),
      resQ: 1.6 + rng() * 1.2,
      gain: 0.42 + rng() * 0.22,
      pan: (rng() * 2 - 1) * 0.75,
    }),
    roller: () => ({
      durSec: 5.0 + rng() * 3.0,
      noise: "pink",
      env: { attack: 0.32, peak: 0.18, release: 0.50 },
      cutoffArc: [220, 1600 + rng() * 600, 240],
      resHz: pick(HARMONICS.low),
      resQ: 1.2 + rng() * 0.6,
      gain: 0.32 + rng() * 0.16,
      pan: (rng() * 2 - 1) * 0.4,
    }),
    wash: () => ({
      durSec: 3.0 + rng() * 2.0,
      noise: rng() < 0.5 ? "pink" : "white",
      env: { attack: 0.40, peak: 0.05, release: 0.55 },
      cutoffArc: [320, 1300 + rng() * 400, 400],
      resHz: pick(HARMONICS.mid),
      resQ: 1.0 + rng() * 0.5,
      gain: 0.18 + rng() * 0.14,
      pan: (rng() * 2 - 1) * 0.6,
    }),
    boom: () => ({
      durSec: 4.0 + rng() * 2.5,
      noise: "pink",
      env: { attack: 0.18, peak: 0.30, release: 0.52 },
      cutoffArc: [120, 480, 140],
      resHz: pick(HARMONICS.low),
      resQ: 2.2 + rng() * 1.4,
      gain: 0.24 + rng() * 0.14,
      pan: (rng() * 2 - 1) * 0.2,
      subHz: pick([55, 65.4, 73.4]),    // A1, C2, D2
    }),
    hiss: () => ({
      durSec: 0.7 + rng() * 0.7,
      noise: "white",
      env: { attack: 0.04, peak: 0.04, release: 0.92 },
      cutoffArc: [3500, 7500 + rng() * 1500, 2500],
      resHz: pick(HARMONICS.vhigh),
      resQ: 1.8 + rng() * 1.2,
      gain: 0.12 + rng() * 0.10,
      pan: (rng() * 2 - 1) * 0.9,
    }),
  };

  // Per-section type-weight tables (probability distribution over types)
  // and event-interval mean (seconds between event starts).
  const SECTION_MIX = {
    "tide-in":  { mix: { wash: 0.5, hiss: 0.25, roller: 0.20, boom: 0.05, crash: 0.0 }, mean: 3.8 },
    "drift 1":  { mix: { wash: 0.28, hiss: 0.18, roller: 0.30, boom: 0.10, crash: 0.14 }, mean: 2.4 },
    "swell":    { mix: { wash: 0.10, hiss: 0.15, roller: 0.20, boom: 0.20, crash: 0.35 }, mean: 1.7 },
    "drift 2":  { mix: { wash: 0.25, hiss: 0.20, roller: 0.28, boom: 0.12, crash: 0.15 }, mean: 2.4 },
    "tide-out": { mix: { wash: 0.55, hiss: 0.30, roller: 0.12, boom: 0.03, crash: 0.0 }, mean: 3.6 },
  };

  function pickType(mix) {
    const r = rng();
    let acc = 0;
    for (const [name, p] of Object.entries(mix)) {
      acc += p;
      if (r < acc) return name;
    }
    return "wash";
  }

  // Schedule events section by section.
  let scheduled = 0;
  for (const sec of score.sections) {
    const recipe = SECTION_MIX[sec.name] ?? SECTION_MIX["drift 1"];
    let t = sec.startSec - 0.4; // allow events that bleed in from prior section
    while (t < sec.endSec - 0.2) {
      // exponential inter-arrival → Poisson-ish density
      const gap = -Math.log(1 - 0.5 * rng() - 0.4999) * recipe.mean;
      t += Math.max(0.2, gap);
      if (t >= sec.endSec) break;
      const type = pickType(recipe.mix);
      const params = RECIPES[type]();
      const ampGate = waveAmpAt(t);     // honors tide-in/tide-out fades
      if (ampGate <= 0.05) continue;
      params.gain *= ampGate;
      params.start = t;
      paintWave(params);
      eventLog.waves.push({ t: params.start, kind: type, dur: params.durSec });
      scheduled++;
    }
  }
  console.log(`     waves: ${scheduled} events`);
}

// ── 2. filter-sweep layer ────────────────────────────────────────────
// White noise through a resonant SVF bandpass. The cutoff sweeps over
// the section, and because Q is high, the noise gets pitched character
// — you hear a singing tone that rides the sweep instead of a plain
// LP wash. Section flags shape the sweep range.
function renderSweeps() {
  if (SKIP.sweeps) return;
  if (SOLO && !ONLY("sweeps")) return;
  console.log("  sweeps …");

  const RATE = 1 / 24.0;            // slower, calmer sweep
  const RATE_OFFSET_R = 0.45;

  // Harmonic filtration: instead of ONE resonant bandpass we run a
  // small CHORD of resonant bandpasses whose centers are harmonic
  // multiples of a slowly-swept fundamental — base, fifth (1.5x),
  // octave (2x), octave+fifth (3x). Each partial has its own slow LFO
  // so the filtered-noise "voices" drift in and out of one another:
  // harmonies of different filtrations of the same noise bed.
  const PARTIALS = [
    { mul: 1.0, gain: 1.00, rate: RATE,        phase: 0.00 },
    { mul: 1.5, gain: 0.58, rate: RATE * 0.83, phase: 0.31 },
    { mul: 2.0, gain: 0.42, rate: RATE * 1.19, phase: 0.62 },
    { mul: 3.0, gain: 0.26, rate: RATE * 1.41, phase: 0.85 },
  ];
  // Per-partial / per-channel SVF state.
  const st = PARTIALS.map(() => ({ lowL: 0, bandL: 0, lowR: 0, bandR: 0 }));
  const damp = 0.14;                // a touch less piercing than before

  // Smoothed noise source — a leaky one-pole lowpass on white noise so
  // the bed is airy rather than sharp/hissy ("less sharp attacks on
  // the noise"). State carried across samples.
  let nL = 0, nR = 0;

  for (let i = 0; i < LEN_SAMP; i++) {
    const tSec = i / SAMPLE_RATE;
    const sec = sectionAt(tSec);
    if (!sec) continue;

    let cutMin = 200, cutMax = 1500;
    if (sec.flags.has("filter-sweep-open")) {
      cutMin = 280; cutMax = 3200;
    } else if (sec.flags.has("sweep-slow")) {
      cutMin = 190; cutMax = 1300;
    } else if (sec.name.startsWith("tide-in") || sec.name.startsWith("tide-out")) {
      cutMin = 170; cutMax = 780;
    }

    // Smooth the raw noise (one-pole LP ≈ 70% carry → softens the edge)
    nL = nL * 0.72 + (rng() * 2 - 1) * 0.28;
    nR = nR * 0.72 + (rng() * 2 - 1) * 0.28;

    // Slow global onset so the bed eases in instead of starting sharp.
    const onset = Math.min(1, tSec / 4.0);
    // Per-section soft fade near edges (1.6s) so section changes glide.
    const edgeIn  = Math.min(1, (tSec - sec.startSec) / 1.6);
    const edgeOut = Math.min(1, (sec.endSec - tSec) / 1.6);
    const sectionEnv = Math.max(0, Math.min(edgeIn, edgeOut));

    let sumL = 0, sumR = 0, gNorm = 0;
    for (let p = 0; p < PARTIALS.length; p++) {
      const P = PARTIALS[p];
      const s = st[p];
      const lfoL = 0.5 + 0.5 * Math.sin(2 * Math.PI * P.rate * tSec + P.phase * 6.283);
      const lfoR = 0.5 + 0.5 * Math.sin(2 * Math.PI * P.rate * tSec + P.phase * 6.283 + RATE_OFFSET_R * 6.283);
      const baseL = cutMin * Math.pow(cutMax / cutMin, lfoL);
      const baseR = cutMin * Math.pow(cutMax / cutMin, lfoR);
      const cutL = Math.min(baseL * P.mul, SAMPLE_RATE / 3);
      const cutR = Math.min(baseR * P.mul, SAMPLE_RATE / 3);
      const fL = 2 * Math.sin(Math.PI * cutL / SAMPLE_RATE);
      const fR = 2 * Math.sin(Math.PI * cutR / SAMPLE_RATE);

      const highL = nL - s.lowL - damp * s.bandL;
      s.bandL += fL * highL;
      s.lowL  += fL * s.bandL;
      const highR = nR - s.lowR - damp * s.bandR;
      s.bandR += fR * highR;
      s.lowR  += fR * s.bandR;

      // each partial breathes with its own LFO
      sumL += s.bandL * P.gain * (0.45 + 0.55 * lfoL);
      sumR += s.bandR * P.gain * (0.45 + 0.55 * lfoR);
      gNorm += P.gain;
    }

    // Quieter overall — roughly a third of the old level.
    const amp = (0.085 / gNorm) * onset * sectionEnv;
    outL[i] += sumL * amp;
    outR[i] += sumR * amp;
  }
}

// ── 2b. shakers layer ────────────────────────────────────────────────
// Short bandpass-filtered noise bursts (~5–8 kHz center) on syncopated
// 8th-note offbeats. Fires through the drift sections, sparse in
// swell, none in tide-in/out. Adds gentle rhythmic motion.
function renderShakers() {
  if (SKIP.shakers) return;
  if (SOLO && !ONLY("shakers")) return;
  console.log("  shakers …");

  const beat_s = 60.0 / BPM;
  const eighth = beat_s / 2;
  const tStart = score.sections.find((s) => s.name === "drift 1")?.startSec ?? 0;
  const tEnd   = score.sections.find((s) => s.name === "drift 2")?.endSec   ?? score.totalSec;
  const swell  = score.sections.find((s) => s.name === "swell");

  for (let t = tStart; t < tEnd; t += eighth) {
    // skip the very first downbeat of each section for breath
    const sec = sectionAt(t);
    if (!sec) continue;
    // pattern: offbeats louder, downbeats quieter, sparse density
    const beatIndex = Math.round((t - sec.startSec) / eighth);
    const isOffbeat = beatIndex % 2 === 1;
    let p = isOffbeat ? 0.85 : 0.25;
    let gain = isOffbeat ? 0.085 : 0.05;
    if (swell && t >= swell.startSec && t < swell.endSec) {
      p *= 0.5;       // sparser during swell, let the bloom breathe
      gain *= 0.85;
    }
    if (rng() > p) continue;

    // jitter timing slightly for human feel
    const jitter = (rng() - 0.5) * 0.012;
    const start = t + jitter;
    const durSec = 0.055 + rng() * 0.040;
    const pan = (rng() * 2 - 1) * 0.55;
    const lGain = gain * Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
    const rGain = gain * Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;

    // bandpass-filtered noise: SVF with Q ≈ 2, center 4500–7000 Hz
    const center = 4500 + rng() * 2500;
    const f = 2 * Math.sin(Math.PI * center / SAMPLE_RATE);
    const damp = 0.6;
    let low = 0, band = 0;

    const startIdx = Math.floor(start * SAMPLE_RATE);
    const nSamp = Math.floor(durSec * SAMPLE_RATE);
    // softer onset (~7ms raised-cosine) so shakers don't "tick"
    const attack = Math.floor(0.007 * SAMPLE_RATE);
    for (let i = 0; i < nSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      const x = rng() * 2 - 1;
      const high = x - low - damp * band;
      band += f * high;
      low  += f * band;
      let env;
      if (i < attack) env = 0.5 - 0.5 * Math.cos((Math.PI * i) / attack);
      else env = Math.pow(0.0006, (i - attack) / (nSamp - attack));
      const s = band * env;
      outL[dst] += s * lGain;
      outR[dst] += s * rGain;
    }
    eventLog.hat.push({ t: start, kind: isOffbeat ? "off" : "down" });
  }
}

// ── 2c. birds layer ──────────────────────────────────────────────────
// Two chirp shapes:
//   1. "chirp"  — quick upward sine sweep f0 → f0 * 1.6, ~80–140 ms
//   2. "trill"  — 3–5 short pulses on alternating semitone pair
// Birds appear sparse-but-rising through drift sections, denser in
// swell, none during tide-in until the wave bed is established, and
// a final lone chirp during tide-out.
function renderBirds() {
  if (SKIP.birds) return;
  if (SOLO && !ONLY("birds")) return;
  console.log("  birds …");

  // probability per 1-second window, by section name
  const densityBySection = {
    "tide-in":  0.10,
    "drift 1":  0.45,
    "swell":    0.75,
    "drift 2":  0.55,
    "tide-out": 0.20,
  };

  const STEP = 1.0;
  for (let t = 0.6; t < score.totalSec; t += STEP) {
    const sec = sectionAt(t);
    if (!sec) continue;
    const density = densityBySection[sec.name] ?? 0.0;
    if (density <= 0) continue;
    if (rng() > density) continue;

    const start = t + rng() * STEP;
    const kind = rng() < 0.55 ? "chirp" : "trill";
    const pan  = (rng() * 2 - 1) * 0.85;
    const gain = 0.07 + rng() * 0.05;
    const lGain = gain * Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
    const rGain = gain * Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;

    eventLog.birds.push({ t: start, kind });
    if (kind === "chirp") {
      const dur = 0.08 + rng() * 0.07;
      const f0  = 1800 + rng() * 1400;
      const f1  = f0 * (1.4 + rng() * 0.6);
      const startIdx = Math.floor(start * SAMPLE_RATE);
      const nSamp = Math.floor(dur * SAMPLE_RATE);
      const attack = Math.floor(0.006 * SAMPLE_RATE);
      let phase = rng() * 2 * Math.PI;
      for (let i = 0; i < nSamp; i++) {
        const dst = startIdx + i;
        if (dst < 0 || dst >= LEN_SAMP) break;
        const u = i / nSamp;
        // exponential glide + tiny vibrato (~30 Hz)
        const vib = 1 + 0.012 * Math.sin(2 * Math.PI * 30 * (i / SAMPLE_RATE));
        const f = f0 * Math.pow(f1 / f0, u) * vib;
        phase += 2 * Math.PI * f / SAMPLE_RATE;
        let env;
        if (i < attack) env = i / attack;
        else env = Math.pow(0.0004, (i - attack) / (nSamp - attack));
        // gentle harmonic — fundamental + soft 2nd harmonic for a bird-like cluck
        const s = (Math.sin(phase) + 0.18 * Math.sin(phase * 2)) * env;
        outL[dst] += s * lGain;
        outR[dst] += s * rGain;
      }
    } else {
      // trill: 3–5 short pulses on alternating semitones
      const pulses = 3 + Math.floor(rng() * 3);
      const pulseDur = 0.04 + rng() * 0.03;
      const gap = 0.012 + rng() * 0.018;
      const fA = 2400 + rng() * 1400;
      const ratio = Math.pow(2, (rng() < 0.5 ? 2 : 3) / 12);   // 2 or 3 st
      const fB = fA * ratio;
      let cursor = start;
      for (let p = 0; p < pulses; p++) {
        const f = (p % 2 === 0) ? fA : fB;
        const startIdx = Math.floor(cursor * SAMPLE_RATE);
        const nSamp = Math.floor(pulseDur * SAMPLE_RATE);
        const attack = Math.floor(0.003 * SAMPLE_RATE);
        let phase = rng() * 2 * Math.PI;
        for (let i = 0; i < nSamp; i++) {
          const dst = startIdx + i;
          if (dst < 0 || dst >= LEN_SAMP) break;
          phase += 2 * Math.PI * f / SAMPLE_RATE;
          let env;
          if (i < attack) env = i / attack;
          else env = Math.pow(0.0006, (i - attack) / (nSamp - attack));
          const s = Math.sin(phase) * env;
          outL[dst] += s * lGain;
          outR[dst] += s * rGain;
        }
        cursor += pulseDur + gap;
      }
    }
  }
}

// ── 3. bubbles layer ─────────────────────────────────────────────────
// Short descending sine chirps. Each bubble: 80–180ms, pitch glides
// from f0 (700–1800 Hz) down to roughly f0 * 0.4. Random pan, sparse
// Poisson distribution gated by section density.
function renderBubbles() {
  if (SKIP.bubbles) return;
  if (SOLO && !ONLY("bubbles")) return;
  console.log("  bubbles …");

  // Walk the timeline; in 0.25s windows, draw a bubble with probability
  // proportional to bubbleDensityAt(t).
  const STEP_SEC = 0.25;
  for (let t = 0; t < score.totalSec; t += STEP_SEC) {
    const density = bubbleDensityAt(t);
    if (density <= 0) continue;
    // expected ~1 bubble per 2s at density=1
    const p = density * 0.13;
    if (rng() > p) continue;

    const startSec = t + rng() * STEP_SEC;
    const durSec   = 0.08 + rng() * 0.10;
    const f0       = 700 + rng() * 1100;
    const f1       = f0 * (0.35 + rng() * 0.15);
    const pan      = (rng() * 2 - 1) * 0.8;    // -0.8..0.8
    const gain     = 0.08 + rng() * 0.06;
    const lGain    = gain * Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
    const rGain    = gain * Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;

    const startIdx = Math.floor(startSec * SAMPLE_RATE);
    const nSamp    = Math.floor(durSec * SAMPLE_RATE);
    const attack   = Math.floor(0.005 * SAMPLE_RATE);
    let phase = rng() * 2 * Math.PI;
    for (let i = 0; i < nSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      const u = i / nSamp;
      // exponential pitch glide f0 → f1
      const f = f0 * Math.pow(f1 / f0, u);
      phase += 2 * Math.PI * f / SAMPLE_RATE;
      // envelope: 5ms attack, exponential decay to end
      let env;
      if (i < attack) env = i / attack;
      else env = Math.pow(0.0008, (i - attack) / (nSamp - attack));
      const s = Math.sin(phase) * env;
      outL[dst] += s * lGain;
      outR[dst] += s * rGain;
    }
    eventLog.bubbles.push({ t: startSec, kind: "bubble" });
  }
}

// ── 4. pad layer ─────────────────────────────────────────────────────
// One long sustained Am pentatonic pad chord (A2 + E3 + A3 + C4) that
// crossfades with a darker variant (A2 + D3 + G3 + C4) in drift-2.
// Sinepower pad voice — fundamental + octave + fifth + sub.
function renderPad() {
  if (SKIP.pad) return;
  if (SOLO && !ONLY("pad")) return;
  console.log("  pad …");

  // Chord schedule by section name. Each entry: { startSec, endSec, midis, gain }
  const blocks = [];
  let warmGain = 0.20;
  for (const s of score.sections) {
    let midis;
    let gain = warmGain;
    if (s.name === "tide-in")        { midis = [45, 52, 57];      gain = warmGain * 0.6; }
    else if (s.name === "drift 1")   { midis = [45, 52, 57, 60]; }
    else if (s.name === "swell")     { midis = [45, 52, 57, 60, 64]; gain = warmGain * 1.15; }
    else if (s.name === "drift 2")   { midis = [43, 50, 55, 60]; }
    else if (s.name === "tide-out")  { midis = [45, 52, 57];      gain = warmGain * 0.55; }
    else                              { midis = [45, 52, 57, 60]; }
    blocks.push({ startSec: s.startSec, endSec: s.endSec, midis, gain, name: s.name });
  }

  // Render each chord-block with very slow attack + release so chord
  // changes between sections bloom gradually instead of splashing.
  // We also shift each block EARLIER by PREROLL_S so the new chord
  // starts attacking before the section boundary — by the time the
  // boundary hits, the chord is already partially bloomed.
  const PREROLL_S = 1.5;
  const ATTACK_S  = 3.0;
  const RELEASE_S = 3.5;
  // pad voice partials (sinepower preset, gentle)
  const PARTIALS = [
    { ratio: 1.0,  amp: 1.00 },
    { ratio: 2.0,  amp: 0.32 },
    { ratio: 1.5,  amp: 0.22 },
    { ratio: 0.5,  amp: 0.30 },   // sub
  ];

  for (const blk of blocks) {
    const dur = blk.endSec - blk.startSec;
    if (dur <= 0) continue;
    // Start each chord PREROLL_S before its section boundary so the new
    // chord blooms gradually into the change rather than splashing.
    const blockStart = Math.max(0, blk.startSec - PREROLL_S);
    const blockDur   = (blk.endSec - blockStart);
    const totalSec   = blockDur + RELEASE_S;
    const startIdx   = Math.floor(blockStart * SAMPLE_RATE);
    const totalSamp  = Math.floor(totalSec * SAMPLE_RATE);
    const attackSamp  = Math.floor(ATTACK_S * SAMPLE_RATE);
    const sustainSamp = Math.floor(blockDur * SAMPLE_RATE) - attackSamp;
    const releaseSamp = Math.floor(RELEASE_S * SAMPLE_RATE);

    // precompute partial phase increments per note
    const notes = blk.midis.map((m) => {
      const f = midiToFreq(m);
      return PARTIALS.map((p) => ({
        omega: 2 * Math.PI * f * p.ratio / SAMPLE_RATE,
        amp: p.amp,
      }));
    });

    for (let i = 0; i < totalSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      let env;
      if (i < attackSamp) {
        env = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackSamp);
      } else if (i < attackSamp + sustainSamp) {
        env = 1.0;
      } else {
        const r = i - (attackSamp + sustainSamp);
        if (r >= releaseSamp) break;
        env = 0.5 + 0.5 * Math.cos((Math.PI * r) / releaseSamp);
      }
      let sample = 0;
      for (const note of notes) {
        for (const p of note) sample += Math.sin(p.omega * i) * p.amp;
      }
      // normalize by # of notes so chords don't clip
      sample /= notes.length;
      const v = sample * env * blk.gain;
      // stereo: slight L/R detune via tiny phase offset (cheap chorus)
      outL[dst] += v;
      outR[dst] += v * 0.96;
    }
  }
}

// ── 5. bells layer ───────────────────────────────────────────────────
// Sparse sinebells from the score, far back in the mix. Voice lifted
// verbatim from pop/bin/melody-bells.mjs (bell preset).
function renderBells() {
  if (SKIP.bells) return;
  if (SOLO && !ONLY("bells")) return;
  console.log("  bells …");

  const BELL_PARTIALS = [
    { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
    { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
    { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
    { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
    { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
    { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
    { ratio: 5.4,  amp: 0.02, decayT60: 0.4 },
  ];
  const RING_TAIL = 3.2;
  const ATTACK_S  = 0.018;
  const BELL_GAIN = 0.34;

  for (const ev of score.events) {
    const gateGain = bellGainAt(ev.startSec);
    if (gateGain <= 0.001) continue;
    eventLog.bells.push({ t: ev.startSec, midi: ev.midi, dur: ev.durSec });
    const f = midiToFreq(ev.midi);
    const startIdx = Math.floor(ev.startSec * SAMPLE_RATE);
    const ringSamp = Math.floor((ev.durSec + RING_TAIL) * SAMPLE_RATE);
    const attackSamp = ATTACK_S * SAMPLE_RATE;
    const partials = BELL_PARTIALS.map((p) => ({
      omega: 2 * Math.PI * f * p.ratio / SAMPLE_RATE,
      amp: p.amp,
      decay: Math.exp(-Math.log(1000) / (p.decayT60 * SAMPLE_RATE)),
    }));
    for (let i = 0; i < ringSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      let s = 0;
      for (const p of partials) {
        const env = p.amp * Math.pow(p.decay, i);
        if (env < 1e-5) continue;
        s += Math.sin(p.omega * i) * env;
      }
      let att = 1;
      if (i < attackSamp) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackSamp);
      const v = s * att * BELL_GAIN * gateGain;
      // stereo bells: slight L bias on odd events, R on even, for spread
      outL[dst] += v;
      outR[dst] += v;
    }
  }
}

// ── 5b. high sung melody ─────────────────────────────────────────────
// Very quiet, very high, long sustained "sung" tones — a slow
// wandering A-minor-pentatonic line floating well above the bells.
// Overlapping legato phrases (each note 3–5.5s, slow cosine attack +
// long cosine release) with two detuned voices, gentle vibrato, soft
// upper harmonics and a faint breath layer so it reads vocal.
function renderHighMelody() {
  if (SKIP.highmel) return;
  if (SOLO && !ONLY("highmel")) return;
  console.log("  high melody …");

  // A-minor pentatonic, octaves 6–7 (E6 G6 A6 C7 D7 E7) — shimmery,
  // not piercing.
  const POOL = [88, 91, 93, 96, 98, 100];
  const VIB_HZ = 4.8, VIB_DEPTH = 0.006;
  const GAIN = 0.05;

  const d1 = score.sections.find((s) => s.name === "drift 1");
  const tOut = score.sections.find((s) => s.name === "tide-out");
  if (!d1) return;
  const tStart = d1.startSec + 6.0;
  const tEnd   = (tOut ? tOut.startSec : score.totalSec) - 2.0;

  let idx = 2;                              // start on A6
  let t = tStart;
  let count = 0;
  while (t < tEnd) {
    const midi = POOL[idx];
    const dur = 3.0 + rng() * 2.5;          // 3–5.5s long notes
    const f = midiToFreq(midi);
    const startIdx = Math.floor(t * SAMPLE_RATE);
    const atk = Math.floor((0.7 + rng() * 0.5) * SAMPLE_RATE);
    const rel = Math.floor(1.6 * SAMPLE_RATE);
    const nSamp = Math.floor(dur * SAMPLE_RATE) + rel;
    const susEnd = nSamp - rel;
    const detune = 1.0 + (rng() - 0.5) * 0.006;
    const w1 = 2 * Math.PI * f / SAMPLE_RATE;
    const w2 = 2 * Math.PI * f * detune / SAMPLE_RATE;
    const wv = 2 * Math.PI * VIB_HZ / SAMPLE_RATE;
    const pan = (rng() - 0.5) * 0.5;
    const lG = Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2;
    const rG = Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;
    let bL = 0, bR = 0;                      // breath bandpass state
    const bf = 2 * Math.sin(Math.PI * Math.min(f * 2, SAMPLE_RATE / 3) / SAMPLE_RATE);
    for (let i = 0; i < nSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      let env;
      if (i < atk) env = 0.5 - 0.5 * Math.cos((Math.PI * i) / atk);
      else if (i < susEnd) env = 1.0;
      else env = 0.5 + 0.5 * Math.cos((Math.PI * (i - susEnd)) / rel);
      const vib = 1 + VIB_DEPTH * Math.sin(wv * i);
      let s = Math.sin(w1 * i * vib)
            + 0.7 * Math.sin(w2 * i * vib)
            + 0.16 * Math.sin(2 * w1 * i * vib)
            + 0.06 * Math.sin(3 * w1 * i * vib);
      s /= 1.92;
      const x = rng() * 2 - 1;              // faint breath ~2nd harmonic
      const hi = x - bL - 0.9 * bR;
      bR += bf * hi; bL += bf * bR;
      s += bR * 0.05;
      const v = s * env * GAIN;
      outL[dst] += v * lG;
      outR[dst] += v * rG;
    }
    eventLog.bells.push({ t, midi, dur });
    count++;
    const step = (rng() < 0.7) ? (rng() < 0.5 ? 1 : -1)
                               : (rng() < 0.5 ? 2 : -2);
    idx = Math.max(0, Math.min(POOL.length - 1, idx + step));
    t += dur * (0.55 + rng() * 0.25);       // overlap → legato
  }
  console.log(`     high melody: ${count} long notes`);
}

// ── 6. kick + long sub-bass layer ────────────────────────────────────
// Two related elements:
//   • kick   — short 808-style hit (pitch glide 78 → 42 Hz over ~250ms),
//              fires on beats 1 + 3 of each bar (half-time pulse at 70
//              BPM = every ~1.71s) during sections flagged "kick"
//   • subBass — sustained sine drone at A1 (55 Hz) under the same
//              sections, with slow attack + release crossfading at
//              section edges. Together they read as "kick + long bass kick"
function renderKick() {
  if (SKIP.kick) return;
  if (SOLO && !ONLY("kick")) return;
  console.log("  kick …");

  const beat_s = 60.0 / BPM;
  const KICK_GAIN = 0.32;
  const SUB_GAIN  = 0.18;
  const SUB_HZ    = 55.0;   // A1
  const SUB_HZ2   = 41.2;   // E1 (fifth-below for drift 2)

  // Kicks: every 2 beats inside any [kick]-flagged section.
  let kickCount = 0;
  for (const sec of score.sections) {
    if (!sec.flags.has("kick")) continue;
    for (let t = sec.startSec; t < sec.endSec - 0.1; t += 2 * beat_s) {
      // 808 kick: sine with fast pitch glide + short decay
      const f0 = 78, f1 = 42;
      const durSec = 0.32;
      const startIdx = Math.floor(t * SAMPLE_RATE);
      const nSamp = Math.floor(durSec * SAMPLE_RATE);
      const attackS = Math.floor(0.003 * SAMPLE_RATE);
      let phase = 0;
      for (let i = 0; i < nSamp; i++) {
        const dst = startIdx + i;
        if (dst < 0 || dst >= LEN_SAMP) break;
        const u = i / nSamp;
        // exponential pitch drop
        const f = f0 * Math.pow(f1 / f0, u);
        phase += 2 * Math.PI * f / SAMPLE_RATE;
        // amp envelope: 3ms attack → fast exp decay
        let env;
        if (i < attackS) env = i / attackS;
        else env = Math.pow(0.0015, (i - attackS) / (nSamp - attackS));
        // light sine + tiny harmonic for body
        const s = (Math.sin(phase) + 0.12 * Math.sin(phase * 2)) * env;
        outL[dst] += s * KICK_GAIN;
        outR[dst] += s * KICK_GAIN;
      }
      eventLog.kick.push({ t, kind: "kick" });
      kickCount++;
    }
  }

  // Sub-bass drone: one sustained sine per [kick] section, crossfading
  // at section edges (1.5s attack, 1.8s release).
  const ATTACK_S  = 1.5;
  const RELEASE_S = 1.8;
  let subBlocks = 0;
  for (const sec of score.sections) {
    if (!sec.flags.has("kick")) continue;
    // drift 2 sits a fifth lower for harmonic motion (E1 instead of A1)
    const f = sec.name === "drift 2" ? SUB_HZ2 : SUB_HZ;
    const dur = sec.endSec - sec.startSec;
    const startIdx = Math.floor(sec.startSec * SAMPLE_RATE);
    const totalSamp = Math.floor((dur + RELEASE_S) * SAMPLE_RATE);
    const attackSamp  = Math.floor(ATTACK_S * SAMPLE_RATE);
    const sustainSamp = Math.floor(dur * SAMPLE_RATE) - attackSamp;
    const releaseSamp = Math.floor(RELEASE_S * SAMPLE_RATE);
    const omega = 2 * Math.PI * f / SAMPLE_RATE;

    for (let i = 0; i < totalSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= LEN_SAMP) break;
      let env;
      if (i < attackSamp) {
        env = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackSamp);
      } else if (i < attackSamp + sustainSamp) {
        env = 1.0;
      } else {
        const r = i - (attackSamp + sustainSamp);
        if (r >= releaseSamp) break;
        env = 0.5 + 0.5 * Math.cos((Math.PI * r) / releaseSamp);
      }
      // fundamental + soft 2nd harmonic for warmth
      const s = (Math.sin(omega * i) + 0.10 * Math.sin(omega * 2 * i)) * env;
      outL[dst] += s * SUB_GAIN;
      outR[dst] += s * SUB_GAIN;
    }
    eventLog.sub.push({ t: sec.startSec, kind: "drone", dur: dur });
    subBlocks++;
  }
  console.log(`     kicks: ${kickCount} hits · sub-bass: ${subBlocks} blocks`);
}

// ── 6b. snare — sharp "pfft" backbeat ────────────────────────────────
// Short airy noise burst on beats 2 & 4 of every [kick] section. Soft
// ~5 ms cosine onset (a "pfft", not a click) → fast exponential decay.
// Broad low-Q bandpass noise (~1.9 kHz) for the air + a fast pitched
// body for snap. Occasional quiet ghost note for groove.
function renderSnare() {
  if (SKIP.snare) return;
  if (SOLO && !ONLY("snare")) return;
  console.log("  snare …");
  const beat_s = 60.0 / BPM;
  const SNARE_GAIN = 0.17;
  let hits = 0;
  for (const sec of score.sections) {
    if (!sec.flags.has("kick")) continue;
    // backbeat = one beat after each kick (kick on 1&3 → snare on 2&4)
    for (let t = sec.startSec + beat_s; t < sec.endSec - 0.1; t += 2 * beat_s) {
      const ghosts = [{ at: t, g: 1.0 }];
      if (rng() < 0.33) ghosts.push({ at: t + beat_s, g: 0.30 });
      for (const G of ghosts) {
        const start = G.at + (rng() - 0.5) * 0.008;
        const startIdx = Math.floor(start * SAMPLE_RATE);
        const durSec = 0.13 + rng() * 0.05;
        const nSamp = Math.floor(durSec * SAMPLE_RATE);
        const atk = Math.floor(0.005 * SAMPLE_RATE);   // soft pfft onset
        const gain = SNARE_GAIN * G.g;
        const center = 1850 + rng() * 350;
        const f = 2 * Math.sin(Math.PI * center / SAMPLE_RATE);
        const damp = 0.95;            // low resonance → broad "pff"
        let low = 0, band = 0;
        const bodyW = 2 * Math.PI * 196 / SAMPLE_RATE;
        for (let i = 0; i < nSamp; i++) {
          const dst = startIdx + i;
          if (dst < 0 || dst >= LEN_SAMP) break;
          const x = rng() * 2 - 1;
          const high = x - low - damp * band;
          band += f * high;
          low  += f * band;
          let env;
          if (i < atk) env = 0.5 - 0.5 * Math.cos((Math.PI * i) / atk);
          else env = Math.pow(0.0008, (i - atk) / (nSamp - atk));
          const bodyEnv = Math.pow(0.0004, i / nSamp);
          const s = band * env * 0.9 + Math.sin(bodyW * i) * bodyEnv * 0.22;
          outL[dst] += s * gain;
          outR[dst] += s * gain * 0.98;
        }
        eventLog.kick.push({ t: start, kind: "snare" });
        hits++;
      }
    }
  }
  console.log(`     snare: ${hits} hits`);
}

// ── AC native zoo samples — whale + owl + bird + frog ────────────────
// Real recordings layered onto the beach scene. Whale in the swell;
// owl in the late drift for mystery; a couple of bird calls scattered
// to ground the synth bird chirps in the real-recorded color.
function renderZooSamples() {
  const whale = loadRawSample(`${ZOO_DIR}/whale.raw`);
  const owl   = loadRawSample(`${ZOO_DIR}/owl.raw`);
  const bird  = loadRawSample(`${ZOO_DIR}/bird.raw`);
  const frog  = loadRawSample(`${ZOO_DIR}/frog.raw`);

  console.log("  zoo …");
  const swell  = score.sections.find((s) => s.name === "swell");
  const drift1 = score.sections.find((s) => s.name === "drift 1");
  const drift2 = score.sections.find((s) => s.name === "drift 2");
  const tideOut = score.sections.find((s) => s.name === "tide-out");
  let n = 0;

  if (whale && swell) {
    // two whale calls in the swell — pitched down a fifth + pitched up a third
    const t1 = swell.startSec + 0.5;
    const t2 = swell.startSec + (swell.endSec - swell.startSec) * 0.55;
    paintSample(whale, t1, 0.32, -0.35, -7);
    paintSample(whale, t2, 0.26, +0.45, -4);
    eventLog.birds.push({ t: t1, kind: "whale", dur: whale.length / SAMPLE_RATE });
    eventLog.birds.push({ t: t2, kind: "whale", dur: whale.length / SAMPLE_RATE });
    n += 2;
  }
  if (owl && drift2) {
    // one lone owl in the late drift
    const t = drift2.startSec + (drift2.endSec - drift2.startSec) * 0.25;
    paintSample(owl, t, 0.20, -0.55, -2);
    eventLog.birds.push({ t, kind: "owl", dur: owl.length / SAMPLE_RATE });
    n += 1;
  }
  if (bird && drift1) {
    // a real bird at the top of drift 1 + another in tide-out
    paintSample(bird, drift1.startSec + 0.7, 0.10, +0.6, 0);
    eventLog.birds.push({ t: drift1.startSec + 0.7, kind: "bird-sample", dur: bird.length / SAMPLE_RATE });
    if (tideOut) {
      paintSample(bird, tideOut.startSec + 0.3, 0.09, -0.5, +2);
      eventLog.birds.push({ t: tideOut.startSec + 0.3, kind: "bird-sample", dur: bird.length / SAMPLE_RATE });
      n += 1;
    }
    n += 1;
  }
  if (frog && drift2) {
    // a single distant frog low in the mix near the end of drift 2
    const t = drift2.startSec + (drift2.endSec - drift2.startSec) * 0.75;
    paintSample(frog, t, 0.07, +0.3, -3);
    eventLog.birds.push({ t, kind: "frog", dur: frog.length / SAMPLE_RATE });
    n += 1;
  }
  console.log(`     zoo: ${n} sample hits`);
}

// ── render everything (per-lane buffers captured via renderLane) ────
// renderLane swaps in a fresh buffer per layer so we can save a
// downsampled-mono copy for the preview-score visualizer to use as
// each lane's actual mix volume display.
renderLane("waves",   renderWaves);
renderLane("hat",     () => { renderSweeps(); renderShakers(); });
renderLane("kick",    () => { renderKick(); renderSnare(); });  // kick+sub+snare
renderLane("sub",     () => {});            // sub is mixed inside kick; placeholder
renderLane("bells",   () => { renderPad(); renderBells(); renderHighMelody(); });
renderLane("bubbles", renderBubbles);
renderLane("birds",   () => { renderBirds(); renderZooSamples(); });

// ── coming-in-from-the-sea FX ─────────────────────────────────────────
// Bitcrush + flange ramp at the start (heavy → clean over ~3s) so the
// piece arrives like a memory tuning in, and again at the tail
// (clean → heavy, then fade-out) so it dissolves back into the surf.
const FX_START_SEC = 3.5;
const FX_TAIL_SEC  = 5.0;
const FX_TAIL_T0   = Math.max(0, LEN_SEC - FX_TAIL_SEC);

for (const buf of [outL, outR]) {
  // start — crushed to clean
  applyBitcrush(buf, {
    bits:       [{ time: 0,   bits: 3 },       { time: 2.5, bits: 14 }, { time: 3.5, bits: 16 }],
    downsample: [{ time: 0,   downsample: 8 }, { time: 2.5, downsample: 1 }],
    mix: 1.0, sampleRate: SAMPLE_RATE,
    startSec: 0, endSec: FX_START_SEC,
  });
  // tail — clean to crushed
  applyBitcrush(buf, {
    bits:       [{ time: 0, bits: 16 },      { time: 3, bits: 6 },        { time: FX_TAIL_SEC, bits: 3 }],
    downsample: [{ time: 0, downsample: 1 }, { time: 3, downsample: 4 },  { time: FX_TAIL_SEC, downsample: 10 }],
    mix: 1.0, sampleRate: SAMPLE_RATE,
    startSec: FX_TAIL_T0, endSec: LEN_SEC,
  });
  // start flange — wide LFO, naturally fades with the bitcrush envelope
  applyFlange(buf, {
    rate: 6, depthMs: 5, baseDelayMs: 5,
    feedback: 0.6, mix: 0.55,
    sampleRate: SAMPLE_RATE,
    startSec: 0, endSec: FX_START_SEC,
  });
  // tail flange — slower swirl as the piece dissolves
  applyFlange(buf, {
    rate: 3, depthMs: 6, baseDelayMs: 6,
    feedback: 0.7, mix: 0.6,
    sampleRate: SAMPLE_RATE,
    startSec: FX_TAIL_T0, endSec: LEN_SEC,
  });
}
// Log FX zones so the preview timeline can show them.
eventLog.fx.push({ t: 0,           kind: "bitcrush+flange", dur: FX_START_SEC });
eventLog.fx.push({ t: FX_TAIL_T0,  kind: "bitcrush+flange", dur: FX_TAIL_SEC  });

// ── ac-native boot + shutdown chime (ported from recap/bin/trance.mjs)
// Triangle C5→E5→G5 on entry, G5→E5→C5 on the way out. One sfx event
// per note so the preview can ripple-"dink" on each beep.
const CHIME_GAIN = 0.5;
const BOOT_NOTES = [
  { tone: 523.25, dur: 0.15, vol: 0.70 }, // C5
  { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
  { tone: 783.99, dur: 0.20, vol: 0.80 }, // G5
];
const SHUT_NOTES = [
  { tone: 783.99, dur: 0.15, vol: 0.70 }, // G5
  { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
  { tone: 523.25, dur: 0.20, vol: 0.80 }, // C5
];
function mixChime(startSec, notes, dir) {
  let t = startSec;
  let idx = 0;
  for (const n of notes) {
    eventLog.sfx.push({ t, name: `${dir}-beep-${++idx}`, dur: n.dur, point: true });
    const f = n.tone;
    const startIdx = Math.floor(t * SAMPLE_RATE);
    const atk = Math.max(1, Math.floor(0.003 * SAMPLE_RATE));
    const decay = n.dur * 0.6;
    const total = Math.floor((n.dur + 0.12) * SAMPLE_RATE);
    for (let i = 0; i < total; i++) {
      const j = startIdx + i;
      if (j < 0 || j >= LEN_SAMP) break;
      const tt = i / SAMPLE_RATE;
      const ph = (f * tt) % 1;
      const tri = 2 * Math.abs(2 * ph - 1) - 1;       // -1..1 triangle
      const env = i < atk
        ? i / atk
        : Math.exp(-(tt - atk / SAMPLE_RATE) / decay);
      const s = tri * env * n.vol * CHIME_GAIN;
      outL[j] += s; outR[j] += s;
    }
    t += n.dur + 0.060;                                // 60 ms gap
  }
}
mixChime(0.05, BOOT_NOTES, "boot");
mixChime(Math.max(0, LEN_SEC - 2.2), SHUT_NOTES, "shutdown");

// fade-in (0.4s) + fade-out (last 2.5s) so neither end clicks
const FADE_IN_SEC  = 0.4;
const FADE_OUT_SEC = 2.5;
const fadeInSamp  = Math.floor(FADE_IN_SEC  * SAMPLE_RATE);
const fadeOutSamp = Math.floor(FADE_OUT_SEC * SAMPLE_RATE);
for (let i = 0; i < fadeInSamp; i++) {
  const g = 0.5 - 0.5 * Math.cos((Math.PI * i) / fadeInSamp);
  outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOutSamp; i++) {
  const idx = LEN_SAMP - fadeOutSamp + i;
  if (idx < 0 || idx >= LEN_SAMP) continue;
  const g = 0.5 + 0.5 * Math.cos((Math.PI * i) / fadeOutSamp);
  outL[idx] *= g; outR[idx] *= g;
}

// ── peak normalize to -1.5 dBFS (stereo) ─────────────────────────────
let peak = 0;
for (let i = 0; i < LEN_SAMP; i++) {
  const aL = Math.abs(outL[i]);
  const aR = Math.abs(outR[i]);
  if (aL > peak) peak = aL;
  if (aR > peak) peak = aR;
}
const tgt = Math.pow(10, -1.5 / 20);
const norm = peak > 0 ? Math.min(1, tgt / peak) : 1;
if (norm < 1) {
  for (let i = 0; i < LEN_SAMP; i++) {
    outL[i] *= norm;
    outR[i] *= norm;
  }
}

// ── interleave + write via ffmpeg (raw f32 stereo → mp3) ─────────────
const interleaved = new Float32Array(LEN_SAMP * 2);
for (let i = 0; i < LEN_SAMP; i++) {
  interleaved[i * 2]     = outL[i];
  interleaved[i * 2 + 1] = outR[i];
}

const tmp = mkdtempSync(`${tmpdir()}/chillwave-`);
const rawPath = `${tmp}/mix.f32.raw`;
writeFileSync(rawPath, Buffer.from(interleaved.buffer, interleaved.byteOffset, interleaved.byteLength));
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "2",
  "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "2",
  OUT_PATH,
]);
rmSync(tmp, { recursive: true, force: true });
if (r.status !== 0) {
  console.error("✗ ffmpeg encode failed");
  process.exit(1);
}
console.log(`✓ ${OUT_PATH}  (peak norm ${norm.toFixed(3)}, ${LEN_SEC.toFixed(1)}s)`);

// ── per-lane mono buffers (display SR) saved as .raw sidecars ────────
// Each lane writes a headerless float32 mono file at LANE_DISPLAY_SR
// next to the mp3. The preview reads these to draw each waveform with
// the actual mix-volume of that lane (not the full mixed audio).
const laneBufferPaths = {};
for (const [key, buf] of Object.entries(laneBuffers)) {
  const p = OUT_PATH.replace(/\.mp3$/, `.lane-${key}.raw`);
  writeFileSync(p, Buffer.from(buf.buffer, buf.byteOffset, buf.byteLength));
  laneBufferPaths[key] = p.replace(`${LANE}/`, "");
}

// ── struct.json sidecar for the score-train preview ──────────────────
// Sorts each lane by time so the visualizer can binary-search per frame.
for (const lane of Object.keys(eventLog)) {
  eventLog[lane].sort((a, b) => a.t - b.t);
}
const struct = {
  meter: 4,
  bpm: BPM,
  scale: "minor",
  rootMidi: 57,    // A3
  totalSec: LEN_SEC,
  laneAudio: {
    sampleRate: LANE_DISPLAY_SR,
    paths: laneBufferPaths,        // relative to the lane dir
  },
  sections: score.sections.map((s) => ({
    name: s.name,
    startSec: s.startSec,
    endSec: s.endSec,
    flags: Array.from(s.flags),
  })),
  events: eventLog,
};
const structPath = OUT_PATH.replace(/\.mp3$/, ".struct.json");
writeFileSync(structPath, JSON.stringify(struct, null, 2));
console.log(`✓ ${structPath}  (${Object.values(eventLog).reduce((n, l) => n + l.length, 0)} events, ${Object.keys(laneBuffers).length} lane buffers)`);
