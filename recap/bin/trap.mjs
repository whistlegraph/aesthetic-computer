#!/usr/bin/env node
// trap.mjs — render a 4/4 trap bed for the pop/big-pictures lane.
//
// Bottom-up: drums are AC-native. Each drum's voice stack is read from
// `system/public/aesthetic.computer/lib/percussion.mjs` (the same kit web
// notepat plays through the AudioContext and fedac/native notepat plays
// through audio.c). We mock `sound.synth` as a node-side buffer mixer —
// the synthesis recipes themselves come from AC's percussion module
// verbatim, so the composition is identical to what the C path produces.
//
// Harmonic content (bass, chord stabs, sparse melody) reuses the piano
// sample bank + sinebells synth from waltz.mjs.
//
// Patterns: 16-step grids per bar (4/4, 16th-note resolution). Library
// adapted from artery/test-hiphop.mjs.
//
// Usage:
//   node bin/trap.mjs                                  # default
//   node bin/trap.mjs jeffrey-24h-2026-05-01           # named audience
//   node bin/trap.mjs --style trap --bars 16 --bpm 140 \
//        --voice sinebells --scale minor --seed plork-test \
//        --out ~/Desktop/trap-test.mp3

import {
  readFileSync,
  writeFileSync,
  existsSync,
  mkdirSync,
  unlinkSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { playPercussion } from "../../system/public/aesthetic.computer/lib/percussion.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

// ── parse args ─────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) {
      flags[key] = next;
      i++;
    } else {
      flags[key] = true;
    }
  } else {
    positional.push(a);
  }
}
const audienceName = positional[0] || null;

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

// ── load audience config (optional) ───────────────────────────────────
let audience = null;
let T = {};
if (audienceName) {
  const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
  audience = mod.audience;
  T = audience.trap || {};
}

const STYLE        = flags.style || T.style || "trap";
const VOICE        = flags.voice || T.voice || "sinebells";
const SEED_STR     = flags.seed || T.seed || (audience?.name ?? audienceName ?? "trap-default");
const BPM          = Number(flags.bpm  ?? T.bpm  ?? 140);
const SCALE_NAME   = flags.scale || T.scale || "minor";
const PROGRESSION  = parseProgression(flags.progression) || T.progression || [0, 5, 3, 4]; // i VI iv V
const VOICE_GAIN   = Number(flags.gain ?? T.voiceGain ?? 0.18);
const DRUM_GAIN    = Number(flags["drum-gain"] ?? T.drumGain ?? 0.85);
const DENSITY      = Number(flags.density ?? T.density ?? 0.5);
const ROOT_OFFSET  = Number(flags.transpose ?? T.transpose ?? 0);
const OUT_PATH     = expandHome(flags.out) || `${ROOT}/out/trap.mp3`;

const _beatSecPre = 60 / BPM;
const _barSecPre  = _beatSecPre * 4; // 4/4
let DURATION_SEC = null;
if (flags.duration !== undefined) DURATION_SEC = Number(flags.duration);
else if (T.duration !== undefined) DURATION_SEC = Number(T.duration);
const BARS = (() => {
  if (flags.bars !== undefined) return Number(flags.bars);
  if (T.bars !== undefined && DURATION_SEC === null) return Number(T.bars);
  if (DURATION_SEC !== null) return Math.max(1, Math.ceil(DURATION_SEC / _barSecPre));
  return 16;
})();

function parseProgression(s) {
  if (!s || s === true) return null;
  return s.split(",").map((x) => Number(x.trim()));
}

const SAMPLE_RATE = 48_000;

// ── deterministic PRNG seeded by audience/style ───────────────────────
function hashString(s) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}
function makeRng(seedStr) {
  let s = hashString(seedStr) || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}
const rng = makeRng(SEED_STR);
// Separate RNG for noise sampling so per-hit noise stays deterministic
// without consuming entropy from the compositional rng.
const noiseRng = makeRng(SEED_STR + ":noise");

// ── musical theory ─────────────────────────────────────────────────────
const SCALES = {
  major:  [0, 2, 4, 5, 7, 9, 11],
  minor:  [0, 2, 3, 5, 7, 8, 10],
  dorian: [0, 2, 3, 5, 7, 9, 10],
  lydian: [0, 2, 4, 6, 7, 9, 11],
};
const SCALE = SCALES[SCALE_NAME] || SCALES.minor;
const ROOT_MIDI = 60 + ROOT_OFFSET;

function scaleNoteMidi(degree, octaveOffset = 0) {
  const len = SCALE.length;
  const idx = ((degree % len) + len) % len;
  const octShift = Math.floor(degree / len);
  return ROOT_MIDI + 12 * (octaveOffset + octShift) + SCALE[idx];
}
function chordMidis(rootDegree, octaveOffset = 0) {
  return [
    scaleNoteMidi(rootDegree,     octaveOffset),
    scaleNoteMidi(rootDegree + 2, octaveOffset),
    scaleNoteMidi(rootDegree + 4, octaveOffset),
  ];
}

// ── 16-step beat patterns (1 bar = 16 sixteenth-notes) ────────────────
// Adapted from artery/test-hiphop.mjs. Each pattern: kick / snare /
// closed-hat / open-hat. 1 = hit, 0 = rest.
const BEAT_PATTERNS = {
  trap: {
    kick:    [1,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
  drill: {
    kick:    [1,0,0,0, 0,0,0,1, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [1,0,0,1, 0,1,0,0, 1,0,0,1, 0,1,0,0],
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  "808": {
    kick:    [1,0,0,0, 0,0,0,0, 1,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,1],
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  lofi: {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,1],
    hat:     [0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0],
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  modern: {
    kick:    [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 1,0,0,1, 0,0,0,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
};

if (!BEAT_PATTERNS[STYLE]) {
  console.error(`trap: unknown style '${STYLE}'. expected: ${Object.keys(BEAT_PATTERNS).join(" | ")}`);
  process.exit(1);
}

// ── voice: piano (sample bank) ────────────────────────────────────────
const PIANO_SAMPLE_DIR = resolve(REPO, "fedac/native/samples/piano");
const PIANO_ANCHORS = [21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96];
let pianoBank = null;

function loadPianoBank() {
  const bank = new Map();
  for (const m of PIANO_ANCHORS) {
    const path = `${PIANO_SAMPLE_DIR}/${m}.raw`;
    if (!existsSync(path)) throw new Error(`trap: missing piano anchor ${path}`);
    const buf = readFileSync(path);
    const f32 = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
    bank.set(m, Float32Array.from(f32));
  }
  console.log(`→ piano bank · ${bank.size} anchors loaded`);
  return bank;
}

function pianoAnchorFor(midi) {
  let best = PIANO_ANCHORS[0];
  for (const a of PIANO_ANCHORS) if (Math.abs(a - midi) < Math.abs(best - midi)) best = a;
  const ratio = Math.pow(2, (midi - best) / 12);
  return { sample: pianoBank.get(best), ratio };
}

function mixEventPiano(ev, out) {
  const { sample, ratio } = pianoAnchorFor(ev.midi);
  const startIdx = Math.floor(ev.startSec * SAMPLE_RATE);
  const durSamples = Math.floor(ev.durSec * SAMPLE_RATE);
  const attack = Math.min(0.005 * SAMPLE_RATE, durSamples * 0.05);
  const release = Math.min(0.08 * SAMPLE_RATE, durSamples * 0.5);
  const lenOut = durSamples + Math.floor(release);
  for (let i = 0; i < lenOut; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    const srcF = i * ratio;
    const s0 = Math.floor(srcF);
    const s1 = s0 + 1;
    if (s1 >= sample.length) break;
    const frac = srcF - s0;
    const v = sample[s0] * (1 - frac) + sample[s1] * frac;
    let env = 1;
    if (i < attack) env = i / attack;
    else if (i > durSamples) env = Math.max(0, 1 - (i - durSamples) / release);
    out[dst] += v * env * ev.gain;
  }
}

// ── voice: sinebells ──────────────────────────────────────────────────
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
  { ratio: 5.4,  amp: 0.02, decayT60: 0.4 },
];
const ATTACK_SEC = 0.012;
const BELL_RING_TAIL = 6.0;
const BELL_GAIN = 0.42;

function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

function mixEventSinebell(ev, out) {
  const startIdx = Math.floor(ev.startSec * SAMPLE_RATE);
  const ringSamples = Math.floor((ev.durSec + BELL_RING_TAIL) * SAMPLE_RATE);
  const attackS = ATTACK_SEC * SAMPLE_RATE;
  const fundFreq = midiToFreq(ev.midi);
  const twoPiOverSr = (2 * Math.PI) / SAMPLE_RATE;
  const partials = BELL_PARTIALS.map((p) => ({
    omega: twoPiOverSr * fundFreq * p.ratio,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.decayT60 * SAMPLE_RATE)),
  }));
  for (let i = 0; i < ringSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let s = 0;
    for (const p of partials) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * BELL_GAIN;
  }
}

// ── route harmonic events to chosen voice ─────────────────────────────
let mixHarmEvent;
if (VOICE === "piano") {
  pianoBank = loadPianoBank();
  mixHarmEvent = mixEventPiano;
} else if (VOICE === "sinebells") {
  console.log("→ harmonic voice · sinebells (no samples; pure synth)");
  mixHarmEvent = mixEventSinebell;
} else {
  console.error(`trap: unknown voice '${VOICE}'. expected: piano | sinebells`);
  process.exit(1);
}

// ── drum mixer: AC-native via percussion.mjs ──────────────────────────
// Implements `sound.synth({type, tone, duration, volume, attack, decay,
// pan})` as a buffer-write into `out` at a fixed startSec. Each call to
// playPercussion fans out N synth calls — they all share startSec.
//
// Noise is shaped through a state-variable bandpass at `tone` (Q≈4),
// clamped at SR/6 for stability. Above the clamp the noise is still
// mixed but with a duller upper edge — adequate for the 8 kHz hi-hats
// in the AC kit.
function mixSynthVoice(out, startSec, params) {
  const type = params?.type || "sine";
  const tone = Number(params?.tone) || 440;
  const duration = Number(params?.duration);
  const volume = Number(params?.volume ?? 1);
  const attack = Math.max(0, Number(params?.attack ?? 0.001));
  const decay  = Number.isFinite(params?.decay)
    ? Math.max(0, Number(params.decay))
    : Math.max(0.001, (Number.isFinite(duration) ? duration : 0.05) - attack);

  if (!Number.isFinite(volume) || volume === 0) return;
  if (!Number.isFinite(duration) || duration <= 0) return;

  const total = Math.max(duration, attack + decay);
  const startIdx = Math.floor(startSec * SAMPLE_RATE);
  const totalSamples = Math.ceil(total * SAMPLE_RATE);
  const attSamples = Math.max(1, Math.floor(attack * SAMPLE_RATE));
  const decSamples = Math.max(1, Math.floor(decay * SAMPLE_RATE));

  const omega = (2 * Math.PI * tone) / SAMPLE_RATE;
  const phaseInc = tone / SAMPLE_RATE;

  // SVF bandpass for noise. Stable for fc < SR/6.
  const fcSafe = Math.min(Math.max(40, tone), SAMPLE_RATE / 6);
  const fParam = 2 * Math.sin(Math.PI * fcSafe / SAMPLE_RATE);
  const dampParam = 1 / 4;
  let svfLow = 0, svfBand = 0;

  for (let i = 0; i < totalSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;

    // Envelope: linear attack, exponential decay.
    let env;
    if (i < attSamples) {
      env = i / attSamples;
    } else {
      const ti = i - attSamples;
      if (ti >= decSamples) continue;
      env = Math.exp(-3 * ti / decSamples);
    }

    let s;
    switch (type) {
      case "sine":
        s = Math.sin(omega * i);
        break;
      case "square":
        s = Math.sin(omega * i) >= 0 ? 1 : -1;
        break;
      case "triangle": {
        const ph = (i * phaseInc) % 1;
        s = ph < 0.5 ? 4 * ph - 1 : 3 - 4 * ph;
        break;
      }
      case "sawtooth": {
        const ph = (i * phaseInc) % 1;
        s = 2 * ph - 1;
        break;
      }
      case "noise":
      case "noise-white": {
        const white = noiseRng() * 2 - 1;
        const high = white - svfLow - dampParam * svfBand;
        svfBand += fParam * high;
        svfLow  += fParam * svfBand;
        s = svfBand * 1.5;
        break;
      }
      default:
        s = 0;
    }

    out[dst] += s * env * volume;
  }
}

function makeBufferSynth(out, startSec) {
  return {
    synth: (params) => {
      mixSynthVoice(out, startSec, params || {});
      return null;
    },
  };
}

function fireDrum(out, startSec, letter, opts = {}) {
  const sound = makeBufferSynth(out, startSec);
  playPercussion(sound, letter, { phase: "both", ...opts });
}

// ── build event list ───────────────────────────────────────────────────
const beatSec = 60 / BPM;
const barSec = beatSec * 4;        // 4/4
const stepSec = beatSec / 4;        // 16th notes
const totalSec = barSec * BARS;
const pattern = BEAT_PATTERNS[STYLE];

const harmonicEvents = []; // { startSec, midi, gain, durSec }
const drumEvents = [];     // { startSec, letter, volume }

for (let bar = 0; bar < BARS; bar++) {
  const barStart = bar * barSec;
  const deg = PROGRESSION[bar % PROGRESSION.length];
  const triad = chordMidis(deg, 0);
  const bass  = scaleNoteMidi(deg, -2);

  // Drums: 16 steps per bar
  for (let step = 0; step < 16; step++) {
    const stepStart = barStart + step * stepSec;
    if (pattern.kick[step]) {
      const accent = step === 0 ? 1.15 : 1.0;
      drumEvents.push({ startSec: stepStart, letter: "c", volume: accent * DRUM_GAIN });
    }
    if (pattern.snare[step]) {
      // Backbeat (steps 4 and 12) hits hardest.
      const accent = (step === 4 || step === 12) ? 1.05 : 0.9;
      drumEvents.push({ startSec: stepStart, letter: "d", volume: accent * DRUM_GAIN });
    }
    if (pattern.hat[step]) {
      // Trap hi-hat velocity variation: downbeat strong, off-beats softer.
      const onBeat = step % 4 === 0;
      const v = (onBeat ? 0.55 : 0.30) + rng() * 0.15;
      drumEvents.push({ startSec: stepStart, letter: "g", volume: v * DRUM_GAIN });
    }
    if (pattern.hatOpen[step]) {
      drumEvents.push({ startSec: stepStart, letter: "a", volume: 0.55 * DRUM_GAIN });
    }
  }

  // Bass on beat 1 — long sustain, octave low.
  harmonicEvents.push({ startSec: barStart, midi: bass, gain: 0.55, durSec: barSec * 0.95 });

  // Chord stabs on backbeats (beats 2 and 4) — short, soft.
  for (const m of triad) {
    harmonicEvents.push({ startSec: barStart + beatSec * 1, midi: m, gain: 0.22, durSec: beatSec * 0.6 });
    harmonicEvents.push({ startSec: barStart + beatSec * 3, midi: m, gain: 0.22, durSec: beatSec * 0.6 });
  }

  // Sparse melody — frequency tunable via DENSITY.
  const wantMelody = rng() < (0.25 + DENSITY * 0.5);
  if (wantMelody) {
    const melDeg = deg + (rng() < 0.5 ? 4 : 2);
    const melMidi = scaleNoteMidi(melDeg, 1);
    const onset = rng() < 0.5 ? 0 : beatSec * 2; // top of bar or halfway
    harmonicEvents.push({
      startSec: barStart + onset + beatSec * 0.05,
      midi: melMidi,
      gain: 0.32,
      durSec: beatSec * 1.6,
    });
  }
}

console.log(
  `→ trap · style=${STYLE} · voice=${VOICE} · ${BARS} bars · ${BPM} bpm · 4/4 · ${SCALE_NAME} · ` +
  `${totalSec.toFixed(1)}s · ${drumEvents.length} drum hits · ${harmonicEvents.length} harm notes · seed=${SEED_STR}`
);

// Export the deterministic event list for downstream tooling.
{
  const eventsPath = `${ROOT}/out/trap-events.json`;
  const dir = OUT_PATH.replace(/\/[^/]+$/, "");
  mkdirSync(dir, { recursive: true });
  writeFileSync(eventsPath, JSON.stringify({
    style: STYLE, voice: VOICE, bpm: BPM, scale: SCALE_NAME, bars: BARS,
    beatSec, barSec, stepSec, totalSec, seed: SEED_STR,
    drum: drumEvents, harm: harmonicEvents,
  }, null, 2));
  console.log(`→ events · ${eventsPath} (${drumEvents.length + harmonicEvents.length} total)`);
}

// ── render ─────────────────────────────────────────────────────────────
const tailSec = VOICE === "sinebells" ? BELL_RING_TAIL : 1.0;
const totalSamples = Math.ceil((totalSec + tailSec) * SAMPLE_RATE);
const out = new Float32Array(totalSamples);

for (const ev of harmonicEvents) mixHarmEvent(ev, out);
for (const ev of drumEvents) fireDrum(out, ev.startSec, ev.letter, { volume: ev.volume });

// Normalize to ~ -3 dBFS peak, then scale to voiceGain.
let peak = 0;
for (let i = 0; i < out.length; i++) {
  const a = Math.abs(out[i]);
  if (a > peak) peak = a;
}
if (peak > 0) {
  const target = 0.7;
  const norm = target / peak;
  const finalGain = norm * VOICE_GAIN / 0.18;
  for (let i = 0; i < out.length; i++) out[i] *= finalGain;
}

// ── write ──────────────────────────────────────────────────────────────
const outDir = dirname(OUT_PATH);
mkdirSync(outDir, { recursive: true });
const tag = audienceName || `${STYLE}-${SEED_STR}`;
const rawPath = `${outDir}/.${tag}-${VOICE}.f32.raw`;

const buf = Buffer.alloc(out.length * 4);
for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
writeFileSync(rawPath, buf);
console.log(`→ wrote ${rawPath} (${(buf.length / 1024 / 1024).toFixed(2)} MB f32 mono ${SAMPLE_RATE}Hz)`);

const ff = spawnSync(
  "ffmpeg",
  [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3",
    OUT_PATH,
  ],
  { stdio: "inherit" }
);
if (ff.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}
try { unlinkSync(rawPath); } catch {}
console.log(`✓ ${OUT_PATH}`);
