#!/usr/bin/env node
// waltz.mjs — render a slow waltz bed for the recap.
//
// Two voices supported:
//   --voice piano      Salamander grand-piano sample bank
//                      (`fedac/native/samples/piano/<midi>.raw`,
//                      mono float32 @ 48kHz, ~3s per anchor —
//                      the same bank notepat plays through fedac/native/audio.c)
//   --voice sinebells  pure-sine bell synth — fundamental + slightly
//                      inharmonic partials, sharp attack, exponential decay.
//                      No samples needed; renders directly.
//
// The rhythmic / harmonic logic is adapted from
// `.vscode/tests/test-generative-waltz.mjs` and `artery/test-trapwaltz.mjs`,
// simplified to a single voice: bass note on beat 1, chord triad on beats
// 2 and 3 ("oom-pah-pah"), with a melodic line on top.
//
// The audience config can carry a `waltz` block that seeds the generator
// (so each cut gets a distinct tune from the same instrument). Defaults
// can also be overridden on the CLI for one-off renders.
//
// Usage:
//   node bin/waltz.mjs                                  # default audience
//   node bin/waltz.mjs jeffrey-24h-2026-05-01           # named audience
//   node bin/waltz.mjs jeffrey-24h-2026-05-01 \
//        --voice sinebells --bpm 64 --scale minor \
//        --bars 24 --density 0.7 --seed bells-test \
//        --out ~/Desktop/waltz-bells.mp3

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
const audienceName = positional[0] || "jeffrey-24h";

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

// ── load audience config ──────────────────────────────────────────────
const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);
const W = audience.waltz || {};

const VOICE        = flags.voice || W.voice || "piano";
const SEED_STR     = flags.seed || W.seed || audience.name || audienceName;
const BPM          = Number(flags.bpm  ?? W.bpm  ?? 80);
const SCALE_NAME   = flags.scale || W.scale || "major";
const PROGRESSION  = parseProgression(flags.progression) || W.progression || [0, 5, 3, 4]; // I vi IV V
const VOICE_GAIN   = Number(flags.gain ?? W.voiceGain ?? 0.18);
const DENSITY      = Number(flags.density ?? W.density ?? 0.5); // 0..1, melody/passing density
const ROOT_OFFSET  = Number(flags.transpose ?? W.transpose ?? 0); // semitones (default C)
const OUT_PATH     = expandHome(flags.out) || `${ROOT}/out/waltz.mp3`;

// Bar count is normally fixed (`--bars` / audience.waltz.bars / 24), but if
// a target duration is given we auto-size BARS so the bed plays once
// through and ends with the narration (no looping in compose). This is
// the desired behavior for the recap pipeline — the waltz is composed,
// not stream-looped, so the cadence resolves musically with the content.
const _beatSecPre = 60 / BPM;
const _barSecPre  = _beatSecPre * 3;
let DURATION_SEC = null;
if (flags.duration !== undefined) {
  DURATION_SEC = Number(flags.duration);
} else if (W.duration !== undefined) {
  DURATION_SEC = Number(W.duration);
} else {
  // Fall back to out/duration.txt (written by align.mjs) when present, so
  // a bare `node bin/waltz.mjs <audience>` after the rest of the pipeline
  // produces a content-length bed without an explicit flag.
  const durFile = `${ROOT}/out/duration.txt`;
  if (existsSync(durFile)) {
    const t = Number(readFileSync(durFile, "utf8").trim());
    if (Number.isFinite(t) && t > 0) DURATION_SEC = t;
  }
}
const BARS = (() => {
  if (flags.bars !== undefined) return Number(flags.bars);
  if (W.bars !== undefined && DURATION_SEC === null) return Number(W.bars);
  if (DURATION_SEC !== null) return Math.max(1, Math.ceil(DURATION_SEC / _barSecPre));
  return 24;
})();

function parseProgression(s) {
  if (!s || s === true) return null;
  return s.split(",").map((x) => Number(x.trim()));
}

const SAMPLE_RATE = 48_000;

// ── deterministic PRNG seeded by audience name ────────────────────────
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

// ── musical theory ─────────────────────────────────────────────────────
const SCALES = {
  major:  [0, 2, 4, 5, 7, 9, 11],
  minor:  [0, 2, 3, 5, 7, 8, 10],
  dorian: [0, 2, 3, 5, 7, 9, 10],
  lydian: [0, 2, 4, 6, 7, 9, 11],
};
const SCALE = SCALES[SCALE_NAME] || SCALES.major;

// ── voice: piano (sample bank) ────────────────────────────────────────
const PIANO_SAMPLE_DIR = resolve(REPO, "fedac/native/samples/piano");
const PIANO_ANCHORS = [21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96];
let pianoBank = null;

function loadPianoBank() {
  const bank = new Map();
  for (const m of PIANO_ANCHORS) {
    const path = `${PIANO_SAMPLE_DIR}/${m}.raw`;
    if (!existsSync(path)) throw new Error(`waltz: missing piano anchor ${path}`);
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
// A bell is sines + slightly inharmonic partials with exponential decay.
// Each partial has its own decay constant — the high partials die first,
// leaving the fundamental to ring out, which is the ear's "bell" cue.
// Ratios pulled from the Risset / Chowning / FOF bell tradition; we let
// the strike attack be a few ms so it has presence in the mix.
// Softened bell — fundamental dominates, the inharmonic clang and high
// shimmer sit way back so the bed reads as "soft chime" not "struck bell."
// Attack also softened (~12ms) to remove the percussive transient.
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 }, // sub / "hum"
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 }, // fundamental — keep loud and long
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 }, // octave (harmonic, gentle)
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 }, // inharmonic clang — way back
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
  { ratio: 5.4,  amp: 0.02, decayT60: 0.4 }, // shimmer — barely there
];
const ATTACK_SEC = 0.012;       // softer than 0.004 — reads as "chime" not "strike"
const BELL_RING_TAIL = 6.0;     // a hair longer for the gentler decay
const BELL_GAIN = 0.42;         // global voice gain (was 0.55)

function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

function mixEventSinebell(ev, out) {
  const startIdx = Math.floor(ev.startSec * SAMPLE_RATE);
  const ringSamples = Math.floor((ev.durSec + BELL_RING_TAIL) * SAMPLE_RATE);
  const attackS = ATTACK_SEC * SAMPLE_RATE;
  const fundFreq = midiToFreq(ev.midi);
  const twoPiOverSr = (2 * Math.PI) / SAMPLE_RATE;

  // Precompute per-partial omega + decay-per-sample. T60 = time to drop 60dB
  // (~ amplitude * 1e-3). decayPerSample = exp(-ln(1000) / (T60 * sr)).
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
      // running envelope: amp at t=0, multiplied by decay each sample
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    // Soft cosine attack — gentler than a linear ramp at the start
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * BELL_GAIN;
  }
}

// ── route to chosen voice ─────────────────────────────────────────────
let mixEvent;
if (VOICE === "piano") {
  pianoBank = loadPianoBank();
  mixEvent = mixEventPiano;
} else if (VOICE === "sinebells") {
  console.log("→ voice · sinebells (no samples; pure synth)");
  mixEvent = mixEventSinebell;
} else {
  console.error(`waltz: unknown voice '${VOICE}'. expected: piano | sinebells`);
  process.exit(1);
}

// ── morph sections ─────────────────────────────────────────────────────
// audience.waltz.morph (optional) is a list of sections the bed walks
// through across the duration. Each section can override progression /
// scale / density / transpose. Sections are weighted by `weight` (default
// 1) and the total bars get distributed proportionally so the whole bed
// resolves at the end of the narration. If `morph` is absent, behavior
// is identical to the prior fixed-progression mode (one virtual section
// using the top-level progression / scale / density / transpose).
//
// Example (in audience.waltz):
//   morph: [
//     { progression: [0, 5, 3, 4],     scale: "major",  density: 0.4 },
//     { progression: [3, 0, 4, 5],     scale: "lydian", density: 0.55, transpose: 2 },
//     { progression: [5, 4, 0, 3],     scale: "dorian", density: 0.65 },
//     { progression: [0, 3, 4, 0, 5, 0], scale: "major", density: 0.5 },
//   ]
const MORPH_RAW = (Array.isArray(W.morph) && W.morph.length > 0) ? W.morph : null;
const MORPH = MORPH_RAW
  ? MORPH_RAW.map((s) => ({
      progression: Array.isArray(s.progression) && s.progression.length > 0 ? s.progression : PROGRESSION,
      scale: SCALES[s.scale] || SCALE,
      scaleName: s.scale || SCALE_NAME,
      density: s.density !== undefined ? Number(s.density) : DENSITY,
      transpose: s.transpose !== undefined ? Number(s.transpose) : ROOT_OFFSET,
      weight: s.weight !== undefined ? Math.max(0.001, Number(s.weight)) : 1,
    }))
  : [
      {
        progression: PROGRESSION,
        scale: SCALE,
        scaleName: SCALE_NAME,
        density: DENSITY,
        transpose: ROOT_OFFSET,
        weight: 1,
      },
    ];

// Distribute BARS across sections by weight. We use floor + fix-up on the
// last section so the sum stays exactly BARS.
const TOTAL_WEIGHT = MORPH.reduce((a, s) => a + s.weight, 0);
const SECTION_BARS = (() => {
  const out = MORPH.map((s) => Math.max(1, Math.floor((s.weight / TOTAL_WEIGHT) * BARS)));
  const sum = out.reduce((a, b) => a + b, 0);
  if (sum !== BARS) out[out.length - 1] += (BARS - sum);
  return out;
})();

function sectionForBar(barIdx) {
  let acc = 0;
  for (let i = 0; i < MORPH.length; i++) {
    acc += SECTION_BARS[i];
    if (barIdx < acc) {
      // Bar offset within the section — used to walk the section's progression.
      const localBar = barIdx - (acc - SECTION_BARS[i]);
      return { section: MORPH[i], localBar, sectionIndex: i };
    }
  }
  // Defensive: last section if we somehow run past the end.
  const last = MORPH[MORPH.length - 1];
  return { section: last, localBar: barIdx, sectionIndex: MORPH.length - 1 };
}

function midiInScale(scale, rootOffset, degree, octaveOffset = 0) {
  const len = scale.length;
  const idx = ((degree % len) + len) % len;
  const octShift = Math.floor(degree / len);
  return (60 + rootOffset) + 12 * (octaveOffset + octShift) + scale[idx];
}
function chordMidisIn(scale, rootOffset, rootDegree, octaveOffset = 0) {
  return [
    midiInScale(scale, rootOffset, rootDegree,     octaveOffset),
    midiInScale(scale, rootOffset, rootDegree + 2, octaveOffset),
    midiInScale(scale, rootOffset, rootDegree + 4, octaveOffset),
  ];
}

// ── build event list ───────────────────────────────────────────────────
const beatSec = 60 / BPM;
const barSec = beatSec * 3;
const totalSec = barSec * BARS;
const events = []; // { startSec, midi, gain, durSec }

for (let bar = 0; bar < BARS; bar++) {
  const barStart = bar * barSec;
  const { section, localBar } = sectionForBar(bar);
  const prog = section.progression;
  const scl = section.scale;
  const transpose = section.transpose;
  const density = section.density;

  const deg = prog[localBar % prog.length];
  const triad = chordMidisIn(scl, transpose, deg, 0);
  const bass  = midiInScale(scl, transpose, deg, -2);

  // Beat 1 — bass
  events.push({ startSec: barStart, midi: bass, gain: 0.55, durSec: beatSec * 1.1 });

  // Beat 2 — triad
  for (const m of triad) {
    events.push({ startSec: barStart + beatSec, midi: m, gain: 0.30, durSec: beatSec * 0.9 });
  }
  // Beat 3 — triad again, slightly softer
  for (const m of triad) {
    events.push({ startSec: barStart + 2 * beatSec, midi: m, gain: 0.26, durSec: beatSec * 0.9 });
  }

  // Melody on beat 1, frequency tunable via density (0 = sparse, 1 = every bar)
  const melodyEveryBar = density >= 0.85;
  const wantMelody = melodyEveryBar || (bar % 2 === 0) || rng() < density * 0.5;
  if (wantMelody) {
    const melDeg = deg + (rng() < 0.5 ? 4 : 2); // 5th or 3rd of chord
    const melMidi = midiInScale(scl, transpose, melDeg, 1);
    events.push({ startSec: barStart + beatSec * 0.05, midi: melMidi, gain: 0.40, durSec: beatSec * 2.0 });
  }

  // Passing tone on beat 3 — chance scales with density
  if (rng() < 0.25 + density * 0.5) {
    const passDeg = deg + 1 + Math.floor(rng() * 3);
    const passMidi = midiInScale(scl, transpose, passDeg, 1);
    events.push({ startSec: barStart + 2 * beatSec + beatSec * 0.5, midi: passMidi, gain: 0.30, durSec: beatSec * 0.6 });
  }

  // Optional: extra melodic ornament at high density
  if (density > 0.65 && rng() < 0.5) {
    const ornDeg = deg + (rng() < 0.5 ? 3 : 5);
    const ornMidi = midiInScale(scl, transpose, ornDeg, 1);
    events.push({ startSec: barStart + beatSec * 1.5, midi: ornMidi, gain: 0.28, durSec: beatSec * 0.5 });
  }
}

if (MORPH_RAW) {
  const summary = MORPH.map((s, i) => `${SECTION_BARS[i]}b·${s.scaleName}·d${s.density.toFixed(2)}·t${s.transpose}`).join(" → ");
  console.log(`→ morph · ${MORPH.length} sections: ${summary}`);
}
console.log(`→ waltz · voice=${VOICE} · ${BARS} bars · ${BPM} bpm · ${SCALE_NAME} · ${(totalSec).toFixed(1)}s · ${events.length} notes · seed=${SEED_STR}`);

// Export the deterministic event list so other tools (waltz-overlay.mjs,
// future "music compositional sweep") can read it without re-deriving.
{
  const eventsPath = `${ROOT}/out/waltz-events.json`;
  const dir = OUT_PATH.replace(/\/[^/]+$/, "");
  mkdirSync(dir, { recursive: true });
  writeFileSync(eventsPath, JSON.stringify({
    voice: VOICE, bpm: BPM, scale: SCALE_NAME, bars: BARS,
    beatSec, barSec, totalSec, seed: SEED_STR,
    events,
  }, null, 2));
  console.log(`→ events · ${eventsPath} (${events.length} notes)`);
}

// ── render ─────────────────────────────────────────────────────────────
// Sinebells need extra tail for the ring-out; piano's natural sample tail
// is already truncated by the per-event release.
const tailSec = VOICE === "sinebells" ? BELL_RING_TAIL : 1.0;
const totalSamples = Math.ceil((totalSec + tailSec) * SAMPLE_RATE);
const out = new Float32Array(totalSamples);

for (const ev of events) mixEvent(ev, out);

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
const rawPath = `${outDir}/.${audienceName}-${VOICE}.f32.raw`;

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
