#!/usr/bin/env node
// render-pt.mjs — headless Pink Trombone renderer.
//
// Loads vendor/pinktrombone/index.html, extracts Thapen's Glottis +
// Tract object literals + simplex noise lib in-place (no transcription,
// drift-free against upstream), runs them inside a node vm sandbox with
// shimmed globals, and renders parameter trajectories to mono 16-bit
// PCM WAV. The biquad-filtered aspiration/fricative noise inputs are
// approximated as white noise — close enough for vowel quality, will
// matter more once fricative phonemes become a target.
//
// This is what fit.py's render_pt() will eventually call (via subprocess).
// For now it's also a smoke-test driver: render a few vowels to disk.
//
// Usage:
//   node bin/render-pt.mjs --vowel ah --duration 1.5 --out ~/Desktop/pt-ah.wav
//   node bin/render-pt.mjs --smoke ~/Desktop                # render the demo set
//   node bin/render-pt.mjs --params '{"tongueIndex":29,"tongueDiameter":2.43,"f0":120}' \
//                           --duration 2 --out out.wav

import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import vm from "node:vm";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const PT_HTML = resolve(ROOT, "vendor/pinktrombone/index.html");

// ── Source extraction ─────────────────────────────────────────────────
// PT's HTML carries the entire source inline. We slice out the named
// regions by anchor strings that have been stable since v1.1 (March
// 2017). If any of these slices fail, the upstream's been re-formatted
// and we should re-pin.
function extractRegion(src, startMarker, endMarker, name) {
  const start = src.indexOf(startMarker);
  if (start < 0) throw new Error(`extract '${name}': missing start marker ${JSON.stringify(startMarker)}`);
  const end = src.indexOf(endMarker, start);
  if (end < 0) throw new Error(`extract '${name}': missing end marker ${JSON.stringify(endMarker)}`);
  return src.slice(start, end + endMarker.length);
}

function loadPT() {
  const html = readFileSync(PT_HTML, "utf8");

  // Math.clamp + Math.moveTowards (two definitions; the second wins,
  // which is the four-arg form we need)
  const mathHelpers = extractRegion(html, "Math.clamp = function", "Math.gaussian", "math-helpers")
    .replace(/Math\.gaussian[\s\S]*$/, "");

  // Simplex noise IIFE — public domain (Stefan Gustavson)
  const simplexNoise = extractRegion(html, "(function(global){", "})(this);", "simplex-noise");

  // Glottis + Tract literals — match by their stable terminator pattern.
  const glottisMatch = html.match(/var Glottis =\s*\{[\s\S]*?\n\}\s*\n\s*\n\s*var Tract/);
  if (!glottisMatch) throw new Error("could not extract Glottis block");
  const glottisBlock = glottisMatch[0].replace(/\n\s*var Tract$/, "");

  const tractMatch = html.match(/var Tract =\s*\{[\s\S]*?\n\};\s*\n/);
  if (!tractMatch) throw new Error("could not extract Tract block");
  const tractBlock = tractMatch[0];

  return { mathHelpers, simplexNoise, glottisBlock, tractBlock };
}

// ── Sandbox + boot ────────────────────────────────────────────────────
function bootSandbox(sampleRate) {
  const sandbox = {
    Math: Object.create(Math),
    Float64Array,
    Float32Array,
    Array,
    Date,
    console,
    sampleRate,
    alwaysVoice: false,
    autoWobble: false,
    backCtx: makeNullCtx(),
    UI: { touchesWithMouse: [] },
    AudioSystem: { blockLength: 512, blockTime: 512 / sampleRate },
    palePink: "#FFEEF5",
    time: 0,
    noise: null,
  };
  sandbox.global = sandbox;
  sandbox.window = sandbox;
  sandbox.this = sandbox;
  vm.createContext(sandbox);

  const { mathHelpers, simplexNoise, glottisBlock, tractBlock } = loadPT();

  // Run Math helpers (extends sandbox.Math.clamp, Math.moveTowards)
  vm.runInContext(mathHelpers, sandbox);
  // Run simplex noise — installs `noise` on the global (the IIFE writes
  // global.noise = {…}). Our sandbox is the global, so this works.
  vm.runInContext(simplexNoise.replace("})(this);", "})(this);\nthis.noise = noise;"), sandbox);
  // Glottis + Tract literals
  vm.runInContext(glottisBlock + ";\n" + tractBlock, sandbox);

  // Skip Glottis.init (it draws the keyboard); init waveform manually
  sandbox.Glottis.setupWaveform(0);
  sandbox.Tract.init();
  return sandbox;
}

function makeNullCtx() {
  const noop = () => {};
  return new Proxy({}, { get: () => noop, set: () => true });
}

// ── Parameter shape (the contract fit.py optimizes over) ──────────────
// All values in PT's native units. The fitter passes a flat dict; it
// can override any subset, the rest fall back to the per-pose defaults.
//   tongueIndex     [12, 29]  front-back tongue position
//   tongueDiameter  [2.05, 3.5]  close-open tongue height
//   lipMul          [0, 1.0]  lip aperture multiplier (0=closed, 1=open)
//   velumOpen       [0.01, 0.4]  nasal port (0.01=closed, 0.4=hum)
//   f0              Hz       fundamental
//   tenseness       [0, 1]   glottal tension
//   loudness        [0, 1]
//
// `pose` (optional) names a vowel preset whose values fill any unset
// fields. Useful for relative perturbations during fitting.

// ── Word recipes — keyframe arrays for short transitions ──────────────
// `t` is normalized [0, 1] across the render duration. PT's
// Tract.movementSpeed (15 cm/s) handles fine-grained smoothing inside
// the gap between any two keyframes for free.
//
// These are hand-crafted from the canonical IPA pose values. The fitter
// can refine them against jeffrey-pvc word recordings later — this is
// just the "sounds plausible from the phonetics" baseline.
const WORD_RECIPES = {
  // /aɪ/ "eye" — back-open → front-close glide, mostly voiced
  eye: [
    { t: 0.0,  pose: "ah", f0: 130, tenseness: 0.65 },
    { t: 0.55, pose: "ah", f0: 130, tenseness: 0.65 },   // hold the start
    { t: 1.0,  pose: "iy", f0: 145, tenseness: 0.60 },
  ],
  // /oʊ/ "owe" — back-open-rounded → back-close-rounded
  owe: [
    { t: 0.0,  pose: "aa",    f0: 125, tenseness: 0.65, lipMulOverride: 0.65 },
    { t: 0.5,  pose: "aa",    f0: 125, tenseness: 0.65, lipMulOverride: 0.55 },
    { t: 1.0,  pose: "uw",    f0: 130, tenseness: 0.60 },
  ],
  // /eɪ/ "ay" — mid-front → close-front
  ay: [
    { t: 0.0,  tongueIndex: 14.5, tongueDiameter: 2.95, lipMul: 1.0, velumOpen: 0.01, f0: 130, tenseness: 0.6 },
    { t: 0.5,  tongueIndex: 14.5, tongueDiameter: 2.95, lipMul: 1.0, velumOpen: 0.01, f0: 130, tenseness: 0.6 },
    { t: 1.0,  pose: "iy", f0: 140, tenseness: 0.6 },
  ],
  // /wi/ "we" — back-close-rounded glide → front-close
  we: [
    { t: 0.0,  pose: "uw", f0: 130, tenseness: 0.55 },
    { t: 0.25, pose: "uw", f0: 132, tenseness: 0.6 },
    { t: 1.0,  pose: "iy", f0: 145, tenseness: 0.6 },
  ],
  // /aʊ/ "ow!" — back-open → back-close-rounded
  ow: [
    { t: 0.0,  pose: "ah", f0: 130, tenseness: 0.7 },
    { t: 0.5,  pose: "ah", f0: 130, tenseness: 0.7 },
    { t: 1.0,  pose: "uw", f0: 130, tenseness: 0.55 },
  ],
};

// ── Vowel poses (defaults) ────────────────────────────────────────────
const VOWEL_POSES = {
  // front-close
  iy: { tongueIndex: 12.9,  tongueDiameter: 2.43, velumOpen: 0.01, lipsRound: false },
  // front-open
  ae: { tongueIndex: 14,    tongueDiameter: 3.4,  velumOpen: 0.01, lipsRound: false },
  // open central
  ah: { tongueIndex: 22.0,  tongueDiameter: 3.5,  velumOpen: 0.01, lipsRound: false },
  // back-open
  aa: { tongueIndex: 25.0,  tongueDiameter: 3.5,  velumOpen: 0.01, lipsRound: false },
  // back-close, rounded
  uw: { tongueIndex: 27.0,  tongueDiameter: 2.3,  velumOpen: 0.01, lipsRound: true  },
  // schwa — neutral
  schwa: { tongueIndex: 17.0, tongueDiameter: 2.8, velumOpen: 0.01, lipsRound: false },
  // nasal hum (closed lips, velum open)
  m: { tongueIndex: 17.0, tongueDiameter: 2.8, velumOpen: 0.4, lipsRound: false, lipsClosed: true },
};

// Apply tract pose into Tract.targetDiameter / restDiameter using PT's
// own setRestDiameter math (transcribed from TractUI line 1541).
// `params` is a fully-resolved object (no pose lookup here).
function applyTractParams(s, params) {
  const Tract = s.Tract;
  const tongueIndex = params.tongueIndex;
  const tongueDiameter = params.tongueDiameter;
  const lipMul = params.lipMul;
  const gridOffset = 1.7;
  for (let i = Tract.bladeStart; i < Tract.lipStart; i++) {
    const t = 1.1 * Math.PI * (tongueIndex - i) / (Tract.tipStart - Tract.bladeStart);
    const fixedTongueDiameter = 2 + (tongueDiameter - 2) / 1.5;
    let curve = (1.5 - fixedTongueDiameter + gridOffset) * Math.cos(t);
    if (i == Tract.bladeStart - 2 || i == Tract.lipStart - 1) curve *= 0.8;
    if (i == Tract.bladeStart    || i == Tract.lipStart - 2) curve *= 0.94;
    Tract.restDiameter[i] = 1.5 - curve;
  }
  // Lip aperture region (PT segments lipStart..n-1)
  for (let i = Tract.lipStart; i < Tract.n; i++) {
    Tract.restDiameter[i] = 1.5 * lipMul;
  }
  for (let i = 0; i < Tract.n; i++) Tract.targetDiameter[i] = Tract.restDiameter[i];
  Tract.velumTarget = params.velumOpen;
}

function resolveParams(input) {
  // Resolve a partial input against an optional pose preset.
  let base;
  if (input.pose && VOWEL_POSES[input.pose]) {
    const p = VOWEL_POSES[input.pose];
    base = {
      tongueIndex: p.tongueIndex,
      tongueDiameter: p.tongueDiameter,
      lipMul: p.lipsClosed ? 0.0 : (p.lipsRound ? 0.6 : 1.0),
      velumOpen: p.velumOpen ?? 0.01,
      f0: 120,
      tenseness: 0.6,
      loudness: 0.8,
    };
  } else {
    // Schwa-ish defaults if no pose given.
    base = {
      tongueIndex: 17, tongueDiameter: 2.8, lipMul: 1.0, velumOpen: 0.01,
      f0: 120, tenseness: 0.6, loudness: 0.8,
    };
  }
  return { ...base, ...input };
}

// Resolve a single keyframe entry's pose-or-explicit-params shape.
function resolveKeyframe(kf) {
  const r = resolveParams(kf);
  if (kf.lipMulOverride !== undefined) r.lipMul = kf.lipMulOverride;
  return { t: kf.t, params: r };
}

// Linear interpolation between two resolved param dicts.
function lerpParams(a, b, alpha) {
  const out = {};
  const keys = ["tongueIndex", "tongueDiameter", "lipMul", "velumOpen",
                "f0", "tenseness", "loudness"];
  for (const k of keys) out[k] = a[k] * (1 - alpha) + b[k] * alpha;
  return out;
}

// Find the params at a given normalized time t ∈ [0, 1] from a sorted
// keyframe array, by piecewise linear interpolation.
function paramsAtT(keyframes, t) {
  if (t <= keyframes[0].t) return keyframes[0].params;
  if (t >= keyframes[keyframes.length - 1].t) return keyframes[keyframes.length - 1].params;
  for (let i = 0; i < keyframes.length - 1; i++) {
    const a = keyframes[i], b = keyframes[i + 1];
    if (t >= a.t && t <= b.t) {
      const span = (b.t - a.t) || 1;
      const alpha = (t - a.t) / span;
      return lerpParams(a.params, b.params, alpha);
    }
  }
  return keyframes[keyframes.length - 1].params;
}

// ── Render ─────────────────────────────────────────────────────────────
function render(input = {}) {
  const sampleRate = input.sampleRate ?? 22050;
  const durationS = input.durationS ?? 1.5;
  const voiced = input.voiced ?? true;
  const rampInS = input.rampInS ?? 0.05;
  const rampOutS = input.rampOutS ?? 0.1;
  const seed = input.seed ?? 1;

  // Resolve keyframes if provided; otherwise treat input as a single
  // steady-state pose (back-compat with v0.1 callers).
  let keyframes = null;
  if (input.keyframes && Array.isArray(input.keyframes) && input.keyframes.length > 0) {
    keyframes = input.keyframes
      .map(resolveKeyframe)
      .sort((x, y) => x.t - y.t);
  }
  const initialParams = keyframes ? keyframes[0].params : resolveParams(input);

  const s = bootSandbox(sampleRate);
  applyTractParams(s, initialParams);

  // Glottis driving values (UI* are the inputs PT reads each block)
  s.Glottis.UIFrequency = initialParams.f0;
  s.Glottis.UITenseness = initialParams.tenseness;
  s.Glottis.smoothFrequency = initialParams.f0;
  s.Glottis.loudness = initialParams.loudness;
  s.alwaysVoice = voiced;
  s.autoWobble = false;     // determinism for tests; fitter can flip on
  s.noise.seed(seed);

  const N = Math.round(durationS * sampleRate);
  const out = new Float32Array(N);
  const blockLen = s.AudioSystem.blockLength;
  const rampInN = Math.round(rampInS * sampleRate);
  const rampOutStart = N - Math.round(rampOutS * sampleRate);

  // Pre-allocate noise buffer for a block.
  const aspNoise = new Float32Array(blockLen);
  const fricNoise = new Float32Array(blockLen);

  for (let blockStart = 0; blockStart < N; blockStart += blockLen) {
    // Inject keyframe-interpolated params for this block (if a
    // trajectory was given). PT's tract.movementSpeed adds another
    // layer of smoothing on top of these per-block targets.
    if (keyframes) {
      const tNorm = blockStart / Math.max(1, N - 1);
      const p = paramsAtT(keyframes, tNorm);
      applyTractParams(s, p);
      s.Glottis.UIFrequency = p.f0;
      s.Glottis.UITenseness = p.tenseness;
      s.Glottis.loudness = p.loudness;
    }
    // Fill noise inputs (white noise; biquad filtering is a v0.2 feature).
    for (let j = 0; j < blockLen; j++) {
      aspNoise[j] = Math.random() * 2 - 1;
      fricNoise[j] = Math.random() * 2 - 1;
    }
    const blockN = Math.min(blockLen, N - blockStart);
    for (let j = 0; j < blockN; j++) {
      const lambda1 = j / blockLen;
      const lambda2 = (j + 0.5) / blockLen;
      const glottalOutput = s.Glottis.runStep(lambda1, aspNoise[j]);
      let vocal = 0;
      s.Tract.runStep(glottalOutput, fricNoise[j], lambda1);
      vocal += s.Tract.lipOutput + s.Tract.noseOutput;
      s.Tract.runStep(glottalOutput, fricNoise[j], lambda2);
      vocal += s.Tract.lipOutput + s.Tract.noseOutput;
      let sample = vocal * 0.125;
      // soft attack/release (PT's intensity ramp handles most of this,
      // but a hard tail-clip on stop avoids the residual tract ringing
      // bleeding into a fade)
      const i = blockStart + j;
      if (i < rampInN) sample *= i / rampInN;
      else if (i >= rampOutStart) sample *= Math.max(0, (N - i) / (N - rampOutStart));
      out[i] = sample;
    }
    s.Glottis.finishBlock();
    s.Tract.finishBlock();
  }
  return { audio: out, sampleRate };
}

// ── WAV encoder (16-bit PCM mono) ─────────────────────────────────────
function encodeWav(float32, sampleRate) {
  const N = float32.length;
  const buf = Buffer.alloc(44 + N * 2);
  // RIFF header
  buf.write("RIFF", 0);
  buf.writeUInt32LE(36 + N * 2, 4);
  buf.write("WAVE", 8);
  buf.write("fmt ", 12);
  buf.writeUInt32LE(16, 16);
  buf.writeUInt16LE(1, 20);          // PCM
  buf.writeUInt16LE(1, 22);          // mono
  buf.writeUInt32LE(sampleRate, 24);
  buf.writeUInt32LE(sampleRate * 2, 28);
  buf.writeUInt16LE(2, 32);
  buf.writeUInt16LE(16, 34);
  buf.write("data", 36);
  buf.writeUInt32LE(N * 2, 40);
  // Find peak for safe normalization (target -3 dBFS).
  let peak = 1e-9;
  for (let i = 0; i < N; i++) peak = Math.max(peak, Math.abs(float32[i]));
  const gain = 0.7079 / peak;        // -3 dBFS
  for (let i = 0; i < N; i++) {
    const v = Math.max(-1, Math.min(1, float32[i] * gain));
    buf.writeInt16LE(Math.round(v * 32767), 44 + i * 2);
  }
  return buf;
}

function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

// ── CLI ───────────────────────────────────────────────────────────────
async function main() {
  const argv = process.argv.slice(2);
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) {
      const key = a.slice(2);
      const next = argv[i + 1];
      if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
      else flags[key] = true;
    }
  }

  if (flags.smoke) {
    const dest = expandHome(typeof flags.smoke === "string" ? flags.smoke : `${homedir()}/Desktop`);
    mkdirSync(dest, { recursive: true });
    const set = [
      { name: "pt-ah",       pose: "ah",    f0: 120, tenseness: 0.6, durationS: 1.5 },
      { name: "pt-ee",       pose: "iy",    f0: 120, tenseness: 0.6, durationS: 1.5 },
      { name: "pt-oo",       pose: "uw",    f0: 120, tenseness: 0.6, durationS: 1.5 },
      { name: "pt-aa",       pose: "aa",    f0: 120, tenseness: 0.65, durationS: 1.5 },
      { name: "pt-schwa",    pose: "schwa", f0: 110, tenseness: 0.55, durationS: 1.5 },
      { name: "pt-hum",      pose: "m",     f0: 110, tenseness: 0.55, durationS: 1.5 },
    ];
    console.log(`→ rendering smoke set into ${dest}`);
    for (const item of set) {
      const t0 = Date.now();
      const { audio, sampleRate } = render(item);
      const wav = encodeWav(audio, sampleRate);
      const out = resolve(dest, `${item.name}.wav`);
      writeFileSync(out, wav);
      console.log(`  ✓ ${item.name}.wav  (${(wav.length/1024).toFixed(0)} KB · ${item.durationS}s · ${Date.now()-t0}ms)`);
    }
    console.log("\ndone. open ~/Desktop and double-click to listen.");
    return;
  }

  if (flags.words) {
    // Render every word recipe to ~/Desktop. Optional override of duration.
    const dest = expandHome(typeof flags.words === "string" ? flags.words : `${homedir()}/Desktop`);
    const durationS = Number(flags.duration ?? 1.0);
    mkdirSync(dest, { recursive: true });
    console.log(`→ rendering words into ${dest}  (${durationS}s each)`);
    for (const [name, keyframes] of Object.entries(WORD_RECIPES)) {
      const t0 = Date.now();
      const { audio, sampleRate } = render({ keyframes, durationS });
      const wav = encodeWav(audio, sampleRate);
      const out = resolve(dest, `pt-word-${name}.wav`);
      writeFileSync(out, wav);
      console.log(`  ✓ pt-word-${name}.wav  (${(wav.length/1024).toFixed(0)} KB · ${Date.now()-t0}ms · ${keyframes.length} keyframes)`);
    }
    return;
  }

  // Single render
  let input;
  if (flags.word) {
    const recipe = WORD_RECIPES[String(flags.word)];
    if (!recipe) {
      console.error(`✗ unknown word: ${flags.word}. options: ${Object.keys(WORD_RECIPES).join(", ")}`);
      process.exit(1);
    }
    input = { keyframes: recipe };
  } else if (flags.params) {
    input = JSON.parse(flags.params);
  } else if (flags.keyframes) {
    input = { keyframes: JSON.parse(flags.keyframes) };
  } else {
    input = { pose: flags.vowel || flags.pose || "schwa" };
  }
  if (flags.duration)  input.durationS = Number(flags.duration);
  if (flags.f0)        input.f0 = Number(flags.f0);
  if (flags.tenseness) input.tenseness = Number(flags.tenseness);
  if (flags["sample-rate"]) input.sampleRate = Number(flags["sample-rate"]);
  if (flags.seed)      input.seed = Number(flags.seed);

  const out = expandHome(flags.out
    || `~/Desktop/pt-${flags.word || input.pose || "params"}.wav`);
  const isStdout = out === "-";
  const quiet = flags.quiet === true || isStdout;
  if (!quiet) {
    const summary = JSON.stringify({ ...input, durationS: input.durationS ?? 1.5 });
    console.log(`→ ${summary} → ${out}`);
  }
  const { audio, sampleRate } = render(input);
  const wav = encodeWav(audio, sampleRate);
  if (isStdout) {
    // raw WAV bytes — used by fit.py subprocess
    process.stdout.write(wav);
  } else {
    mkdirSync(dirname(out), { recursive: true });
    writeFileSync(out, wav);
    if (!quiet) console.log(`✓ ${out}  (${(wav.length/1024).toFixed(0)} KB)`);
  }
}

main().catch((e) => { console.error(e); process.exit(1); });
