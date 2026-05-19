#!/usr/bin/env node
// skrill.mjs — the "skrill": a resynthesized Skrillex-style growl/talking
// bass for the dance lane. A verb you apply to a note line.
//
// Unlike supersaw.mjs / sinepower.mjs, the skrill CANNOT express itself
// as a fan-out of plain `sound.synth({type:...})` voices — the AC voice
// contract (see bus.mjs) is oscillator + AD envelope only, and the
// Skrillex sound is defined precisely by the two blocks AC lacks:
//
//   1. FM    — a modulator osc driving a carrier, modulation INDEX swept
//              hard, packing inharmonic partials into the 100–400 Hz
//              growl band. This is the timbre.
//   2. A swept resonant FORMANT pair — two band-pass peaks (F1/F2) whose
//              centers are morphed between vowels by a beat-synced LFO.
//              The moving formant is what makes it *talk*. This is the
//              signature.
//
// So the skrill hand-rolls its own per-sample DSP straight into the
// Float32 render buffer (the same way fx.mjs processes buffers), then
// glues with tanh saturation + optional bitcrush + a clean sine sub.
//
// The wub is BEAT-SYNCED: the LFO rate is a note division ("1/8",
// "1/4", "1/8t" triplet, "1/8d" dotted, or a raw Hz number), locked to
// `bpm`, so the talking pattern sits on the trance grid instead of
// drifting. That makes `skrill` composable as a verb in a .np line:
//
//   F1  skrill:ow:1/8      → growl, vowel "ow", wub every 8th note
//   A#1 skrill:talk:1/4    → 3-vowel talking bass, quarter-note wub
//
// Library:
//   import { mixEventSkrill, renderSkrill, SKRILL_PRESETS } from "...";
//   mixEventSkrill(ev, out, opts)        // node bed renderer (the /pop path)
//   renderSkrill(ev, opts) -> Float32    // the bare DSP engine
//   playSkrill(sound, ev, opts)          // live-AC seam (custom voice)
//
// CLI demo:
//   node pop/dance/synths/skrill.mjs                      # growl preset
//   node pop/dance/synths/skrill.mjs --preset talk --lfo 1/4
//   node pop/dance/synths/skrill.mjs --preset screech --bpm 140 --out ~/sk.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "growl";
const DEFAULT_BPM = 138;

// ── vowel formant table (F1, F2 in Hz) ─────────────────────────────────
// The "talking" character is a morph between these resonant-peak pairs.
const VOWELS = {
  ah: [ 700, 1220 ],   // open  — "aah"
  ow: [ 500,  900 ],   // round — "wow"
  oo: [ 300,  870 ],   // closed back — "ooo"
  ee: [ 270, 2300 ],   // closed front — "eee" (bright/screechy)
  eh: [ 530, 1840 ],   // mid front — "ehh"
  uh: [ 640, 1190 ],   // neutral — "uhh"
};

// ── presets ────────────────────────────────────────────────────────────
// modRatio  : modulator freq ÷ carrier freq (1 = same; 2 = octave-up bite)
// fmIndex   : peak FM depth (carrier phase kick per modulator unit)
// vowelA/B  : the two vowels the LFO morphs between (the wub)
// vowelSeq  : if set, an S&H-stepped vowel sequence → talking bass
// lfoShape  : "sine" | "tri" | "saw" | "square" | "sh"
// lfo       : default note-division for the wub
// q         : formant resonance (higher = sharper, more vocal)
// drive     : tanh saturation amount (the grit)
// crush     : 0 = off, else bit depth (e.g. 6 = harsh)
// subGain   : clean unmodulated sine sub an octave down (body)
// edge      : raw-saw blended into the FM carrier (cuts through a mix)
export const SKRILL_PRESETS = {
  // Mid talking bass — the canonical Scary-Monsters wub.
  growl: {
    modRatio: 1.0, fmIndex: 6.0, vowelA: "ah", vowelB: "ow",
    lfoShape: "sine", lfo: "1/8", q: 7, drive: 2.6, crush: 0,
    subGain: 0.38, edge: 0.22, attack: 0.006, decay: 0.10,
  },
  // 3-vowel sequenced talking bass ("yoi-yoi-yow").
  talk: {
    modRatio: 1.0, fmIndex: 5.0, vowelSeq: ["ee", "ah", "ow"],
    lfoShape: "sh", lfo: "1/8", q: 9, drive: 2.2, crush: 0,
    subGain: 0.34, edge: 0.18, attack: 0.006, decay: 0.10,
  },
  // Detuned Reese-ish — slower, fatter, less talk, more rumble.
  reese: {
    modRatio: 0.5, fmIndex: 2.2, vowelA: "uh", vowelB: "oo",
    lfoShape: "tri", lfo: "1/4", q: 4, drive: 1.8, crush: 0,
    subGain: 0.55, edge: 0.30, detune: 16, attack: 0.010, decay: 0.18,
  },
  // High, fast, chopped — the screech/lead growl.
  screech: {
    modRatio: 2.0, fmIndex: 10.0, vowelA: "ee", vowelB: "ah",
    lfoShape: "sh", lfo: "1/16", q: 8, drive: 4.0, crush: 7,
    subGain: 0.12, edge: 0.35, attack: 0.004, decay: 0.07,
  },
  // Clean foundation — minimal FM, no wub, just the sine sub + body.
  sub: {
    modRatio: 1.0, fmIndex: 0.6, vowelA: "oo", vowelB: "oo",
    lfoShape: "sine", lfo: "1/4", q: 3, drive: 1.2, crush: 0,
    subGain: 0.85, edge: 0.06, lfoDepth: 0.0, attack: 0.012, decay: 0.22,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// Deterministic xorshift RNG (same pattern as supersaw.mjs) — keeps the
// sample-&-hold robotic stutter reproducible per event.
function makeRng(seedStr) {
  let s = 2166136261 >>> 0;
  for (let i = 0; i < seedStr.length; i++) {
    s ^= seedStr.charCodeAt(i);
    s = Math.imul(s, 16777619);
  }
  s = s >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

// Resolve a beat-synced wub rate → cycles/sec.
//   "1/n"  → one LFO cycle every (4/n) quarter-note beats
//   "1/nt" → triplet  (n × 3/2 subdivisions per whole note)
//   "1/nd" → dotted   (n × 2/3 subdivisions per whole note)
//   number → raw Hz (escape hatch; drifts off-grid)
export function resolveLfoHz(lfo, bpm) {
  if (typeof lfo === "number" && Number.isFinite(lfo)) return Math.max(0.01, lfo);
  const m = String(lfo).match(/^1\/(\d+(?:\.\d+)?)([td])?$/);
  const beatSec = 60 / bpm;            // one quarter-note
  if (!m) return 1 / (beatSec * 2);    // fallback: 1/8
  let n = parseFloat(m[1]);
  if (m[2] === "t") n *= 1.5;          // triplet feel
  if (m[2] === "d") n /= 1.5;          // dotted feel
  const cycleSec = beatSec * (4 / n);  // 4 = quarter-notes per whole note
  return 1 / Math.max(1e-4, cycleSec);
}

// One unipolar LFO sample in [0,1]. `rng`/`shStep` drive sample-&-hold.
function lfoValue(shape, phase, sh) {
  switch (shape) {
    case "tri":    return phase < 0.5 ? phase * 2 : 2 - phase * 2;
    case "saw":    return phase;
    case "square": return phase < 0.5 ? 0 : 1;
    case "sh":     return sh.value;                       // held per cycle
    case "sine":
    default:       return 0.5 + 0.5 * Math.sin(2 * Math.PI * phase);
  }
}

// ── the DSP engine ─────────────────────────────────────────────────────
// Renders ONE skrill event into a fresh mono Float32Array. ev: { midi,
// durSec, gain?, preset?, lfo?, vowel? }. Per-note `preset`/`lfo`/`vowel`
// override opts so a .np verb token maps straight through.
export function renderSkrill(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const bpm        = opts.bpm        ?? DEFAULT_BPM;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(SKRILL_PRESETS[presetName] || SKRILL_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) {
    return new Float32Array(0);
  }
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack ?? 0.006;
  const decay  = opts.decay  ?? P.decay  ?? 0.10;
  // Match the bus.mjs / native envelope window: auto-extend so the
  // linear decay completes to true silence without an end click.
  const durS = Math.ceil(ev.durSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const decS = Math.max(1, Math.floor(decay * sampleRate));
  const ns   = Math.max(durS, attS + decS);
  const decayStart = ns - decS;
  const out = new Float32Array(ns);

  const carrierF = midiToFreq(ev.midi);
  const modF     = carrierF * (P.modRatio ?? 1.0);
  const lfoHz    = resolveLfoHz(ev.lfo || opts.lfo || P.lfo, bpm);
  const lfoDepth = P.lfoDepth ?? 1.0;

  // Vowel endpoints (per-note `vowel` pins one end for verb control).
  const seq = P.vowelSeq && P.vowelSeq.length ? P.vowelSeq : null;
  const vA = VOWELS[ev.vowel || P.vowelA] || VOWELS.ah;
  const vB = VOWELS[P.vowelB] || vA;

  const rng = makeRng(`skrill:${presetName}:${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`);

  // State.
  let modPhase = 0, carPhase = 0, lfoPhase = rng();
  // Two Chamberlin SVF band-pass filters (formant F1/F2).
  let l1 = 0, b1 = 0, l2 = 0, b2 = 0;
  const damp = 1 / Math.max(0.5, P.q ?? 6);   // 1/Q → resonance
  const sh = { value: rng(), seqIdx: 0 };
  const detune = (P.detune ?? 0) / 1200;       // reese: extra detuned saw
  const TWO_PI = 2 * Math.PI;

  for (let i = 0; i < ns; i++) {
    // ── envelope (linear attack → hold → linear decay) ──
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // ── beat-synced wub LFO ──
    lfoPhase += lfoHz / sampleRate;
    if (lfoPhase >= 1) {
      lfoPhase -= 1;
      sh.value = rng();                         // new S&H value per cycle
      if (seq) sh.seqIdx = (sh.seqIdx + 1) % seq.length;
    }
    let lv = lfoValue(P.lfoShape || "sine", lfoPhase, sh);
    lv = 0.5 + (lv - 0.5) * lfoDepth;           // depth around center

    // ── FM source ── modulator (saw) → carrier (sine), index swept.
    modPhase += modF / sampleRate; if (modPhase >= 1) modPhase -= 1;
    const mod = 2 * modPhase - 1;               // bipolar saw modulator
    // "Constant modulation": index breathes with the wub — the thing
    // every Skrillex tutorial stresses.
    const idx = (P.fmIndex ?? 5) * (0.35 + 0.65 * lv);
    carPhase += carrierF / sampleRate; if (carPhase >= 1) carPhase -= 1;
    let voice = Math.sin(TWO_PI * carPhase + idx * mod);
    // Raw-saw edge so it cuts through a dense trance bed.
    voice = voice * (1 - (P.edge ?? 0.2)) + (2 * carPhase - 1) * (P.edge ?? 0.2);
    // Reese: a second detuned saw layer underneath.
    if (detune) {
      const dp = (carPhase + i * (carrierF * detune) / sampleRate) % 1;
      voice += (2 * dp - 1) * 0.5;
    }

    // ── swept formant pair (the talking) ──
    let f1t, f2t;
    if (seq) {                                  // sequenced talking bass
      const cv = VOWELS[seq[sh.seqIdx]] || vA;
      f1t = cv[0]; f2t = cv[1];
    } else {                                    // smooth A↔B morph
      f1t = vA[0] + (vB[0] - vA[0]) * lv;
      f2t = vA[1] + (vB[1] - vA[1]) * lv;
    }
    const fc1 = Math.min(Math.max(60, f1t), sampleRate / 6);
    const fc2 = Math.min(Math.max(60, f2t), sampleRate / 6);
    const k1 = 2 * Math.sin(Math.PI * fc1 / sampleRate);
    const k2 = 2 * Math.sin(Math.PI * fc2 / sampleRate);
    let h1 = voice - l1 - damp * b1; b1 += k1 * h1; l1 += k1 * b1;
    let h2 = voice - l2 - damp * b2; b2 += k2 * h2; l2 += k2 * b2;
    // F1 carries the body, F2 the brightness/intelligibility.
    let s = b1 * 1.0 + b2 * 0.7;

    // ── grit: tanh saturation + optional bitcrush ──
    s = Math.tanh(s * (P.drive ?? 2.5));
    if (P.crush && P.crush > 0) {
      const steps = Math.pow(2, P.crush);
      s = Math.round(s * steps) / steps;
    }

    // ── clean sine sub an octave down (unmodulated body) ──
    const sub = Math.sin(TWO_PI * carPhase * 0.5) * (P.subGain ?? 0.35);

    out[i] = (s * 0.78 + sub) * env * gain;
  }
  return out;
}

// ── node-side buffer mixer (the /pop bed-render path) ──────────────────
export function mixEventSkrill(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderSkrill(ev, { ...opts, sampleRate });
  const startIdx = Math.floor((ev.startSec ?? 0) * sampleRate);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
  }
}

// ── live-AC seam ───────────────────────────────────────────────────────
// supersaw/sinepower fan out plain sound.synth() voices; the skrill
// can't (no FM/formant in the voice contract). When the live runtime
// grows a buffer/custom voice this renders through it; until bed
// integration lands it is a documented no-op rather than a broken path.
export function playSkrill(sound, ev, opts = {}) {
  if (!sound?.synth) return;
  const seg = renderSkrill(ev, opts);
  if (!seg.length) return;
  // Custom-voice seam: AC's synth supports type:"custom" with a
  // generator. Wired here for when dance-bed integration validates it.
  try {
    let pos = 0;
    sound.synth({
      type: "custom",
      tone: midiToFreq(ev.midi),
      duration: seg.length / (opts.sampleRate ?? DEFAULT_SAMPLE_RATE),
      volume: 1,
      attack: 0,
      decay: 0,
      generator: () => (pos < seg.length ? seg[pos++] : 0),
    });
  } catch {
    /* live custom-voice path not yet validated — see header */
  }
}

// ── CLI demo ───────────────────────────────────────────────────────────
const isMain =
  process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
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
  function expandHome(p) {
    if (!p) return p;
    if (p === "~") return homedir();
    if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
    return p;
  }

  const HERE = dirname(fileURLToPath(import.meta.url));
  const SR = 48_000;
  const preset = flags.preset || DEFAULT_PRESET;
  const bpm = Number(flags.bpm ?? DEFAULT_BPM);
  const lfo = flags.lfo || undefined;
  const outPath = expandHome(flags.out) || resolve(HERE, `skrill-${preset}.mp3`);

  // A harmonized A-minor hook so the wub talks ACROSS a melody, not one
  // pedal note. Each step is a 3-note voicing: melody on top, a diatonic
  // third under it, a fifth/bass below — three skrill voices growling in
  // parallel + a clean sub on the bass. Notes are already in the growl
  // pocket (A3–C5); `--transpose` (semitones) shifts the lot, formants
  // stay fixed in Hz. `--mono` plays just the melody line + sub.
  const transpose = Number(flags.transpose ?? 0);
  const mono = !!flags.mono;
  const beat = 60 / bpm;
  // [ melody, third, bass ] , duration in beats — descending minor hook.
  const HOOK = [
    { v: [69, 64, 57], b: 0.75 },   // Am    A4 E4 A3
    { v: [72, 67, 60], b: 0.25 },   // C     C5 G4 C4
    { v: [71, 67, 55], b: 0.50 },   // Em/G  B4 G4 G3
    { v: [69, 64, 57], b: 0.50 },   // Am    A4 E4 A3
    { v: [67, 62, 59], b: 0.50 },   // G/B   G4 D4 B3
    { v: [64, 59, 52], b: 1.00 },   // Em    E4 B3 E3
    { v: [65, 57, 50], b: 0.50 },   // Dm/F  F4 A3 D3
    { v: [64, 59, 52], b: 1.50 },   // Em    E4 B3 E3
  ];
  const VOICE_GAIN = mono ? [0.85] : [0.78, 0.5, 0.5];
  let t = 0;
  const events = [];
  let noteCount = 0;
  for (let loop = 0; loop < 2; loop++) {
    for (const step of HOOK) {
      const dur = step.b * beat;
      const voices = mono ? [step.v[0]] : step.v;
      for (let n = 0; n < voices.length; n++) {
        events.push({
          startSec: t, midi: voices[n] + transpose, durSec: dur,
          gain: 0.95 * VOICE_GAIN[n], preset, lfo,
        });
        noteCount++;
      }
      // Clean sub doubling the bass voice.
      events.push({
        startSec: t, midi: step.v[2] + transpose - 12, durSec: dur,
        gain: 0.7, preset: "sub",
      });
      t += dur;
    }
  }

  const total = t + 0.6;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(
    `→ skrill demo · preset=${preset} · bpm=${bpm} · wub=${lfo || SKRILL_PRESETS[preset]?.lfo} · ${mono ? "mono" : "harmonized"} · ${noteCount} notes`,
  );
  for (const ev of events) mixEventSkrill(ev, out, { sampleRate: SR, bpm, preset, lfo });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const n = 0.85 / peak; for (let i = 0; i < out.length; i++) out[i] *= n; }

  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, buf);

  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1",
    "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3",
    outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
