#!/usr/bin/env node
// audio-preview.mjs — fast audio-only mix for iterating on the bed
// (voice + waltz + drum kit) without paying the full slides/compose
// cost. Outputs `recap/out/audio.mp3`.
//
// Drum kit is synthesized in node and pre-spliced into ONE perc.wav
// before ffmpeg, so the final ffmpeg call is always exactly three
// inputs (voice + bed + perc) regardless of how many kit pieces fire.
//
// Reads from the audience config:
//   audience.waltz.bpm  — default tempo (CLI --bpm overrides)
//   audience.kick / .snare / .hihat — per-layer defaults
//
// Usage:
//   node bin/audio-preview.mjs <audience>
//   node bin/audio-preview.mjs <audience> --rap                                    # use beat-quantized vocal
//   node bin/audio-preview.mjs <audience> --no-snare --no-hihat                    # kick only
//   node bin/audio-preview.mjs <audience> --no-bed                                 # drop the waltz
//   node bin/audio-preview.mjs <audience> --bpm 86 --time-sig 4                    # 4/4 hip-hop
//   node bin/audio-preview.mjs <audience> --kick-pattern boom-bap                  # 1 + "and of 2"
//   node bin/audio-preview.mjs <audience> --hat-grid 16                            # 16th-note hihats
//
// Knob flags:
//   --kick-pattern    every-beat | beat1 | beat1+3 | boom-bap
//   --kick-gain N     loudness 0.0..1.5
//   --kick-dur N      kick body length in seconds
//   --kick-pitch-end  end frequency of the pitch sweep (lower = subbier)
//   --snare-pattern   2+4 | every-beat (default 2+4 — classic backbeat)
//   --snare-gain N
//   --hat-grid        4 | 8 | 16 (8ths default)
//   --hat-gain N
//   --hat-open        treat as open hihat (longer decay)
//   --bed-gain N      waltz level (default 0.18); 0 == off
//   --bpm N           override audience BPM
//   --time-sig 3|4    beats per bar

import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const SR = 48000;

// ── WAV mono writer (16-bit PCM) ──────────────────────────────────────
function writeMonoWav(outPath, samples /* Float32 in [-1,1] */) {
  const N = samples.length;
  const dataBytes = N * 2;
  const buf = Buffer.alloc(44 + dataBytes);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataBytes, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 2, 28);
  buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(dataBytes, 40);
  for (let i = 0; i < N; i++) {
    const v = Math.max(-1, Math.min(1, samples[i])) * 32767;
    buf.writeInt16LE(Math.floor(v), 44 + i * 2);
  }
  mkdirSync(dirname(outPath), { recursive: true });
  writeFileSync(outPath, buf);
}

// ── synth 808 kick: sine pitch sweep + click attack + exp decay ───────
function makeKickSample(opts = {}) {
  const dur = opts.dur ?? 0.6;
  const pitchStart = opts.pitchStart ?? 140;
  const pitchEnd = opts.pitchEnd ?? 55;
  const pitchTau = opts.pitchTau ?? 0.035;
  const ampTau = opts.ampTau ?? 0.18;
  const clickDur = 0.004;
  const N = Math.floor(SR * dur);
  const out = new Float32Array(N);
  let phase = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const freq = pitchEnd + (pitchStart - pitchEnd) * Math.exp(-t / pitchTau);
    phase += (2 * Math.PI * freq) / SR;
    let s = Math.sin(phase) * Math.exp(-t / ampTau);
    if (t < clickDur) s += (Math.random() * 2 - 1) * 0.4 * (1 - t / clickDur);
    out[i] = s * 0.95;
  }
  return out;
}

// ── synth snare: bandpass noise + low tonal body, sharp envelope ──────
function makeSnareSample(opts = {}) {
  const dur = opts.dur ?? 0.18;
  const ampTau = opts.ampTau ?? 0.045;
  const tonalFreq = opts.tonalFreq ?? 180;
  const N = Math.floor(SR * dur);
  const out = new Float32Array(N);
  // Two simple one-pole HP+LP combo to approximate a band-pass on noise.
  // (Crude but cheap; good enough for backbeat texture.)
  let hpPrev = 0, lpPrev = 0;
  const alphaHp = Math.exp(-2 * Math.PI * 700 / SR);    // ~700 Hz HPF
  const alphaLp = Math.exp(-2 * Math.PI * 4500 / SR);   // ~4.5 kHz LPF
  let phase = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const noiseIn = Math.random() * 2 - 1;
    // hp = noiseIn - hpPrev * alphaHp  (simple high-pass-ish)
    const hp = noiseIn - alphaHp * hpPrev;
    hpPrev = noiseIn;
    // lp on hp
    const lp = (1 - alphaLp) * hp + alphaLp * lpPrev;
    lpPrev = lp;
    phase += (2 * Math.PI * tonalFreq) / SR;
    const tonal = Math.sin(phase) * 0.35;
    const env = Math.exp(-t / ampTau);
    out[i] = (lp * 0.7 + tonal * 0.3) * env;
  }
  return out;
}

// ── synth hihat: high-pass noise, very short envelope ─────────────────
function makeHihatSample(opts = {}) {
  const open = opts.open === true;
  const dur = opts.dur ?? (open ? 0.25 : 0.05);
  const ampTau = open ? 0.10 : 0.018;
  const N = Math.floor(SR * dur);
  const out = new Float32Array(N);
  let hpPrev = 0;
  const alphaHp = Math.exp(-2 * Math.PI * 7000 / SR);
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const noiseIn = Math.random() * 2 - 1;
    const hp = noiseIn - alphaHp * hpPrev;
    hpPrev = noiseIn;
    out[i] = hp * Math.exp(-t / ampTau) * 0.7;
  }
  return out;
}

// ── synth bass: sub sine with TRUE ADSR — flat sustain plateau ────────
// Inspired by pop's sinebells but voiced low. Hits hold at a sustained
// plateau level for most of the duration, then release out smoothly.
// Sustain = constant amplitude (NOT exponential decay), so the bass
// genuinely RINGS through the half-bar.
function makeBassSample(opts = {}) {
  const freq = opts.freq ?? 55;
  const dur = opts.dur ?? 1.6;
  const attackMs = 8;
  const decayMs = 60;                        // attack peak → sustain level
  const releaseMs = 220;                     // sustain → silence at the tail
  const sustainLevel = 0.85;
  const N = Math.floor(SR * dur);
  const attackN = Math.floor((attackMs / 1000) * SR);
  const decayN = Math.floor((decayMs / 1000) * SR);
  const releaseN = Math.floor((releaseMs / 1000) * SR);
  const sustainN = Math.max(0, N - attackN - decayN - releaseN);
  const out = new Float32Array(N);
  let phase = 0;
  for (let i = 0; i < N; i++) {
    phase += (2 * Math.PI * freq) / SR;
    const s = Math.sin(phase) + 0.18 * Math.sin(2 * phase);
    let env;
    if (i < attackN) {
      env = i / Math.max(1, attackN);                                      // attack ramp
    } else if (i < attackN + decayN) {
      const r = (i - attackN) / Math.max(1, decayN);
      env = 1 - (1 - sustainLevel) * r;                                    // peak → sustain
    } else if (i < attackN + decayN + sustainN) {
      env = sustainLevel;                                                  // FLAT plateau
    } else {
      const r = (i - attackN - decayN - sustainN) / Math.max(1, releaseN);
      env = sustainLevel * Math.max(0, 1 - r);                             // linear release
    }
    out[i] = s * env * 0.55;
  }
  return out;
}

// ── synth pluck for melody: sine + 3rd harmonic + ADSR ────────────────
// Slightly bell-like at higher pitches. Used to play the lead melody
// over the chord progression. attack ~12ms, decay to 30% over ~150ms,
// release tail ~400ms.
function makePluckSample(opts = {}) {
  const freq = opts.freq ?? 440;
  const dur = opts.dur ?? 0.55;
  const decayTau = opts.decayTau ?? 0.18;
  const attackMs = 10;
  const N = Math.floor(SR * dur);
  const attackN = Math.floor((attackMs / 1000) * SR);
  const out = new Float32Array(N);
  let phase = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    phase += (2 * Math.PI * freq) / SR;
    // Sine + soft 2nd + tinier 3rd = pluck-ish timbre
    const s =
      Math.sin(phase) +
      0.25 * Math.sin(2 * phase) +
      0.08 * Math.sin(3 * phase);
    let env = Math.exp(-t / decayTau);
    if (i < attackN) env *= i / attackN;
    out[i] = s * env * 0.45;
  }
  return out;
}

// ── helpers for the melody schedule ───────────────────────────────────
const NOTE_NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
function noteToFreq(name) {
  const m = name.match(/^([A-G])(#|b)?(\d+)$/);
  if (!m) return 440;
  const base = { C:0,D:2,E:4,F:5,G:7,A:9,B:11 }[m[1]];
  const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
  const oct = Number(m[3]);
  const midi = 12 * (oct + 1) + base + acc;
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// ── splice a sample N times into a target buffer ──────────────────────
function spliceHits(target, sample, hitTimesSec, gain) {
  for (const t of hitTimesSec) {
    const start = Math.floor(t * SR);
    for (let k = 0; k < sample.length; k++) {
      const dst = start + k;
      if (dst < 0) continue;
      if (dst >= target.length) break;
      target[dst] += sample[k] * gain;
    }
  }
}

// ── arg parsing ───────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--no-")) { flags[a.slice(2)] = false; continue; }
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}
const audienceName = positional[0];
if (!audienceName) {
  console.error("usage: audio-preview.mjs <audience> [flags — see header]");
  process.exit(2);
}

const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
const audience = mod.audience || mod.default;

const useAutotune = flags.autotune === true;
const useRap = flags.rap === true || useAutotune;
const voicePath = useAutotune
  ? `${ROOT}/out/recap-rap-tuned.mp3`
  : useRap
    ? `${ROOT}/out/recap-rap.mp3`
    : `${ROOT}/out/recap.mp3`;
const waltzPath = `${ROOT}/out/waltz.mp3`;
const rapDurationPath = `${ROOT}/out/recap-rap.duration.txt`;
const durationPath = `${ROOT}/out/duration.txt`;

if (!existsSync(voicePath)) {
  const hint = useAutotune ? " — run bin/autotune.mjs first" :
               useRap ? " — run bin/rap.mjs first" : " — run tts.mjs first";
  console.error(`✗ missing ${voicePath}${hint}`);
  process.exit(1);
}
const haveBedFile = existsSync(waltzPath);
if (!haveBedFile) console.warn(`  ⚠ no ${waltzPath} — bed will be voice-only`);

let totalSec;
if (useRap && existsSync(rapDurationPath)) {
  totalSec = parseFloat(readFileSync(rapDurationPath, "utf8").trim());
} else if (existsSync(durationPath)) {
  totalSec = parseFloat(readFileSync(durationPath, "utf8").trim());
} else {
  const r = spawnSync(
    "/opt/homebrew/opt/ffmpeg-full/bin/ffprobe",
    ["-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", voicePath],
    { encoding: "utf8" },
  );
  totalSec = parseFloat((r.stdout || "0").trim()) || 0;
}
if (!Number.isFinite(totalSec) || totalSec <= 0) { console.error("✗ could not determine total duration"); process.exit(1); }

const bpm = flags.bpm ? parseFloat(flags.bpm) : (audience.waltz && audience.waltz.bpm) || 86;
const beatsPerBar = flags["time-sig"] ? parseInt(flags["time-sig"], 10) : 4;
const beatSec = 60 / bpm;
const barSec = beatSec * beatsPerBar;

// ── per-layer config (audience defaults + CLI overrides) ──────────────
// Kick less subby: pitchEnd raised from 50→78 so it doesn't pile on
// the bass + waltz low end. Tighter dur. Mid-low punchy thump.
const kickCfg = {
  enabled: flags.kick !== false,
  pattern: "boom-bap",
  gain: 0.55,
  dur: 0.40, pitchStart: 130, pitchEnd: 78,
  ...(audience.kick || {}),
};
const snareCfg = {
  enabled: flags.snare !== false,
  pattern: "2+4",
  gain: 0.45,                            // backbeat present but not aggressive
  dur: 0.16, tonalFreq: 180,
  ...(audience.snare || {}),
};
const hatCfg = {
  enabled: flags.hihat !== false,
  grid: 8,
  gain: 0.22,                            // hat ticking quietly under everything
  open: false,
  ...(audience.hihat || {}),
};
// Synth bass — sub sine on root note. Default ON: gives the "groove"
// underlayer that replaces the waltz bed.
const bassCfg = {
  enabled: flags.bass !== false,
  pattern: "beat1+3",
  freq: 73,                              // D2 — sits higher so it doesn't muddy
  gain: 0.45,
  dur: 0.55,
  ...(audience.bass || {}),
};
// Melody — morphs across the track. Each section's `progression` is
// played for its weighted share of the total duration; when a new
// section starts, its progression starts from bar 0 (so chord motion
// stays coherent). Inspired by audience.waltz.morph in pop/trance.
// The FINAL section should usually mirror the FIRST so the melody
// "returns to its original form" by the end.
const melodyCfg = {
  enabled: flags.melody !== false,
  gain: 0.55,
  octave: 0,
  morph: [
    // 1) OPENING — Am / F / C / G arpeggio. Quarter-note density.
    {
      label: "open",
      weight: 1.2,
      gain: 0.55,
      octave: 0,
      density: 1,
      progression: [
        ["A3", "C4", "E4", "A4"],
        ["F3", "A3", "C4", "F4"],
        ["C4", "E4", "G4", "C5"],
        ["G3", "B3", "D4", "G4"],
      ],
    },
    // 2) DEVELOP — same chords, higher voicing + 7ths, 8th-note density.
    {
      label: "develop",
      weight: 1.6,
      gain: 0.55,
      octave: 0,
      density: 2,
      progression: [
        ["A4", "C5", "E5", "G5"],
        ["F4", "A4", "C5", "E5"],
        ["C5", "E5", "G5", "B5"],
        ["G4", "B4", "D5", "F5"],
      ],
    },
    // 3) PEAK — modal shift Dm / Am / Bb / F. 16th-note density (busiest).
    {
      label: "peak",
      weight: 1.8,
      gain: 0.62,
      octave: 0,
      density: 4,
      progression: [
        ["D4", "F4", "A4", "D5"],
        ["A3", "C4", "E4", "A4"],
        ["A#3", "D4", "F4", "A#4"],
        ["F3", "A3", "C4", "F4"],
      ],
    },
    // 4) RETURN — same as opening, brings melody home. Quarter notes again.
    {
      label: "return",
      weight: 1.2,
      gain: 0.55,
      octave: 0,
      density: 1,
      progression: [
        ["A3", "C4", "E4", "A4"],
        ["F3", "A3", "C4", "F4"],
        ["C4", "E4", "G4", "C5"],
        ["G3", "B3", "D4", "G4"],
      ],
    },
  ],
  ...(audience.melody || {}),
};
// CLI overrides
if (typeof flags["kick-pattern"] === "string") kickCfg.pattern = flags["kick-pattern"];
if (flags["kick-gain"] !== undefined) kickCfg.gain = parseFloat(flags["kick-gain"]);
if (flags["kick-dur"] !== undefined) kickCfg.dur = parseFloat(flags["kick-dur"]);
if (flags["kick-pitch-end"] !== undefined) kickCfg.pitchEnd = parseFloat(flags["kick-pitch-end"]);
if (typeof flags["snare-pattern"] === "string") snareCfg.pattern = flags["snare-pattern"];
if (flags["snare-gain"] !== undefined) snareCfg.gain = parseFloat(flags["snare-gain"]);
if (flags["hat-grid"] !== undefined) hatCfg.grid = parseInt(flags["hat-grid"], 10);
if (flags["hat-gain"] !== undefined) hatCfg.gain = parseFloat(flags["hat-gain"]);
if (flags["hat-open"] === true) hatCfg.open = true;
if (typeof flags["bass-pattern"] === "string") bassCfg.pattern = flags["bass-pattern"];
if (flags["bass-freq"] !== undefined) bassCfg.freq = parseFloat(flags["bass-freq"]);
if (flags["bass-gain"] !== undefined) bassCfg.gain = parseFloat(flags["bass-gain"]);
if (flags["melody-gain"] !== undefined) melodyCfg.gain = parseFloat(flags["melody-gain"]);
if (flags["melody-octave"] !== undefined) melodyCfg.octave = parseInt(flags["melody-octave"], 10);
if (flags.melody === false) melodyCfg.enabled = false;

// Waltz bed is now OFF by default — the synth bass + kit forms the
// groove. Pass --bed-gain 0.3 to bring it back as a wash.
const bedGain = flags["bed-gain"] !== undefined ? parseFloat(flags["bed-gain"]) : 0.0;
const bedOff = flags.bed === false || bedGain <= 0 || !haveBedFile;

// ── hit-time schedulers ───────────────────────────────────────────────
const guard = totalSec - 0.05;
function kickTimes() {
  if (!kickCfg.enabled) return [];
  const times = [];
  if (kickCfg.pattern === "every-beat") {
    for (let t = 0; t < guard; t += beatSec) times.push(t);
  } else if (kickCfg.pattern === "beat1+3") {
    for (let t = 0; t < guard; t += barSec) {
      times.push(t);
      const third = t + 2 * beatSec;
      if (third < guard) times.push(third);
    }
  } else if (kickCfg.pattern === "boom-bap") {
    // 1 + the "and" of 2. In 3/4 falls back to beat1+3.
    for (let t = 0; t < guard; t += barSec) {
      times.push(t);
      const swing = beatsPerBar === 4 ? t + 2.5 * beatSec : t + 2 * beatSec;
      if (swing < guard) times.push(swing);
    }
  } else {
    for (let t = 0; t < guard; t += barSec) times.push(t);
  }
  return times;
}
function snareTimes() {
  if (!snareCfg.enabled) return [];
  const times = [];
  if (snareCfg.pattern === "every-beat") {
    for (let t = 0; t < guard; t += beatSec) times.push(t);
  } else {
    // 2+4 backbeat. In 3/4: just beat 2.
    for (let t = 0; t < guard; t += barSec) {
      const two = t + 1 * beatSec;
      const four = t + 3 * beatSec;
      if (two < guard) times.push(two);
      if (beatsPerBar === 4 && four < guard) times.push(four);
    }
  }
  return times;
}
function hatTimes() {
  if (!hatCfg.enabled) return [];
  const step = beatSec * (4 / hatCfg.grid);
  const times = [];
  for (let t = 0; t < guard; t += step) times.push(t);
  return times;
}
function bassTimes() {
  if (!bassCfg.enabled) return [];
  const times = [];
  if (bassCfg.pattern === "every-beat") {
    for (let t = 0; t < guard; t += beatSec) times.push(t);
  } else if (bassCfg.pattern === "beat1") {
    for (let t = 0; t < guard; t += barSec) times.push(t);
  } else {
    for (let t = 0; t < guard; t += barSec) {
      times.push(t);
      const third = t + 2 * beatSec;
      if (third < guard) times.push(third);
    }
  }
  return times;
}

// Melody schedule: morph progression across sections. Per section,
// `density` controls notes-per-beat:
//   1.0 = one note per beat (default, quarter notes)
//   2.0 = 8th notes
//   4.0 = 16th notes
// Optional counter-melody: a second voice plays a chord tone offset
// from the main note (defaults to the next chord tone in the row).
function melodyEvents() {
  if (!melodyCfg.enabled) return [];
  const events = [];

  const morph = (melodyCfg.morph && melodyCfg.morph.length)
    ? melodyCfg.morph
    : [{ label: "static", weight: 1, gain: melodyCfg.gain, octave: melodyCfg.octave, density: 1, progression: melodyCfg.progression || [["A3", "C4", "E4", "A4"]] }];
  const totalWeight = morph.reduce((a, m) => a + m.weight, 0);
  const totalBars = Math.max(1, Math.ceil(guard / barSec));

  const sectionForBar = new Array(totalBars).fill(0);
  let bIdx = 0;
  for (let s = 0; s < morph.length; s++) {
    const span = Math.max(1, Math.round(totalBars * morph[s].weight / totalWeight));
    const end = Math.min(totalBars, bIdx + span);
    for (let i = bIdx; i < end; i++) sectionForBar[i] = s;
    bIdx = end;
  }
  while (bIdx < totalBars) { sectionForBar[bIdx++] = morph.length - 1; }

  const barsInSection = new Array(totalBars).fill(0);
  {
    let runStart = 0;
    for (let b = 0; b < totalBars; b++) {
      if (b > 0 && sectionForBar[b] !== sectionForBar[b - 1]) runStart = b;
      barsInSection[b] = b - runStart;
    }
  }

  // Walk by density-aware sub-beat. For density=2, two events per beat.
  for (let barNum = 0; barNum < totalBars; barNum++) {
    const sec = morph[Math.min(sectionForBar[barNum] || 0, morph.length - 1)];
    const density = sec.density ?? 1;
    const row = sec.progression[barsInSection[barNum] % sec.progression.length];
    if (!row) continue;
    const stepsPerBar = beatsPerBar * density;
    const stepSec = barSec / stepsPerBar;
    const octave = (sec.octave ?? melodyCfg.octave) ?? 0;
    const sectionGain = sec.gain ?? melodyCfg.gain;
    const counterOn = (sec.counter !== false) && (melodyCfg.counter !== false);
    const counterGain = (sec.counterGain ?? melodyCfg.counterGain ?? 0.55) * sectionGain;
    for (let step = 0; step < stepsPerBar; step++) {
      const t = barNum * barSec + step * stepSec;
      if (t >= guard) break;
      const rowIdx = step % row.length;
      const noteName = row[rowIdx];
      if (!noteName) continue;
      let freq = noteToFreq(noteName) * Math.pow(2, octave);
      events.push({ t, freq, note: noteName, gain: sectionGain, section: sec.label || `s${sectionForBar[barNum]}` });
      // Counter-melody: pick another chord tone from the row (offset)
      if (counterOn) {
        const counterIdx = (rowIdx + 2) % row.length;        // 3rd up in the arpeggio
        const counterName = row[counterIdx];
        if (counterName) {
          let counterFreq = noteToFreq(counterName) * Math.pow(2, octave);
          // bring counter to the OCTAVE above main so it harmonizes
          while (counterFreq < freq) counterFreq *= 2;
          events.push({ t, freq: counterFreq, note: counterName, gain: counterGain, section: sec.label, counter: true });
        }
      }
    }
  }
  return events;
}

const kHits = kickTimes();
const sHits = snareTimes();
const hHits = hatTimes();
const bHits = bassTimes();
const mEvents = melodyEvents();

console.log(`audio-preview · ${audienceName} · ${bpm} bpm ${beatsPerBar}/4 · total ${totalSec.toFixed(2)}s`);
console.log(`  voice  : ${voicePath.replace(REPO + "/", "")}${useAutotune ? "  [RAP+TUNED]" : useRap ? "  [RAP]" : ""}`);
console.log(`  bed    : ${bedOff ? "(off)" : `waltz.mp3 @ gain=${bedGain.toFixed(2)}`}`);
console.log(`  bass   : ${bassCfg.enabled ? `synth ${bassCfg.freq}Hz · ${bassCfg.pattern} · gain=${bassCfg.gain} · ${bHits.length} hits` : "(off)"}`);
if (melodyCfg.enabled) {
  const morph = melodyCfg.morph || [{ label: "static" }];
  const sections = morph.map((m) => m.label || "?").join(" → ");
  const sectionCounts = morph.map((m, i) => `${m.label || `s${i}`}=${mEvents.filter((e) => e.section === (m.label || `s${i}`)).length}`).join(" ");
  console.log(`  melody : synth pluck · morph(${sections}) · ${mEvents.length} notes [${sectionCounts}]`);
} else {
  console.log(`  melody : (off)`);
}
console.log(`  kick   : ${kickCfg.enabled ? `synth 808 · ${kickCfg.pattern} · gain=${kickCfg.gain} · ${kHits.length} hits` : "(off)"}`);
console.log(`  snare  : ${snareCfg.enabled ? `synth · ${snareCfg.pattern} · gain=${snareCfg.gain} · ${sHits.length} hits` : "(off)"}`);
console.log(`  hihat  : ${hatCfg.enabled ? `synth ${hatCfg.open ? "open" : "closed"} · ${hatCfg.grid}ths · gain=${hatCfg.gain} · ${hHits.length} hits` : "(off)"}`);

// ── build perc track (node-side splice into one buffer) ───────────────
const percBuf = new Float32Array(Math.ceil(totalSec * SR));
if (bassCfg.enabled) {
  const sample = makeBassSample({
    freq: bassCfg.freq,
    dur: bassCfg.dur,
    ampTau: bassCfg.ampTau,
  });
  spliceHits(percBuf, sample, bHits, bassCfg.gain);
}
if (melodyCfg.enabled) {
  // Each melody note is a freshly-synthesized pluck at its freq; use
  // the per-event gain (varies per morph section).
  for (const e of mEvents) {
    const sample = makePluckSample({ freq: e.freq, dur: 0.55 });
    spliceHits(percBuf, sample, [e.t], e.gain ?? melodyCfg.gain);
  }
}
if (kickCfg.enabled) {
  const sample = makeKickSample({ dur: kickCfg.dur, pitchStart: kickCfg.pitchStart, pitchEnd: kickCfg.pitchEnd });
  spliceHits(percBuf, sample, kHits, kickCfg.gain);
}
if (snareCfg.enabled) {
  const sample = makeSnareSample({ dur: snareCfg.dur, tonalFreq: snareCfg.tonalFreq });
  spliceHits(percBuf, sample, sHits, snareCfg.gain);
}
if (hatCfg.enabled) {
  const sample = makeHihatSample({ open: hatCfg.open });
  spliceHits(percBuf, sample, hHits, hatCfg.gain);
}

// soft clip (tanh-ish) to keep peaks tame
for (let i = 0; i < percBuf.length; i++) {
  const x = percBuf[i];
  percBuf[i] = Math.tanh(x);
}

const percPath = `${ROOT}/out/perc.wav`;
writeMonoWav(percPath, percBuf);

// ── final 3-input mix: voice + bed + perc ─────────────────────────────
const ff = "/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg";
const args = ["-hide_banner", "-loglevel", "warning", "-y", "-i", voicePath];
if (!bedOff) args.push("-i", waltzPath);
args.push("-i", percPath);

// Voice is FORWARD in the mix: pre-boost via volume + extra weight in
// the final amix. Perc bus dropped slightly so the kit/bass/melody
// underlayer the vocal instead of competing with it.
const filters = [`[0:a]aresample=48000,volume=1.6[voice]`];
let bedLabel = null, percLabel = "perc";
const percIdx = bedOff ? 1 : 2;
if (!bedOff) {
  filters.push(`[1:a]aresample=48000,apad=whole_dur=${totalSec},atrim=duration=${totalSec},volume=${bedGain}[bed]`);
  bedLabel = "bed";
}
filters.push(`[${percIdx}:a]aresample=48000,aformat=channel_layouts=stereo,apad=whole_dur=${totalSec},atrim=duration=${totalSec}[${percLabel}]`);

const mixLabels = ["[voice]"];
const mixWeights = ["1.8"];                          // voice dominates
if (bedLabel) { mixLabels.push(`[${bedLabel}]`); mixWeights.push(String(bedGain)); }
mixLabels.push(`[${percLabel}]`); mixWeights.push("0.80");
filters.push(`${mixLabels.join("")}amix=inputs=${mixLabels.length}:normalize=0:duration=first:weights=${mixWeights.join(" ")}[mix]`);

args.push(
  "-filter_complex", filters.join(";"),
  "-map", "[mix]",
  "-ac", "2", "-ar", "48000",
  "-c:a", "libmp3lame", "-b:a", "192k",
  `${ROOT}/out/audio.mp3`,
);

const t0 = Date.now();
const r = spawnSync(ff, args, { stdio: ["ignore", "ignore", "inherit"] });
if (r.status !== 0) { console.error(`✗ ffmpeg exited ${r.status}`); process.exit(r.status || 1); }
console.log(`✓ ${ROOT}/out/audio.mp3 · ${((Date.now() - t0) / 1000).toFixed(1)}s`);
