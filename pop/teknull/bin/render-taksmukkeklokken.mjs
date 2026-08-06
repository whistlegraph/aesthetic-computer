#!/usr/bin/env node
// taksmukkeklokken — Teknull's 2:00 form rebuilt with conventional synthesis.
// Melody/harmony scheduling uses the shared Clock melody language; audio is
// generated bottom-up from oscillators, envelopes, filters and synthesized noise.

import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, resolve, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { compileClockPattern } from "./clock-offline-scheduler.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const SR = 48_000;
const BPM = 140;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const BARS = 70;
const TOTAL_SEC = BARS * BAR;
const NS = Math.round(TOTAL_SEC * SR);
const UNIT_SEC = BEAT / 2; // Clock duration 2 = one quarter note.

const arg = (name, fallback) => {
  const i = process.argv.indexOf(name);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const SEED = Number(arg("--seed", "41203")) >>> 0;
const SWING = Math.max(0, Math.min(0.18, Number(arg("--swing", "0.08"))));
const ACID_DENSITY = Math.max(0.35, Math.min(1, Number(arg("--acid-density", "0.82"))));
const SCHEDULE_ONLY = process.argv.includes("--schedule-only");
const COOLER = process.argv.includes("--cooler");
const STEM = COOLER ? "taksmukkeklokken-cooler" : "taksmukkeklokken";
const WAV = resolve(OUT, `${STEM}.24bit-48k.wav`);
const MP3 = resolve(OUT, `${STEM}.mp3`);
const SIDECAR = resolve(OUT, `${STEM}.events.json`);
const RAW = resolve(OUT, `.${STEM}.f32le`);
mkdirSync(OUT, { recursive: true });

let rngState = SEED || 1;
function random() {
  rngState = (rngState + 0x6d2b79f5) >>> 0;
  let x = Math.imul(rngState ^ (rngState >>> 15), 1 | rngState);
  x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
  return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
}
const jitter = (amount) => (random() * 2 - 1) * amount;
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;
const sectionFor = (bar) => bar < 8 ? "intro" : bar < 16 ? "build" : bar < 24 ? "acid" : bar < 40 ? "drop-a" : bar < 48 ? "breakdown" : bar < 64 ? "drop-b" : "outro";

const programs = {
  acid: "{sawtooth:0.78}3a.._3a4a_3a_3e_3a_4c__3a_3g4a",
  acidFill: "{square:0.72}3a..3a4a3a3c3e3g4a4c4e4a4g4e4c3g3a",
  hook: "{square:0.58}4a.4c4e4d4e4d4c4a",
  hookLift: "{sawtooth:0.56}4e.4g5a4g5c4g4e5a",
  clockSecond: "{sine:0.34}6a.6e6a_6e6c6e6g",
  clockSecondTurn: "{sine:0.38}6a..6e7a6a6c6e6g7a7c7e7a7g7e7c6g6a",
  clockMinute: "{sine:0.31}5a.5c5e5d5e5d5c5a",
  clockMinuteLift: "{sine:0.33}5e.5g6a5g6c5g5e6a",
  clockChime: "{sine:0.30}4a5e > {sine:0.27}5a6c",
  clockChimeLift: "{sine:0.31}4e5c > {sine:0.29}5e6a",
  clockHour: {
    Am: "{sine:0.25}4a,, {sine:0.14}5e,, {sine:0.09}6a,,",
    F:  "{sine:0.25}4f,, {sine:0.14}5c,, {sine:0.09}5a,,",
    C:  "{sine:0.25}4c,, {sine:0.14}5g,, {sine:0.09}6c,,",
    G:  "{sine:0.25}3g,, {sine:0.14}4d,, {sine:0.09}5g,,",
  },
  chords: {
    Am: "{triangle:0.34}3a,, {sawtooth:0.16}4c,, {square:0.10}4e,,",
    F:  "{triangle:0.34}3a,, {sawtooth:0.16}4c,, {square:0.10}4f,,",
    C:  "{triangle:0.34}3g,, {sawtooth:0.16}4c,, {square:0.10}4e,,",
    G:  "{triangle:0.34}3g,, {sawtooth:0.16}3b,, {square:0.10}4d,,",
  },
};

const scheduled = [];
const compiledPatterns = [];
function scheduleClock(source, bar, voice, section = sectionFor(bar)) {
  const c = compileClockPattern({
    source,
    startSec: t(bar),
    cycles: 1,
    unitSec: UNIT_SEC,
    swingUnitSec: BEAT / 16,
    section,
    voice,
  });
  if (Math.abs(c.durationSec - BAR) > 1e-7) throw new Error(`${voice} Clock pattern is ${c.durationSec}s, expected one bar (${BAR}s): ${source}`);
  compiledPatterns.push({ bar, section, voice, source, eventCount: c.events.length });
  scheduled.push(...c.events);
}

// The dynamic notes are still Clock parameters: deterministic choices resolve
// to a concrete Clock source string before the shared parser schedules them.
const acidScale = ["3a", "3c", "3d", "3e", "3g", "4a", "4c"];
function acidForBar(bar) {
  if (bar % 4 === 3) return programs.acidFill;
  const tokens = ["3a", "_", "3a", "4a", "_", "3a", "_", "3e", "_", "3a", "4c", "_", "_", "3a", "3g", "4a"];
  const mutateAt = [7, 10, 14][Math.floor(random() * 3)];
  tokens[mutateAt] = acidScale[Math.floor(random() * acidScale.length)];
  for (let i = 0; i < tokens.length; i++) if (tokens[i] !== "_" && random() > ACID_DENSITY) tokens[i] = "_";
  tokens[0] = "3a";
  return `{sawtooth:${bar % 2 ? "0.72" : "0.78"}}${tokens[0]}..${tokens.slice(1).join("")}`;
}

for (let bar = 8; bar < 64; bar += 2) {
  if (bar >= 40 && bar < 48) continue;
  const prog = ["Am", "F", "C", "G"][(Math.floor((bar - 8) / 2)) % 4];
  scheduleClock(programs.chords[prog], bar, "pad");
  scheduleClock(programs.chords[prog], bar + 1, "pad");
}
for (let bar = 16; bar < 40; bar++) scheduleClock(acidForBar(bar), bar, "acid");
for (let bar = 48; bar < 64; bar++) {
  scheduleClock(acidForBar(bar), bar, "acid");
  if (bar % 2 === 1) scheduleClock(programs.acidFill.replace(/[34](?=[a-g])/g, (o) => String(Number(o) + 1)), bar, "acid-echo");
}
for (let bar = 24; bar < 40; bar++) scheduleClock(bar >= 32 ? programs.hookLift : programs.hook, bar, "hook");
for (let bar = 48; bar < 64; bar++) scheduleClock(bar >= 56 ? programs.hookLift : programs.hook, bar, "hook");
// Clock sequential syntax becomes the breakdown's explicit two-stage gesture.
for (let bar = 40; bar < 48; bar++) {
  const src = bar < 44
    ? "{triangle:0.30}3a4c > {sine:0.26}4e5a"
    : "{triangle:0.30}3f4a > {sine:0.28}4c5e";
  scheduleClock(src, bar, "breakdown-clock");
}

if (COOLER) {
  // Three interlocking hands, all expressed and tracked as real Clock programs.
  // The sparse hour hand sustains harmony; minute and second hands articulate it.
  for (let bar = 8; bar < 68; bar++) {
    const inBreak = bar >= 40 && bar < 48;
    const hour = ["Am", "F", "C", "G"][Math.floor(bar / 2) % 4];
    if (bar % 2 === 0 || inBreak) scheduleClock(programs.clockHour[hour], bar, "clock-hour");
    if (bar >= 12 && (!inBreak || bar >= 44)) {
      scheduleClock(bar % 4 === 3 ? programs.clockSecondTurn : programs.clockSecond, bar, "clock-second");
    }
    if (bar >= 16 && bar % 2 === 0 && !inBreak) {
      scheduleClock(bar >= 48 ? programs.clockMinuteLift : programs.clockMinute, bar, "clock-minute");
    }
    if ([15, 23, 31, 39, 47, 55, 63, 67].includes(bar)) {
      scheduleClock(bar >= 47 ? programs.clockChimeLift : programs.clockChime, bar, "clock-chime");
    }
  }
}

function drum(voice, startSec, velocity, section, extra = {}) {
  scheduled.push({ type: "drum", voice, startSec, durationSec: extra.durationSec || 0.1, velocity, section, ...extra });
}
function musical(voice, startSec, durationSec, frequency, velocity, section, extra = {}) {
  scheduled.push({ type: "note", voice, startSec, durationSec, frequency, velocity, section, ...extra });
}

for (let bar = 0; bar < BARS; bar++) {
  const section = sectionFor(bar);
  const inBreak = section === "breakdown";
  if (!inBreak && bar < 68) {
    for (let beat = 0; beat < 4; beat++) {
      if (COOLER && ((bar === 23 && beat === 3) || (bar === 63 && beat >= 3))) continue;
      drum("kick", t(bar, beat), bar < 8 ? 0.58 + bar * 0.04 : 0.92, section, { durationSec: COOLER ? 0.46 : 0.42 });
    }
    if (bar >= 4) for (const beat of [0.5, 1.5, 2.5, 3.5]) {
      const late = Math.floor(beat) % 2 ? SWING * BEAT : 0;
      drum("closed-hat", t(bar, beat) + late - 0.006 + jitter(0.003), bar < 8 ? 0.25 : 0.42, section, { durationSec: 0.055, pan: jitter(0.5) });
    }
    if (bar >= 6) for (const beat of [1, 3]) drum("clap", t(bar, beat) + 0.008, 0.68, section, { durationSec: 0.18, pan: jitter(0.12) });
    if (bar >= 8) for (const beat of [0.5, 1.5, bar % 2 ? 2.75 : 2.5, 3.5]) {
      const roots = [55, 43.65, 65.41, 49];
      const root = COOLER ? roots[Math.floor(bar / 2) % 4] : 55;
      musical("sub", t(bar, beat), COOLER ? 0.34 : 0.29, root * (bar % 4 === 3 && beat > 3 ? 2 : 1), COOLER ? 0.72 : 0.66, section);
    }
    if (bar >= 4) drum("open-hat", t(bar, bar % 4 === 2 ? 1.5 : 3.5) - 0.004, 0.34, section, { durationSec: 0.17, pan: bar % 2 ? 0.35 : -0.35 });
  }
  if ((section === "drop-a" || section === "drop-b") && bar < 64) {
    const chord = [[220, 261.63, 329.63], [220, 261.63, 349.23], [196, 261.63, 329.63], [196, 246.94, 293.66]][Math.floor((bar % 8) / 2)];
    for (const beat of bar % 2 ? [0.5, 2.25, 3.75] : [0.75, 1.75, 3.25]) for (const f of chord) musical("stab", t(bar, beat), 0.19, f, 0.34, section);
  }
  if (section === "drop-b") for (let k = 0; k < 16; k++) if (k % 8 !== 7) drum("ride", t(bar, k / 4) + (k % 2 ? SWING * BEAT : 0), k % 4 === 2 ? 0.26 : 0.16, section, { durationSec: 0.06, pan: jitter(0.7) });
}
for (const bar of [8, 16, 24, 32, 40, 48, 56, 64]) drum("crash", t(bar), 0.52, sectionFor(bar), { durationSec: 0.9, pan: jitter(0.35) });
for (const bar of [14, 22, 46, 62]) drum("riser", t(bar), 0.28, sectionFor(bar), { durationSec: 2 * BAR });
for (const bar of [40, 64, 68]) musical("subdrop", t(bar), 1.8, 52, 0.72, sectionFor(bar), { frequencyEnd: 29 });
if (COOLER) {
  for (const bar of [24, 48, 64]) drum("reverse-kick", t(bar) - 0.62, 0.62, sectionFor(bar), { durationSec: 0.62 });
  // The clock briefly loses mechanical certainty at the breakdown threshold.
  for (let i = 0; i < 7; i++) musical("clock-loose", t(40) - 0.72 + i * (0.065 + i * 0.009), 0.26, 880 * (i % 2 ? 1.5 : 1), 0.36 - i * 0.025, "drop-a", { pan: i % 2 ? 0.72 : -0.72 });
}

scheduled.sort((a, b) => a.startSec - b.startSec || a.voice.localeCompare(b.voice));
for (const e of scheduled) {
  if (!(e.startSec >= 0 && e.startSec < TOTAL_SEC + 1e-9)) throw new Error(`event out of bounds: ${JSON.stringify(e)}`);
  e.durationSec = Math.min(e.durationSec, TOTAL_SEC - e.startSec);
}

const baseSidecar = {
  schema: "aesthetic.computer/pop-events/v1",
  track: STEM,
  relationship: "standard-synthesis reinterpretation of teknull; canonical teknull files unchanged",
  deterministic: true,
  seed: SEED,
  transport: { bpm: BPM, meter: "4/4", key: "A natural minor", bars: BARS, durationSec: TOTAL_SEC, sampleRate: SR },
  resolvedParameters: { seed: SEED, bpm: BPM, swing: SWING, acidDensity: ACID_DENSITY, clockDurationUnitSec: UNIT_SEC, coolerPass: COOLER },
  provenance: {
    clockPiece: relative(ROOT, resolve(ROOT, "system/public/aesthetic.computer/disks/clock.mjs")),
    sharedClockParser: relative(ROOT, resolve(ROOT, "system/public/aesthetic.computer/lib/melody-parser.mjs")),
    offlineAdapter: relative(ROOT, resolve(HERE, "clock-offline-scheduler.mjs")),
    renderer: relative(ROOT, fileURLToPath(import.meta.url)),
    synthesis: COOLER
      ? "sample-free standard DSP with interlocking sine-wave clock hands, phase-related bell partials, sine kick/sub, filtered noise drums, acid and pads"
      : "sample-free standard DSP: sine kick/sub, filtered noise hats/claps/rides, saw/square acid/stabs/hooks, triangle/saw/square pads",
  },
  clockPrograms: programs,
  compiledPatterns,
  sections: [
    { label: "intro", bars: [0, 7] }, { label: "build", bars: [8, 15] },
    { label: "acid", bars: [16, 23] }, { label: "drop-a", bars: [24, 39] },
    { label: "breakdown", bars: [40, 47] }, { label: "drop-b", bars: [48, 63] },
    { label: "outro", bars: [64, 69] },
  ],
  summary: {
    eventCount: scheduled.length,
    eventCountsByVoice: Object.fromEntries([...new Set(scheduled.map((e) => e.voice))].sort().map((v) => [v, scheduled.filter((e) => e.voice === v).length])),
    eventCountsBySection: Object.fromEntries([...new Set(scheduled.map((e) => e.section))].map((s) => [s, scheduled.filter((e) => e.section === s).length])),
    firstEventSec: scheduled[0]?.startSec ?? null,
    lastEventSec: scheduled.at(-1)?.startSec ?? null,
  },
  events: scheduled,
  qc: null,
};
writeFileSync(SIDECAR, JSON.stringify(baseSidecar, null, 2) + "\n");
console.log(`clock schedule: ${compiledPatterns.length} patterns → ${scheduled.length} events`);
if (SCHEDULE_ONLY) process.exit(0);

const musicL = new Float32Array(NS), musicR = new Float32Array(NS);
const drumsL = new Float32Array(NS), drumsR = new Float32Array(NS);
function addPair(L, R, index, value, gain, pan = 0) {
  if (index < 0 || index >= NS) return;
  const a = (Math.max(-1, Math.min(1, pan)) + 1) * Math.PI / 4;
  L[index] += value * gain * Math.cos(a);
  R[index] += value * gain * Math.sin(a);
}
function oscSample(type, phase) {
  phase -= Math.floor(phase);
  if (type === "square") return phase < 0.5 ? 1 : -1;
  if (type === "triangle") return 1 - 4 * Math.abs(phase - 0.5);
  if (type === "sawtooth") return phase * 2 - 1;
  return Math.sin(phase * Math.PI * 2);
}
function synthTone(e) {
  const isClock = e.voice.startsWith("clock-") || e.voice === "breakdown-clock";
  const clockTail = e.voice === "clock-chime" ? 1.35 : e.voice === "clock-hour" ? 0.85 : e.voice === "clock-minute" ? 0.38 : isClock ? 0.18 : 0;
  const renderDuration = Math.min(TOTAL_SEC - e.startSec, e.durationSec + clockTail);
  const n = Math.max(1, Math.floor(renderDuration * SR));
  const i0 = Math.round(e.startSec * SR), f0 = e.frequency;
  const wave = e.voice === "pad" || e.voice === "breakdown-clock" ? e.waveType : e.voice === "hook" ? e.waveType : "sawtooth";
  const clockStep = Math.round(e.startSec / (BEAT / 4));
  const clockPan = e.voice === "clock-hour" ? (e.trackIndex - 1) * 0.42
    : e.voice === "clock-minute" ? (clockStep % 2 ? 0.52 : -0.52)
    : e.voice === "clock-chime" ? (e.trackIndex ? 0.68 : -0.68)
    : ((clockStep % 8) / 7) * 1.5 - 0.75;
  const pan = e.pan ?? (isClock ? clockPan : e.voice === "acid-echo" ? 0.46 : e.trackIndex !== undefined ? (e.trackIndex - 1) * 0.34 : jitter(0.18));
  let phase = random(), phase2 = random(), phase3 = random(), lp = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const time = j / SR, p = j / n;
    let env;
    if (isClock) {
      const attack = e.voice === "clock-hour" ? 0.018 : 0.0025;
      const decay = e.voice === "clock-chime" ? 0.72 : e.voice === "clock-hour" ? 1.2 : e.voice === "clock-minute" ? 0.34 : 0.12;
      env = Math.min(1, time / attack) * Math.exp(-time / decay);
      if (time < e.durationSec) env = Math.max(env, Math.min(1, time / attack) * 0.28);
    }
    else if (e.voice === "pad") env = Math.min(1, time / 0.22) * Math.min(1, (e.durationSec - time) / 0.32);
    else if (e.voice === "hook") env = Math.min(1, time / 0.012) * Math.exp(-1.8 * p) * Math.min(1, (e.durationSec - time) / 0.08);
    else env = Math.min(1, time / 0.006) * Math.exp(-5.2 * p) * Math.min(1, (e.durationSec - time) / 0.025);
    const vib = e.voice === "hook" ? 1 + 0.0025 * Math.sin(2 * Math.PI * 5.1 * time) : 1;
    phase += f0 * vib / SR;
    phase2 += f0 * (isClock ? 2.006 : 1.003) * vib / SR;
    phase3 += f0 * (isClock ? 3.993 : 2.001) * vib / SR;
    if (isClock) {
      // Slightly inharmonic sine partials make clock metal without samples or noise.
      const fm = 0.11 * Math.sin(2 * Math.PI * f0 * 1.414 * time) * Math.exp(-time / 0.09);
      const bell = Math.sin(2 * Math.PI * (phase + fm))
        + 0.46 * Math.sin(2 * Math.PI * phase2) * Math.exp(-time / 0.21)
        + 0.22 * Math.sin(2 * Math.PI * phase3) * Math.exp(-time / 0.13);
      const clockGain = e.voice === "clock-hour" ? 0.22 : e.voice === "clock-chime" ? 0.31 : e.voice === "clock-minute" ? 0.24 : 0.19;
      addPair(musicL, musicR, i0 + j, Math.tanh(bell * 0.82) * env, e.velocity * clockGain, pan);
      continue;
    }
    const det = oscSample(wave, phase) + 0.35 * oscSample(wave, phase * 1.003 + 0.19);
    const cutoff = e.voice.startsWith("acid") ? 420 + 3400 * Math.exp(-4 * p) * (0.65 + e.velocity) : e.voice === "hook" ? 3100 : 1450;
    const a = 1 - Math.exp(-2 * Math.PI * cutoff / SR);
    lp += a * (det * 0.7 - lp);
    const drive = Math.tanh(lp * (e.voice.startsWith("acid") ? 2.6 : 1.5));
    addPair(musicL, musicR, i0 + j, drive * env, e.velocity * (e.voice === "pad" ? 0.28 : e.voice === "acid-echo" ? 0.18 : 0.34), pan);
  }
}
function subTone(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let ph = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const p = j / n, f = e.frequency + ((e.frequencyEnd || e.frequency) - e.frequency) * p;
    ph += 2 * Math.PI * f / SR;
    const env = Math.min(1, j / (0.008 * SR)) * Math.sin(Math.PI * Math.min(1, p)) ** 0.45;
    addPair(musicL, musicR, i0 + j, Math.sin(ph) * env, e.velocity * 0.48, 0);
  }
}
function stab(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let ph = random(), lp = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    ph += e.frequency / SR;
    const saw = oscSample("sawtooth", ph) + 0.45 * oscSample("square", ph * 0.997 + 0.3);
    const a = 1 - Math.exp(-2 * Math.PI * (350 + 3200 * Math.exp(-j / (0.045 * SR))) / SR);
    lp += a * (saw * 0.55 - lp);
    const env = Math.min(1, j / (0.004 * SR)) * Math.exp(-j / (0.09 * SR));
    addPair(musicL, musicR, i0 + j, Math.tanh(lp * 1.8) * env, e.velocity * 0.42, jitter(0.4));
  }
}
function kick(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let ph = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const time = j / SR, f = 43 + 105 * Math.exp(-time / 0.025);
    ph += 2 * Math.PI * f / SR;
    const body = Math.sin(ph) * Math.exp(-time / 0.18);
    const click = (random() * 2 - 1) * Math.exp(-time / 0.006);
    addPair(drumsL, drumsR, i0 + j, Math.tanh(body * 1.8 + click * 0.18), e.velocity * 0.78, 0);
  }
}
function reverseKick(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let ph = 0, lp = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const p = j / n;
    const f = 34 + 92 * p * p;
    ph += 2 * Math.PI * f / SR;
    const white = random() * 2 - 1;
    lp += 0.025 * (white - lp);
    const swell = p * p * (1 - 0.18 * p);
    const value = Math.sin(ph) * swell + (white - lp) * 0.08 * p ** 3;
    addPair(drumsL, drumsR, i0 + j, Math.tanh(value * 1.5), e.velocity * 0.64, 0);
  }
}
function noiseDrum(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let lp = 0, last = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const time = j / SR, white = random() * 2 - 1;
    lp += 0.08 * (white - lp);
    const hp = white - lp;
    let value = hp, env = Math.exp(-time / 0.035);
    if (e.voice === "clap") {
      const burst = Math.exp(-Math.max(0, time - 0.000) / 0.025) + 0.8 * Math.exp(-Math.max(0, time - 0.027) / 0.023) * (time >= 0.027) + 0.6 * Math.exp(-Math.max(0, time - 0.051) / 0.04) * (time >= 0.051);
      value = white - 0.6 * last; env = burst;
    } else if (e.voice === "open-hat" || e.voice === "crash" || e.voice === "ride") env = Math.exp(-time / (e.voice === "crash" ? 0.33 : e.voice === "open-hat" ? 0.09 : 0.045));
    last = white;
    addPair(drumsL, drumsR, i0 + j, value * env, e.velocity * (e.voice === "clap" ? 0.42 : 0.27), e.pan || 0);
  }
}
function riser(e) {
  const n = Math.floor(e.durationSec * SR), i0 = Math.round(e.startSec * SR);
  let lp = 0;
  for (let j = 0; j < n && i0 + j < NS; j++) {
    const p = j / n, white = random() * 2 - 1, a = 0.008 + p * 0.42;
    lp += a * (white - lp);
    addPair(musicL, musicR, i0 + j, (white - lp * 0.5) * p * p, e.velocity * 0.32, p * 1.4 - 0.7);
  }
}

for (const e of scheduled) {
  if (e.voice === "kick") kick(e);
  else if (e.voice === "reverse-kick") reverseKick(e);
  else if (["closed-hat", "open-hat", "clap", "ride", "crash"].includes(e.voice)) noiseDrum(e);
  else if (e.voice === "riser") riser(e);
  else if (["sub", "subdrop"].includes(e.voice)) subTone(e);
  else if (e.voice === "stab") stab(e);
  else if (e.type === "note" && e.frequency) synthTone(e);
}

// Short cross-fed room: enough location to bind the synths without washing out
// the 140 BPM transients. Delays are prime-ish and stay beneath the dry signal.
for (const [delaySec, gain] of [[0.037, 0.10], [0.061, 0.075], [0.089, 0.05]]) {
  const d = Math.round(delaySec * SR);
  for (let i = d; i < NS; i++) {
    const wetL = musicR[i - d] * gain, wetR = musicL[i - d] * gain;
    musicL[i] += wetL; musicR[i] += wetR;
  }
}

// Conventional kick sidechain and fixed two-minute fade.
const duck = new Float32Array(NS).fill(1);
for (const e of scheduled.filter((x) => x.voice === "kick")) {
  const i0 = Math.round(e.startSec * SR), n = Math.round(0.22 * SR);
  for (let j = 0; j < n && i0 + j < NS; j++) duck[i0 + j] = Math.min(duck[i0 + j], 0.38 + 0.62 * (j / n) ** 1.8);
}
const interleaved = new Float32Array(NS * 2);
let preMasterPeak = 0;
for (let i = 0; i < NS; i++) {
  const fadeIn = Math.min(1, i / (0.008 * SR));
  const fadeOut = Math.min(1, (NS - 1 - i) / (1.15 * SR));
  const g = Math.max(0, Math.min(fadeIn, fadeOut));
  const l = Math.tanh((musicL[i] * duck[i] + drumsL[i]) * 0.78) * g;
  const r = Math.tanh((musicR[i] * duck[i] + drumsR[i]) * 0.78) * g;
  interleaved[i * 2] = l; interleaved[i * 2 + 1] = r;
  preMasterPeak = Math.max(preMasterPeak, Math.abs(l), Math.abs(r));
}
writeFileSync(RAW, Buffer.from(interleaved.buffer));

function run(command, args, capture = false) {
  const r = spawnSync(command, args, { stdio: capture ? ["ignore", "pipe", "pipe"] : "inherit", encoding: capture ? "utf8" : undefined, maxBuffer: 64 << 20 });
  if (r.status !== 0) throw new Error(`${command} failed (${r.status})\n${r.stderr || ""}`);
  return r;
}
const af = "highpass=f=25,lowpass=f=18500,loudnorm=I=-14:TP=-1.2:LRA=10";
run("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", RAW, "-af", af, "-ar", String(SR), "-c:a", "pcm_s24le", WAV]);
run("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", WAV, "-c:a", "libmp3lame", "-b:a", "320k", MP3]);
unlinkSync(RAW);

function probe(path) {
  const r = run("ffprobe", ["-v", "error", "-show_entries", "format=duration,size:stream=sample_rate,channels,bits_per_raw_sample,codec_name", "-of", "json", path], true);
  return JSON.parse(r.stdout);
}
function loudness(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", path, "-filter_complex", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8", maxBuffer: 16 << 20 });
  const text = r.stderr || "";
  const summary = text.slice(text.lastIndexOf("Summary:"));
  const I = /I:\s*(-?[\d.]+) LUFS/.exec(summary)?.[1];
  const LRA = /LRA:\s*(-?[\d.]+) LU/.exec(summary)?.[1];
  const peak = /Peak:\s*(-?[\d.]+) dBFS/.exec(summary)?.[1];
  return { integratedLufs: I === undefined ? null : Number(I), loudnessRangeLu: LRA === undefined ? null : Number(LRA), truePeakDbfs: peak === undefined ? null : Number(peak) };
}
const wavProbe = probe(WAV), mp3Probe = probe(MP3);
const qc = {
  preMasterPeak,
  wav: { ...wavProbe, ...loudness(WAV), sha256: createHash("sha256").update(readFileSync(WAV)).digest("hex") },
  mp3: { ...mp3Probe, ...loudness(MP3), sha256: createHash("sha256").update(readFileSync(MP3)).digest("hex") },
};
const wavDuration = Number(qc.wav.format.duration);
if (Math.abs(wavDuration - TOTAL_SEC) > 0.002) throw new Error(`WAV duration ${wavDuration} != ${TOTAL_SEC}`);
if (qc.wav.truePeakDbfs > -0.8) throw new Error(`true peak too high: ${qc.wav.truePeakDbfs} dBFS`);
baseSidecar.qc = qc;
writeFileSync(SIDECAR, JSON.stringify(baseSidecar, null, 2) + "\n");
console.log(`WAV ${wavDuration.toFixed(3)}s · ${qc.wav.integratedLufs} LUFS · ${qc.wav.truePeakDbfs} dBFS TP`);
console.log(`MP3 ${Number(qc.mp3.format.duration).toFixed(3)}s · ${qc.mp3.integratedLufs} LUFS · ${qc.mp3.truePeakDbfs} dBFS TP`);
console.log(WAV); console.log(MP3); console.log(SIDECAR);
