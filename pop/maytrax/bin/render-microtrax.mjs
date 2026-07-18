#!/usr/bin/env node
// microtrax — pianotrax's ragtime bones rebuilt as a two-minute microtekno toy.
// Every sound is a sine: short additive bells, bass pips, and pitch-falling
// sine-boom kicks. No piano samples, noise, filters, or distortion.
//
// 72 bars × 4 beats × 60 / 144 BPM = exactly 120 seconds.
//
//   node pop/maytrax/bin/render-microtrax.mjs
//   node pop/maytrax/bin/render-microtrax.mjs --out ~/microtrax.mp3

import { mkdirSync, unlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const FEM = process.argv.includes("--fem");
const TITLE = FEM ? "femrag" : "microtrax";
const SR = 48_000;
const BPM = 144;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const BARS = 72;
const SECONDS = BARS * BAR;
const NS = Math.ceil(SECONDS * SR);
const TAU = Math.PI * 2;
const SWING = .62;
const out = [new Float32Array(NS), new Float32Array(NS)];
let voices = 0;

const argv = (flag) => {
  const i = process.argv.indexOf(flag);
  return i < 0 ? null : process.argv[i + 1];
};
const expandHome = (p) => !p ? p : p === "~" ? homedir()
  : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;
const hz = (midi) => 440 * 2 ** ((midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;
const swung = (beat) => {
  const eighth = beat * 2;
  const whole = Math.floor(eighth + 1e-9);
  const fraction = eighth - whole;
  return Math.floor(whole / 2) * BEAT + (whole % 2 ? SWING * BEAT : 0)
    + fraction * BEAT / 2;
};

function oscillator(t0, duration, freq, gain, {
  attack = .003, release = .08, pan = 0, phase = 0, freqEnd = freq,
} = {}) {
  const begin = Math.max(0, Math.floor(t0 * SR));
  const end = Math.min(NS, Math.ceil((t0 + duration) * SR));
  const angle = (pan + 1) * Math.PI / 4;
  const lr = [Math.cos(angle), Math.sin(angle)];
  let p = phase;
  voices++;
  for (let i = begin; i < end; i++) {
    const elapsed = i / SR - t0;
    const remaining = duration - elapsed;
    const a = Math.min(1, elapsed / attack);
    const r = Math.min(1, remaining / release);
    const env = Math.sin(Math.max(0, a) * Math.PI / 2) ** 2
      * Math.sin(Math.max(0, r) * Math.PI / 2) ** 2;
    const f = freq * (freqEnd / freq) ** (elapsed / duration);
    p += TAU * f / SR;
    const sample = Math.sin(p) * gain * env;
    out[0][i] += sample * lr[0];
    out[1][i] += sample * lr[1];
  }
}

// A bell made exclusively from short sine partials. The mild inharmonicity is
// what separates it from a piano-like harmonic stack.
function bell(t0, midi, gain = .06, pan = 0, length = .22) {
  const f = hz(midi);
  // femrag combines bowl-like body modes with the first FEM-derived handbell
  // shell modes used by pop/bell. Short decay turns the shell into a playable
  // marimba bar rather than a sustained church bell.
  const partials = FEM ? [
    [.461, .22, length * 1.35], [.688, .25, length * 1.18], [1, 1, length],
    [3.479, .17, length * .54], [3.559, .14, length * .48], [4.81, .08, length * .34],
  ] : [
    [1, 1, length], [2.01, .28, length * .62], [3.97, .12, length * .38],
  ];
  partials.forEach(([ratio, level, duration], i) => oscillator(t0, duration, f * ratio, gain * level, {
    attack: .0015, release: duration * .88, pan: pan + (i - 1) * .04, phase: i * 1.1,
  }));
}

function boom(t0, gain = .18) {
  oscillator(t0, .34, 148, gain, {
    attack: .0015, release: .31, freqEnd: 43, pan: 0,
  });
  oscillator(t0, .19, 58, gain * .45, {
    attack: .002, release: .17, freqEnd: 45, pan: 0, phase: .7,
  });
}

const CHORDS = [
  { bass: 45, notes: [57, 61, 64, 69] }, // A
  { bass: 38, notes: [57, 62, 66, 69] }, // D
  { bass: 42, notes: [57, 61, 66, 69] }, // F#m
  { bass: 40, notes: [56, 59, 64, 68] }, // E
];
const RAG = [0, 2, 1, 3, 0, 2, 1, 3];
const LEAD = [
  [[69, 0], [73, 1.5], [76, 2.5], [73, 3.25]],
  [[78, 0], [76, .75], [73, 1.5], [71, 2.5], [73, 3.25]],
  [[69, 0], [71, .75], [73, 1.5], [76, 2.25], [78, 3]],
  [[76, 0], [73, 1], [71, 2], [68, 2.75], [69, 3.5]],
];

function kickBar(bar, weight = 1, pickup = false) {
  for (let beat = 0; beat < 4; beat++) boom(at(bar, beat), .16 * weight * (beat === 0 ? 1.08 : 1));
  if (pickup) boom(at(bar, 3.75), .075 * weight);
}

function ragBar(bar, density = 1, octave = 0, { pattern = true, oom = true } = {}) {
  const chord = CHORDS[bar % 4];
  if (pattern) for (let e = 0; e < 8; e++) {
    if (density < .7 && e % 2) continue;
    const tt = at(bar) + swung(e * .5);
    const midi = chord.notes[RAG[e]] + octave;
    bell(tt, midi, (e % 2 ? .035 : .052) * density, e % 2 ? .46 : -.38, .16);
  }
  // Ragtime oom-pah reduced to tiny sine pips: bass on beats, chord answers.
  if (oom) {
    for (const beat of [0, 2]) bell(at(bar, beat), chord.bass, .072 * density, -.55, .2);
    for (const beat of [1, 3]) chord.notes.slice(1, 4).forEach((m, i) =>
      bell(at(bar, beat) + i * .008, m + octave, .024 * density, .18 + i * .18, .13));
  }
}

function leadBar(bar, amount = 1, octave = 0) {
  LEAD[bar % 4].forEach(([midi, beat], i) => {
    const tt = at(bar) + swung(beat);
    bell(tt, midi + octave, .075 * amount, .18 + (i % 2) * .35, .24);
    // A very short upper answer makes the melody read as microtekno, not keys.
    if (i % 2 === 0) bell(tt + BEAT * .25, midi + 12 + octave, .025 * amount, -.58, .1);
  });
}

function ticks(bar, amount = 1) {
  for (let s = 0; s < 16; s++) {
    if (s % 4 === 0) continue;
    const note = [93, 97, 100, 97][(s + bar) % 4];
    bell(at(bar, s / 4), note, .010 * amount * (s % 2 ? 1 : .65), s % 2 ? .82 : -.82, .055);
  }
}

// 0–8: use the old ~1:22 micro-break as the opening vocabulary. The kick is
// continuous but restrained; the thin upper rag and late ticks make a clearer
// runway into the tune than the earlier miniature full-arrangement intro.
for (let bar = 0; bar < 8; bar++) {
  const rise = .72 + bar * .025;
  kickBar(bar, rise);
  if (bar >= 1) ragBar(bar, .34 + bar * .025, 12, { pattern: bar >= 2, oom: true });
  if (bar >= 3) leadBar(bar, .24 + (bar - 3) * .035, bar < 6 ? -12 : 0);
  if (bar >= 4) ticks(bar, .18 + (bar - 4) * .07);
}

// 8–24: first full ragtime statement over an uninterrupted four-floor boom.
for (let bar = 8; bar < 24; bar++) {
  kickBar(bar, .92, bar % 4 === 3);
  ragBar(bar, .82);
  leadBar(bar, .78);
  if (bar >= 12) ticks(bar, .45);
}

// 24–32: micro-break — not a kick break. Notes contract to little syncopations.
for (let bar = 24; bar < 32; bar++) {
  kickBar(bar, .78);
  ragBar(bar, .5, bar >= 28 ? 12 : 0);
  if (bar % 2 === 0) leadBar(bar, .42, -12);
  ticks(bar, .28);
}

// 32–52: central machine-rag bloom, with 16th ticks and octave answers.
for (let bar = 32; bar < 52; bar++) {
  kickBar(bar, 1, bar % 2 === 1);
  ragBar(bar, 1);
  leadBar(bar, 1, bar >= 44 ? 12 : 0);
  ticks(bar, .8);
  if (bar % 4 === 3) {
    const chord = CHORDS[bar % 4];
    for (let s = 0; s < 8; s++) bell(at(bar, 2) + swung(s * .25), chord.notes[s % 4] + 12, .025, s % 2 ? .7 : -.7, .09);
  }
}

// 52–60: second micro-break. The continuous kick now feels like the melody.
for (let bar = 52; bar < 60; bar++) {
  kickBar(bar, .84);
  ragBar(bar, .42, 12);
  if (bar >= 56) ticks(bar, .38);
}

// 60–68: compact final strain, brighter and busier but still all tiny notes.
for (let bar = 60; bar < 68; bar++) {
  const fade = 1 - Math.max(0, bar - 64) * .08;
  kickBar(bar, .98 * fade, bar % 2 === 1);
  ragBar(bar, .94 * fade, bar % 4 === 2 ? 12 : 0);
  leadBar(bar, .92 * fade, 12);
  ticks(bar, .72 * fade);
}

// 68–72: strip the clock down without ever dropping the main kick.
for (let bar = 68; bar < 72; bar++) {
  const fade = 1 - (bar - 68) * .2;
  kickBar(bar, .82 * fade);
  ragBar(bar, .5 * fade);
  if (bar === 68) leadBar(bar, .55);
}
bell(at(71, 3), 45, .085, 0, .42);

let peak = 0;
for (const channel of out) for (const sample of channel) peak = Math.max(peak, Math.abs(sample));
const gain = peak ? .88 / peak : 1;
const fadeIn = Math.floor(.006 * SR);
const fadeOut = Math.floor(.55 * SR);
for (const channel of out) {
  for (let i = 0; i < NS; i++) channel[i] *= gain;
  for (let i = 0; i < fadeIn; i++) channel[i] *= i / fadeIn;
  for (let i = 0; i < fadeOut; i++) channel[NS - 1 - i] *= i / fadeOut;
}

const outPath = expandHome(argv("--out")) || resolve(HERE, "..", "out", `${TITLE}.mp3`);
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const buffer = Buffer.alloc(NS * 8);
for (let i = 0; i < NS; i++) {
  buffer.writeFloatLE(out[0][i], i * 8);
  buffer.writeFloatLE(out[1][i], i * 8 + 4);
}
writeFileSync(rawPath, buffer);
const ffmpeg = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", "acompressor=threshold=-18dB:ratio=1.8:attack=18:release=100:makeup=1.5:knee=6,alimiter=limit=.96:attack=4:release=70",
  "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", `title=${TITLE}`, "-metadata", "artist=jeffrey", "-metadata", "album=pixsies",
  outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ffmpeg.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}
console.log(`✓ ${outPath} · ${BPM} BPM · ${BARS} bars · ${SECONDS.toFixed(1)} s · ${voices} ${FEM ? "FEM-marimba" : "sine"} voices`);
