#!/usr/bin/env node
// render-sineabye.mjs — nullabye reimagined as a two-minute sine lullaby.
//
// Pitched layers are sine oscillators. A very quiet filtered-noise hat and
// percussion bed adds continuity without becoming the source of a pitched voice.
//
// 38 bars × 4 beats × 60 / 76 BPM = exactly 120 seconds.
//
// Run:
//   node pop/nullabye/bin/render-sineabye.mjs
//   node pop/nullabye/bin/render-sineabye.mjs --out ~/sineabye.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const BPM = 76;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 38;
const TOTAL_SEC = TOTAL_BARS * BAR; // 120.0 exactly
const NS = Math.ceil(TOTAL_SEC * SR);
const TAU = Math.PI * 2;

const arg = (name) => {
  const i = process.argv.indexOf(name);
  return i >= 0 ? process.argv[i + 1] : null;
};
const expandHome = (p) => !p ? p
  : p === "~" ? homedir()
  : p.startsWith("~/") ? resolve(homedir(), p.slice(2))
  : p;
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

const HZ = {
  A1: 55.00, C2: 65.41, F2: 87.31, G2: 98.00, A2: 110.00,
  C3: 130.81, D3: 146.83, E3: 164.81, F3: 174.61, G3: 196.00, A3: 220.00, B3: 246.94,
  C4: 261.63, D4: 293.66, E4: 329.63, F4: 349.23, G4: 392.00, A4: 440.00, B4: 493.88,
  C5: 523.25, D5: 587.33, E5: 659.26, F5: 698.46, G5: 783.99, A5: 880.00,
  C6: 1046.50, D6: 1174.66, E6: 1318.51, G6: 1567.98,
};

const out = [new Float32Array(NS), new Float32Array(NS)];
let voiceCount = 0;
let noiseSeed = 0x51aeab1e;
const random = () => ((noiseSeed = (noiseSeed * 1664525 + 1013904223) >>> 0) / 4294967296) * 2 - 1;

// Equal-power stereo placement and sin² attack/release keep a bare sine
// graceful at both edges. Optional drift is slow phase modulation, not EQ.
function sine(t0, duration, freq, gain, {
  attack = 0.08, release = 0.35, pan = 0, phase = 0,
  freqEnd = freq, drift = 0, driftRate = 0.08,
} = {}) {
  const start = Math.max(0, Math.floor(t0 * SR));
  const end = Math.min(NS, Math.ceil((t0 + duration) * SR));
  const panAngle = (pan + 1) * Math.PI / 4;
  const channelGain = [Math.cos(panAngle), Math.sin(panAngle)];
  let p = phase;
  voiceCount++;

  for (let i = start; i < end; i++) {
    const elapsed = i / SR - t0;
    const remain = duration - elapsed;
    const a = attack <= 0 ? 1 : Math.min(1, elapsed / attack);
    const r = release <= 0 ? 1 : Math.min(1, remain / release);
    const env = Math.sin(a * Math.PI / 2) ** 2 * Math.sin(r * Math.PI / 2) ** 2;
    const progress = elapsed / duration;
    const f = freq * Math.pow(freqEnd / freq, progress)
      * (1 + drift * Math.sin(TAU * driftRate * elapsed + phase));
    p += TAU * f / SR;
    const sample = Math.sin(p) * gain * env;
    out[0][i] += sample * channelGain[0];
    out[1][i] += sample * channelGain[1];
  }
}

function noiseHit(t0, duration, gain, { low = 2500, high = 9000, pan = 0 } = {}) {
  const start = Math.max(0, Math.floor(t0 * SR));
  const end = Math.min(NS, Math.ceil((t0 + duration) * SR));
  const hpA = Math.exp(-TAU * low / SR), lpA = Math.exp(-TAU * high / SR);
  const angle = (pan + 1) * Math.PI / 4;
  const lr = [Math.cos(angle), Math.sin(angle)];
  let lowState = 0, highState = 0;
  for (let i = start; i < end; i++) {
    const elapsed = i / SR - t0;
    const env = Math.sin(Math.min(1, elapsed / .008) * Math.PI / 2) ** 2
      * Math.exp(-elapsed * 5.2 / duration);
    const white = random();
    lowState = (1 - hpA) * white + hpA * lowState;
    const hp = white - lowState;
    highState = (1 - lpA) * hp + lpA * highState;
    const sample = highState * gain * env;
    out[0][i] += sample * lr[0]; out[1][i] += sample * lr[1];
  }
}

// Modal gong: a struck circular plate approximated by slowly beating,
// inharmonic sine modes. Sparse placement lets each large bloom finish speaking.
function gong(t0, fundamental, gain = .045, pan = 0) {
  [[1, 1, 8.5], [1.41, .52, 7.2], [1.98, .34, 6.1], [2.91, .2, 4.8], [4.07, .1, 3.5]]
    .forEach(([ratio, level, duration], i) => sine(t0, duration, fundamental * ratio, gain * level, {
      attack: .018 + i * .006, release: duration * .92, pan: pan + (i - 2) * .06,
      phase: i * 1.13, drift: .0012 / (i + 1), driftRate: .07 + i * .013,
    }));
}

const CHORDS = [
  { tones: [HZ.C3, HZ.G3, HZ.E4, HZ.D5], root: HZ.C2 },
  { tones: [HZ.A2, HZ.E3, HZ.C4, HZ.B4], root: HZ.A1 },
  { tones: [HZ.F3, HZ.C4, HZ.A4, HZ.G4], root: HZ.F2 },
  { tones: [HZ.G3, HZ.D4, HZ.B4, HZ.A4], root: HZ.G2 },
];

const THEME_A = [
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, .5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.E5, 1, 1], [HZ.D5, 2, 2]],
  [[HZ.C5, 0, 1.5], [HZ.D5, 1.5, .5], [HZ.E5, 2, 1], [HZ.G5, 3, 1]],
  [[HZ.D5, 0, 3]],
];
const THEME_B = [
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, .5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.A5, 1, 1], [HZ.C6, 2, 2]],
  [[HZ.A5, 0, 1.5], [HZ.G5, 1.5, .5], [HZ.E5, 2, 1], [HZ.C5, 3, 1]],
  [[HZ.D5, 0, 2], [HZ.E5, 2, 2]],
];
const BRIDGE = [
  [[HZ.A5, 0, 2], [HZ.C6, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.E5, 1, 1], [HZ.D5, 2, 2]],
  [[HZ.F5, 0, 2], [HZ.A5, 2, 1], [HZ.G5, 3, 1]],
  [[HZ.E5, 0, 4]],
];

function padBar(bar, amount = 1, transpose = 1) {
  const chord = CHORDS[bar % 4];
  chord.tones.forEach((f, i) => sine(t(bar), BAR + .12, f * transpose, .045 * amount, {
    attack: .85, release: 1.15, pan: [-.7, .55, -.25, .75][i],
    phase: i * 1.7, drift: .0007, driftRate: .035 + i * .008,
  }));
}

function melody(bar0, phrase, amount = 1, transpose = 1, echo = false) {
  phrase.forEach((notes, b) => notes.forEach(([f, beat, beats], i) => {
    const start = t(bar0 + b, beat);
    const duration = beats * BEAT + .28;
    sine(start, duration, f * transpose, .1 * amount, {
      attack: .045, release: .3, pan: -.12, phase: i * .8,
      drift: .00035, driftRate: .11,
    });
    if (echo) sine(start + .5 * BEAT, duration * .72, f * transpose * 2, .018 * amount, {
      attack: .09, release: .45, pan: .68, phase: 1.2,
    });
  }));
}

function foundation(bar, amount = 1, transpose = 1) {
  const root = CHORDS[bar % 4].root;
  sine(t(bar), BAR + .08, root * transpose, .075 * amount, {
    attack: .18, release: .7, pan: -.08, drift: .00025, driftRate: .04,
  });
  // Heartbeat: still a sine, with a short downward pitch curve.
  for (const beat of [0, 2]) sine(t(bar, beat), .32, 61, .11 * amount, {
    attack: .006, release: .25, pan: .05, freqEnd: 43,
  });
}

function mobile(bar, amount = 1, transpose = 1) {
  const chord = CHORDS[bar % 4];
  for (let beat = .5; beat < 4; beat += 1) {
    const f = chord.tones[(Math.floor(beat) + bar) % chord.tones.length] * 4;
    sine(t(bar, beat), .22, f * transpose, .021 * amount, {
      attack: .012, release: .18, pan: beat % 2 ? -.82 : .82,
    });
  }
}

function noiseBed(bar, amount = 1) {
  for (const beat of [.5, 1.5, 2.5, 3.5])
    noiseHit(t(bar, beat), .075, .095 * amount, { low: 4200, high: 10500, pan: beat % 2 ? .62 : -.62 });
  for (const beat of [1, 3])
    noiseHit(t(bar, beat), .14, .068 * amount, { low: 620, high: 2100, pan: beat === 1 ? -.28 : .28 });
  noiseHit(t(bar), BAR, .018 * amount, { low: 1800, high: 6200 });
}

// The filtered-noise ensemble is a continuous member of the arrangement, not
// a section effect. It grows into the body and bows slightly under the ending.
for (let bar = 0; bar < TOTAL_BARS; bar++) {
  const amount = bar < 4 ? .58 + bar * .07 : bar < 32 ? .86 : .8 - (bar - 32) * .055;
  noiseBed(bar, amount);
}

// 0–4 · immediate first verse. An earlier cut waited through four isolated
// tones here; starting the tune at bar zero keeps the opening from feeling as
// though playback has stopped before the song arrives.
for (let bar = 0; bar < 4; bar++) padBar(bar, .62);
melody(0, THEME_A, .72);

// 4–12 · the original melody, close and sparse.
for (let bar = 4; bar < 12; bar++) padBar(bar, .8);
melody(4, THEME_A, .9);
melody(8, THEME_B, .95);
for (let bar = 8; bar < 12; bar++) foundation(bar, .55);

// 12–20 · fuller statement: bass, heartbeat, and a high sine mobile arrive.
for (let bar = 12; bar < 20; bar++) {
  padBar(bar, 1);
  foundation(bar, .72);
  mobile(bar, .7);
}
melody(12, THEME_A, 1, true);
melody(16, THEME_B, 1.05, true);

gong(t(19, 3.5), HZ.C2, .038, -.2);

// 20–28 · bridge: the whole arrangement lifts together to consonant F major.
for (let bar = 20; bar < 28; bar++) {
  padBar(bar, .92, 4 / 3);
  foundation(bar, .58, 4 / 3);
  mobile(bar, .46, 4 / 3);
  [HZ.F4, HZ.A4, HZ.C5, HZ.G5].forEach((f, i) => sine(t(bar), BAR + .08, f, .018, {
    attack: .7, release: .82, pan: [-.68, .42, -.25, .7][i], phase: i * 1.3,
  }));
}
melody(20, BRIDGE, .9, 4 / 3, true);
melody(24, THEME_B, .82, 4 / 3, true);
gong(t(27, 3.5), HZ.F2, .042, .22);

// 28–34 · homecoming: the theme returns, then gradually loses its mobile.
for (let bar = 28; bar < 34; bar++) {
  const fade = 1 - (bar - 28) * .07;
  padBar(bar, fade);
  foundation(bar, .65 * fade);
  if (bar < 32) mobile(bar, .45 * fade);
}
melody(28, THEME_A, 1.02, 1, true);
melody(32, THEME_B.slice(0, 2), .85);

// 34–38 · the melodic line gets the last word: E–D–C, then a small C–D–E–C
// answer. No low drone after it, so the composition ends where the tune ends.
for (let bar = 34; bar < 38; bar++) padBar(bar, .42 - (bar - 34) * .07);
sine(t(34), 2 * BEAT, HZ.E5, .075, { attack: .08, release: .55, pan: -.2 });
sine(t(35), 2 * BEAT, HZ.D5, .068, { attack: .08, release: .6, pan: .2 });
[[HZ.C5, 0, 1], [HZ.D5, 1, 1], [HZ.E5, 2, 1], [HZ.C5, 3, 1.8]].forEach(([f, beat, beats], i) =>
  sine(t(36, beat), beats * BEAT, f, .072 - i * .006, { attack: .07, release: i === 3 ? 1.25 : .38, pan: (i - 1.5) * .15 }));
gong(t(33, 3.5), HZ.C2, .026, 0);

// A restrained cross-channel room: three early reflections add space without
// smearing the exposed melody or turning the noise percussion into a wash.
for (const [delaySec, amount] of [[.071, .07], [.113, .045], [.181, .028]]) {
  const delay = Math.floor(delaySec * SR);
  for (let i = delay; i < NS; i++) {
    const l = out[0][i - delay], r = out[1][i - delay];
    out[0][i] += r * amount;
    out[1][i] += l * amount;
  }
}

// Peak-normalize, then leave a short true-silence landing at exactly 2:00.
let peak = 0;
for (const channel of out) for (const sample of channel) peak = Math.max(peak, Math.abs(sample));
const gain = peak > 0 ? .86 / peak : 1;
const fadeIn = Math.floor(.02 * SR);
const fadeOut = Math.floor(.8 * SR);
for (const channel of out) {
  for (let i = 0; i < NS; i++) channel[i] *= gain;
  for (let i = 0; i < fadeIn; i++) channel[i] *= i / fadeIn;
  for (let i = 0; i < fadeOut; i++) channel[NS - 1 - i] *= i / fadeOut;
}

const outPath = expandHome(arg("--out")) || resolve(HERE, "..", "out", "sineabye.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const buffer = Buffer.alloc(NS * 2 * 4);
for (let i = 0; i < NS; i++) {
  buffer.writeFloatLE(out[0][i], i * 8);
  buffer.writeFloatLE(out[1][i], i * 8 + 4);
}
writeFileSync(rawPath, buffer);

const ffmpeg = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", [
    "highpass=f=28",
    "equalizer=f=72:t=q:w=.8:g=1.2",
    "equalizer=f=310:t=q:w=1.1:g=-.8",
    "equalizer=f=7200:t=q:w=.9:g=-1.1",
    "lowpass=f=15800",
    "acompressor=threshold=-23dB:ratio=1.8:attack=28:release=210:makeup=1.8:knee=7",
    "loudnorm=I=-13:TP=-1.2:LRA=10",
    "alimiter=limit=.95:attack=6:release=100",
  ].join(","),
  "-c:a", "libmp3lame", "-q:a", "2", outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ffmpeg.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}

const struct = {
  _comment: "Section map for sineabye: sine voices with filtered-noise percussion and modal gongs.",
  meter: 4, bpm: BPM, scale: "major", rootMidi: 60, totalSec: TOTAL_SEC,
  sections: [
    { name: "opening-verse", startSec: 0, endSec: t(4) },
    { name: "theme", startSec: t(4), endSec: t(12) },
    { name: "full-statement", startSec: t(12), endSec: t(20) },
    { name: "lifted-bridge", startSec: t(20), endSec: t(28) },
    { name: "homecoming", startSec: t(28), endSec: t(34) },
    { name: "dissolve", startSec: t(34), endSec: TOTAL_SEC },
  ],
};
writeFileSync(resolve(HERE, "..", "out", "sineabye.struct.json"), JSON.stringify(struct, null, 2) + "\n");
console.log(`✓ ${outPath} · pop-mastered sine voices + filtered percussion · ${voiceCount} voices · ${TOTAL_SEC.toFixed(1)} s`);
