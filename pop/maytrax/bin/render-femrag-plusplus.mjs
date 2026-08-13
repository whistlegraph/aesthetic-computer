#!/usr/bin/env node
// femrag++ — femrag's FEM-marimba rag grown into a drum-and-bass engine.
// Act one keeps femrag's sine-only vocabulary; then a snare-roll buildup
// tears the floor out and the track drops into straight two-step drum and
// bass: noise snares and hats, a tanh-driven sub bassline, and the rag
// bells riding double-time on top.
//
// 80 bars × 4 beats × 60 / 144 BPM = 133.33 seconds ≈ 2:13.
//
//   node pop/maytrax/bin/render-femrag-plusplus.mjs
//   node pop/maytrax/bin/render-femrag-plusplus.mjs --out ~/femrag-plusplus.mp3

import { mkdirSync, unlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const TITLE = "femrag++";
const SLUG = "femrag-plusplus";
const SR = 48_000;
const BPM = 144;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const BARS = 80;
const SECONDS = BARS * BAR;
const NS = Math.ceil(SECONDS * SR);
const TAU = Math.PI * 2;
const SWING = .62;
const out = [new Float32Array(NS), new Float32Array(NS)];
let voices = 0;
// Every musical hit also lands here, so the video renderer can draw the
// exact same performance the ear hears.
const EVENTS = [];

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

// Deterministic noise so every render is the same take.
let seed = 0x9e3779b9;
const rand = () => {
  seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5;
  seed >>>= 0;
  return seed / 0xffffffff * 2 - 1;
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

// Band-passed noise burst through a state-variable filter — the ++ over
// femrag's sine-only rule. Center frequency can sweep for risers.
function noiseBurst(t0, duration, gain, {
  attack = .001, release = .08, pan = 0, center = 1800, centerEnd = center,
  q = 1.2,
} = {}) {
  const begin = Math.max(0, Math.floor(t0 * SR));
  const end = Math.min(NS, Math.ceil((t0 + duration) * SR));
  const angle = (pan + 1) * Math.PI / 4;
  const lr = [Math.cos(angle), Math.sin(angle)];
  let low = 0, band = 0;
  voices++;
  for (let i = begin; i < end; i++) {
    const elapsed = i / SR - t0;
    const remaining = duration - elapsed;
    const a = Math.min(1, elapsed / attack);
    const r = Math.min(1, remaining / release);
    const env = Math.sin(Math.max(0, a) * Math.PI / 2) ** 2
      * Math.sin(Math.max(0, r) * Math.PI / 2) ** 2;
    const fc = center * (centerEnd / center) ** (elapsed / duration);
    const f = 2 * Math.sin(Math.PI * Math.min(fc, SR * .22) / SR);
    const high = rand() - low - band / q;
    band += f * high;
    low += f * band;
    const sample = band * gain * env;
    out[0][i] += sample * lr[0];
    out[1][i] += sample * lr[1];
  }
}

// Sub bass: a sine pushed through tanh so it grows teeth. Optional amplitude
// wobble reads as the classic dnb LFO bass.
function sub(t0, duration, midi, gain, {
  drive = 2.6, wobble = 0, slideTo = midi, attack = .004, release = .07,
} = {}) {
  EVENTS.push({ i: "sub", t: t0, dur: duration, midi, gain, wobble });
  const begin = Math.max(0, Math.floor(t0 * SR));
  const end = Math.min(NS, Math.ceil((t0 + duration) * SR));
  const f0 = hz(midi), f1 = hz(slideTo);
  let p = 0;
  voices++;
  for (let i = begin; i < end; i++) {
    const elapsed = i / SR - t0;
    const remaining = duration - elapsed;
    const a = Math.min(1, elapsed / attack);
    const r = Math.min(1, remaining / release);
    const env = Math.sin(Math.max(0, a) * Math.PI / 2) ** 2
      * Math.sin(Math.max(0, r) * Math.PI / 2) ** 2;
    const f = f0 * (f1 / f0) ** (elapsed / duration);
    p += TAU * f / SR;
    const wob = wobble ? .55 + .45 * Math.sin(TAU * wobble * elapsed - Math.PI / 2) : 1;
    const sample = Math.tanh(Math.sin(p) * drive) * gain * env * wob;
    out[0][i] += sample * .5;
    out[1][i] += sample * .5;
  }
}

// femrag's FEM-marimba bell: bowl body modes + the first FEM-derived handbell
// shell modes from pop/bell, decayed short enough to play like a bar.
function bell(t0, midi, gain = .06, pan = 0, length = .22) {
  EVENTS.push({ i: "bell", t: t0, midi, gain, pan, length });
  const f = hz(midi);
  const partials = [
    [.461, .22, length * 1.35], [.688, .25, length * 1.18], [1, 1, length],
    [3.479, .17, length * .54], [3.559, .14, length * .48], [4.81, .08, length * .34],
  ];
  partials.forEach(([ratio, level, duration], i) => oscillator(t0, duration, f * ratio, gain * level, {
    attack: .0015, release: duration * .88, pan: pan + (i - 1) * .04, phase: i * 1.1,
  }));
}

function boom(t0, gain = .18) {
  EVENTS.push({ i: "boom", t: t0, gain });
  oscillator(t0, .34, 148, gain, {
    attack: .0015, release: .31, freqEnd: 43, pan: 0,
  });
  oscillator(t0, .19, 58, gain * .45, {
    attack: .002, release: .17, freqEnd: 45, pan: 0, phase: .7,
  });
}

// The dnb kick: femrag's boom with a tanh click so it punches through snares.
function kick(t0, gain = .2) {
  boom(t0, gain);
  oscillator(t0, .02, 320, gain * .5, {
    attack: .0005, release: .015, freqEnd: 90, pan: 0,
  });
}

function snare(t0, gain = .16, { ghost = false } = {}) {
  EVENTS.push({ i: "snare", t: t0, gain, ghost });
  const body = ghost ? .6 : 1;
  oscillator(t0, .09, 220, gain * .5 * body, {
    attack: .001, release: .07, freqEnd: 165, pan: 0,
  });
  noiseBurst(t0, ghost ? .07 : .15, gain * body, {
    attack: .0008, release: ghost ? .05 : .12, center: 2100, q: .9,
    pan: ghost ? .3 : 0,
  });
  if (!ghost) noiseBurst(t0, .05, gain * .5, {
    attack: .0005, release: .04, center: 5200, q: 1.6, pan: -.15,
  });
}

function hat(t0, gain = .05, open = false) {
  EVENTS.push({ i: "hat", t: t0, gain, open });
  noiseBurst(t0, open ? .16 : .035, gain, {
    attack: .0006, release: open ? .13 : .025, center: 8600, q: 2.2,
    pan: open ? -.5 : .55,
  });
}

function riser(t0, duration, gain = .1) {
  EVENTS.push({ i: "riser", t: t0, dur: duration });
  noiseBurst(t0, duration, gain, {
    attack: duration * .8, release: duration * .15,
    center: 500, centerEnd: 7500, q: 1.4, pan: 0,
  });
  oscillator(t0, duration, hz(57), gain * .5, {
    attack: duration * .85, release: duration * .12, freqEnd: hz(81), pan: .2,
  });
  oscillator(t0, duration, hz(57) * 1.011, gain * .5, {
    attack: duration * .85, release: duration * .12, freqEnd: hz(81) * 1.011,
    pan: -.2, phase: 2,
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

function ragBar(bar, density = 1, octave = 0, { pattern = true, oom = true, straight = false } = {}) {
  const chord = CHORDS[bar % 4];
  const step = (beat) => straight ? beat * BEAT : swung(beat);
  if (pattern) for (let e = 0; e < 8; e++) {
    if (density < .7 && e % 2) continue;
    const tt = at(bar) + step(e * .5);
    const midi = chord.notes[RAG[e]] + octave;
    bell(tt, midi, (e % 2 ? .035 : .052) * density, e % 2 ? .46 : -.38, .16);
  }
  if (oom) {
    for (const beat of [0, 2]) bell(at(bar, beat), chord.bass, .072 * density, -.55, .2);
    for (const beat of [1, 3]) chord.notes.slice(1, 3).forEach((m, i) =>
      bell(at(bar, beat + .5) + i * .008, m + octave, .022 * density, .18 + i * .22, .13));
  }
}

function leadBar(bar, amount = 1, octave = 0, { straight = false } = {}) {
  const step = (beat) => straight ? beat * BEAT : swung(beat);
  LEAD[bar % 4].forEach(([midi, beat], i) => {
    const tt = at(bar) + step(beat);
    bell(tt, midi + octave, .075 * amount, .18 + (i % 2) * .35, .24);
    if (i % 2 === 0) bell(tt + BEAT * .25, midi + 12 + octave, .025 * amount, -.58, .1);
  });
}

// High bells in a regular 3-over-2 cross-rhythm — six even hits per bar —
// voiced from the current chord so the shimmer always agrees with the harmony.
function ticks(bar, amount = 1) {
  const notes = CHORDS[bar % 4].notes;
  for (let s = 1; s < 6; s++) {
    const note = notes[[0, 2, 1, 3, 2, 1][s]] + 24;
    bell(at(bar, s * 2 / 3), note, .012 * amount * (s % 2 ? 1 : .7), s % 2 ? .82 : -.82, .06);
  }
}

// Straight two-step: kick on 1 and the and-of-2, snare on 2 and 4, ghosts and
// 16th hats between. `fill` swaps the last half-bar for a tumbling snare run.
function dnbBar(bar, weight = 1, { fill = false, doubleKick = false } = {}) {
  kick(at(bar, 0), .2 * weight);
  kick(at(bar, 2.5), .19 * weight);
  if (doubleKick) kick(at(bar, 1.75), .13 * weight);
  snare(at(bar, 1), .15 * weight);
  if (!fill) {
    snare(at(bar, 3), .15 * weight);
    snare(at(bar, 3.5), .05 * weight, { ghost: true });
    if (bar % 2 === 1) snare(at(bar, 1.75), .045 * weight, { ghost: true });
  } else {
    for (let s = 0; s < 6; s++) {
      snare(at(bar, 2.5 + s * .25), (.06 + s * .016) * weight, { ghost: s < 3 });
    }
  }
  for (let s = 0; s < 16; s++) {
    if (s === 0 || s === 10) continue;
    hat(at(bar, s / 4), .032 * weight * (s % 2 ? .7 : 1), fill ? false : s === 14 && bar % 4 === 3);
  }
}

// The dnb bassline walks the rag's roots an octave down, sliding and wobbling.
function bassBar(bar, weight = 1, { wobble = false } = {}) {
  const root = CHORDS[bar % 4].bass - 12;
  if (wobble) {
    sub(at(bar, 0), BEAT * 3.4, root, .21 * weight, {
      drive: 3.2, wobble: bar % 2 ? 6 : 4.5,
    });
  } else {
    sub(at(bar, 0), BEAT * 1.4, root, .21 * weight, { drive: 3, slideTo: root });
    sub(at(bar, 1.5), BEAT * .4, root + 12, .12 * weight, { drive: 2.4 });
    sub(at(bar, 2), BEAT * 1.3, root, .2 * weight, {
      drive: 3, slideTo: bar % 4 === 3 ? root - 5 : root,
    });
    sub(at(bar, 3.5), BEAT * .45, root + 7, .13 * weight, { drive: 2.6, slideTo: root });
  }
}

// The donk: a driven sine that falls an octave in 150 ms — UK donk dink.
function donk(t0, gain = .15, midi = 57) {
  EVENTS.push({ i: "donk", t: t0, gain });
  sub(t0, .15, midi, gain, {
    drive: 5, slideTo: midi - 12, attack: .0015, release: .05,
  });
}

// Ragga-donk kit: swung two-step with a dembow lean — donks riding every
// swung offbeat, hats swung hard, the snare talking patois on 3.
function raggaBar(bar, weight = 1) {
  kick(at(bar, 0), .2 * weight);
  kick(at(bar) + swung(2.5), .18 * weight);
  snare(at(bar, 1), .12 * weight);
  snare(at(bar, 3), .16 * weight);
  snare(at(bar) + swung(1.75), .05 * weight, { ghost: true });
  if (bar % 2 === 1) snare(at(bar) + swung(3.75), .045 * weight, { ghost: true });
  for (const beat of [.5, 1.5, 2.5, 3.5]) donk(at(bar) + swung(beat), .15 * weight);
  for (let e = 1; e < 8; e++) {
    if (e === 5) continue;
    hat(at(bar) + swung(e * .5), .034 * weight * (e % 2 ? .75 : 1), e === 7 && bar % 2 === 1);
  }
}

// The deep wub: slow quarter-note wobble an octave under the dnb bassline,
// with a driven octave doubling so the wub reads on small speakers.
function wubBar(bar, weight = 1) {
  const root = CHORDS[bar % 4].bass - 12;
  const rate = bar % 4 === 3 ? 3 : 2.25;
  sub(at(bar, 0), BEAT * 3.6, root, .2 * weight, { drive: 3.6, wobble: rate });
  sub(at(bar, 0), BEAT * 3.6, root + 12, .07 * weight, { drive: 4, wobble: rate });
}

// ── act one · femrag compressed to a fuse ───────────────────────────────────

// 0–4: the runway — the drop's two-step skeleton arrives FIRST, quiet, with
// the rag straight-time on top. The whole track lives in one rhythm world.
for (let bar = 0; bar < 4; bar++) {
  const rise = .5 + bar * .1;
  kick(at(bar, 0), .17 * rise);
  kick(at(bar, 2.5), .16 * rise);
  snare(at(bar, 1), .09 * rise, { ghost: bar < 2 });
  snare(at(bar, 3), .09 * rise, { ghost: bar < 2 });
  if (bar >= 1) ragBar(bar, .45 + bar * .08, 12, { oom: false, straight: true });
  if (bar >= 2) ticks(bar, .3 + (bar - 2) * .2);
}

// 4–12: the drop's groove at half power — full dnb kit muted, rag and lead
// straight-time, sub pips walking the roots. The drop will only turn it up.
for (let bar = 4; bar < 12; bar++) {
  const grow = .55 + (bar - 4) * .04;
  dnbBar(bar, grow, { fill: bar === 11 });
  ragBar(bar, .82, 12, { oom: false, straight: true });
  leadBar(bar, .7, bar < 8 ? 0 : 12, { straight: true });
  ticks(bar, .5);
  if (bar >= 6) sub(at(bar, 0), BEAT * 1.6, CHORDS[bar % 4].bass - 12, .08 + (bar - 6) * .015, { drive: 2 });
}

// ── the buildup · snare roll 8ths → 32nds in four bars ─────────────────────

for (let bar = 12; bar < 16; bar++) {
  const heat = (bar - 12) / 4;
  kickBar(bar, .95 + heat * .2);
  ragBar(bar, .5 - heat * .3, 12, { oom: false, straight: true });
  ticks(bar, .4 + heat * .5);
  const division = [4, 8, 16, 32][bar - 12];
  for (let s = 0; s < division; s++) {
    snare(at(bar, s * 4 / division), .05 + heat * .11 + s / division * .05, {
      ghost: division <= 8 && s % 2 === 1,
    });
  }
  sub(at(bar, 0), BAR * .95, 33, .1 + heat * .12, { drive: 2.8, wobble: 2 + heat * 7 });
}
riser(at(12), BAR * 4 - BEAT * .5, .12);
// The last half-beat before the drop goes silent — the intake of breath.
snare(at(15, 3.5), .22);

// ── first drop (0:27) · straight two-step drum and bass ────────────────────

// 16–32: straight time, heavy sub walking the rag roots, bells double-time
// on top so the old tune rides the new engine.
for (let bar = 16; bar < 32; bar++) {
  const w = bar === 16 ? 1.1 : 1;
  dnbBar(bar, w, { fill: bar % 8 === 7, doubleKick: bar % 4 === 2 });
  bassBar(bar, 1);
  ragBar(bar, .85, 12, { oom: false, straight: true });
  if (bar >= 20) leadBar(bar, .7, 12, { straight: true });
  if (bar >= 24) ticks(bar, .6);
}
sub(at(16, 0), BEAT * 3.8, 21, .24, { drive: 3.4, slideTo: 33 }); // drop stinger

// 32–40: breakdown — half-time feel, wobble bass, the rag drifts back swung
// like a memory of act one.
for (let bar = 32; bar < 40; bar++) {
  kick(at(bar, 0), .17);
  snare(at(bar, 2), .13);
  bassBar(bar, .9, { wobble: true });
  ragBar(bar, .4, bar >= 36 ? 12 : 0);
  if (bar % 2 === 0) leadBar(bar, .5, 0);
  hat(at(bar, 1.5), .03); hat(at(bar, 3.5), .03, bar % 2 === 1);
}

// 40–44: second buildup, straight to 16ths then 32nds.
for (let bar = 40; bar < 44; bar++) {
  const heat = (bar - 40) / 4;
  kickBar(bar, 1 + heat * .15);
  ticks(bar, .5 + heat * .5);
  const division = [4, 8, 16, 32][bar - 40];
  for (let s = 0; s < division; s++) {
    snare(at(bar, s * 4 / division), .05 + heat * .12 + s / division * .05, {
      ghost: division <= 8 && s % 2 === 1,
    });
  }
  sub(at(bar, 0), BAR * .95, 33, .12 + heat * .12, { drive: 3, wobble: 4 + heat * 8 });
  for (let e = 0; e < 4; e++) bell(at(bar, e), 69 + 12 + (bar - 40) * 2 + e, .04 + heat * .03, e % 2 ? .6 : -.6, .1);
}
riser(at(41), BAR * 3 - BEAT * .25, .13);

// 44–56: second drop — fills every fourth bar, double kicks, lead up top.
for (let bar = 44; bar < 56; bar++) {
  const w = bar === 44 ? 1.15 : 1.05;
  dnbBar(bar, w, { fill: bar % 4 === 3, doubleKick: bar % 2 === 1 });
  bassBar(bar, 1.1);
  ragBar(bar, .9, 12, { oom: false, straight: true });
  leadBar(bar, .85, 12, { straight: true });
  ticks(bar, .75);
}
sub(at(44, 0), BEAT * 3.8, 21, .26, { drive: 3.6, slideTo: 33 });

// ── 56–72 (1:33) · the ragga-donk · deep wub, swing back on, serious wibes ─

for (let bar = 56; bar < 72; bar++) {
  const w = bar === 56 ? 1.15 : 1.05;
  raggaBar(bar, w);
  wubBar(bar, 1);
  ragBar(bar, .8, 12, { oom: false });
  if (bar % 4 >= 2) leadBar(bar, .6, 12);
  if (bar % 8 === 4) for (let s = 0; s < 8; s++) {
    bell(at(bar, 2) + s * BEAT / 4, CHORDS[bar % 4].notes[s % 4] + 24, .028, s % 2 ? .7 : -.7, .08);
  }
}
sub(at(56, 0), BEAT * 3.8, 21, .28, { drive: 4, slideTo: 33 });

// 72–80: outro — the wub winds down and femrag's swing gets the last word.
for (let bar = 72; bar < 76; bar++) {
  const fade = 1 - (bar - 72) * .18;
  raggaBar(bar, .8 * fade);
  wubBar(bar, .75 * fade);
  ragBar(bar, .55 * fade, 12, { oom: false });
}
for (let bar = 76; bar < 80; bar++) {
  const fade = 1 - (bar - 76) * .2;
  kickBar(bar, .7 * fade);
  ragBar(bar, .45 * fade);
  if (bar === 76) leadBar(bar, .5);
  if (bar === 78) leadBar(bar, .35, -12);
}
bell(at(79, 3), 45, .085, 0, .42);
sub(at(79, 3), BEAT * 1.2, 33, .1, { drive: 2, release: .4 });

// ── master ─────────────────────────────────────────────────────────────────

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

const outPath = expandHome(argv("--out")) || resolve(HERE, "..", "out", `${SLUG}.mp3`);
mkdirSync(dirname(outPath), { recursive: true });
writeFileSync(outPath.replace(/\.mp3$/, ".events.json"),
  JSON.stringify({ bpm: BPM, bars: BARS, seconds: SECONDS, events: EVENTS }));

// The arrangement, as the video pipeline reads it. Bar ranges here are the
// same ones the section loops above use — change one, change both.
const SECTIONS = [
  ["runway", 0, 4], ["groove", 4, 12], ["buildup1", 12, 16],
  ["drop1a", 16, 24], ["drop1b", 24, 32], ["breakdown", 32, 40],
  ["buildup2", 40, 44], ["drop2a", 44, 52], ["drop2b", 52, 56],
  ["ragga-a", 56, 64], ["ragga-b", 64, 72], ["outro", 72, 80],
];
writeFileSync(outPath.replace(/\.mp3$/, ".struct.json"), JSON.stringify({
  totalSec: SECONDS,
  sections: SECTIONS.map(([name, from, to]) => ({
    name, startSec: at(from), endSec: at(to),
  })),
}, null, 2));
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
  "-af", "acompressor=threshold=-16dB:ratio=2.4:attack=12:release=90:makeup=2:knee=6,alimiter=limit=.96:attack=4:release=70",
  "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", `title=${TITLE}`, "-metadata", "artist=jeffrey", "-metadata", "album=pixsies",
  outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ffmpeg.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}
console.log(`✓ ${outPath} · ${BPM} BPM · ${BARS} bars · ${SECONDS.toFixed(1)} s · ${voices} voices`);
