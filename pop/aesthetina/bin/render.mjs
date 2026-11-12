#!/usr/bin/env node
// aesthetina — an ocarina made of a keyboard. See ../README.md for the law.
// The aesthetina engine: instruments + mix. node bin/render.mjs [--arr=country]
// renders arrangements/<arr>.mjs → out/aesthetina[-<arr>].{wav,mp3}.

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { renderBell } from "../../lib/bell.mjs";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
// which arrangement: arrangements/<name>.mjs (play) + <name>.json (bars, bpm, swing, seed)
const ARR_NAME = process.argv.find((a) => a.startsWith("--arr="))?.slice(6) ?? "country";
const ARR = await import(join(HERE, "..", "arrangements", `${ARR_NAME}.mjs`));
const META = JSON.parse(readFileSync(join(HERE, "..", "arrangements", `${ARR_NAME}.json`), "utf8"));
const OUT = join(HERE, "..", "out");
const SR = 44100;
const BPM = META.bpm ?? 128;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const S16 = BEAT / 4;
const BARS = META.bars;
// the room everything plays in (meta "room"): the club, or a small bedroom (short, dark, mostly dry)
const ROOM = { club: { verb: [0.82, 0.3, 1], hall: [0.93, 0.2, 1], wet: 0.55, hallWet: 0.5, voxSend: 0.7 },
  bedroom: { verb: [0.62, 0.5, 0.3], hall: [0.72, 0.45, 0.42], wet: 0.34, hallWet: 0.2, voxSend: 0.3, pump: 2, plate: 0.5 } }[META.room ?? "club"];
const LEN = Math.ceil((BARS * BAR + 3) * SR);
const TAU = Math.PI * 2;

// ── deterministic randomness ────────────────────────────────────────────
function mulberry(seed) {
  return () => {
    seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = mulberry(META.seed ?? 0xae57);
const hash = (s) => { let h = 2166136261; for (const c of s) h = Math.imul(h ^ c.charCodeAt(0), 16777619); return h >>> 0; };

// ── notepat's QWERTY map (disks/notepat.mjs NOTE_TO_KEYBOARD_KEY), as MIDI ─
const NOTE_TO_KEY = {
  "-a#": "z", "-b": "x", c: "c", "c#": "v", d: "d", "d#": "s", e: "e", f: "f",
  "f#": "w", g: "g", "g#": "r", a: "a", "a#": "q", b: "b", "+c": "h", "+c#": "t",
  "+d": "i", "+d#": "y", "+e": "j", "+f": "k", "+f#": "u", "+g": "l", "+g#": "o",
  "+a": "m", "+a#": "p", "+b": "n",
};
const PC = { c: 0, "c#": 1, d: 2, "d#": 3, e: 4, f: 5, "f#": 6, g: 7, "g#": 8, a: 9, "a#": 10, b: 11 };
const KEY_MIDI = {};
for (const [note, key] of Object.entries(NOTE_TO_KEY)) {
  const oct = note.startsWith("-") ? -1 : note.startsWith("+") ? 1 : 0;
  KEY_MIDI[key] = 60 + oct * 12 + PC[note.replace(/^[-+]/, "")];
}
const hz = (m) => 440 * 2 ** ((m - 69) / 12);

// ── buses ───────────────────────────────────────────────────────────────
const bus = () => ({ L: new Float32Array(LEN), R: new Float32Array(LEN) });
const B = { drums: bus(), keys: bus(), bass: bus(), pad: bus(), lead: bus(), bells: bus(), verb: bus(), hall: bus() };
function put(b, i, v, pan = 0, send = 0) {
  if (i < 0 || i >= LEN) return;
  const l = Math.cos((pan + 1) * Math.PI / 4), r = Math.sin((pan + 1) * Math.PI / 4);
  b.L[i] += v * l; b.R[i] += v * r;
  if (send) { B.verb.L[i] += v * l * send; B.verb.R[i] += v * r * send; }
}

// two-pole resonator: a damped sine mode excited by an impulse
function mode(b, t, f, amp, decay, pan, send = 0, phase = 0) {
  const i0 = Math.round(t * SR), n = Math.min(LEN - i0, Math.ceil(decay * 7 * SR));
  let ph = phase;
  for (let k = 0; k < n; k++) {
    put(b, i0 + k, amp * Math.exp(-k / (decay * SR)) * Math.sin(TAU * ph), pan, send);
    ph += f / SR; if (ph >= 1) ph -= 1;
  }
}
// band-passed noise burst (RBJ biquad)
function burst(b, t, fc, q, amp, dur, pan, send = 0) {
  const w = TAU * fc / SR, al = Math.sin(w) / (2 * q), a0 = 1 + al;
  const b0 = al / a0, b2 = -al / a0, a1 = -2 * Math.cos(w) / a0, a2 = (1 - al) / a0;
  let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
  const i0 = Math.round(t * SR), n = Math.ceil(dur * 4 * SR);
  for (let k = 0; k < n; k++) {
    const x = rand() * 2 - 1;
    const y = b0 * x + b2 * x2 - a1 * y1 - a2 * y2;
    x2 = x1; x1 = x; y2 = y1; y1 = y;
    put(b, i0 + k, amp * y * Math.exp(-k / (dur * SR)), pan, send);
  }
}

// ── SWITCH ──────────────────────────────────────────────────────────────
// Each key has its own plate/case modes, hashed from its name.
function keyVoice(name) {
  const r = mulberry(hash(name));
  return {
    click: 3800 + r() * 2600,
    ping: 2900 + r() * 2200,
    thock: [190 + r() * 120, 520 + r() * 260, 1150 + r() * 500],
    pan: ((hash(name + "pan") % 1000) / 1000 - 0.5) * 0.9,
  };
}
const VOICES = {};
const kv = (name) => (VOICES[name] ??= keyVoice(name));

function keyDown(b, t, name, vel = 1, opts = {}) {
  const v = kv(name), pan = opts.pan ?? v.pan, send = opts.send ?? 0.06;
  // tactile click: the leaf snapping past the bump
  burst(b, t, v.click, 3, 0.55 * vel, 0.0016, pan, send);
  mode(b, t, v.click * 0.82, 0.12 * vel, 0.004, pan, send);
  // bottom-out thock, a few ms later
  const tb = t + 0.006 + rand() * 0.004;
  mode(b, tb, v.thock[0], 0.42 * vel, 0.018, pan, send);
  mode(b, tb, v.thock[1], 0.22 * vel, 0.010, pan, send);
  mode(b, tb, v.thock[2], 0.10 * vel, 0.006, pan, send);
  burst(b, tb, 900, 0.8, 0.14 * vel, 0.003, pan, send);
  // spring ping
  mode(b, tb, v.ping, 0.025 * vel * (opts.ping ?? 1), 0.05 * (opts.ring ?? 1), pan, send);
}
function keyUp(b, t, name, vel = 1, opts = {}) {
  const v = kv(name), pan = opts.pan ?? v.pan;
  burst(b, t, v.click * 1.15, 4, 0.18 * vel, 0.0012, pan, 0.04);
  mode(b, t, v.thock[1] * 1.3, 0.06 * vel, 0.006, pan, 0.04);
  mode(b, t, v.ping, 0.04 * vel * (opts.ping ?? 1), 0.07 * (opts.ring ?? 1), pan, 0.04);
}
function spacebar(b, t, vel = 1) {
  mode(b, t, 118, 0.5 * vel, 0.035, 0, 0.03);
  mode(b, t, 262, 0.3 * vel, 0.02, 0, 0.03);
  mode(b, t, 610, 0.14 * vel, 0.01, 0, 0.03);
  burst(b, t, 700, 0.7, 0.2 * vel, 0.004, 0, 0.03);
  // stabiliser wire rattle
  for (let k = 0; k < 3; k++) burst(b, t + 0.004 + k * (0.003 + rand() * 0.003), 2400 + rand() * 1500, 5, 0.06 * vel, 0.001, (rand() - 0.5) * 0.6);
}

// ── BREATH ──────────────────────────────────────────────────────────────
function ocarina(b, t, f, dur, amp, o = {}) {
  const pan = o.pan ?? 0, send = o.send ?? 0.25;
  const att = o.attack ?? 0.022, rel = o.release ?? 0.06;
  const h2 = o.h2 ?? 0.035, h3 = o.h3 ?? 0.018;
  const vibD = o.vib ?? 0.12, vibDelay = o.vibDelay ?? 0.16;
  const scoop = o.scoop ?? -0.3; // semitones, recovered over ~40 ms
  const i0 = Math.round(t * SR), n = Math.ceil((dur + rel * 5) * SR);
  // breath noise filtered around the pitch (state-variable bandpass)
  let lp = 0, bp = 0;
  const fq = 2 * Math.sin(Math.PI * Math.min(f, SR / 6) / SR), damp = 0.12;
  let ph = 0, ph2 = 0, ph3 = 0, vph = rand();
  for (let k = 0; k < n; k++) {
    const s = k / SR;
    const env = (s < att ? (s / att) ** 1.6 : s < dur ? 1 : Math.exp(-(s - dur) / rel))
      * (o.decay ? (o.floor ?? 0) + (1 - (o.floor ?? 0)) * Math.exp(-s / o.decay) : 1); // pluck
    if (s > dur && env < 1e-4) break;
    const vib = vibD * Math.min(1, Math.max(0, (s - vibDelay) / 0.25)) * Math.sin(TAU * vph);
    vph += 5.2 / SR;
    const semis = scoop * Math.exp(-s / 0.04) + vib / 1;
    const fi = f * 2 ** (semis / 12) * (o.glide ? 2 ** (o.glide * Math.min(1, s / dur) / 12) : 1);
    ph += fi / SR; if (ph >= 1) ph -= 1;
    ph2 += 2 * fi / SR; if (ph2 >= 1) ph2 -= 1;
    ph3 += 3 * fi / SR; if (ph3 >= 1) ph3 -= 1;
    const x = rand() * 2 - 1;
    lp += fq * bp; const hp = x - lp - damp * bp; bp += fq * hp;
    const chiff = s < 0.03 ? (rand() * 2 - 1) * 0.18 * (1 - s / 0.03) : 0;
    const tone = Math.sin(TAU * ph) + h2 * Math.sin(TAU * ph2) + h3 * Math.sin(TAU * ph3);
    put(b, i0 + k, amp * env * (tone + 0.07 * bp + chiff), pan, send);
  }
}

// typed line: each letter is a keystroke + an ocarina note held until the next
function typeLine(text, t0, steps, o = {}) {
  const chars = [...text];
  const end = t0 + (o.until ?? steps[steps.length - 1] + 4) * S16;
  chars.forEach((ch, i) => {
    const t = t0 + sw(steps[i]) * S16;
    const next = i + 1 < chars.length ? t0 + sw(steps[i + 1]) * S16 : end;
    if (ch === " ") { spacebar(B.keys, t, 0.5); keyUp(B.keys, t + 0.09, "space", 0.4); return; }
    keyDown(B.keys, t, ch, o.keyVel ?? 0.55);
    keyUp(B.keys, next - 0.02, ch, (o.keyVel ?? 0.55) * 0.7);
  });
  // breath: one slurred voice per word, sliding key to key
  let word = [];
  const flush = (stop) => { if (word.length) slideVoice(B.lead, word, stop, o); word = []; };
  chars.forEach((ch, i) => {
    const t = t0 + sw(steps[i]) * S16;
    if (ch === " ") { flush(t - 0.03); return; }
    word.push({ t: t + 0.008, m: KEY_MIDI[ch] + (o.octave ?? 0) * 12 });
  });
  flush(end - 0.05);
}

// a slurred ocarina line: the pitch glides into each new key instead of
// jumping, overshoots a hair, and the breath dips slightly at each change.
function slideVoice(b, notes, stop, o = {}) {
  const amp = o.amp ?? 0.16, pan = o.pan ?? 0, send = o.send ?? 0.3;
  const glideBase = o.glide ?? 0.11, vibD = o.vib ?? 0.14;
  const t0 = notes[0].t, rel = 0.07;
  const i0 = Math.round(t0 * SR), n = Math.ceil((stop - t0 + rel * 6) * SR);
  let ph = 0, ph2 = 0, ph3 = 0, vph = rand(), lp = 0, bp = 0, idx = 0;
  for (let k = 0; k < n; k++) {
    const T = t0 + k / SR;
    while (idx + 1 < notes.length && T >= notes[idx + 1].t) idx++;
    const cur = notes[idx], prev = notes[idx - 1];
    const since = T - cur.t;
    let semis = cur.m;
    if (prev) {
      const jump = cur.m - prev.m, g = glideBase * (1 + Math.abs(jump) / 10);
      const x = Math.min(1, since / g);
      const ease = x < 1 ? 1 - (1 - x) ** 3 : 1;
      const over = 0.18 * Math.sign(jump) * Math.sin(Math.PI * Math.min(1, since / (g * 1.8))) * (x >= 1 ? 1 : x);
      semis = prev.m + jump * ease + over;
    } else semis += -0.5 * Math.exp(-since / 0.05);
    semis -= (o.twang ?? 1.3) * Math.exp(-since / 0.045); // twang: every key bends up into its note
    const vib = vibD * Math.min(1, Math.max(0, (since - 0.14) / 0.3)) * Math.sin(TAU * vph);
    vph += 5.2 / SR;
    const f = hz(semis + vib);
    const s = T - t0;
    const env = (s < 0.03 ? (s / 0.03) ** 1.6 : 1) * (T < stop ? 1 : Math.exp(-(T - stop) / rel))
      * ((o.floor ?? 0.18) + (1 - (o.floor ?? 0.18)) * Math.exp(-since / (o.decay ?? 0.32))) // each key plucks, then decays
      * (prev ? 1 - 0.18 * Math.exp(-since / 0.03) * Math.min(1, since / 0.004) : 1);
    if (T > stop && env < 1e-4) break;
    ph += f / SR; if (ph >= 1) ph -= 1;
    ph2 += 2 * f / SR; if (ph2 >= 1) ph2 -= 1;
    ph3 += 3 * f / SR; if (ph3 >= 1) ph3 -= 1;
    const fq = 2 * Math.sin(Math.PI * Math.min(f, SR / 6) / SR);
    lp += fq * bp; const hp = (rand() * 2 - 1) - lp - 0.12 * bp; bp += fq * hp;
    const chiff = since < 0.02 ? (rand() * 2 - 1) * (prev ? 0.06 : 0.18) * (1 - since / 0.02) : 0;
    const tone = Math.sin(TAU * ph) + 0.035 * Math.sin(TAU * ph2) + 0.018 * Math.sin(TAU * ph3);
    put(b, i0 + k, amp * env * (tone + 0.07 * bp + chiff), pan, send);
  }
}

// ── DRUMS ───────────────────────────────────────────────────────────────
const kicks = [];
function kick(t, vel = 1) {
  kicks.push(t);
  // the huff: a hard breath into a big vessel
  const i0 = Math.round(t * SR), n = Math.ceil(0.5 * SR);
  let ph = 0;
  for (let k = 0; k < n; k++) {
    const s = k / SR;
    const f = 45 + 95 * Math.exp(-s / 0.028);
    ph += f / SR; if (ph >= 1) ph -= 1;
    const env = Math.min(1, s / 0.003) * Math.exp(-s / 0.17);
    put(B.drums, i0 + k, vel * 0.78 * Math.tanh(1.6 * env * Math.sin(TAU * ph)));
  }
  spacebar(B.drums, t, 0.55 * vel);
}
function hat(t, vel, open = false) {
  const name = "hat" + (Math.floor(t / S16) % 3);
  burst(B.drums, t, 7200 + rand() * 1400, 2.5, 0.22 * vel, open ? 0.006 : 0.0016, 0.25, 0.04);
  keyUp(B.drums, t, name, vel * 0.8, { pan: 0.3, ping: open ? 3 : 1, ring: open ? 2.2 : 1 });
}
function clap(t, vel = 1) {
  const mash = ["j", "k", "l", ";", "h"];
  mash.forEach((k, i) => keyDown(B.drums, t + i * 0.0028 + rand() * 0.002, k, vel * 0.7, { pan: (i - 2) * 0.12, send: 0.22 }));
  burst(B.drums, t + 0.008, 1800, 1.2, 0.3 * vel, 0.03, 0, 0.3);
}
function roll(t0, n16, vel = 0.5) {
  const letters = "asdfjkl;ghqwer";
  for (let i = 0; i < n16 * 2; i++) {
    const t = t0 + i * S16 / 2 + (rand() - 0.5) * 0.008;
    keyDown(B.keys, t, letters[Math.floor(rand() * letters.length)], vel * (0.5 + i / (n16 * 4)));
  }
}

// ── ARRANGEMENT (80 bars) — see ../ARRANGEMENT.md ────────────────────────
// swing: the shuffle. Each beat's first 8th stretches toward 2/3 of the beat
// (SWING 1 = full triplet), so 16th grids, typed words and voices all lope.
const SWING = META.swing ?? 0.62;
const sw = (s) => {
  const beat = Math.floor(s / 4), f = s - beat * 4, split = 2 + (2 / 3) * SWING;
  return beat * 4 + (f < 2 ? (f * split) / 2 : split + ((f - 2) * (4 - split)) / 2);
};
const at = (bar, s16 = 0) => bar * BAR + sw(s16) * S16;
const HOOK = "aesthetina";
const HOOK_STEPS = [0, 3, 6, 8, 10, 12, 14, 16, 19, 22]; // the clave; last 'a' rings to step 30
const COUNTER = "aesthetic computer";
// the hook's complement inside the same two bars: a hocket, never a collision
const COUNTER_STEPS = [1, 2, 4, 5, 7, 9, 11, 13, 15, 17, 18, 20, 21, 23, 24, 25, 26, 27];
const VESPER = "vespertine";
const ADIEU = "adieu", ADIEU_STEPS = [0, 4, 8, 12, 20];
const AEIOU = "aeiou";
const pat = (s) => [...s].map((c) => c === "x");
const E7_16 = pat("x..x.x.x..x.x.x."), E9_16 = pat("x.xx.x.x.xx.x.x."), E3_8 = pat("x..x..x."), E7_12 = pat("x.xx.x.xx.x.");
const B1_WORDS = ["tea", "daw", "rib", "ace"], DROP_WORDS = ["ace", "fad", "big", "tea"];

// a chord word: three keys strummed, three held vessels
function strum(word, t, dur, o = {}) {
  [...word].forEach((ch, i) => {
    const tk = t + i * 0.028;
    keyDown(B.keys, tk, ch, o.keyVel ?? 0.5);
    keyUp(B.keys, tk + dur - 0.05, ch, 0.35);
    ocarina(B.pad, tk + 0.008, hz(KEY_MIDI[ch] + (o.octave ?? 0) * 12), dur - 0.06, o.amp ?? 0.05,
      { pan: (i - 1) * 0.45, send: 0.5, attack: o.attack ?? 0.01, release: 0.25, vib: 0.08, scoop: -0.6, decay: 0.55, floor: 0.08 });
  });
}
// the hook stretched in time (prolation): factor × slower, octaves down, breath only
function augmented(t, factor, octave, amp, bus) {
  const notes = HOOK_STEPS.map((s, i) => ({ t: t + sw(s * factor) * S16 + 0.008, m: KEY_MIDI[HOOK[i]] + octave * 12 }));
  slideVoice(bus, notes, t + 30 * S16 * factor - 0.05, { amp, send: 0.2, glide: 0.11 * Math.sqrt(factor), vib: 0.05, decay: 0.5 * factor, floor: 0.3 });
}
// boot: the word typed as switches, with only the first n letters breathing
function bootHook(t, nBreath) {
  HOOK_STEPS.forEach((s, i) => {
    const tk = t + sw(s) * S16, next = t + sw(HOOK_STEPS[i + 1] ?? 30) * S16;
    keyDown(B.keys, tk, HOOK[i], 0.5);
    keyUp(B.keys, next - 0.02, HOOK[i], 0.35);
    if (i < nBreath) ocarina(B.lead, tk + 0.008, hz(KEY_MIDI[HOOK[i]]), next - tk - 0.06, 0.14, { send: 0.45, decay: 0.3, scoop: -1.3 });
  });
}
// tuplet ladder: beat k of the 8 gets k evenly spaced keystrokes
function ladder(t0) {
  const letters = "asdfghjkl";
  for (let k = 1; k <= 8; k++) for (let j = 0; j < k; j++) {
    const t = t0 + (k - 1) * BEAT + j * BEAT / k;
    keyDown(B.keys, t, letters[(k + j) % letters.length], 0.35 + k * 0.05, { send: 0.15 });
  }
}
function hatsBar(bar, pattern, ups = true, vel = 1) {
  for (let s = 0; s < 16; s++) {
    const t = at(bar, s) + (s % 2 ? S16 * 0.08 : 0);
    if (pattern[s]) hat(t, vel * (s % 4 === 0 ? 0.55 : 0.75), s % 4 === 2);
    else if (ups) hat(t, vel * 0.25);
  }
}
// ── country: boom-chick, air, bells, phone keys ─────────────────────────
// the chord under each bar (the same plan the choir sings)
function chordAt(bar) {
  if (ARR.chordAt) return ARR.chordAt(bar); // an arrangement can carry its own changes
  if (bar >= 16 && bar < 28) return ["tea", "taw", "tea", "daw", "tea", "taw"][(bar - 16) >> 1];
  if (bar >= 36 && bar < 44) return B1_WORDS[(bar - 36) >> 1];
  if (bar >= 44 && bar < 52) return B1_WORDS[(bar - 44) % 4];
  if (bar >= 56 && bar < 64) return ["tea", "taw"][((bar - 56) >> 1) % 2];
  if (bar >= 64 && bar < 72) return DROP_WORDS[(bar - 64) >> 1];
  return "tea";
}
const ROOT = { tea: 45, taw: 42, daw: 50, rib: 44, ace: 45, fad: 50, big: 43 };
// boom on 1 and 3 (root, then the fifth), chick on 2 and 4 (a short strum)
function subBar(bar) {
  const word = chordAt(bar), r = ROOT[word], fifth = r + 7 <= 52 ? r + 7 : r - 5;
  [r, fifth].forEach((m, k) => ocarina(B.bass, at(bar, k * 8), hz(m), BEAT * 0.8, 0.36,
    { h2: 0.5, h3: 0.25, vib: 0, scoop: -1.5, send: 0.03, attack: 0.004, release: 0.05, decay: 0.2 }));
  for (const beat of [1, 3]) {
    const t = at(bar, beat * 4);
    [...word].forEach((ch, i) => {
      keyDown(B.keys, t + i * 0.012, ch, 0.3, { send: 0.05 });
      ocarina(B.pad, t + i * 0.012, hz(KEY_MIDI[ch] - 12), BEAT * 0.28, 0.035, { pan: (i - 1) * 0.5, send: 0.1, attack: 0.004, release: 0.03, vib: 0, scoop: -0.8, decay: 0.06 });
    });
  }
}
// Menu Band's cabin air: a resonant bandpass whistle at each chord pitch and
// its octave over a brown + pink floor; swells in (0.7 s), drifts out (2.4 s)
function air(t0, dur, midis, amp = 0.05) {
  const n = Math.ceil((dur + 6) * SR), i0 = Math.round(t0 * SR);
  for (const ch of ["L", "R"]) {
    const bp = [];
    for (const m of midis) for (const [mul, q, g] of [[1, 38, 1], [2, 30, 0.35]]) {
      const w = TAU * hz(m) * mul / SR, al = Math.sin(w) / (2 * q), a0 = 1 + al;
      bp.push({ b0: al / a0, b2: -al / a0, a1: -2 * Math.cos(w) / a0, a2: (1 - al) / a0, g, x1: 0, x2: 0, y1: 0, y2: 0 });
    }
    let brown = 0, lp = 0, p1 = 0, p2 = 0, p3 = 0, wander = 0.5, env = 0;
    for (let k = 0; k < n; k++) {
      const x = rand() * 2 - 1;
      let whistle = 0;
      for (const f of bp) {
        const y = f.b0 * x + f.b2 * f.x2 - f.a1 * f.y1 - f.a2 * f.y2;
        f.x2 = f.x1; f.x1 = x; f.y2 = f.y1; f.y1 = y; whistle += f.g * y;
      }
      brown = brown * 0.998 + x * 0.02; lp += 0.05 * (brown - lp);
      p1 = 0.997 * p1 + 0.03 * x; p2 = 0.985 * p2 + 0.03 * x; p3 = 0.95 * p3 + 0.03 * x;
      wander = Math.min(1, Math.max(0, wander + (rand() - 0.5) * 0.0004));
      const target = k / SR < dur ? 1 : 0;
      env += (target - env) / ((target ? 0.7 : 2.4) * SR);
      const v = (whistle * (30 + wander * 300) * 0.004 + lp * 3.2 + (p1 + p2 + p3) * 0.5) * env * amp;
      const i = i0 + k; if (i >= 0 && i < LEN) { B.pad[ch][i] += v; B.verb[ch][i] += v * 0.3; }
    }
  }
}
// FEM bells (pop/bell): brass handbells, a bronze church toll, glass in the stillness
const BELLS = new Map();
function bell(t, midi, geometry = "handbell", material = "brass", amp = 0.2, pan = 0, dur = 3) {
  const key = `${midi}|${geometry}|${material}|${dur}`;
  if (!BELLS.has(key)) {
    const b = renderBell({ freq: hz(midi), geometry, material, dur, sr: SR });
    let pk = 0; for (const v of b.L) pk = Math.max(pk, Math.abs(v));
    BELLS.set(key, { L: b.L, R: b.R, g: 1 / (pk || 1) });
  }
  const b = BELLS.get(key), i0 = Math.round(t * SR);
  const l = Math.cos((pan + 1) * Math.PI / 4) * Math.SQRT2, r = Math.sin((pan + 1) * Math.PI / 4) * Math.SQRT2;
  for (let k = 0; k < b.L.length && i0 + k < LEN; k++) {
    B.bells.L[i0 + k] += b.L[k] * b.g * amp * l; B.bells.R[i0 + k] += b.R[k] * b.g * amp * r;
    B.hall.L[i0 + k] += b.L[k] * b.g * amp * 0.9; B.hall.R[i0 + k] += b.R[k] * b.g * amp * 0.9;
  }
}
// c_robo_dtmf (cc.oooooooooooooooo.cc, via Menu Band's CC kit): phone-key "6",
// gated to a blip and repitched (by playback rate) onto the chord
const DTMF = (() => {
  const f = join(HERE, "..", "samples", "c_robo_dtmf", "dtmf1.wav");
  if (!existsSync(f)) return null;
  const raw = execFileSync("ffmpeg", ["-loglevel", "error", "-i", f, "-f", "f32le", "-ac", "1", "-ar", String(SR), "-"], { maxBuffer: 1 << 26 });
  return new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4);
})();
function dtmf(t, semis = 0, amp = 0.12, len = 0.07, pan = 0) {
  if (!DTMF) return;
  const rate = 2 ** (semis / 12), n = Math.round(len * SR), i0 = Math.round(t * SR);
  for (let k = 0; k < n; k++) {
    const src = k * rate, j = Math.floor(src), fr = src - j;
    if (j + 1 >= DTMF.length) break;
    const env = Math.min(1, k / 40) * (k > n - 200 ? (n - k) / 200 : 1);
    put(B.drums, i0 + k, amp * env * (DTMF[j] * (1 - fr) + DTMF[j + 1] * fr), pan, 0.1);
  }
}
const E5_8 = pat("x.xx.xx.");
function teaPad(bar, bars, amp = 0.05) {
  ocarina(B.pad, at(bar), hz(57), BAR * bars - 0.1, amp, { pan: -0.4, send: 0.5, attack: 0.6, release: 0.8, vib: 0.06 });
  ocarina(B.pad, at(bar), hz(61), BAR * bars - 0.1, amp * 0.7, { pan: 0.1, send: 0.5, attack: 0.8, release: 0.8, vib: 0.06 });
  ocarina(B.pad, at(bar), hz(64), BAR * bars - 0.1, amp * 0.8, { pan: 0.4, send: 0.5, attack: 0.9, release: 0.8, vib: 0.06 });
}

// the arrangement plays every part through the instruments above
ARR.play({ SR, BPM, BEAT, BAR, S16, BARS, LEN, TAU, at, sw, KEY_MIDI, hz, rand, B, put, mode, burst, keyDown, keyUp, spacebar, ocarina, slideVoice, typeLine, kick, hat, clap, roll, strum, augmented, bootHook, ladder, hatsBar, subBar, teaPad, air, bell, dtmf, chordAt, ROOT, HOOK, HOOK_STEPS, COUNTER, COUNTER_STEPS, VESPER, ADIEU, ADIEU_STEPS, AEIOU, pat, E7_16, E9_16, E3_8, E7_12, E5_8, B1_WORDS, DROP_WORDS });

// ── MIX ─────────────────────────────────────────────────────────────────
// sidechain: the kick is a breath, so everything else inhales around it
const duck = new Float32Array(LEN).fill(1);
for (const t of kicks) {
  const i0 = Math.round(t * SR);
  for (let k = 0; k < 0.4 * SR && i0 + k < LEN; k++) {
    const s = k / SR, g = 1 - 0.82 * Math.min(1, s / 0.004) * Math.exp(-s / 0.11);
    duck[i0 + k] = Math.min(duck[i0 + k], g);
  }
}
// Freeverb-style room on the send bus
function reverb({ L, R }, fb = 0.82, damp = 0.3, size = 1) {
  const combs = [1116, 1188, 1277, 1356, 1422, 1491].map((d) => Math.round(d * size)), aps = [556, 441, 341].map((d) => Math.round(d * size));
  const run = (x, spread) => {
    const y = new Float32Array(LEN);
    for (const d0 of combs) {
      const d = d0 + spread, buf = new Float32Array(d); let p = 0, f = 0;
      for (let i = 0; i < LEN; i++) { const o = buf[p]; f = o * (1 - damp) + f * damp; buf[p] = x[i] + f * fb; p = (p + 1) % d; y[i] += o / combs.length; }
    }
    for (const d0 of aps) {
      const d = d0 + spread, buf = new Float32Array(d); let p = 0;
      for (let i = 0; i < LEN; i++) { const o = buf[p]; buf[p] = y[i] + o * 0.5; y[i] = o - y[i]; p = (p + 1) % d; }
    }
    return y;
  };
  return { L: run(L, 0), R: run(R, 23) };
}
// jeffrey's sung hook (bin/sing-hook.py), if rendered
const VOX = join(OUT, ARR_NAME === "country" ? "vocal-stem.wav" : `vocal-${ARR_NAME}.wav`);
B.vox = bus();
if (existsSync(VOX)) {
  const raw = execFileSync("ffmpeg", ["-loglevel", "error", "-i", VOX, "-f", "f32le", "-ac", "2", "-ar", String(SR), "-"], { maxBuffer: 1 << 30 });
  const v = new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4);
  for (let i = 0; i < Math.min(LEN, v.length / 2); i++) {
    B.vox.L[i] += v[2 * i] * 1.6; B.vox.R[i] += v[2 * i + 1] * 1.6;
    B.verb.L[i] += v[2 * i] * ROOM.voxSend; B.verb.R[i] += v[2 * i + 1] * ROOM.voxSend;
  }
}
// a vocal plate that breathes with the kick (sidechained): it ducks hard on every
// hit and blooms back in the gap. Pumping reverb is meta "room".pump (club: none).
B.plate = bus();
if (ROOM.plate) for (let i = 0; i < LEN; i++) { B.plate.L[i] = B.vox.L[i] * 0.5; B.plate.R[i] = B.vox.R[i] * 0.5; }
// the lyric stem (sung phrases + stitched lyric lines): its own group, almost dry
B.lyric = bus();
const LYR = VOX.replace(/\.wav$/, "-lyric.wav");
if (existsSync(LYR)) {
  const raw = execFileSync("ffmpeg", ["-loglevel", "error", "-i", LYR, "-f", "f32le", "-ac", "2", "-ar", String(SR), "-"], { maxBuffer: 1 << 30 });
  const v = new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4);
  for (let i = 0; i < Math.min(LEN, v.length / 2); i++) {
    B.lyric.L[i] += v[2 * i] * 1.6; B.lyric.R[i] += v[2 * i + 1] * 1.6;
    B.verb.L[i] += v[2 * i] * 0.12; B.verb.R[i] += v[2 * i + 1] * 0.12;
  }
}
const wet = reverb(B.verb, ...ROOM.verb);
const hall = reverb(B.hall, ...ROOM.hall); // the bells' space (club: a ~5 s hall)
const plate = ROOM.plate ? reverb(B.plate, 0.86, 0.4, 0.75) : null;
// stems: the mix in seven groups (out/stems-<arr>/), so bin/master.mjs can place them in space
const S = Object.fromEntries(["drums", "keys", "low", "lead", "bells", "vox", "lyric", "space"].map((k) => [k, bus()]));
const G = { drums: 1.0, keys: 0.9, bass: 0.9, pad: 1.0, lead: 1.0 };
const pump = ROOM.pump ?? 0;
for (let i = 0; i < LEN; i++) {
  const d = duck[i], dp = pump ? d ** pump : 0.5 + 0.5 * d;
  for (const ch of ["L", "R"]) {
    S.drums[ch][i] = B.drums[ch][i] * G.drums;
    S.keys[ch][i] = B.keys[ch][i] * G.keys * (0.6 + 0.4 * d);
    S.low[ch][i] = B.bass[ch][i] * G.bass * d + B.pad[ch][i] * G.pad * d;
    S.lead[ch][i] = B.lead[ch][i] * G.lead * (0.75 + 0.25 * d);
    S.bells[ch][i] = B.bells[ch][i] * (0.7 + 0.3 * d) + hall[ch][i] * ROOM.hallWet;
    S.vox[ch][i] = B.vox[ch][i] * (0.8 + 0.2 * d);
    S.lyric[ch][i] = B.lyric[ch][i];
    S.space[ch][i] = wet[ch][i] * ROOM.wet * dp + (plate ? plate[ch][i] * ROOM.plate * d ** 3 : 0);
  }
}
const L = new Float32Array(LEN), R = new Float32Array(LEN);
for (const st of Object.values(S)) for (let i = 0; i < LEN; i++) { L[i] += st.L[i]; R[i] += st.R[i]; }
{
  const dir = join(OUT, `stems-${ARR_NAME}`); mkdirSync(dir, { recursive: true });
  for (const [name, st] of Object.entries(S)) {
    const body = Buffer.alloc(LEN * 8);
    for (let i = 0; i < LEN; i++) { body.writeFloatLE(st.L[i], i * 8); body.writeFloatLE(st.R[i], i * 8 + 4); }
    const h = Buffer.alloc(44);
    h.write("RIFF", 0); h.writeUInt32LE(36 + body.length, 4); h.write("WAVEfmt ", 8); h.writeUInt32LE(16, 16);
    h.writeUInt16LE(3, 20); h.writeUInt16LE(2, 22); h.writeUInt32LE(SR, 24); h.writeUInt32LE(SR * 8, 28);
    h.writeUInt16LE(8, 32); h.writeUInt16LE(32, 34); h.write("data", 36); h.writeUInt32LE(body.length, 40);
    writeFileSync(join(dir, `${name}.wav`), Buffer.concat([h, body]));
  }
}
// gentle master: normalise peak, soft clip
let peak = 0; for (let i = 0; i < LEN; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
const g = 0.9 / peak;
const pcm = Buffer.alloc(LEN * 4);
for (let i = 0; i < LEN; i++) {
  const fade = i > LEN - SR ? (LEN - i) / SR : 1;
  pcm.writeInt16LE(Math.round(Math.tanh(L[i] * g * 1.1) * fade * 32000), i * 4);
  pcm.writeInt16LE(Math.round(Math.tanh(R[i] * g * 1.1) * fade * 32000), i * 4 + 2);
}
const head = Buffer.alloc(44);
head.write("RIFF", 0); head.writeUInt32LE(36 + pcm.length, 4); head.write("WAVEfmt ", 8);
head.writeUInt32LE(16, 16); head.writeUInt16LE(1, 20); head.writeUInt16LE(2, 22);
head.writeUInt32LE(SR, 24); head.writeUInt32LE(SR * 4, 28); head.writeUInt16LE(4, 32); head.writeUInt16LE(16, 34);
head.write("data", 36); head.writeUInt32LE(pcm.length, 40);
mkdirSync(OUT, { recursive: true });
const STEM = ARR_NAME === "country" ? "aesthetina" : `aesthetina-${ARR_NAME}`;
const wav = join(OUT, `${STEM}.wav`), mp3 = join(OUT, `${STEM}.mp3`);
writeFileSync(wav, Buffer.concat([head, pcm]));
execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", wav, "-af", "loudnorm=I=-12:TP=-1.2:LRA=9",
  "-ar", "44100", "-b:a", "320k", "-metadata", `title=Aesthetina${ARR_NAME === "country" ? "" : " (" + ARR_NAME + ")"}`, "-metadata", "artist=Aesthetic Dot Computer", mp3]);
console.log(`${wav}\n${mp3}\n${(LEN / SR).toFixed(1)} s · ${BPM} BPM · ${BARS} bars · peak ${peak.toFixed(2)}`);
