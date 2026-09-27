#!/usr/bin/env node
// climbalift.mjs — the Climb and the Lift, cut out of Note(s)pat(ial) Native
// and rebuilt as a big-room track: real progressions, a top line that
// behaves like a hook, electric guitar, and explosions that fly around the
// ring. Still the ring: five seats around the listener and a held voice at
// the center, every voice placed through the measured KEMAR head, so the
// hook hops seat to seat and the debris of each explosion circles the room.
//
//   node pop/notespatial/bin/climbalift.mjs [--out print.wav] [--no-guitar]
//
// Form (128 BPM, D major, 108 bars, 3:22):
//   intro 8 · verse 16 · pre 8 · CHORUS 16 · verse 8 · pre 8 · CHORUS 16 ·
//   bridge 8 · CHORUS 16 (tutti) · outro 4
// Harmony: verse Bm G D A · pre G A Bm A · chorus D D Bm Bm G G A D (the
// theme's own harmony) · bridge F#m D E A (the bright II). The chorus top
// line is the suite's theme, up a whole step; the verse, pre-chorus and
// bridge lines are new.
//
// Deterministic. Output: a 32-bit float stereo binaural print at 44.1 kHz,
// peak 0.95, for pop/notespatial/bin/master-v2.sh (ARC=0).

import { writeFileSync, readFileSync, mkdtempSync, existsSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { mixEventSupersaw } from '../../dance/synths/supersaw.mjs';
import { readWavMono } from '../../lib/wav.mjs';
import { mixEventWobble } from '../../dance/synths/wobble.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, '../../..');
const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const NO_GUITAR = args.includes('--no-guitar');
const OUT = resolve(opt('out', join(HERE, '../out/climbalift-print.wav')));

// ── clock ─────────────────────────────────────────────────────────────
const SR = 44100, BPM = 128, BEAT = 60 / BPM, BAR = 4 * BEAT;
const FORM = [['intro', 8], ['verse1', 8], ['pre1', 8], ['chorus1', 16], ['break1', 8], ['verse2', 8], ['pre2', 8], ['chorus2', 16], ['bridge', 8], ['break2', 8], ['chorus3', 16], ['outro', 4]];
// key shifts by section (semitones): the breaks jump a minor third and a fourth, the last chorus a whole step
const TR = { break1: 3, break2: 5, chorus3: 2, outro: 2 };
const SEC = {}; { let b = 0; for (const [n, bars] of FORM) { SEC[n] = { bar: b, t: b * BAR, bars, end: (b + bars) * BAR }; b += bars; } }
const BARS = FORM.reduce((a, [, b]) => a + b, 0), DUR = BARS * BAR + 4, N = Math.ceil(DUR * SR);
const hz = m => 440 * 2 ** ((m - 69) / 12);
let seed = 20260925; const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296 * 2 - 1;

// ── the room: 5 ring seats (0 front, clockwise), 5 = held center, 6 = sub ──
const RING = 5, CENTER = 5, SUB = 6, OUTPUTS = 7;
const seatAz = k => k === SUB || k === CENTER ? 0 : k / RING * 360;
const layer = () => Array.from({ length: OUTPUTS }, () => new Float32Array(N));
const L = { drums: layer(), sub: layer(), bass: layer(), walk: layer(), pad: layer(), lead: layer(), voice: layer(), guitar: layer(), fx: layer() };

// ── tiny additive synth (the suite's bell / pluck / pad recipes) ──────
function tone(buf, t, dur, f, g, { wave = 'sine', attack = .005, decay = dur * .6 } = {}) {
  const s0 = Math.round(t * SR), n = Math.round(dur * SR), a = Math.max(1, attack * SR), d = Math.max(1, decay * SR), inc = f / SR;
  let ph = rnd() * .5 + .5;
  for (let i = 0; i < n && s0 + i < N; i++) {
    const env = i < a ? i / a : i > n - d ? Math.exp(-4 * (i - (n - d)) / d) : 1;
    ph += inc; if (ph >= 1) ph -= 1;
    const v = wave === 'sine' ? Math.sin(ph * 2 * Math.PI) : wave === 'triangle' ? 4 * Math.abs(ph - .5) - 1 : wave === 'saw' ? 2 * ph - 1 : rnd();
    buf[s0 + i] += v * env * g;
  }
}
const bell = (buf, t, dur, m, g) => { tone(buf, t, dur, hz(m), g, { attack: .004, decay: dur * .8 }); tone(buf, t, dur * .55, hz(m) * 2, g * .28, { attack: .002, decay: dur * .45 }); tone(buf, t, dur * .3, hz(m) * 3, g * .1, { attack: .002, decay: dur * .25 }); tone(buf, t, dur, hz(m) * 2 ** (18 / 1200), g * .45, { attack: .45, decay: dur * .7 }); };
const pluck = (buf, t, dur, m, g) => { tone(buf, t, dur, hz(m), g, { attack: .003, decay: dur * .78 }); tone(buf, t, dur * .5, hz(m) * 2, g * .22, { attack: .002, decay: dur * .42 }); };
const saw = (buf, t, dur, m, g, preset = 'lead') => mixEventSupersaw({ startSec: t, midi: m, gain: g, durSec: dur }, buf, { sampleRate: SR, preset });

// ── the wub ───────────────────────────────────────────────────────────
function wub(t, dur, m, g, preset = 'bomp') {
  const tmp = new Float32Array(Math.ceil((dur + 1) * SR));
  mixEventWobble({ startSec: 0, midi: m, durSec: dur, gain: g }, tmp, { preset, bpm: BPM, sampleRate: SR });
  const s0 = Math.round(t * SR);
  for (let i = 0; i < tmp.length && s0 + i < N; i++) { L.sub[SUB][s0 + i] += tmp[i] * .8; L.bass[2][s0 + i] += tmp[i] * .45; L.bass[3][s0 + i] += tmp[i] * .45; }
}
function wubs(sec, prog, { tr = 0, preset = 'bomp', from = 0, to = sec.bars, g = .5, len = 2 } = {}) { // one wub per half bar on the root
  for (let bar = from; bar < to; bar++) for (const b of len === 2 ? [0, 2] : [0]) wub(sec.t + bar * BAR + b * BEAT, len * BEAT * .96, chordOf(prog, bar, tr)[0] - 24, g, preset);
}
// ── glock: a bright bar struck hard — fundamental, 2.76 and 5.4 partials, fast ring ──
const glock = (buf, t, dur, m, g) => { const f = hz(m); tone(buf, t, Math.max(dur, .22), f, g, { attack: .001, decay: .2 }); tone(buf, t, .12, f * 2.76, g * .5, { attack: .001, decay: .1 }); tone(buf, t, .07, f * 5.4, g * .25, { attack: .001, decay: .06 }); tone(buf, t, .006, 6000, g * .3, { wave: 'noise', attack: .0005, decay: .005 }); };
// ── drums ─────────────────────────────────────────────────────────────
const kicks = [];
function kick(t, g = 1) {
  kicks.push(t);
  const s0 = Math.round(t * SR); let ph = 0;
  for (let i = 0; i < SR * .42 && s0 + i < N; i++) { // body: 160 → 42 Hz in 60 ms, then the boom
    const x = i / SR, f = 42 + 118 * Math.exp(-x / .028); ph += f / SR;
    const env = Math.exp(-x / .16) * Math.min(1, i / 8);
    const v = Math.sin(ph * 2 * Math.PI);
    L.drums[0][s0 + i] += v * env * .9 * g;                        // front seat
    L.sub[SUB][s0 + i] += v * env * .42 * g * (f < 90 ? 1 : .3);     // the sub cabinet takes the boom
  }
  tone(L.drums[0], t, .012, 3200, .35 * g, { wave: 'noise', attack: .0005, decay: .01 }); // the click
}
function clap(t, g = 1) {
  for (const [dt, gg] of [[0, .6], [.011, .7], [.023, .8], [.034, 1]]) for (const k of [2, 3]) tone(L.drums[k], t + dt, .06 + (gg === 1 ? .16 : 0), 0, .5 * gg * g, { wave: 'noise', attack: .001, decay: gg === 1 ? .13 : .04 });
  tone(L.drums[2], t, .09, 190, .18 * g, { wave: 'triangle', attack: .001, decay: .07 });
}
let hatHop = 0;
const hat = (t, open = false, g = 1) => { const k = hatHop++ % RING; tone(L.drums[k], t, open ? .16 : .035, 0, (open ? .28 : .18) * g, { wave: 'noise', attack: .0005, decay: open ? .13 : .025 }); };
const crash = (t, g = 1) => { for (let k = 0; k < RING; k++) tone(L.fx[k], t + k * .012, 1.4, 0, .28 * g, { wave: 'noise', attack: .002, decay: 1.1 }); };
function roll(t0, bars) { // snare roll: eighths → sixteenths → thirty-seconds, rising
  const t1 = t0 + bars * BAR; let t = t0, n = 0;
  while (t < t1) { const u = (t - t0) / (bars * BAR), step = u < .5 ? BEAT / 2 : u < .8 ? BEAT / 4 : BEAT / 8; clap(t, .25 + .6 * u); t += step; n++; }
}
function riser(t0, bars, g = 1) { // noise sweeping up, spinning around the ring one seat per beat
  const s0 = Math.round(t0 * SR), n = Math.round(bars * BAR * SR); let lp = 0;
  for (let i = 0; i < n && s0 + i < N; i++) {
    const u = i / n, w = rnd(); lp += (.02 + .9 * u * u) * (w - lp);
    const seat = Math.floor((i / SR) / BEAT) % RING;
    L.fx[seat][s0 + i] += (w - lp) * u * u * .45 * g;
  }
}
// The spatial explosion: a boom and a bright blast at one seat, then the
// debris — bright ticks and a falling shriek — thrown around the ring seat
// by seat, and a supersaw scream that drops in pitch.
function explode(t, seat, size = 1) {
  const s0 = Math.round(t * SR); let ph = 0, lp = 0;
  for (let i = 0; i < SR * 1.1 * size && s0 + i < N; i++) {
    const x = i / SR, f = 32 + 90 * Math.exp(-x / .05); ph += f / SR;
    const boom = Math.sin(ph * 2 * Math.PI) * Math.exp(-x / (.35 * size));
    const w = rnd(); lp += .12 * (w - lp);
    const blast = (w * Math.exp(-x / .06) + lp * Math.exp(-x / (.5 * size))) * .9;
    L.fx[seat][s0 + i] += (boom * .8 + blast * .6) * size;
    L.sub[SUB][s0 + i] += boom * .4 * size;
  }
  for (let d = 0; d < 9; d++) { // the debris circles the room, clockwise, fading
    const k = (seat + 1 + d) % RING, td = t + .09 + d * (.075 + .02 * d);
    tone(L.fx[k], td, .09, 0, .32 * size * (1 - d / 10), { wave: 'noise', attack: .001, decay: .06 });
    tone(L.fx[k], td, .12, 2400 * 2 ** (-d / 3), .12 * size * (1 - d / 10), { wave: 'saw', attack: .002, decay: .09 });
  }
  for (const [dt, m, g] of [[0, 98, .5], [.06, 93, .4], [.13, 86, .35]]) saw(L.fx[(seat + 2) % RING], t + dt, .5, m, g * size, 'stab'); // the scream
}

// ── harmony ───────────────────────────────────────────────────────────
const CH = { D: [62, 66, 69], A: [57, 61, 64], Bm: [59, 62, 66], G: [55, 59, 62], 'F#m': [54, 57, 61], E: [52, 56, 59], Em: [52, 55, 59], 'F#': [54, 58, 61] };
const PROG = { verse: ['Bm', 'Bm', 'G', 'G', 'D', 'D', 'A', 'A'], pre: ['G', 'A', 'Bm', 'A', 'G', 'A', 'Bm', 'A'], chorus: ['Bm', 'Bm', 'G', 'G', 'D', 'D', 'A', 'Bm'], bridge: ['F#m', 'F#m', 'D', 'D', 'Em', 'Em', 'F#', 'F#'] };
const chordAt = (prog, bar) => PROG[prog][bar % 8];
const chordOf = (prog, bar, tr = 0) => CH[chordAt(prog, bar)].map(m => m + tr);
// the guitar's shapes, as the strings sound them (low to high), so a key shift is a barre
const SHAPE = { D: [50, 57, 62, 66], A: [45, 52, 57, 61, 64], Bm: [47, 54, 59, 62, 66], G: [43, 47, 50, 55, 59, 67], 'F#m': [42, 49, 54, 57, 61, 66], E: [40, 47, 52, 56, 59, 64], Em: [40, 47, 52, 55, 59, 64], 'F#': [42, 49, 54, 58, 61, 66] };
const gtrChords = (prog, tr = 0) => PROG[prog].map(c => SHAPE[c].map(m => m + tr).join(',')).join('|');

// ── lines: [beat within the 8-bar block, beats, midi] ─────────────────
const THEME = [ // the suite's theme, up a whole step
  [[0, 1, 74], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 78], [3.5, .5, 76], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
  [[0, 1, 78], [1, .5, 81], [1.5, .5, 83], [2, 1, 81], [3, 1, 78], [4, 1.5, 76], [5.5, .5, 74], [6, 2, 71]],
  [[0, 1, 74], [1, 1, 79], [2, .5, 81], [2.5, .5, 83], [3, 1, 81], [4, 1, 79], [5, .5, 78], [5.5, .5, 76], [6, 2, 74]],
  [[0, 1, 76], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 85], [3.5, .5, 81], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
].flatMap((p, i) => p.map(([a, d, m]) => [i * 8 + a, d, m]));
const VERSE = [ // over Bm Bm G G D D A A
  [0, .5, 74], [.5, .5, 76], [1, 1, 78], [2.5, .5, 76], [3, 1, 74], [4.5, .5, 71], [5, 1.5, 74], [7, 1, 76],
  [8, .5, 79], [8.5, .5, 78], [9, 1, 76], [10.5, .5, 74], [11, 1, 71], [12.5, 1, 74], [14, 2, 78],
  [16, .5, 81], [16.5, .5, 78], [17, 1, 74], [18.5, .5, 76], [19, 1, 78], [20.5, .5, 78], [21, 1.5, 81], [23, 1, 79],
  [24, .5, 81], [24.5, .5, 79], [25, 1, 78], [26.5, .5, 76], [27, 1, 73], [28.5, 1, 76], [30, 2, 73],
];
const VERSE_B = VERSE.map(([a, d, m], i) => i >= VERSE.length - 2 ? [a, d, m + 5] : [a, d, m]); // second time: ends up, into the pre
const PRE = [ // over G A Bm A ×2, rising registers (the Climb)
  [0, 1, 67], [1, 1, 71], [2, 1, 74], [3, 1, 79], [4, 1, 69], [5, 1, 73], [6, 1, 76], [7, 1, 81],
  [8, 1, 71], [9, 1, 74], [10, 1, 78], [11, 1, 83], [12, 2, 81], [14, 1, 79], [15, 1, 78],
  [16, 1, 79], [17, 1, 83], [18, 1, 86], [19, 1, 91], [20, 1, 81], [21, 1, 85], [22, 1, 88], [23, 1, 93],
  [24, 1, 83], [25, 1, 86], [26, 1, 90], [27, 1, 95], [28, 3, 93], [31, 1, 91],
];
const BRIDGE = [ // over F#m F#m D D E E A A
  [0, 2, 73], [2, 1, 71], [3, 1, 69], [4, 1, 69], [5, 1, 66], [6, 2, 69],
  [8, 1, 74], [9, 1, 78], [10, 2, 81], [12, 1.5, 79], [13.5, .5, 78], [14, 2, 76],
  [16, 2, 80], [18, 1, 78], [19, 1, 76], [20, 1, 76], [21, 1, 73], [22, 2, 80],
  [24, 1, 81], [25, 1, 83], [26, 2, 85], [28, 4, 81],
];
const WEAVE = (bar, chord) => { const deg = bar % 4 < 2 ? [2, 1, 0] : [0, 1, 2]; return [.5, 2, 3.5].map((b, k) => [b, chord[deg[k]] + 12]); }; // the Climb's 3+3+2 line

// ── placement helpers ─────────────────────────────────────────────────
let hookHop = 0;
const hookSeat = (t, sec, spinFrom = Infinity) => { // one seat per bar; in the spin, per half-bar then per beat
  const u = t - sec.t, bar = Math.floor(u / BAR);
  if (bar < spinFrom) return bar % RING;
  const x = u - spinFrom * BAR, steps = Math.floor(x / (BEAT * (x < 2 * BAR ? 2 : 1)));
  return (spinFrom + steps) % RING;
};
const play = (line, t0, seatOf, inst, g, durScale = .92) => { for (const [a, d, m] of line) inst(seatOf(t0 + a * BEAT), t0 + a * BEAT, d * BEAT * durScale, m, g); };
const ghastly = line => line.map(([a, d, m]) => [a, d, a >= 24 && a < 32 && m % 12 === 9 ? m + 1 : m]); // over the last two bars' F#, A → A#
const lead = (seat, t, dur, m, g) => { saw(L.lead[seat], t, dur, m, g); saw(L.lead[seat], t, dur, m + 12, g * .28); tone(L.lead[seat], t, dur, hz(m) * 2, g * .12, { attack: .01, decay: dur * .6 }); }; // shrill: an octave and a bright sine on top
let GHOST = true;
const voice = (seat, t, dur, m, g) => { bell(L.voice[CENTER], t, dur, m, g); if (GHOST) ghost(t, dur, m, g); };
// SALEM: under every sung note an octave-down copy, 28 cents flat, slow to arrive and warbling — the screwed voice — placed behind the listener;
// above it a high shimmer, an octave and a twelfth up, slower still, at the front pair.
function ghost(t, dur, m, g) {
  const f = hz(m - 12) * 2 ** (-28 / 1200), d = Math.max(dur, 1.2 * BEAT);
  for (const k of [2, 3]) { tone(L.pad[k], t + .02, d * 1.3, f * (k === 2 ? 1 : 2 ** (9 / 1200)), g * .26, { wave: 'triangle', attack: .35, decay: d * .7 }); tone(L.pad[k], t + .02, d * 1.3, f * 2, g * .06, { attack: .4, decay: d * .6 }); }
  for (const [k, mul, gg] of [[4, 2, .07]]) tone(L.pad[k], t + .05, d * 1.6, hz(m) * mul, g * gg, { attack: .5, decay: d * .9 });
}
// a drone behind you: two low triangles a few cents apart, beating slowly
function drone(t0, bars, m, g = .16) { for (const [k, cents] of [[2, 0], [3, 7]]) { tone(L.pad[k], t0, bars * BAR, hz(m) * 2 ** (cents / 1200), g, { wave: 'triangle', attack: 2 * BEAT, decay: 2 * BAR }); tone(L.pad[k], t0, bars * BAR, hz(m) * 2 * 2 ** (-cents / 1200), g * .5, { attack: 3 * BEAT, decay: 2 * BAR }); } }
// wind: filtered noise breathing in slow swells, drifting around the ring
function wind(t0, bars, g = .05) {
  const s0 = Math.round(t0 * SR), n = Math.round(bars * BAR * SR); let lp = 0, hp = 0, c = .02;
  for (let i = 0; i < n && s0 + i < N; i++) {
    const x = i / SR, w = rnd(); if ((i & 1023) === 0) c = .01 + .08 * (.5 + .5 * Math.sin(x * .37 + 1.3)) * (.6 + .4 * Math.sin(x * .11));
    lp += c * (w - lp); hp += .002 * (lp - hp);
    const swell = .5 + .5 * Math.sin(x / (2 * BAR) * Math.PI * 2 - Math.PI / 2), seat = Math.floor(x / (2 * BAR)) % RING;
    L.fx[seat][s0 + i] += (lp - hp) * swell * g * Math.min(1, x / 2, (bars * BAR - x) / 2);
  }
}
// a reversed cymbal into a downbeat: bright noise swelling exponentially, from behind to the front
function reverseCrash(t1, bars = 1, g = .5) {
  const s0 = Math.round((t1 - bars * BAR) * SR), n = Math.round(bars * BAR * SR); let hp = 0;
  for (let i = 0; i < n && s0 + i < N; i++) { const u = i / n, w = rnd(); hp += .3 * (w - hp); const seat = u < .5 ? 3 : u < .8 ? 2 : 0; L.fx[seat][s0 + i] += (w - hp) * Math.exp(6 * (u - 1)) * g; }
}
// tape stop: a chord dropping two octaves in a second and a half, as the room falls out
function tapeStop(t, chord, g = .35) {
  for (const [k, m] of chord.entries()) { const s0 = Math.round(t * SR), n = Math.round(1.5 * SR), f0 = hz(m + 12); let ph = 0;
    for (let i = 0; i < n && s0 + i < N; i++) { const u = i / n, f = f0 * 2 ** (-2 * u * u); ph += f / SR; L.pad[(k * 2) % RING][s0 + i] += (2 * (ph % 1) - 1) * (1 - u) * g * .5 + Math.sin(ph * 2 * Math.PI) * (1 - u) * g * .5; } }
}

// ── sections ──────────────────────────────────────────────────────────
function drums(sec, { four = true, claps = true, hats = true, open = true, from = 0, to = sec.bars, half = false } = {}) {
  for (let bar = from; bar < to; bar++) for (let b = 0; b < 4; b++) {
    const t = sec.t + bar * BAR + b * BEAT;
    if (four && (!half || b % 2 === 0)) kick(t, half ? .8 : 1);
    if (claps && (b === 1 || b === 3)) clap(t, half ? .6 : .9);
    if (hats) { hat(t, false, .45); if (open) hat(t + BEAT / 2, true, .7); }
  }
}
function bassline(sec, prog, { style = 'eighths', from = 0, to = sec.bars, tr = 0 } = {}) {
  for (let bar = from; bar < to; bar++) {
    const root = chordOf(prog, bar, tr)[0] - 24, t0 = sec.t + bar * BAR;
    if (style === 'bounce') for (const b of [.5, 1.5, 2.5, 3.5]) { saw(L.bass[3][0] ? L.bass[3] : L.bass[3], t0 + b * BEAT, .42 * BEAT, root + 12, .55, 'stab'); tone(L.sub[SUB], t0 + b * BEAT, .45 * BEAT, hz(root), .38, { attack: .004, decay: .3 * BEAT }); }
    else if (style === 'eighths') for (let b = 0; b < 4; b += .5) { tone(L.sub[SUB], t0 + b * BEAT, .45 * BEAT, hz(root), .32, { attack: .004, decay: .3 * BEAT }); tone(L.bass[3], t0 + b * BEAT, .45 * BEAT, hz(root + 12), .12, { wave: 'saw', attack: .004, decay: .3 * BEAT }); }
    else if (style === 'long') { tone(L.sub[SUB], t0, 3.8 * BEAT, hz(root), .45, { attack: .02, decay: 2 * BEAT }); tone(L.bass[3], t0, 3.8 * BEAT, hz(root + 12), .18, { wave: 'triangle', attack: .02, decay: 2 * BEAT }); }
  }
}
let walkHop = 0;
function walk(sec, prog, { from = 0, to = sec.bars, g = .3, tr = 0 } = {}) { // chord tones in eighths, hopping seats
  for (let bar = from; bar < to; bar++) { const c = chordOf(prog, bar, tr); for (let e = 0; e < 8; e++) { const m = c[[0, 1, 2, 1][e % 4]] + 12 + (e >= 4 ? 12 : 0); pluck(L.walk[walkHop++ % RING], sec.t + bar * BAR + e * BEAT / 2, .55 * BEAT, m, g); } }
}
function pads(sec, prog, { from = 0, to = sec.bars, g = .16, tr = 0 } = {}) {
  for (let bar = from; bar < to; bar += 2) { const c = chordOf(prog, bar, tr); c.forEach((m, k) => saw(L.pad[[1, 4, 2][k]], sec.t + bar * BAR, 2 * BAR * .98, m, g, 'pad')); }
}
function weave(sec, prog, { from = 0, to = sec.bars, g = .2 } = {}) {
  for (let bar = from; bar < to; bar++) for (const [k, [b, m]] of WEAVE(bar, CH[chordAt(prog, bar)]).entries()) tone(L.walk[(RING * 8 - bar * 2 - k) % RING], sec.t + bar * BAR + b * BEAT, .38 * BEAT, hz(m), g, { wave: 'triangle', attack: .006, decay: .25 * BEAT });
}
function guitar(sec, chords, { pattern = 'D..UD.DU', drive = .85, mute = 'open', bars = sec.bars, g = .5, seeds = [1, 2], seats = [1, 4], damp = .15 } = {}) {
  if (NO_GUITAR) return;
  const bin = join(ROOT, 'pop/guitar/c/strum');
  if (!existsSync(bin)) { console.warn('no strum binary; skipping guitar'); return; }
  seeds.forEach((sd, i) => {
    const wav = join(WORK, `gtr-${sec.bar}-${i}.wav`);
    const r = spawnSync(bin, ['--chord', chords, '--pattern', pattern, '--bpm', String(BPM), '--bars', String(bars), '--electric', '--drive', String(drive), '--mute', mute, '--damp', String(damp), '--sr', String(SR), '--seed', String(sd), '--tail', '1.5', '--out', wav], { encoding: 'utf8' });
    if (r.status !== 0) { console.warn('strum failed', r.stderr); return; }
    const { samples: mono } = readWavMono(wav), s0 = Math.round(sec.t * SR), dst = L.guitar[seats[i]];
    let pk = 0; for (const v of mono) pk = Math.max(pk, Math.abs(v));
    const norm = pk > 0 ? g / pk : 0;
    for (let n = 0; n < mono.length && s0 + n < N; n++) dst[s0 + n] += mono[n] * norm;
  });
}
const WORK = mkdtempSync(join(tmpdir(), 'climbalift-'));

// intro: the held voice states the hook alone, the pad swells, the kick walks in
{ const s = SEC.intro;
  play(THEME.filter(([a]) => a < 16), s.t, () => CENTER, voice, .5);
  play(THEME.filter(([a]) => a >= 16), s.t, () => CENTER, voice, .55);
  pads(s, 'chorus', { g: .1 });
  drums(s, { from: 4, claps: false, open: false });
  riser(s.t + 6 * BAR, 2, .6); drone(s.t, 8, 38, .1); reverseCrash(s.end, 1, .4); }
// verse 1
{ const s = SEC.verse1;
  drums(s, { open: false, claps: false, to: 4 }); drums(s, { open: false, from: 4 });
  bassline(s, 'verse');
  play(VERSE_B, s.t, () => CENTER, voice, .5);
  guitar(s, gtrChords('verse'), { pattern: 'DDDDDDDD', mute: 'palm', drive: .7, g: .4 }); drone(s.t, 8, 35, .12); }
// pre-chorus 1: the Climb — registers stack, the 3+3+2 line weaves, the roll, the silence, the blast
function pre(s, size) {
  drums(s, { open: false, to: 6 }); bassline(s, 'pre', { style: 'eighths', to: 7 });
  play(PRE.filter(([a]) => a < 16), s.t, t => hookSeat(t, s), (k, t, d, m, g) => pluck(L.walk[k], t, d, m, g), .4);
  play(PRE.filter(([a]) => a >= 16), s.t, t => hookSeat(t, s), lead, .32 * size);
  weave(s, 'pre', { to: 7, g: .14 });
  roll(s.t + 6 * BAR, 1.75); riser(s.t + 4 * BAR, 3.75, size);
  explode(s.end - .5 * BEAT, 0, .5 * size); reverseCrash(s.end, 2, .55 * size); // the door
}
pre(SEC.pre1, 1);
// chorus: the hook in shrill saws hopping the ring, the voice under it, the bounce, power chords, explosions
function chorus(s, { answer = false, spin = false, lift = false, tutti = false, tr = 0 } = {}) {
  explode(s.t, 0, 1.3); crash(s.t);
  drums(s, { open: true });
  GHOST = false;
  for (let bar = 0; bar < s.bars; bar++) { const root = chordOf('chorus', bar, tr)[0] - 24, t0 = s.t + bar * BAR; for (const b of [.5, 1.5, 2.5, 3.5]) tone(L.sub[SUB], t0 + b * BEAT, .45 * BEAT, hz(root), .34, { attack: .004, decay: .3 * BEAT }); }
  if (answer) pads(s, 'chorus', { g: .12, tr });
  wubs(s, 'chorus', { tr, preset: 'bomp', g: .5 });
  const seatOf = t => hookSeat(t, s, spin ? 12 : Infinity);
  const line = THEME.map(([a, d, m]) => [a, d, (lift && a % 8 >= 4 ? m + 12 : m) + tr]);
  play(ghastly(line), s.t, seatOf, lead, .5); play(ghastly(line), s.t + 8 * BAR, seatOf, lead, .52);
  play(line, s.t, () => CENTER, voice, .38); play(line, s.t + 8 * BAR, () => CENTER, voice, .4);
  if (answer) { const ans = line.filter(([a]) => a % 8 >= 4).map(([a, d, m]) => [a + .5, d * .6, m - 12]); const back = t => (RING - hookSeat(t, s)) % RING; play(ans, s.t, back, (k, t, d, m, g) => saw(L.lead[k], t, d, m, g, 'stab'), .3); play(ans, s.t + 8 * BAR, back, (k, t, d, m, g) => saw(L.lead[k], t, d, m, g, 'stab'), .32); }
  guitar(s, gtrChords('chorus', tr), { pattern: 'D...D.D.', drive: .9, g: .42 });
  for (let bar = 8; bar < s.bars; bar += 8) explode(s.t + bar * BAR, (bar / 4) % RING, 1);
  GHOST = true;
  if (tutti) { const t = s.end - 2 * BAR; for (let k = 0; k < RING; k++) saw(L.lead[k], t, 2 * BAR, [59, 62, 66, 71, 74][k] + tr, .4); bell(L.voice[CENTER], t, 2 * BAR, 83 + tr, .5); wub(t, 2 * BAR, 35 + tr, .6, 'reese'); explode(t, 2, 1.5); }
}
chorus(SEC.chorus1);
function glockBreak(s, tr, preset) {
  explode(s.t, 1, 1.2); crash(s.t);
  for (let bar = 0; bar < s.bars; bar++) for (let e = 0; e < 16; e++) { // the kit at double time: kicks on a 16th pattern, snares on every offbeat eighth, hats on every 16th
    const t = s.t + bar * BAR + e * BEAT / 4;
    if ([1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0][e]) kick(t, .95);
    if (e % 4 === 2) clap(t, .8);
    hat(t, e % 4 === 2, .45);
  }
  const prog = 'chorus', arp = []; // chord tones over three octaves, up then down, in 32nds — the glock runs the ring one seat a note
  for (let bar = 0; bar < s.bars; bar++) { const c = chordOf(prog, bar, tr); const up = [0, 1, 2].flatMap(o => c.map(m => m + 12 * o + 12)); const seq = bar % 2 ? [...up, ...up.slice().reverse()] : [...up.slice().reverse(), ...up]; for (let n = 0; n < 32; n++) { const t = s.t + bar * BAR + n * BEAT / 8; glock(L.walk[(hookHop++) % RING], t, BEAT / 8, seq[n % seq.length] + (n >= 16 && bar % 4 === 3 ? 12 : 0), .26); } }
  wubs(s, prog, { tr, preset, g: .55 });
  guitar(s, gtrChords(prog, tr), { pattern: 'DDDDDDDD', drive: .95, g: .5, damp: .3 });
  if (preset === 'bomp') { GHOST = false; play(THEME.filter(([a]) => a < 16).map(([a, d, m]) => [a * .5, d * .5, m + tr]), s.t + 4 * BAR, () => CENTER, voice, .45); GHOST = true; } // the hook at double speed, once, in the new key
  riser(s.t + 6 * BAR, 2, 1); reverseCrash(s.end, 1, .5);
}
glockBreak(SEC.break1, TR.break1, 'row');
// verse 2: breakdown — kick and hats only, clean picked guitar, echoes of the verse line
{ const s = SEC.verse2;
  drums(s, { claps: false, open: false, half: true }); bassline(s, 'verse', { style: 'long' });
  play(VERSE_B.filter(([a]) => a < 32), s.t, () => CENTER, voice, .5); for (let k = 0; k < 8; k++) bell(L.voice[(k * 2) % RING === CENTER ? 0 : (k * 2) % RING], s.t + 7 * BAR + 2 * BEAT + k * BEAT / 4, BEAT / 4, 73, .4 * (1 - k / 9)); // the stutter into the pre
  guitar(s, gtrChords('verse'), { pattern: 'D.d.U.d.', drive: .25, g: .38, damp: .05 }); tapeStop(s.t, CH.Bm, .4); drone(s.t, 8, 35, .14); wind(s.t, 8, .04); }
pre(SEC.pre2, 1.15);
chorus(SEC.chorus2, { answer: true, spin: true });
// bridge: half-time, the bright II, the voice alone with pads and guitar swells, then the last riser
{ const s = SEC.bridge;
  drums(s, { half: true, hats: false, claps: true, to: 6 }); bassline(s, 'bridge', { style: 'long' }); pads(s, 'bridge', { g: .2 });
  play(BRIDGE, s.t, () => CENTER, voice, .55);
  guitar(s, gtrChords('bridge'), { pattern: 'D.......', drive: .45, g: .42, damp: .02 });
  roll(s.t + 6 * BAR, 1.75); riser(s.t + 4 * BAR, 3.75, 1.2); explode(s.end - .5 * BEAT, 3, .6); tapeStop(s.t, CH['F#m'], .4); drone(s.t, 8, 42, .12); wind(s.t, 8, .04); reverseCrash(s.end, 2, .6); }
glockBreak(SEC.break2, TR.break2, 'bomp');
chorus(SEC.chorus3, { answer: true, spin: true, lift: true, tutti: true, tr: TR.chorus3 });
// outro: the hook's last five notes handed back around the ring, backwards; one knock from behind
{ const s = SEC.outro;
  const last = THEME.slice(-5).reverse();
  last.forEach(([, , m], k) => pluck(L.walk[(RING - 1 - k) % RING], s.t + k * BEAT * 1.5, BEAT * 1.4, m + TR.outro, .4));
  bell(L.voice[CENTER], s.t + 8 * BEAT, 4 * BEAT, 74 + TR.outro, .45);
  tone(L.drums[3], s.t + 13 * BEAT, .27, 130, .35, { attack: .012, decay: .22 }); drone(s.t, 4, 38, .12); wind(s.t, 4, .06); }

// ── sidechain: every kick ducks the pads, bass, guitar and (lightly) the lead ──
const duck = new Float32Array(N).fill(1);
for (const t of kicks) { const s0 = Math.round(t * SR); for (let i = 0; i < SR * .32 && s0 + i < N; i++) { const x = i / SR, g = x < .012 ? 1 - .85 * (x / .012) : .15 + .85 * (1 - Math.exp(-(x - .012) / .09)) ** 1; duck[s0 + i] = Math.min(duck[s0 + i], Math.min(1, g)); } }
const apply = (lay, depth) => { for (const buf of lay) for (let i = 0; i < N; i++) buf[i] *= 1 - depth * (1 - duck[i]); };
apply(L.pad, 1); apply(L.bass, .9); apply(L.guitar, .75); apply(L.lead, .3); apply(L.walk, .5); apply(L.sub, .5);

// ── mix the layers into seat feeds ────────────────────────────────────
const MIX = { drums: 1, sub: .85, bass: .95, walk: .8, pad: .95, lead: .85, voice: .9, guitar: .8, fx: .95 };
// the level arc: verses and the bridge sit under the choruses; ramps over the last two beats before each section
const ARC = { intro: .55, verse1: .72, pre1: .85, chorus1: 1, break1: 1, verse2: .6, pre2: .9, chorus2: 1, bridge: .62, break2: 1.02, chorus3: 1.05, outro: .5 };
const arcAt = t => { let g = ARC.intro; for (const [n] of FORM) { const s = SEC[n], u = (t - (s.t - 2 * BEAT)) / (2 * BEAT); if (u >= 1) g = ARC[n]; else if (u > 0) g = g + (ARC[n] - g) * u; } return g; };
const arc = new Float32Array(N); for (let i = 0; i < N; i += 64) { const g = arcAt(i / SR); for (let j = i; j < Math.min(N, i + 64); j++) arc[j] = g; }
const feeds = layer();
for (const [name, lay] of Object.entries(L)) for (let k = 0; k < OUTPUTS; k++) { const src = lay[k], dst = feeds[k], g = MIX[name]; for (let i = 0; i < N; i++) dst[i] += src[i] * g * arc[i]; }
const rms = feeds.map(f => { let e = 0; for (const v of f) e += v * v; return (10 * Math.log10(e / f.length + 1e-12)).toFixed(1); });
console.log('feed RMS dBFS  ring', rms.slice(0, RING).join(' '), ' C', rms[CENTER], ' SUB', rms[SUB]);

// ── the head: measured KEMAR, seat energies equalized (as notespatial-native-render.mjs) ──
const binPath = join(ROOT, 'fedac/native/tools/hrir/kemar-compact.bin'), idxPath = join(ROOT, 'fedac/native/tools/hrir/kemar-compact.json');
const idx = JSON.parse(readFileSync(idxPath, 'utf8')), raw = readFileSync(binPath), taps = idx.taps, el0 = idx.elevations.find(e => e.el === 0);
const ir = azDeg => {
  let az = ((azDeg % 360) + 360) % 360, swap = false; if (az > 180) { az = 360 - az; swap = true; }
  const a = el0.azimuths.reduce((b, c) => Math.abs(c.az - az) < Math.abs(b.az - az) ? c : b);
  const Lr = new Float32Array(taps), Rr = new Float32Array(taps);
  for (let i = 0; i < taps; i++) { Lr[i] = raw.readInt16LE(a.off + i * 2) / 32768; Rr[i] = raw.readInt16LE(a.off + taps * 2 + i * 2) / 32768; }
  let e = 0; for (let i = 0; i < taps; i++) e += Lr[i] * Lr[i] + Rr[i] * Rr[i];
  const gEq = Math.sqrt(1.84 / e); for (let i = 0; i < taps; i++) { Lr[i] *= gEq; Rr[i] *= gEq; }
  return swap ? [Rr, Lr] : [Lr, Rr];
};
const inputs = [], graph = [];
for (let k = 0; k < OUTPUTS; k++) {
  writeFileSync(join(WORK, `seat${k}.f32`), Buffer.from(feeds[k].buffer));
  const [Lr, Rr] = ir(seatAz(k)); writeFileSync(join(WORK, `ir${k}L.f32`), Buffer.from(Lr.buffer)); writeFileSync(join(WORK, `ir${k}R.f32`), Buffer.from(Rr.buffer));
  inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(WORK, `seat${k}.f32`));
}
for (let k = 0; k < OUTPUTS; k++) for (const ear of 'LR') inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(WORK, `ir${k}${ear}.f32`));
for (let k = 0; k < OUTPUTS; k++) { const w = k === CENTER ? 1.1 : 1; graph.push(`[${k}:a]asplit[s${k}a][s${k}b]`, `[s${k}a][${OUTPUTS + k * 2}:a]afir=gtype=none:dry=1:wet=${w}[l${k}]`, `[s${k}b][${OUTPUTS + k * 2 + 1}:a]afir=gtype=none:dry=1:wet=${w}[r${k}]`); }
graph.push(Array.from({ length: OUTPUTS }, (_, k) => `[l${k}]`).join('') + `amix=inputs=${OUTPUTS}:normalize=0[L]`, Array.from({ length: OUTPUTS }, (_, k) => `[r${k}]`).join('') + `amix=inputs=${OUTPUTS}:normalize=0[R]`, '[L][R]join=inputs=2:channel_layout=stereo[out]');
const mixed = join(WORK, 'binaural.f32');
const r = spawnSync('ffmpeg', ['-y', '-v', 'error', ...inputs, '-filter_complex', graph.join(';'), '-map', '[out]', '-f', 'f32le', '-c:a', 'pcm_f32le', mixed], { stdio: 'inherit' });
if (r.status !== 0) throw Error('ffmpeg binaural mix failed');
const st = new Float32Array(readFileSync(mixed).buffer), n = st.length >> 1;
let pk = 0; for (const v of st) pk = Math.max(pk, Math.abs(v));
const g = .95 / pk, pcm = Buffer.alloc(44 + n * 8);
pcm.write('RIFF', 0); pcm.writeUInt32LE(36 + n * 8, 4); pcm.write('WAVE', 8); pcm.write('fmt ', 12); pcm.writeUInt32LE(16, 16); pcm.writeUInt16LE(3, 20); pcm.writeUInt16LE(2, 22); pcm.writeUInt32LE(SR, 24); pcm.writeUInt32LE(SR * 8, 28); pcm.writeUInt16LE(8, 32); pcm.writeUInt16LE(32, 34); pcm.write('data', 36); pcm.writeUInt32LE(n * 8, 40);
for (let i = 0; i < n; i++) { pcm.writeFloatLE(st[i * 2] * g, 44 + i * 8); pcm.writeFloatLE(st[i * 2 + 1] * g, 48 + i * 8); }
writeFileSync(OUT, pcm);
console.log(`${BARS} bars · ${(BARS * BAR / 60) | 0}:${String(Math.round(BARS * BAR % 60)).padStart(2, '0')} · ${kicks.length} kicks · ${OUT}`);
