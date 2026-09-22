#!/usr/bin/env node
// Note(s)pat(ial) Native — the 13-minute form for five laptops in a ring
// and one held at the center.
//
// Five machines stand in a pentagon around the audience, seat 1 at the
// front, numbers clockwise. The sixth is in the performer's hands at the
// center, on a small speaker: it is the piece's voice. The piece begins
// there, spreads to the ring, and comes back.
//
// Musical time is phrases: eight bars, a two-bar harmonic rhythm
// (C Am F G, resolving to C in bar 8), the theme stated over it, the ring
// accompanying in halves (question from the front, answer from the back),
// a cadence breath in bar 8 and a fill lap into the next downbeat. The
// tempo pushes a little after every cadence on the way up and eases on
// the way home; the opening and the close are free.
//
// Envelopes are built from what the runtime has (linear attack, linear
// decay over the tail): a note is a stack of sine partials with shorter
// tails on the upper ones, so the center voice rings like a bell and the
// ring plucks.
//
// Spatial writing uses two kinds of lane: pinned (one laptop; a hop stays
// a hop under clock skew) and orbiting (continuous, equal-power handoff).
//
//   node fedac/native/tools/compose-notespatial-native.mjs
//     → scores/notespatial-native.nsscore, plus one file per section

import { mkdir, writeFile } from 'node:fs/promises';

const RING = 5, CENTER = 5, SEATS = 6, TAU = Math.PI * 2;
const hz = n => 440 * 2 ** ((n - 69) / 12);
const r4 = x => +x.toFixed(4);
const ringAz = k => (k % RING) / RING * TAU;
const NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const noteName = n => NAMES[n % 12] + (Math.floor(n / 12) - 1);

// ── lanes ─────────────────────────────────────────────────────────────
const lanes = [];
const lane = (name, color, spatial) => (lanes.push({ name, color, ...spatial, events: [] }), lanes.length - 1);
const VOICE = lane('voice (held)', [255, 232, 150], { center: true });
const WALK = Array.from({ length: RING }, (_, k) => lane(`walk ${k + 1}`, [255, 190, 100], { az: ringAz(k) }));
const ECHO = Array.from({ length: RING }, (_, k) => lane(`echo ${k + 1}`, [180, 160, 225], { az: ringAz(k) }));
const TAP_F = lane('tap front', [110, 205, 230], { az: ringAz(0) });
const TAP_BL = lane('tap back left', [110, 205, 230], { az: ringAz(3) });
const TAP_BR = lane('tap back right', [110, 205, 230], { az: ringAz(2) });
const BASS = lane('bass', [234, 128, 88], { az: ringAz(3) });
const KICK = lane('kick', [255, 107, 125], { az: ringAz(0) });
const SNARE = lane('snare', [122, 223, 153], { az: ringAz(2) });
const PAD = [0, 1, 2].map(m => lane(`pad ${'abc'[m]}`, [156, 179, 237], { orbitSeconds: 60, azOffset: 0 }));
const TOP = lane('top line', [225, 235, 170], { orbitSeconds: 24, orbitDirection: -1 });
const THEME = lane('theme (orbit)', [255, 194, 72], { orbitSeconds: 12.5 });
const ANSWER = lane('answer (orbit)', [116, 210, 235], { orbitSeconds: 15, orbitDirection: -1 });
const HATS = lane('hats', [230, 233, 241], { orbitSeconds: 5 });
PAD.forEach((i, m) => { lanes[i].azOffset = m * TAU / 3 - i / lanes.length * TAU; }); // spread 120° apart
for (const l of lanes) if (!(l.center || Number.isFinite(l.az) || l.orbitSeconds > 0)) throw Error(`${l.name}: neither pinned, centered nor orbiting`);

function ev(i, t, dur, midi, g, wave = 'sine', attack = .01, decay = .06, freq = null) {
  if (!(dur > 0) || !(g > 0)) return;
  const e = { t: r4(t), dur: r4(dur), hz: +(freq ?? hz(midi)).toFixed(2), g: +g.toFixed(3), wave, attack: +attack.toFixed(4), decay: +decay.toFixed(4) };
  if (midi !== null && freq === null) e.note = noteName(midi);
  lanes[i].events.push(e);
}
// instruments: stacks of partials, upper ones shorter
function bell(i, t, dur, midi, g) { // the held voice
  ev(i, t, dur, midi, g, 'sine', .004, dur * .8);
  ev(i, t, dur * .55, midi + 12, g * .28, 'sine', .002, dur * .45, null, hz(midi) * 2);
  ev(i, t, dur * .3, midi + 19, g * .1, 'sine', .002, dur * .25, null, hz(midi) * 3);
}
function pluck(i, t, dur, midi, g, partial = true) { // the ring
  ev(i, t, dur, midi, g, 'sine', .003, dur * .78);
  if (partial) ev(i, t, dur * .5, midi + 12, g * .22, 'sine', .002, dur * .42, null, hz(midi) * 2);
}
const soft = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'sine', .02, dur * .7); // echoes
const pad = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'sine', .6, dur * .5);
function tap(i, t, low, g) {
  ev(i, t, .27, null, g, 'sine', .012, .22, null, low ? 130 : 330);
  ev(i, t + .02, .31, null, g * .5, 'sine', .02, .26, null, low ? 90 : 220);
  ev(i, t, .03, null, g * .35, 'noise', .001, .02, null, 1000);
}
function orbitAngle(i, t) { const l = lanes[i]; return i / lanes.length * TAU + (l.azOffset || 0) + t / l.orbitSeconds * TAU * (l.orbitDirection === -1 ? -1 : 1); }
const ringSeatOf = angle => ((Math.round(angle / TAU * RING) % RING) + RING) % RING;

// ── material ──────────────────────────────────────────────────────────
const TRIAD = { C: [60, 64, 67], Am: [57, 60, 64], F: [53, 57, 60], G: [55, 59, 62] };
const BAR_CHORDS = ['C', 'C', 'Am', 'Am', 'F', 'F', 'G', 'C']; // two-bar harmonic rhythm, cadence in bar 8
// The theme in C (the D-major theme of compose-spatial-groove.mjs, down a step): four 2-bar phrases
const THEME_PHRASES = [
  [[0, 1, 72], [1, .5, 76], [1.5, .5, 79], [2, 1.5, 76], [3.5, .5, 74], [4, 1, 76], [5, 1, 74], [6, 2, 72]],
  [[0, 1, 76], [1, .5, 79], [1.5, .5, 81], [2, 1, 79], [3, 1, 76], [4, 1.5, 74], [5.5, .5, 72], [6, 2, 69]],
  [[0, 1, 72], [1, 1, 77], [2, .5, 79], [2.5, .5, 81], [3, 1, 79], [4, 1, 77], [5, .5, 76], [5.5, .5, 74], [6, 2, 72]],
  [[0, 1, 74], [1, .5, 76], [1.5, .5, 79], [2, 1.5, 83], [3.5, .5, 79], [4, 1, 76], [5, 1, 74], [6, 2, 72]],
];
const swing = u => u <= .5 ? u * 1.2 : .6 + (u - .5) * .8; // 60:40
const FRONT = [4, 0, 1], BACK = [2, 3];

const movements = [], tempo = [];
const movement = (name, sub, t0, t1, level) => movements.push({ name, sub, t0: r4(t0), t1: r4(t1), level });

// ── I · Appear (free) ─────────────────────────────────────────────────
// The held laptop breathes, states the theme's first phrase twice, and
// the ring learns it: the same notes hop seat to seat, each pass faster.
let t = 0;
tempo.push({ t: 0, bpm: null });
for (const [m, dur, gap] of [[60, 3.2, 2.6], [60, 3.0, 2.2], [67, 3.4, 2.0], [64, 3.0, 1.8], [60, 3.6, 2.4]]) { bell(VOICE, t, dur, m, .5); t += dur + gap; }
const P1 = THEME_PHRASES[0];
for (let pass = 0; pass < 2; pass++) {
  const q = pass === 0 ? 1.15 : .95; // seconds per beat
  for (const [a, d, m] of P1) bell(VOICE, t + a * q, d * q * .9, m, .5);
  t += 8 * q + 2.2;
}
let hop = 0;
for (let pass = 0; pass < 5; pass++) {
  const q = [1.0, .82, .66, .54, .44][pass];
  for (const [a, d, m] of P1) { pluck(WALK[hop % RING], t + a * q, Math.max(.25, d * q * .9), m, .46); hop++; }
  if (pass < 4) bell(VOICE, t + 6 * q, 2 * q, 72, .3); // the held laptop keeps the last note under each pass
  t += 8 * q + (pass < 4 ? q * 2 : 0);
}
// pickup: a fast lap up the scale into the first downbeat
const T0 = t + 1.4;
[60, 62, 64, 65, 67].forEach((m, k) => pluck(WALK[k], T0 - (5 - k) * .16, .15, m, .4, false));
movement('I · Appear', 'the held laptop breathes the theme; the ring learns it', 0, T0, .3);

// ── the grid: phrases ─────────────────────────────────────────────────
let cursor = T0, bpm = 96, beat = 60 / 96;
let walkHop = 0, echoHop = 0;
function setTempo(b) { bpm = b; beat = 60 / b; tempo.push({ t: r4(cursor), bpm: b }); }
const barLen = () => 4 * beat;

// One eight-bar phrase. Options shape what plays and where.
function phrase(o) {
  const {
    tr = 0, lvl = 1, register = 0,
    theme = false, themeWhere = 'center', // 'center' | 'orbit'
    walk = true, walkWhere = 'split',      // 'split' | 'ring' | 'front' | 'seat1'
    walkPartials = true,
    echoes = 0, echoOf = 'theme',           // echoes answer the theme (or the walk when there is none)
    taps = true, bass = true, pads = false, top = false, hats = false, drums = false, answer = false,
    cadence = true, fill = true, lift = false, breakBar = false, finalChord = false, centerHold = false,
  } = o;
  const soft_ = .86 ** Math.max(0, register), start = cursor;
  const bt = (bar, b) => start + bar * barLen() + b * beat;
  const chordAt = bar => TRIAD[BAR_CHORDS[bar]].map(n => n + tr);
  for (let bar = 0; bar < 8; bar++) {
    const chord = chordAt(bar), last = bar === 7;
    const swell = lvl * (1 + .06 * Math.min(bar, 6) - (last ? .1 : 0)); // crescendo into bar 7, breath in 8
    // the theme: two bars per phrase, from the center or the orbiting lane
    if (theme && bar % 2 === 0) {
      const ph = THEME_PHRASES[bar / 2];
      for (const [a, d, m] of ph) {
        const up = lift && a >= 4 ? 12 : 0, midi = m + tr + register * 12 + up, tn = bt(bar, a), dur = d * beat * .92;
        if (themeWhere === 'center') bell(VOICE, tn, dur, midi, .5 * soft_ * swell);
        else ev(THEME, tn, dur, midi, .55 * soft_ * swell, 'sine', .004, dur * .7);
        if (answer && a >= 4 && bar >= 2) ev(ANSWER, tn + .5 * beat, dur * .6, midi - 12, .26 * swell, 'triangle', .006, dur * .4);
        if (echoes && echoOf === 'theme' && (a === 0 || a === 2 || a === 4)) {
          const base = themeWhere === 'orbit' ? ringSeatOf(orbitAngle(THEME, tn)) : echoHop;
          const plan = [[.375, 2, .34], [.75, 3, .18], [1.5, 1, .09]];
          for (let e = 0; e < echoes; e++) soft(ECHO[(base + plan[e][1]) % RING], tn + plan[e][0] * 2 * beat, .7, midi, .5 * soft_ * swell * plan[e][2]);
          echoHop++;
        }
      }
    }
    // the walk: three chord tones per two beats, hopping seats
    if (walk && walkWhere) {
      const cycles = last && cadence ? 1 : 2; // cadence bar: rest after the downbeat
      for (let c = 0; c < cycles; c++) for (let j = 0; j < 3; j++) {
        const tn = bt(bar, c * 2 + swing(j / 3) * 2), midi = chord[j] + 12 + register * 12;
        const seat = walkWhere === 'ring' ? walkHop % RING
          : walkWhere === 'split' ? (bar < 4 ? FRONT[walkHop % 3] : BACK[walkHop % 2])
          : walkWhere === 'front' ? FRONT[walkHop % 3] : 0;
        pluck(WALK[seat], tn, .6 * beat, midi, .34 * soft_ * swell, walkPartials);
        if (echoes && echoOf === 'walk' && j === 0) soft(ECHO[(seat + 2) % RING], tn + .75 * beat, .6, midi, .12 * soft_ * swell);
        walkHop++;
      }
      // the fill: a lap up the scale in the last two beats, landing on the next downbeat
      if (last && fill) [0, 2, 4, 5, 7].forEach((step, k) => pluck(WALK[k], bt(7, 2 + k * .4), .3 * beat, 60 + tr + step + Math.max(0, register) * 12, .3 * soft_ * lvl, false));
    }
    // taps: front on the beat, back off it; bar 8 keeps only the downbeat
    if (taps) for (let c = 0; c < (last ? 1 : 2); c++) {
      tap(TAP_F, bt(bar, c * 2), true, .2 * swell);
      if (!last) tap(c % 2 ? TAP_BL : TAP_BR, bt(bar, c * 2 + swing(.5) * 2), false, .14 * swell);
    }
    if (bass) {
      const root = chord[0] - 12;
      if (drums) { for (const b of [0, 1.5, 2, 3.5]) ev(BASS, bt(bar, b), .42 * beat, root, .5 * swell, 'triangle', .008, .1); }
      else if (last) ev(BASS, bt(bar, 0), 3.6 * beat, root, .4 * swell, 'triangle', .02, 2 * beat);
      else for (const b of [0, 2]) ev(BASS, bt(bar, b), 1.6 * beat, root, .42 * swell, 'triangle', .015, .8 * beat);
    }
    if (pads && bar % 2 === 0) chord.forEach((p, m) => pad(PAD[m], bt(bar, 0), 7.6 * beat, p, .13 * swell));
    if (top && bar % 2 === 1) {
      const high = Math.min(106, chord[1] + 24 + register * 12), tn = bt(bar, .18 * 2);
      ev(TOP, tn, .40, high, .055 * soft_ * swell, 'sine', .06, .3);
      ev(TOP, tn + .46 * beat, .52, Math.min(108, high + 2), .04 * soft_ * swell, 'sine', .08, .4);
    }
    if (hats && !(last && breakBar)) for (let b = 0; b < 4; b += .5) ev(HATS, bt(bar, b), b % 1 ? .045 : .027, null, (b % 1 ? .3 : .19) * swell, 'noise', .001, .02, null, 7000);
    if (drums) {
      for (const a of [0, 2, ...(bar % 4 === 3 ? [3.5] : [])]) { ev(KICK, bt(bar, a), .055, null, .8 * swell, 'sine', .001, .04, null, 150); ev(KICK, bt(bar, a) + .06, .16, null, .75 * swell, 'sine', .001, .11, null, 78); }
      if (!(last && finalChord)) for (const a of [1, 3]) { ev(SNARE, bt(bar, a), .11, null, .4 * swell, 'noise', .001, .08, null, 2400); ev(SNARE, bt(bar, a), .095, null, .45 * swell, 'triangle', .001, .07, null, 185); }
    }
  }
  if (centerHold) bell(VOICE, bt(7, 0), 4 * beat, 67 + tr, .5 * lvl); // the dominant, held into the next section
  if (finalChord) { // every laptop strikes the chord on bar 7's downbeat and holds through bar 8
    [60, 64, 67, 72, 76].forEach((m, k) => pluck(WALK[k], bt(6, 0), 8 * beat, m + tr, .5 * lvl, false));
    bell(VOICE, bt(6, 0), 8 * beat, 84 + tr, .55 * lvl);
    ev(BASS, bt(6, 0), 8 * beat, 48 + tr, .5 * lvl, 'triangle', .01, 4 * beat);
  }
  cursor += 8 * barLen();
}
const section = (name, sub, level, body) => { const t0 = cursor; body(); movement(name, sub, t0, cursor, level); };

// ── II · Ring ─────────────────────────────────────────────────────────
section('II · Ring', 'the pulse arrives; the ring answers itself, then the held laptop sings the theme', .55, () => {
  setTempo(96); phrase({ lvl: .7, walkWhere: 'front' });
  setTempo(98); phrase({ lvl: .8, walkWhere: 'split' });
  setTempo(100); phrase({ lvl: .85, theme: true });
  setTempo(102); phrase({ lvl: .9, theme: true, echoes: 1 });
});
// ── III · Echoes ──────────────────────────────────────────────────────
section('III · Echoes', 'every phrase of the theme is answered around the ring; the harmony revolves', .75, () => {
  setTempo(104); phrase({ lvl: .9, theme: true, echoes: 1, pads: true });
  setTempo(106); phrase({ lvl: .92, theme: true, echoes: 2, pads: true });
  setTempo(108); phrase({ lvl: .9, echoes: 1, echoOf: 'walk', pads: true, walkWhere: 'ring' });
  setTempo(108); phrase({ lvl: .95, theme: true, echoes: 2, pads: true });
  setTempo(110); phrase({ lvl: 1, theme: true, echoes: 3, pads: true });
  setTempo(110); phrase({ lvl: .9, echoes: 1, echoOf: 'walk', pads: true, walkWhere: 'ring' });
  setTempo(112); phrase({ lvl: 1, theme: true, echoes: 3, pads: true });
  setTempo(112); phrase({ lvl: 1, theme: true, echoes: 3, pads: true, cadence: false, fill: false, centerHold: true });
});
// ── IV · Climb ────────────────────────────────────────────────────────
section('IV · Climb', 'four registers, an octave apart; a high line glides against the ring; the floor drops out', .9, () => {
  const C = { pads: true, walkWhere: 'ring', theme: true };
  setTempo(112); phrase({ ...C, lvl: .9, register: -1, echoes: 2 });
  setTempo(114); phrase({ ...C, lvl: .95, register: -1, echoes: 3 });
  setTempo(116); phrase({ ...C, lvl: .95, register: 0, echoes: 3 });
  setTempo(116); phrase({ ...C, lvl: 1, register: 0, echoes: 3 });
  setTempo(118); phrase({ ...C, lvl: 1, register: 1, echoes: 3, top: true });
  setTempo(120); phrase({ ...C, lvl: 1, register: 1, echoes: 3, top: true });
  setTempo(120); phrase({ ...C, lvl: 1, register: 2, echoes: 3, top: true, hats: true });
  setTempo(122); phrase({ ...C, lvl: 1, register: 2, echoes: 3, top: true, hats: true });
  // the break: bar 8 is a rising lap into silence, then the drop
  setTempo(124); phrase({ ...C, lvl: 1, register: 2, echoes: 2, top: true, hats: true, fill: false, breakBar: true });
  const b8 = cursor - barLen();
  [62, 64, 66, 67, 69].forEach((m, k) => pluck(WALK[k], b8 + k * .5 * beat, .45 * beat, m, .45, false)); // D E F# G A
});
// ── V · Lift ──────────────────────────────────────────────────────────
section('V · Lift', 'up a whole step: the theme leaves the hands and orbits over kick and snare', 1, () => {
  const D = { tr: 2, register: -1, walkWhere: 'ring', walkPartials: false, theme: true, themeWhere: 'orbit', drums: true, taps: false, pads: true, echoes: 2, fill: false, cadence: false };
  setTempo(120); phrase({ ...D, lvl: .95 });
  setTempo(120); phrase({ ...D, lvl: 1 });
  setTempo(120); phrase({ ...D, lvl: 1, answer: true });
  setTempo(122); phrase({ ...D, lvl: 1, answer: true, hats: true });
  setTempo(122); phrase({ ...D, lvl: 1.05, answer: true, hats: true, lift: true });
  setTempo(124); phrase({ ...D, lvl: 1.05, answer: true, hats: true, lift: true });
  setTempo(124); phrase({ ...D, lvl: 1, answer: true, hats: true });
  setTempo(118); phrase({ ...D, lvl: 1, echoes: 1, finalChord: true });
  cursor += 2.6; // general pause
});
// ── VI · Return ───────────────────────────────────────────────────────
section('VI · Return', 'home in C; something leaves every phrase; the ring narrows to the front and the theme comes back to the hands', .6, () => {
  setTempo(112); phrase({ lvl: .9, theme: true, echoes: 3, pads: true, walkWhere: 'ring' });
  setTempo(108); phrase({ lvl: .85, theme: true, echoes: 2, pads: true, bass: false });
  setTempo(104); phrase({ lvl: .8, theme: true, echoes: 1, pads: true, bass: false });
  setTempo(102); phrase({ lvl: .75, theme: true, echoes: 1, pads: true, walkWhere: 'front', bass: false });
  setTempo(100); phrase({ lvl: .7, theme: true, echoes: 1, walkWhere: 'front', bass: false, taps: false });
  setTempo(96); phrase({ lvl: .6, theme: true, walkWhere: 'seat1', walkPartials: false, bass: false, taps: false });
  setTempo(92); phrase({ lvl: .55, theme: true, walk: false, bass: false, taps: false, fill: false });
  setTempo(88); phrase({ lvl: .5, theme: true, walk: false, bass: false, taps: false, fill: false, cadence: false });
});
tempo.push({ t: r4(cursor), bpm: null });

// ── VII · Vanish (free) ───────────────────────────────────────────────
// The ring gives the theme's last phrase back, one laptop at a time,
// backwards around the room; the held laptop closes on C.
t = cursor + 1.2;
const LAST = [83, 79, 76, 74, 72];
for (let k = 0; k < RING; k++) { pluck(WALK[(RING - k) % RING], t, 2.4, LAST[k], .42); t += 3.2 + k * .4; }
for (const [m, dur, gap] of [[67, 3.4, 3.0], [64, 3.8, 4.2], [60, 4.4, 5.6]]) { bell(VOICE, t, dur, m, .45); t += dur + gap; }
ev(VOICE, t, 10, 60, .45, 'sine', 3.5, 5); // C4 rising out of nothing, then gone
const END = t + 10 + 4;
movement('VII · Vanish', 'the ring hands the last phrase back; the held laptop closes on C', cursor, END, .3);

// ── the field turns ───────────────────────────────────────────────────
// fieldShift rotates the WHOLE room, pinned lanes included: laptops trade
// channels. Used three ways, after Special Sign and wannadash: a spring
// blast at the drop (the field kicked one seat over and swinging back), an
// eight-turn super-spin through the peak that fuses into a hum, and a slow
// tour at the start of the Return. Zero elsewhere so a hop stays a hop.
const turns = [];
const V = movements.find(m => m.name.startsWith('V ')), VI = movements.find(m => m.name.startsWith('VI '));
const lift = tempo.filter(x => x.t >= V.t0 && x.t < V.t1);
const peak0 = lift[4].t, peak1 = lift[6].t; // phrases 5 and 6 of the Lift
const ease5 = u => u * u * u * (u * (u * 6 - 15) + 10);
function shiftAt(t) {
  let f = 0;
  if (t >= V.t0 && t < V.t0 + 4.8) { // the blast: one seat over, springing back at 0.92 Hz, damping 0.58
    const x = t - V.t0, w = 2 * Math.PI * .92;
    f += .4 * Math.exp(-.58 * w * x) * Math.cos(w * x);
  }
  if (t >= peak0 && t < peak1) f += 16 * ease5((t - peak0) / (peak1 - peak0)); // eight turns, quintic
  else if (t >= peak1) f += 16;
  if (t >= VI.t0 && t < VI.t0 + 20) f += 2 * ease5((t - VI.t0) / 20); // the tour: one slow lap
  else if (t >= VI.t0 + 20) f += 2;
  return f;
}
const SHIFT_HZ = 25;
for (let i = 0; i <= Math.ceil(END * SHIFT_HZ); i++) turns.push(+shiftAt(i / SHIFT_HZ).toFixed(4));

for (const l of lanes) l.events.sort((a, b) => a.t - b.t);
// Each machine has a dominant color; notes wear the color of the laptop they land on.
const seatColors = [[255, 110, 110], [255, 180, 70], [120, 220, 130], [95, 170, 255], [200, 130, 255], [255, 240, 200]];
const score = {
  name: 'Note(s)pat(ial) Native', geometry: 'ring', seats: SEATS, ring: RING, center: CENTER, seatColors,
  dur: r4(END), gain: .36, swing: .6, tempo, movements, fieldShift: turns, lanes,
};

// ── write ─────────────────────────────────────────────────────────────
const dir = new URL('../scores/', import.meta.url);
await mkdir(dir, { recursive: true });
await writeFile(new URL('notespatial-native.nsscore', dir), JSON.stringify(score) + '\n');
const slug = s => s.toLowerCase().replace(/^[ivx]+ · /, '').replace(/[^a-z]+/g, '-').replace(/^-|-$/g, '');
await Promise.all(movements.map((m, n) => {
  const part = {
    ...score, fieldShift: undefined, name: `${score.name} — ${m.name}`, dur: r4(m.t1 - m.t0 + 2), movements: [{ ...m, t0: 0, t1: r4(m.t1 - m.t0) }],
    tempo: tempo.filter(x => x.t >= m.t0 && x.t < m.t1).map(x => ({ ...x, t: r4(x.t - m.t0) })),
    lanes: lanes.map(l => ({ ...l, events: l.events.filter(e => e.t >= m.t0 && e.t < m.t1).map(e => ({ ...e, t: r4(e.t - m.t0) })) })),
  };
  return writeFile(new URL(`notespatial-native-${n + 1}-${slug(m.name)}.nsscore`, dir), JSON.stringify(part) + '\n');
}));

// ── report ────────────────────────────────────────────────────────────
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const all = lanes.flatMap((l, i) => l.events.map(e => ({ ...e, lane: i }))).sort((a, b) => a.t - b.t);
function maxVoices(a, b) {
  let mx = 0;
  const evs = all.filter(e => e.t >= a && e.t < b);
  for (const e of evs) { const n = evs.filter(o => o.t <= e.t && o.t + o.dur > e.t).length; if (n > mx) mx = n; }
  return mx;
}
console.log(`${score.name}: ${mmss(END)} (${END.toFixed(1)} s), ${lanes.length} lanes, ${all.length} events, ring of ${RING} + center`);
console.log('section        start   length  events  maxvoices  tempo');
for (const m of movements) {
  const evs = all.filter(e => e.t >= m.t0 && e.t < m.t1);
  const bpms = tempo.filter(x => x.t >= m.t0 && x.t < m.t1 && x.bpm).map(x => x.bpm);
  const tp = bpms.length ? `${bpms[0]}→${bpms.at(-1)}` : 'free';
  console.log(`${m.name.padEnd(14)} ${mmss(m.t0).padStart(6)}  ${mmss(m.t1 - m.t0).padStart(6)}  ${String(evs.length).padStart(6)}  ${String(maxVoices(m.t0, m.t1)).padStart(9)}  ${tp}`);
}
