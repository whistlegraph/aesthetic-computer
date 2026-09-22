#!/usr/bin/env node
// Note(s)pat(ial) Native — a suite in eleven chapters for five laptops in
// a ring and one held at the center.
//
// Five machines stand in a pentagon around the audience, seat 1 at the
// front, numbers clockwise. The sixth is in the performer's hands at the
// center on a small speaker: it is the piece's voice.
//
// Not one build. Eleven chapters, each its own little ode with its own
// key, meter, tempo, tune and spatial gesture, cut together the way a
// cartoon score is: on the downbeat, with stingers and silences. What
// ties them is the theme (a diatonic tune in four two-bar phrases), the
// held voice as narrator, and the room itself.
//
//   I    Overture      free      the hands breathe the theme; the ring learns it
//   II   The Walk      C  4/4    pulse, the walk in halves, the theme in the hands
//   III  Waltz         Am 3/4    oom (back) pah (left) pah (right); the ring takes the tune, the hands answer in counterpoint
//   IV   Chase         F  4/4    two runners circle the ring, one chasing the other's inversion; a crash
//   V    Sneak         Cm 4/4    an offbeat bass creeps one seat a bar; a whole-tone tiptoe; the reveal
//   VI   Lullaby       F  6/8    a cradle of arpeggios around the room; the whole field turns once
//   VII  The Climb     C  4/4    four registers, a high line against the ring, the break
//   VIII The Lift      D  4/4    the theme leaves the hands and orbits; the blast; the eight-turn spin; the tutti
//   IX   Fanfare       G  4/4    front pair calls, back pair answers, brass in triangle waves; a stinger
//   X    Return        C  4/4    the theme back in the hands; the tour; the mirror; subtraction
//   XI   Vanish        free      the ring hands back the last phrase; the hands close on C; one tap from behind
//
// Envelopes are stacks of sine partials with shorter upper tails (bell,
// pluck, brass). The held voice warbles against a detuned copy, deeper as
// the piece goes on. The whole field can turn (fieldShift), so laptops
// trade channels: a blast, a spin, a tour.
//
//   node fedac/native/tools/compose-notespatial-native.mjs
//     → scores/notespatial-native.nsscore, plus one file per chapter

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
PAD.forEach((i, m) => { lanes[i].azOffset = m * TAU / 3 - i / lanes.length * TAU; });
for (const l of lanes) if (!(l.center || Number.isFinite(l.az) || l.orbitSeconds > 0)) throw Error(`${l.name}: neither pinned, centered nor orbiting`);

// ── instruments ───────────────────────────────────────────────────────
// label: undefined → named from midi; null → unlabeled (partials, drums). freq overrides the pitch.
function ev(i, t, dur, midi, g, wave = 'sine', attack = .01, decay = .06, label, freq = null) {
  if (!(dur > 0) || !(g > 0)) return;
  if (freq === null && midi === null) throw Error('event with neither midi nor freq');
  const e = { t: r4(t), dur: r4(dur), hz: +(freq ?? hz(midi)).toFixed(2), g: +g.toFixed(3), wave, attack: +attack.toFixed(4), decay: +decay.toFixed(4) };
  if (label === undefined && midi !== null) e.note = noteName(midi);
  lanes[i].events.push(e);
}
let warbleCents = 0; // the wannadash wiggle: a detuned copy beating a few Hz, ramped in after the onset
function bell(i, t, dur, midi, g) { // the held voice
  ev(i, t, dur, midi, g, 'sine', .004, dur * .8);
  ev(i, t, dur * .55, midi + 12, g * .28, 'sine', .002, dur * .45, null, hz(midi) * 2);
  ev(i, t, dur * .3, midi + 19, g * .1, 'sine', .002, dur * .25, null, hz(midi) * 3);
  if (warbleCents > 0 && dur > .5) ev(i, t, dur, null, g * .5, 'sine', .45, dur * .7, null, hz(midi) * 2 ** (warbleCents / 1200));
}
function pluck(i, t, dur, midi, g, partial = true) { // the ring
  ev(i, t, dur, midi, g, 'sine', .003, dur * .78);
  if (partial) ev(i, t, dur * .5, midi + 12, g * .22, 'sine', .002, dur * .42, null, hz(midi) * 2);
}
function brass(i, t, dur, midi, g) { // fanfare: triangle body, sine octave, quick bite
  ev(i, t, dur, midi, g, 'triangle', .012, dur * .35);
  ev(i, t, dur * .8, midi + 12, g * .25, 'sine', .01, dur * .3, null, hz(midi) * 2);
}
const stacc = (i, t, dur, midi, g, wave = 'sine') => ev(i, t, dur, midi, g, wave, .003, dur * .6);
const soft = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'sine', .02, dur * .7); // echoes
const pad = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'sine', .6, dur * .5);
function tap(i, t, low, g) {
  ev(i, t, .27, null, g, 'sine', .012, .22, null, low ? 130 : 330);
  ev(i, t + .02, .31, null, g * .5, 'sine', .02, .26, null, low ? 90 : 220);
  ev(i, t, .03, null, g * .35, 'noise', .001, .02, null, 1000);
}
const crash = (t, g) => { for (let k = 0; k < RING; k++) ev(WALK[k], t + k * .012, .5, null, g, 'noise', .002, .4, null, 3000); ev(VOICE, t, .4, null, g * .6, 'noise', .002, .3, null, 3000); };
function orbitAngle(i, t) { const l = lanes[i]; return i / lanes.length * TAU + (l.azOffset || 0) + t / l.orbitSeconds * TAU * (l.orbitDirection === -1 ? -1 : 1); }
const ringSeatOf = angle => ((Math.round(angle / TAU * RING) % RING) + RING) % RING;

// ── material ──────────────────────────────────────────────────────────
const TRIAD = { C: [60, 64, 67], Am: [57, 60, 64], F: [53, 57, 60], G: [55, 59, 62] };
const BAR_CHORDS = ['C', 'C', 'Am', 'Am', 'F', 'F', 'G', 'C'];
// The theme: four two-bar phrases in C (the D-major theme of compose-spatial-groove.mjs, down a step)
const THEME_PHRASES = [
  [[0, 1, 72], [1, .5, 76], [1.5, .5, 79], [2, 1.5, 76], [3.5, .5, 74], [4, 1, 76], [5, 1, 74], [6, 2, 72]],
  [[0, 1, 76], [1, .5, 79], [1.5, .5, 81], [2, 1, 79], [3, 1, 76], [4, 1.5, 74], [5.5, .5, 72], [6, 2, 69]],
  [[0, 1, 72], [1, 1, 77], [2, .5, 79], [2.5, .5, 81], [3, 1, 79], [4, 1, 77], [5, .5, 76], [5.5, .5, 74], [6, 2, 72]],
  [[0, 1, 74], [1, .5, 76], [1.5, .5, 79], [2, 1.5, 83], [3.5, .5, 79], [4, 1, 76], [5, 1, 74], [6, 2, 72]],
];
const swing = u => u <= .5 ? u * 1.2 : .6 + (u - .5) * .8;
const FRONT = [4, 0, 1], BACK = [2, 3];
const MIRROR = [0, 4, 3, 2, 1]; // the ring flipped about the front axis

const movements = [], tempo = [], turnsPlan = [];
let cursor = 0, beat = 60 / 96;
function setTempo(b) { beat = 60 / b; tempo.push({ t: r4(cursor), bpm: b }); }
const free = () => tempo.push({ t: r4(cursor), bpm: null });
function chapter(name, sub, level, body) { const t0 = cursor; body(); movements.push({ name, sub, t0: r4(t0), t1: r4(cursor), level }); }
// a melody as [midi, beats] pairs, played from t0; `where(k, t)` picks the lane for the k-th note
function melody(seq, t0, where, g, inst, dur = .9) { let t = t0, k = 0; for (const [m, d] of seq) { if (m) inst(where(k, t), t, d * beat * dur, m, g); t += d * beat; k++; } return t; }
let walkHop = 0, echoHop = 0;

// ── the eight-bar phrase engine (chapters II, VII, VIII, X) ───────────
function phrase(o) {
  const {
    tr = 0, lvl = 1, register = 0,
    theme = false, themeWhere = 'center', walk = true, walkWhere = 'split', walkPartials = true,
    echoes = 0, echoOf = 'theme', taps = true, bass = true, pads = false, top = false, hats = false, drums = false, answer = false,
    cadence = true, fill = true, lift = false, breakBar = false, finalChord = false, centerHold = false, mirror = false,
  } = o;
  const soft_ = .86 ** Math.max(0, register), start = cursor, barLen = 4 * beat;
  const bt = (bar, b) => start + bar * barLen + b * beat;
  const seatMap = k => mirror ? MIRROR[k] : k;
  const chordAt = bar => TRIAD[BAR_CHORDS[bar]].map(n => n + tr);
  for (let bar = 0; bar < 8; bar++) {
    const chord = chordAt(bar), last = bar === 7;
    const swell = lvl * (1 + .06 * Math.min(bar, 6) - (last ? .1 : 0));
    if (theme && bar % 2 === 0) {
      for (const [a, d, m] of THEME_PHRASES[bar / 2]) {
        const up = lift && a >= 4 ? 12 : 0, midi = m + tr + register * 12 + up, tn = bt(bar, a), dur = d * beat * .92;
        if (themeWhere === 'center') bell(VOICE, tn, dur, midi, .5 * soft_ * swell);
        else { ev(THEME, tn, dur, midi, .55 * soft_ * swell, 'sine', .004, dur * .7); if (warbleCents > 0 && dur > .5) ev(THEME, tn, dur, null, .27 * soft_ * swell, 'sine', .45, dur * .6, null, hz(midi) * 2 ** (warbleCents / 1200)); }
        if (answer && a >= 4 && bar >= 2) ev(ANSWER, tn + .5 * beat, dur * .6, midi - 12, .26 * swell, 'triangle', .006, dur * .4);
        if (echoes && echoOf === 'theme' && (a === 0 || a === 2 || a === 4)) {
          const base = themeWhere === 'orbit' ? ringSeatOf(orbitAngle(THEME, tn)) : echoHop;
          const plan = [[.375, 2, .34], [.75, 3, .18], [1.5, 1, .09]];
          for (let e = 0; e < echoes; e++) soft(ECHO[seatMap((base + plan[e][1]) % RING)], tn + plan[e][0] * 2 * beat, .7, midi, .5 * soft_ * swell * plan[e][2]);
          echoHop++;
        }
      }
    }
    if (walk && walkWhere) {
      const cycles = last && cadence ? 1 : 2;
      for (let c = 0; c < cycles; c++) for (let j = 0; j < 3; j++) {
        const tn = bt(bar, c * 2 + swing(j / 3) * 2), midi = chord[j] + 12 + register * 12;
        const seat = walkWhere === 'ring' ? walkHop % RING : walkWhere === 'split' ? (bar < 4 ? FRONT[walkHop % 3] : BACK[walkHop % 2]) : walkWhere === 'front' ? FRONT[walkHop % 3] : 0;
        pluck(WALK[seatMap(seat)], tn, .6 * beat, midi, .34 * soft_ * swell, walkPartials);
        if (echoes && echoOf === 'walk' && j === 0) soft(ECHO[seatMap((seat + 2) % RING)], tn + .75 * beat, .6, midi, .12 * soft_ * swell);
        walkHop++;
      }
      if (last && fill) [0, 2, 4, 5, 7].forEach((step, k) => pluck(WALK[seatMap(k)], bt(7, 2 + k * .4), .3 * beat, 60 + tr + step + Math.max(0, register) * 12, .3 * soft_ * lvl, false));
    }
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
      const high = Math.min(106, chord[1] + 24 + register * 12), tn = bt(bar, .36);
      ev(TOP, tn, .40, high, .055 * soft_ * swell, 'sine', .06, .3);
      ev(TOP, tn + .46 * beat, .52, Math.min(108, high + 2), .04 * soft_ * swell, 'sine', .08, .4);
    }
    if (hats && !(last && breakBar)) for (let b = 0; b < 4; b += .5) ev(HATS, bt(bar, b), b % 1 ? .045 : .027, null, (b % 1 ? .3 : .19) * swell, 'noise', .001, .02, null, 7000);
    if (drums) {
      for (const a of [0, 2, ...(bar % 4 === 3 ? [3.5] : [])]) { ev(KICK, bt(bar, a), .055, null, .8 * swell, 'sine', .001, .04, null, 150); ev(KICK, bt(bar, a) + .06, .16, null, .75 * swell, 'sine', .001, .11, null, 78); }
      if (!(last && finalChord)) for (const a of [1, 3]) { ev(SNARE, bt(bar, a), .11, null, .4 * swell, 'noise', .001, .08, null, 2400); ev(SNARE, bt(bar, a), .095, null, .45 * swell, 'triangle', .001, .07, null, 185); }
    }
  }
  if (centerHold) bell(VOICE, bt(7, 0), 4 * beat, 67 + tr, .5 * lvl);
  if (finalChord) {
    [60, 64, 67, 72, 76].forEach((m, k) => pluck(WALK[k], bt(6, 0), 8 * beat, m + tr, .5 * lvl, false));
    bell(VOICE, bt(6, 0), 8 * beat, 84 + tr, .55 * lvl);
    ev(BASS, bt(6, 0), 8 * beat, 48 + tr, .5 * lvl, 'triangle', .01, 4 * beat);
  }
  cursor += 8 * barLen;
}

// ── I · Overture (free) ───────────────────────────────────────────────
free();
chapter('I · Overture', 'the held laptop breathes the theme; the ring learns it', .3, () => {
  let t = 0;
  for (const [m, dur, gap] of [[60, 3.2, 2.6], [67, 3.4, 2.0], [64, 3.0, 1.8], [60, 3.6, 2.4]]) { bell(VOICE, t, dur, m, .5); t += dur + gap; }
  const P1 = THEME_PHRASES[0];
  for (const [a, d, m] of P1) bell(VOICE, t + a * 1.1, d * 1.1 * .9, m, .5);
  t += 8 * 1.1 + 2.2;
  let hop = 0;
  for (let pass = 0; pass < 4; pass++) {
    const q = [.95, .74, .56, .44][pass];
    for (const [a, d, m] of P1) { pluck(WALK[hop % RING], t + a * q, Math.max(.25, d * q * .9), m, .46); hop++; }
    if (pass < 3) bell(VOICE, t + 6 * q, 2 * q, 72, .3);
    t += 8 * q + (pass < 3 ? q * 2 : 0);
  }
  cursor = t + 1.4;
  [60, 62, 64, 65, 67].forEach((m, k) => pluck(WALK[k], cursor - (5 - k) * .16, .15, m, .4, false)); // pickup lap
});

// ── II · The Walk (C, 4/4) ────────────────────────────────────────────
warbleCents = 4;
chapter('II · The Walk', 'the pulse arrives front and back; the ring walks in halves; the theme in the hands', .55, () => {
  setTempo(100); phrase({ lvl: .7, walkWhere: 'front' });
  setTempo(102); phrase({ lvl: .8, walkWhere: 'split' });
  setTempo(104); phrase({ lvl: .9, theme: true, echoes: 1 });
});

// ── III · Waltz for Five Laptops (Am, 3/4) ────────────────────────────
warbleCents = 6;
chapter('III · Waltz', 'oom at the back, pah left, pah right; the ring takes the tune and the hands answer in counterpoint', .65, () => {
  setTempo(132);
  const A = [[76, 2], [77, 1], [76, 3], [81, 2], [79, 1], [76, 3], [74, 2], [76, 1], [77, 2], [76, 1], [74, 2], [72, 1], [71, 3]];
  const A2 = [...A.slice(0, 10), [74, 1], [72, 1], [71, 1], [69, 3]];
  const COUNTER = [[72, 6], [77, 3], [76, 3], [68, 6], [72, 6], [77, 3], [76, 3], [71, 6], [69, 6], [72, 3], [74, 3]];
  const HA = [['Am', 45, [57, 60, 64]], ['Am', 45, [57, 60, 64]], ['Dm', 50, [62, 65, 69]], ['E7', 52, [64, 68, 71]], ['Am', 45, [57, 60, 64]], ['F', 53, [65, 69, 72]], ['Dm', 50, [62, 65, 69]], ['E7', 52, [64, 68, 71]]];
  const HA2 = [...HA.slice(0, 6), ['E7', 52, [64, 68, 71]], ['Am', 45, [57, 60, 64]]];
  const start = cursor, barLen = 3 * beat, bt = (bar, b) => start + bar * barLen + b * beat;
  const hands = (k, t) => VOICE, hop = () => WALK[walkHop++ % RING];
  function oompah(bar, harm, lvl) {
    const [, root, chord] = harm[bar % 8];
    ev(BASS, bt(bar, 0), .55 * beat, root, .5 * lvl, 'triangle', .008, .25 * beat);
    for (const b of [1, 2]) chord.forEach(m => pluck(WALK[b === 1 ? 1 : 4], bt(bar, b), .5 * beat, m, .17 * lvl, false));
  }
  // A A' in the hands
  for (let bar = 0; bar < 16; bar++) oompah(bar, bar < 8 ? HA : HA2, .8);
  melody(A, bt(0, 0), hands, .5, bell); melody(A2, bt(8, 0), hands, .52, bell);
  // B: the ring hops the tune around the room, the hands hold a counterline
  for (let bar = 16; bar < 32; bar++) oompah(bar, bar < 24 ? HA : HA2, .85);
  melody(A, bt(16, 0), hop, .4, pluck); melody(A2, bt(24, 0), hop, .42, pluck);
  melody(COUNTER, bt(16, 0), hands, .36, bell, .96);
  // A A' again, the ring doubling a third below, softly
  for (let bar = 32; bar < 48; bar++) oompah(bar, bar < 40 ? HA : HA2, .95);
  melody(A, bt(32, 0), hands, .55, bell); melody(A2, bt(40, 0), hands, .58, bell);
  const third = seq => seq.map(([m, d]) => [m - (m === 71 || m === 76 ? 3 : 4), d]);
  melody(third(A), bt(32, 0), hop, .16, pluck); melody(third(A2), bt(40, 0), hop, .17, pluck);
  cursor = bt(48, 0);
  // tag: the last four bars twice, easing, ending on A with every laptop
  const TAG = [[74, 2], [76, 1], [77, 2], [76, 1], [74, 1], [72, 1], [71, 1], [69, 3]];
  for (const b of [124, 112]) {
    setTempo(b);
    const s = cursor;
    for (let bar = 0; bar < 4; bar++) { const [, root, chord] = HA2[4 + bar]; ev(BASS, s + bar * 3 * beat, .55 * beat, root, .45, 'triangle', .008, .25 * beat); for (const k of [1, 2]) chord.forEach(m => pluck(WALK[k === 1 ? 1 : 4], s + (bar * 3 + k) * beat, .5 * beat, m, .15, false)); }
    melody(TAG, s, hands, .5, bell);
    cursor = s + 12 * beat;
  }
  [45, 57, 60, 64, 69].forEach((m, k) => pluck(WALK[k], cursor - 3 * beat, 3 * beat, m, .35, false));
  cursor += .6;
});

// ── IV · Chase (F, 4/4) ───────────────────────────────────────────────
warbleCents = 0;
chapter('IV · Chase', 'two runners circle the ring, one chasing the inversion of the other, faster and faster; a crash', .8, () => {
  const SCALE = [65, 67, 69, 70, 72, 74, 76, 77];
  const run = [...SCALE, ...[...SCALE].reverse().slice(1, 7)]; // up and down, 14 eighths, then a breath
  for (let round = 0; round < 2; round++) { // twice: the second chase is faster and ends the same way
  for (const [b, reps] of round === 0 ? [[152, 3], [168, 3], [184, 2]] : [[168, 2], [184, 2], [200, 2]]) {
    setTempo(b);
    const start = cursor, eighth = beat / 2;
    for (let r = 0; r < reps; r++) {
      const t0 = start + r * 8 * beat;
      run.forEach((m, i) => stacc(WALK[(walkHop + i) % RING], t0 + i * eighth, eighth * .8, m, .4));
      run.forEach((m, i) => stacc(WALK[(walkHop + i - 2 + RING * 2) % RING], t0 + (i + 4) * eighth, eighth * .8, 144 - m - 12, .3, 'triangle')); // the pursuer: inverted, an octave down, two seats behind
      walkHop += run.length;
      for (let k = 0; k < 4; k++) tap(TAP_F, t0 + k * 2 * beat, true, .18);
      bell(VOICE, t0 + 7 * beat, beat * .9, r % 2 ? 77 : 84, .35); // the hands call out the top at the breath
    }
    cursor = start + reps * 8 * beat;
  }
  // the chromatic finale: everyone up a half step at a time, then the pile-up
  const start = cursor, e = beat / 2;
  for (let i = 0; i < 12; i++) stacc(WALK[i % RING], start + i * e * (1 - i * .03), e * .7, 65 + i, .45);
  const pile = start + 12 * e * .8;
  [77, 78, 80, 83].forEach((m, k) => stacc(WALK[k], pile, 1.2 * beat, m, .5, 'triangle'));
  bell(VOICE, pile, 1.2 * beat, 86, .5);
  crash(pile + 1.2 * beat, .5);
  cursor = pile + 1.2 * beat + .6 + (round === 0 ? 1.6 : 2.4); // silence
  if (round === 0) bell(VOICE, cursor - 1.2, 1.0, 72, .3); // the hands: "again"
  }
});

// ── V · Sneak (Cm, 4/4) ───────────────────────────────────────────────
chapter('V · Sneak', 'an offbeat bass creeps one seat a bar around the audience; the hands tiptoe down a whole-tone stair; the reveal', .5, () => {
  setTempo(84);
  const start = cursor, barLen = 4 * beat, bt = (bar, b) => start + bar * barLen + b * beat;
  function creep(bar, fast) {
    const seat = bar % RING, root = bar % 2 ? 43 : 48;
    const beats = fast ? [.5, 1.5, 2.5, 3.5] : [1, 3];
    for (const b of beats) stacc(WALK[seat], bt(bar, b), .16, root, .42, 'triangle');
    ev(WALK[seat], bt(bar, 0), .03, null, .12, 'noise', .001, .02, null, 2500);
  }
  const TIPTOE = [[75, 1], [73, 1], [71, 1], [0, 1], [69, 1], [67, 1], [65, 1], [0, 1], [63, 1], [61, 1], [59, 1], [0, 1], [0, 4]];
  for (let bar = 0; bar < 12; bar++) creep(bar, false);
  melody(TIPTOE, bt(0, 0), () => VOICE, .42, (i, t, d, m, g) => ev(i, t, d, m, g, 'sine', .003, d * .55), .55);
  melody(TIPTOE.map(([m, d]) => [m ? m - 12 : 0, d]), bt(4, 0), () => VOICE, .4, (i, t, d, m, g) => ev(i, t, d, m, g, 'sine', .003, d * .55), .55);
  // the scurry: up the stair fast, then the reveal chord and silence
  const scurry = [59, 61, 63, 65, 67, 69, 71, 73, 75, 77];
  scurry.forEach((m, i) => stacc(WALK[i % RING], bt(8, 0) + i * beat / 2, beat * .35, m, .35));
  bell(VOICE, bt(11, 0), 2 * beat, 79, .3);
  [60, 63, 67, 72, 75].forEach((m, k) => pluck(WALK[k], bt(12, 0), 2 * beat, m, .55, false));
  bell(VOICE, bt(12, 0), 2 * beat, 84, .55);
  // silence for two beats, then the sneak again at double speed, and a last low C
  for (let bar = 13; bar < 19; bar++) creep(bar, true);
  melody(TIPTOE.slice(0, 11), bt(13, 0), () => VOICE, .38, (i, t, d, m, g) => ev(i, t, d, m, g, 'sine', .003, d * .55), .3);
  stacc(WALK[4], bt(19, 0), 1.5 * beat, 36, .5, 'triangle');
  bell(VOICE, bt(19, 0), 2 * beat, 60, .4);
  cursor = bt(20, 0) + 1.2;
});

// ── VI · Lullaby (F, 6/8) ─────────────────────────────────────────────
warbleCents = 8;
chapter('VI · Lullaby', 'a cradle of arpeggios rocks around the room; the tune in the hands; the whole field turns once', .45, () => {
  setTempo(72); // dotted-quarter beats, two to a bar
  const start = cursor, eighth = beat / 3, barLen = 2 * beat, bt = (bar, e) => start + bar * barLen + e * eighth;
  const CH = [[53, 57, 60], [53, 57, 60], [58, 62, 65], [60, 64, 67], [53, 57, 60], [50, 53, 57], [58, 62, 65], [60, 64, 67]];
  const TUNE = [[81, 3], [79, 2], [77, 1], [76, 3], [77, 3], [79, 2], [81, 2], [82, 2], [81, 6], [79, 3], [77, 2], [76, 1], [74, 3], [76, 3], [77, 2], [79, 2], [76, 2], [77, 6]];
  const BARS = 44;
  for (let bar = 0; bar < BARS; bar++) {
    const ch = CH[bar % 8], fade = bar >= 36 ? 1 - (bar - 36) / 8 : 1;
    for (let e = 0; e < 6; e++) pluck(WALK[walkHop++ % RING], bt(bar, e), eighth * 1.6, ch[e % 3] + (e >= 3 ? 12 : 0), .17 * fade, false);
    if (bar % 8 === 0 && bar < 36) ch.forEach((p, m) => pad(PAD[m], bt(bar, 0), 8 * barLen * .95, p + 12, .11));
  }
  const rock = (i, t, d, m, g) => bell(i, t, d, m, g);
  const eighthMel = (seq, t0, where, g, inst) => { let t = t0; for (const [m, d] of seq) { if (m) inst(where, t, d * eighth * .92, m, g); t += d * eighth; } };
  eighthMel(TUNE, bt(0, 0), VOICE, .46, rock);
  eighthMel(TUNE, bt(8, 0), VOICE, .48, rock);
  // the hum: the tune an octave down in the hands, echoed by the ring a bar later
  eighthMel(TUNE.map(([m, d]) => [m - 12, d]), bt(16, 0), VOICE, .42, rock);
  eighthMel(TUNE.map(([m, d]) => [m - 12, d]), bt(24, 0), VOICE, .4, rock);
  { let t = bt(17, 0), k = 0; for (const [m, d] of TUNE) { soft(ECHO[(k * 2) % RING], t, d * eighth * .9, m, .14); t += d * eighth; k++; } }
  { let t = bt(25, 0), k = 0; for (const [m, d] of TUNE) { soft(ECHO[(k * 2 + 1) % RING], t, d * eighth * .9, m - 12, .12); t += d * eighth; k++; } }
  turnsPlan.push({ type: 'tour', t0: start, t1: start + BARS * barLen, laps: 1 });
  bell(VOICE, bt(40, 0), 4 * barLen, 65, .35);
  cursor = start + BARS * barLen + 1.0;
});

// ── VII · The Climb (C, 4/4) ──────────────────────────────────────────
warbleCents = 16;
chapter('VII · The Climb', 'four registers an octave apart, a high line gliding against the ring, hats, then the break', .9, () => {
  const C = { pads: true, walkWhere: 'ring', theme: true };
  setTempo(112); phrase({ ...C, lvl: .9, register: -1, echoes: 2 });
  setTempo(116); phrase({ ...C, lvl: .95, register: 0, echoes: 3 });
  setTempo(120); phrase({ ...C, lvl: 1, register: 1, echoes: 3, top: true });
  setTempo(124); phrase({ ...C, lvl: 1, register: 2, echoes: 2, top: true, hats: true, fill: false, breakBar: true });
  const b8 = cursor - 4 * beat;
  [62, 64, 66, 67, 69].forEach((m, k) => pluck(WALK[k], b8 + k * .5 * beat, .45 * beat, m, .45, false)); // D E F# G A into silence
});

// ── VIII · The Lift (D, 4/4) ──────────────────────────────────────────
warbleCents = 22;
chapter('VIII · The Lift', 'up a whole step: the theme leaves the hands and orbits over kick and snare; the blast; the eight-turn spin; the tutti', 1, () => {
  const D = { tr: 2, register: -1, walkWhere: 'ring', walkPartials: false, theme: true, themeWhere: 'orbit', drums: true, taps: false, pads: true, echoes: 2, fill: false, cadence: false };
  turnsPlan.push({ type: 'blast', t: cursor });
  setTempo(120); phrase({ ...D, lvl: .95 });
  setTempo(120); phrase({ ...D, lvl: 1, answer: true });
  setTempo(122); phrase({ ...D, lvl: 1, answer: true, hats: true });
  const spin0 = cursor;
  setTempo(124); phrase({ ...D, lvl: 1.05, answer: true, hats: true, lift: true });
  setTempo(124); phrase({ ...D, lvl: 1.05, answer: true, hats: true, lift: true });
  turnsPlan.push({ type: 'spin', t0: spin0, t1: cursor, turns: 8 });
  setTempo(122); phrase({ ...D, lvl: 1, answer: true, hats: true });
  setTempo(118); phrase({ ...D, lvl: 1, echoes: 1, finalChord: true });
  cursor += 2.6; // general pause
});

// ── IX · Fanfare (G, 4/4) ─────────────────────────────────────────────
warbleCents = 0;
chapter('IX · Fanfare', 'the front pair calls, the back pair answers, brass in triangle waves; the hands take the theme in G; a stinger', .85, () => {
  setTempo(132);
  const start = cursor, barLen = 4 * beat, bt = (bar, b) => start + bar * barLen + b * beat;
  const CALL = [[74, 1.5], [74, .5], [79, 2], [83, 1.5], [81, .5], [79, 2]];
  const ANS = [[71, 1.5], [71, .5], [74, 2], [79, 1.5], [78, .5], [74, 2]];
  const front = (k) => WALK[[0, 1][k % 2]], back = (k) => WALK[[2, 3][k % 2]];
  for (let rep = 0; rep < 2; rep++) {
    melody(CALL, bt(rep * 4, 0), front, .45, brass, .85);
    melody(ANS, bt(rep * 4 + 2, 0), back, .4, brass, .85);
    for (let b = 0; b < 4; b++) tap(TAP_F, bt(rep * 4 + b, 0), true, .22);
  }
  // tutti: the chord in dotted rhythm from every seat, the theme's head in G from the hands
  const G = [55, 59, 62, 67, 71];
  for (let bar = 8; bar < 16; bar++) for (const [b, d] of [[0, 1.5], [1.5, .5], [2, 2]]) G.forEach((m, k) => brass(WALK[k], bt(bar, b), d * beat * .85, m, .3));
  const themeG = THEME_PHRASES[0].map(([a, d, m]) => [m + 7, d]);
  melody(themeG, bt(8, 0), () => VOICE, .55, bell, .9);
  melody(THEME_PHRASES[3].map(([a, d, m]) => [m + 7, d]), bt(12, 0), () => VOICE, .55, bell, .9);
  for (let bar = 8; bar < 16; bar++) { ev(BASS, bt(bar, 0), .5 * beat, 43, .5, 'triangle', .008, .2); ev(BASS, bt(bar, 2), .5 * beat, 50, .45, 'triangle', .008, .2); }
  // call and answer once more, then the stinger
  melody(CALL, bt(16, 0), front, .5, brass, .85);
  melody(ANS.map(([m, d]) => [m + 12, d]), bt(18, 0), back, .45, brass, .85);
  for (let b = 0; b < 4; b++) tap(TAP_F, bt(16 + b, 0), true, .22);
  G.forEach((m, k) => brass(WALK[k], bt(20, 0), .35 * beat, m + 12, .6));
  bell(VOICE, bt(20, 0), .35 * beat, 91, .6);
  ev(BASS, bt(20, 0), .35 * beat, 43, .6, 'triangle', .005, .1);
  cursor = bt(20, 0) + .35 * beat + 1.6; // silence
});

// ── X · Return (C, 4/4) ───────────────────────────────────────────────
warbleCents = 9;
chapter('X · Return', 'home in C, the theme back in the hands; the room takes one slow tour; the ring mirrors; something leaves every phrase', .6, () => {
  turnsPlan.push({ type: 'tour', t0: cursor, t1: cursor + 20, laps: 1 });
  setTempo(104); phrase({ lvl: .9, theme: true, echoes: 3, pads: true, walkWhere: 'ring' });
  setTempo(100); phrase({ lvl: .85, theme: true, echoes: 2, pads: true, mirror: true, bass: false });
  setTempo(96); phrase({ lvl: .75, theme: true, echoes: 1, pads: true, walkWhere: 'front', bass: false });
  setTempo(94); phrase({ lvl: .7, theme: true, echoes: 1, walkWhere: 'front', mirror: true, bass: false, taps: false });
  setTempo(92); phrase({ lvl: .65, theme: true, echoes: 0, walkWhere: 'seat1', walkPartials: false, bass: false, taps: false });
  setTempo(88); phrase({ lvl: .5, theme: true, walk: false, bass: false, taps: false, fill: false, cadence: false });
});
free();

// ── XI · Vanish (free) ────────────────────────────────────────────────
warbleCents = 0;
chapter('XI · Vanish', 'the ring hands the last phrase back, one laptop at a time; the hands close on C; one tap from behind', .3, () => {
  let t = cursor + 1.2;
  const LAST = [83, 79, 76, 74, 72];
  for (let k = 0; k < RING; k++) { pluck(WALK[(RING - k) % RING], t, 2.4, LAST[k], .42); t += 3.2 + k * .4; }
  for (const [m, dur, gap] of [[67, 3.4, 3.0], [64, 3.8, 4.2], [60, 4.4, 5.6]]) { bell(VOICE, t, dur, m, .45); t += dur + gap; }
  ev(VOICE, t, 10, 60, .45, 'sine', 3.5, 5);
  tap(TAP_BL, t + 10 + 2.6, true, .3); // the hang-up
  cursor = t + 10 + 2.6 + 3;
});
const END = cursor;

// ── the field turns ───────────────────────────────────────────────────
const ease5 = u => u * u * u * (u * (u * 6 - 15) + 10);
function shiftAt(t) {
  let f = 0;
  for (const p of turnsPlan) {
    if (p.type === 'blast' && t >= p.t && t < p.t + 4.8) { const x = t - p.t, w = 2 * Math.PI * .92; f += .4 * Math.exp(-.58 * w * x) * Math.cos(w * x); }
    if (p.type === 'spin') { if (t >= p.t1) f += 2 * p.turns; else if (t >= p.t0) f += 2 * p.turns * ease5((t - p.t0) / (p.t1 - p.t0)); }
    if (p.type === 'tour') { if (t >= p.t1) f += 2 * p.laps; else if (t >= p.t0) f += 2 * p.laps * ease5((t - p.t0) / (p.t1 - p.t0)); }
  }
  return f;
}
const SHIFT_HZ = 25, turns = [];
for (let i = 0; i <= Math.ceil(END * SHIFT_HZ); i++) turns.push(+shiftAt(i / SHIFT_HZ).toFixed(4));

for (const l of lanes) l.events.sort((a, b) => a.t - b.t);
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
console.log('chapter          start   length  events  maxvoices  tempo');
for (const m of movements) {
  const evs = all.filter(e => e.t >= m.t0 && e.t < m.t1);
  const bpms = tempo.filter(x => x.t >= m.t0 && x.t < m.t1 && x.bpm).map(x => x.bpm);
  const tp = bpms.length ? `${bpms[0]}→${bpms.at(-1)}` : 'free';
  console.log(`${m.name.padEnd(16)} ${mmss(m.t0).padStart(6)}  ${mmss(m.t1 - m.t0).padStart(6)}  ${String(evs.length).padStart(6)}  ${String(maxVoices(m.t0, m.t1)).padStart(9)}  ${tp}`);
}
