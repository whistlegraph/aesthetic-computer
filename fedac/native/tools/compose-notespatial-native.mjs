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
//   VIII The Lift      D  4/4    the theme leaves the hands and hops the ring; the blast; the eight-turn spin; the tutti
//   IX   Fanfare       G  4/4    front pair calls, back pair answers, brass in triangle waves; a stinger
//   X    Return        C  4/4    the theme back in the hands; the tour; the mirror; subtraction
//   XI   Vanish        free      the ring hands back the last phrase; the hands close on C; one tap from behind
//
// Envelopes are stacks of sine partials with shorter upper tails (bell,
// pluck, brass). The held voice warbles against a detuned copy, deeper as
// the piece goes on. The whole field can turn (fieldShift), so laptops
// trade channels: a blast, a spin, a tour.
//
// Written for an audience INSIDE the ring, on laptop speakers (see
// papers/chamber-platter/digest/05-hearing-inside-the-ring.md): low roots
// carry their octave and twelfth because a laptop passes little under
// 200 Hz; anything meant to be placed has a fast attack and an octave above
// 1.5 kHz; the theme and its answer hop seat to seat rather than glide,
// since off-center nobody hears a crossfade as motion; the walk keeps to the
// far side of the ring from the tune; a runner holds a seat 200 ms or more.
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
// The theme and its answer travel the ring. By default they HOP: each note is
// pinned whole to the seat nearest its orbit angle at onset, so one laptop
// plays it (no two-laptop crossfade to flam under clock skew, and no phantom
// image between seats, which only a centered listener would hear anyway;
// chamber-platter digest 05). `--glide` writes them to continuously panning
// lanes instead, the pre-September-23 behaviour.
const GLIDE = process.argv.includes('--glide');
const THEME_ORBIT = { orbitSeconds: 12.5, orbitDirection: 1 }, ANSWER_ORBIT = { orbitSeconds: 15, orbitDirection: -1 };
const THEME = GLIDE ? lane('theme (orbit)', [255, 194, 72], THEME_ORBIT) : Array.from({ length: RING }, (_, k) => lane(`theme ${k + 1}`, [255, 194, 72], { az: ringAz(k) }));
const ANSWER = GLIDE ? lane('answer (orbit)', [116, 210, 235], ANSWER_ORBIT) : Array.from({ length: RING }, (_, k) => lane(`answer ${k + 1}`, [116, 210, 235], { az: ringAz(k) }));
const HATS = lane('hats', [230, 233, 241], { orbitSeconds: 5 });
PAD.forEach((i, m) => { lanes[i].azOffset = m * TAU / 3 - i / lanes.length * TAU; });
for (const l of lanes) if (!(l.center || Number.isFinite(l.az) || l.orbitSeconds > 0)) throw Error(`${l.name}: neither pinned, centered nor orbiting`);

// ── instruments ───────────────────────────────────────────────────────
// label: undefined → named from midi; null → unlabeled (partials, drums). freq overrides the pitch.
function ev(i, t, dur, midi, g, wave = 'sine', attack = .01, decay = .06, label, freq = null, extra = null) {
  if (!(dur > 0) || !(g > 0)) return;
  if (freq === null && midi === null) throw Error('event with neither midi nor freq');
  const e = { t: r4(t), dur: r4(dur), hz: +(freq ?? hz(midi)).toFixed(2), g: +g.toFixed(3), wave, attack: +attack.toFixed(4), decay: +decay.toFixed(4) };
  if (label === undefined && midi !== null) e.note = noteName(midi);
  if (Number.isInteger(extra?.gm)) e.gm = extra.gm; // a GM program the runtime passes as gmProgram; `wave` is its fallback
  lanes[i].events.push(e);
}
let warbleCents = 0; // the wannadash wiggle: a detuned copy beating a few Hz, ramped in after the onset
const warble = (i, t, dur, midi, g, tail = .7) => { if (warbleCents > 0 && dur > .5) ev(i, t, dur, null, g, 'sine', .45, dur * tail, null, hz(midi) * 2 ** (warbleCents / 1200)); };

// ── voicings ──────────────────────────────────────────────────────────
// One score, several orchestras. A voicing maps each instrument family
// (held, ring, echo, pad, bass, top, brass, theme, answer, stacc) to a
// recipe; `--voicing NAME` picks a table and `--set held=whistle,ring=marimba`
// overrides families one at a time. `sine` is the September 22 orchestra
// and the default. Recipes are stacks of sine partials (any laptop, and the
// render previews them faithfully), the engine's own instruments (whistle:
// the STK flute waveguide; harp: Karplus-Strong; piano: the Salamander
// bank), or GM programs by number, which the runtime passes as gmProgram
// and which fall back to the named wave on a machine that has not
// implemented the program. Drums, taps and the crash are the same in every
// voicing.
const R = {};
// sine stacks: the original orchestra
R.bell = (i, t, dur, midi, g) => { // the held voice
  ev(i, t, dur, midi, g, 'sine', .004, dur * .8);
  ev(i, t, dur * .55, midi + 12, g * .28, 'sine', .002, dur * .45, null, hz(midi) * 2);
  ev(i, t, dur * .3, midi + 19, g * .1, 'sine', .002, dur * .25, null, hz(midi) * 3);
  warble(i, t, dur, midi, g * .5);
};
R.pluck = (i, t, dur, midi, g, partial = true) => { // the ring
  ev(i, t, dur, midi, g, 'sine', .003, dur * .78);
  if (partial === true) ev(i, t, dur * .5, midi + 12, g * .22, 'sine', .002, dur * .42, null, hz(midi) * 2);
};
R.brassTri = (i, t, dur, midi, g) => { // fanfare: triangle body, sine octave, quick bite
  ev(i, t, dur, midi, g, 'triangle', .012, dur * .35);
  ev(i, t, dur * .8, midi + 12, g * .25, 'sine', .01, dur * .3, null, hz(midi) * 2);
};
R.staccSine = (i, t, dur, midi, g, wave = 'sine') => ev(i, t, dur, midi, g, typeof wave === 'string' ? wave : 'sine', .003, dur * .6);
// Echoes: a sine with a short octave on top, so the answer carries energy
// above 1.5 kHz and can be placed by level as well as timing (digest 05 R2).
R.soft = (i, t, dur, midi, g) => { ev(i, t, dur, midi, g, 'sine', .02, dur * .7); ev(i, t, dur * .5, midi + 12, g * .22, 'sine', .002, dur * .4, null, hz(midi) * 2); };
R.padSine = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'sine', .6, dur * .5);
R.topSine = (i, t, dur, midi, g, attack = .06, decay = dur * .75) => ev(i, t, dur, midi, g, 'sine', attack, decay);
R.themeSine = (i, t, dur, midi, g) => { // the travelling theme: bell-like, with the octave that makes it placeable
  ev(i, t, dur, midi, g, 'sine', .004, dur * .7);
  ev(i, t, dur * .5, midi + 12, g * .25, 'sine', .002, dur * .4, null, hz(midi) * 2);
  warble(i, t, dur, midi, g * .5, .6);
};
R.answerTri = (i, t, dur, midi, g) => ev(i, t, dur, midi, g, 'triangle', .006, dur * .667);
// A low note on a laptop: the speaker rolls off under about 200 Hz, so the
// root is carried by its octave and twelfth and heard as the residue pitch
// (Ritsma 1967; digest 05 R1). The triangle fundamental stays for the small
// speaker and for whatever the laptops do pass.
R.bassResidue = (i, t, dur, midi, g, attack = .015, decay = null) => {
  const d = decay ?? dur * .5;
  ev(i, t, dur, midi, g, 'triangle', attack, d);
  ev(i, t, dur * .9, midi + 12, g * .5, 'sine', attack, d * .8, null, hz(midi) * 2);
  ev(i, t, dur * .7, midi + 19, g * .28, 'sine', attack, d * .6, null, hz(midi) * 3);
};
// mallets: modal ratios, relative gains and T60s from pop/marimba/synths/marimba.mjs
// (Rossing 2000 ch. 4; Fletcher & Rossing 1998 ch. 19). Each partial rings for
// its own T60, or a little past the written note, whichever is shorter; rings
// halve every two octaves above middle C; a 30 ms noise tick is the mallet.
// `ring` lets a bar sound past the written note (1 = damped at the note's end);
// a partial under 0.02 of gain is not written, so the 32-voice cap is spent on
// what can be heard. The mallets voicing peaks at the Lift's tutti chord.
// A caller asking for a plain sound (pluck's `partial = false`: tutti chords,
// fills, pickup laps) gets the fundamental alone, as the sine voicing does.
function modal(ratios, amps, t60, { attack = .001, tick = 0, ring = 1.15, minGain = .02 } = {}) {
  return (i, t, dur, midi, g, partial = true) => {
    const reg = 2 ** (-(midi - 60) / 24), plain = partial === false;
    ratios.forEach((r, k) => {
      if (plain && k > 0) return;
      if (g * amps[k] < minGain) return;
      const d = Math.max(.08, Math.min(dur * ring, t60[k] * reg));
      ev(i, t, d, k ? null : midi, g * amps[k], 'sine', attack, d * .85, k ? null : undefined, hz(midi) * r);
    });
    if (tick > 0 && !plain) ev(i, t, .03, null, g * tick, 'noise', .001, .025, null, 3000);
  };
}
R.marimba = modal([1, 4, 9.2], [1, .32, .10], [1.6, .32, .09], { tick: .08, ring: 1, minGain: .045 }); // the walk keeps two partials; the tick and the 9.2 partial appear only on louder notes
R.xylophone = modal([1, 3, 6, 9.6], [1, .55, .28, .1], [.45, .18, .07, .025], { tick: .05 });
R.vibraphone = modal([1, 4, 10], [1, .22, .08], [4.5, .9, .25], { attack: .002, ring: 1.1 });
R.glockenspiel = modal([1, 2.76, 5.4, 8.93], [.7, 1, .45, .18], [2.2, 1.4, .45, .12], { ring: 1.5 });
R.gamelan = modal([1, 2.4, 4.7, 7.2], [1, .5, .3, .12], [3.2, 1.2, .45, .12], { attack: .003 });
R.kalimba = modal([1, 5.9, 8.1], [1, .2, .08], [1.8, .3, .12], { attack: .002, ring: 1 });
R.woodblock = modal([1, 1.8, 2.7, 4.1, 6.3], [1, .85, .65, .4, .2], [.18, .1, .06, .03, .015], { tick: .35 });
R.gong = modal([1, 2.4], [1, .5], [3.2, 1.2], { attack: .25, ring: 1 }); // the gamelan bar struck softly, as a pad: two partials, since three pad lanes hold them for whole phrases
R.bassMarimba = (i, t, dur, midi, g, attack = .002) => modal([1, 2, 3], [1, .5, .28], [2.4, 1.9, 1.2], { attack: typeof attack === 'number' ? attack : .002, ring: 1 })(i, t, dur, midi, g); // the bass bar with the residue octave and twelfth (R1)
// the engine's own instruments
R.whistle = (i, t, dur, midi, g) => ev(i, t, dur, midi, g * .9, 'whistle', .05, dur * .3);
R.harp = (i, t, dur, midi, g) => { const d = Math.max(dur, .3); ev(i, t, d, midi, g, 'harp', .001, d * .6); };
R.piano = (i, t, dur, midi, g) => ev(i, t, dur, midi, g * .9, 'piano', .001, dur * .35);
R.sawBass = (i, t, dur, midi, g, attack = .01, decay = null) => { // a sawtooth has every harmonic, so a laptop passes its pitch on its own
  const a = typeof attack === 'number' ? attack : .01, d = typeof decay === 'number' ? decay : dur * .5;
  ev(i, t, dur, midi, g * .6, 'sawtooth', a, d);
  ev(i, t, dur * .9, midi + 12, g * .3, 'sine', a, d * .8, null, hz(midi) * 2);
};
R.sawBrass = (i, t, dur, midi, g) => { ev(i, t, dur, midi, g * .55, 'sawtooth', .02, dur * .4); ev(i, t, dur, null, g * .4, 'square', .03, dur * .35, null, hz(midi) * 2 ** (6 / 1200)); }; // a detuned pair: the chorus of digest 04, rule 3
R.sawPad = (i, t, dur, midi, g) => { ev(i, t, dur, midi, g * .5, 'sawtooth', .6, dur * .5); ev(i, t, dur, null, g * .5, 'sawtooth', .6, dur * .5, null, hz(midi) * 2 ** (-7 / 1200)); };
R.squareReed = (i, t, dur, midi, g) => { ev(i, t, dur, midi, g * .6, 'square', .03, dur * .4); ev(i, t, dur, null, g * .25, 'sine', .01, dur * .5, null, hz(midi) * 2); };
// GM programs, 0-based: 8 celesta, 11 vibraphone, 12 marimba, 13 xylophone, 38 synth bass 1,
// 61 brass section, 71 clarinet, 73 flute, 79 ocarina, 89 warm pad. The wave is the fallback.
const gm = (program, wave = 'sine', attack = .005, tail = .5) => (i, t, dur, midi, g, a, d) =>
  ev(i, t, dur, midi, g, wave, typeof a === 'number' ? a : attack, typeof d === 'number' ? d : dur * tail, undefined, null, { gm: program });
R.gmFlute = gm(73, 'whistle', .05, .3); R.gmMarimba = gm(12, 'sine', .001, .8); R.gmCelesta = gm(8, 'sine', .001, .7);
R.gmPad = gm(89, 'triangle', .6, .5); R.gmBass = gm(38, 'sawtooth', .01, .5); R.gmOcarina = gm(79, 'sine', .04, .5);
R.gmBrass = gm(61, 'sawtooth', .02, .4); R.gmVibes = gm(11, 'sine', .002, .8); R.gmClarinet = gm(71, 'square', .02, .5); R.gmXylo = gm(13, 'sine', .001, .6);

const VOICINGS = {
  sine:    { held: 'bell', ring: 'pluck', echo: 'soft', pad: 'padSine', bass: 'bassResidue', top: 'topSine', brass: 'brassTri', theme: 'themeSine', answer: 'answerTri', stacc: 'staccSine' },
  mallets: { held: 'vibraphone', ring: 'marimba', echo: 'kalimba', pad: 'gong', bass: 'bassMarimba', top: 'glockenspiel', brass: 'gamelan', theme: 'marimba', answer: 'xylophone', stacc: 'xylophone' },
  native:  { held: 'whistle', ring: 'harp', echo: 'harp', pad: 'sawPad', bass: 'sawBass', top: 'whistle', brass: 'sawBrass', theme: 'harp', answer: 'piano', stacc: 'harp' },
  gm:      { held: 'gmFlute', ring: 'gmMarimba', echo: 'gmCelesta', pad: 'gmPad', bass: 'gmBass', top: 'gmOcarina', brass: 'gmBrass', theme: 'gmVibes', answer: 'gmClarinet', stacc: 'gmXylo' },
};
const opt = (k, d) => { const i = process.argv.indexOf('--' + k); return i >= 0 ? process.argv[i + 1] : d; };
const VOICING = opt('voicing', 'sine');
if (!VOICINGS[VOICING]) throw Error(`no voicing "${VOICING}"; have ${Object.keys(VOICINGS).join(', ')}`);
const SETS = process.argv.flatMap((a, i) => a === '--set' ? (process.argv[i + 1] || '').split(',') : []).filter(Boolean).map(s => s.split('='));
const table = { ...VOICINGS[VOICING] };
for (const [family, recipe] of SETS) {
  if (!(family in table)) throw Error(`--set: no family "${family}"; have ${Object.keys(table).join(', ')}`);
  if (!R[recipe]) throw Error(`--set: no recipe "${recipe}"; have ${Object.keys(R).join(', ')}`);
  table[family] = recipe;
}
const I = Object.fromEntries(Object.entries(table).map(([f, r]) => [f, R[r]]));
const bell = (...a) => I.held(...a), pluck = (...a) => I.ring(...a), soft = (...a) => I.echo(...a), pad = (...a) => I.pad(...a);
const bassNote = (...a) => I.bass(...a), topNote = (...a) => I.top(...a), brass = (...a) => I.brass(...a);
const themeNote = (...a) => I.theme(...a), answerNote = (...a) => I.answer(...a), stacc = (...a) => I.stacc(...a); // phrase() destructures `theme`, `answer` and `top` as flags, hence the names
const FX = opt('fx', 'none');
if (!['none', 'studio'].includes(FX)) throw Error('--fx none|studio');
const TAG = ((VOICING === 'sine' && !SETS.length ? '' : '-' + [VOICING, ...SETS.map(([f, r]) => `${f}-${r}`)].join('-')) + (FX === 'studio' ? '-fx' : '')).toLowerCase();
function tap(i, t, low, g) {
  ev(i, t, .27, null, g, 'sine', .012, .22, null, low ? 130 : 330);
  ev(i, t + .02, .31, null, g * .5, 'sine', .02, .26, null, low ? 90 : 220);
  ev(i, t, .03, null, g * .35, 'noise', .001, .02, null, 1000);
}
// Kick: the 78 Hz body is below what a laptop passes, so its second harmonic
// and a click carry the hit; the body stays for any speaker that has it.
function kick(t, g) {
  ev(KICK, t, .03, null, .3 * g, 'noise', .001, .02, null, 2500); // 30 ms: nothing shorter survives a 25 ms scheduler frame
  ev(KICK, t, .055, null, .8 * g, 'sine', .001, .04, null, 150);
  ev(KICK, t + .06, .16, null, .75 * g, 'sine', .001, .11, null, 78);
  ev(KICK, t + .06, .14, null, .45 * g, 'sine', .001, .1, null, 156);
}
const hits = []; // moments the studio effects plan may glitch on: the crashes and the hang-up
const crash = (t, g) => { hits.push(t); for (let k = 0; k < RING; k++) ev(WALK[k], t + k * .012, .5, null, g, 'noise', .002, .4, null, 3000); ev(VOICE, t, .4, null, g * .6, 'noise', .002, .3, null, 3000); };
function orbitAngle(i, t) { const l = lanes[i]; return i / lanes.length * TAU + (l.azOffset || 0) + t / l.orbitSeconds * TAU * (l.orbitDirection === -1 ? -1 : 1); }
const ringSeatOf = angle => ((Math.round(angle / TAU * RING) % RING) + RING) % RING;
// Hop law for the theme and the answer: the seat nearest the orbit angle,
// measured from `hopOrigin` so the theme leaves the hands at the front seat.
let hopOrigin = 0;
const hopSeat = (o, t) => ringSeatOf((t - hopOrigin) / o.orbitSeconds * TAU * (o.orbitDirection === -1 ? -1 : 1));
const themeSeat = t => GLIDE ? ringSeatOf(orbitAngle(THEME, t)) : hopSeat(THEME_ORBIT, t);
const themeLane = t => GLIDE ? THEME : THEME[hopSeat(THEME_ORBIT, t)];
const answerLane = t => GLIDE ? ANSWER : ANSWER[hopSeat(ANSWER_ORBIT, t)];

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
        else themeNote(themeLane(tn), tn, dur, midi, .55 * soft_ * swell);
        if (answer && a >= 4 && bar >= 2) answerNote(answerLane(tn + .5 * beat), tn + .5 * beat, dur * .6, midi - 12, .26 * swell);
        if (echoes && echoOf === 'theme' && (a === 0 || a === 2 || a === 4)) {
          const base = themeWhere === 'orbit' ? themeSeat(tn) : echoHop;
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
        // While the theme travels, the walk keeps to the two seats across the ring from it (144° or more), so the tune is not masked by its own accompaniment (digest 05 R4).
        const seat = walkWhere === 'ring' ? (theme && themeWhere === 'orbit' ? (themeSeat(tn) + 2 + walkHop % 2) % RING : walkHop % RING) : walkWhere === 'split' ? (bar < 4 ? FRONT[walkHop % 3] : BACK[walkHop % 2]) : walkWhere === 'front' ? FRONT[walkHop % 3] : 0;
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
      if (drums) { for (const b of [0, 1.5, 2, 3.5]) bassNote(BASS, bt(bar, b), .42 * beat, root, .5 * swell, .008, .1); }
      else if (last) bassNote(BASS, bt(bar, 0), 3.6 * beat, root, .4 * swell, .02, 2 * beat);
      else for (const b of [0, 2]) bassNote(BASS, bt(bar, b), 1.6 * beat, root, .42 * swell, .015, .8 * beat);
    }
    if (pads && bar % 2 === 0) chord.forEach((p, m) => pad(PAD[m], bt(bar, 0), 7.6 * beat, p, .13 * swell));
    if (top && bar % 2 === 1) {
      const high = Math.min(106, chord[1] + 24 + register * 12), tn = bt(bar, .36);
      topNote(TOP, tn, .40, high, .055 * soft_ * swell, .06, .3);
      topNote(TOP, tn + .46 * beat, .52, Math.min(108, high + 2), .04 * soft_ * swell, .08, .4);
    }
    if (hats && !(last && breakBar)) for (let b = 0; b < 4; b += .5) ev(HATS, bt(bar, b), b % 1 ? .045 : .027, null, (b % 1 ? .3 : .19) * swell, 'noise', .001, .02, null, 7000);
    if (drums) {
      for (const a of [0, 2, ...(bar % 4 === 3 ? [3.5] : [])]) kick(bt(bar, a), swell);
      if (!(last && finalChord)) for (const a of [1, 3]) { ev(SNARE, bt(bar, a), .11, null, .4 * swell, 'noise', .001, .08, null, 2400); ev(SNARE, bt(bar, a), .095, null, .45 * swell, 'triangle', .001, .07, null, 185); }
    }
  }
  if (centerHold) bell(VOICE, bt(7, 0), 4 * beat, 67 + tr, .5 * lvl);
  if (finalChord) {
    [60, 64, 67, 72, 76].forEach((m, k) => pluck(WALK[k], bt(6, 0), 8 * beat, m + tr, .5 * lvl, false));
    bell(VOICE, bt(6, 0), 8 * beat, 84 + tr, .55 * lvl);
    bassNote(BASS, bt(6, 0), 8 * beat, 48 + tr, .5 * lvl, .01, 4 * beat);
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
    bassNote(BASS, bt(bar, 0), .55 * beat, root, .5 * lvl, .008, .25 * beat);
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
    for (let bar = 0; bar < 4; bar++) { const [, root, chord] = HA2[4 + bar]; bassNote(BASS, s + bar * 3 * beat, .55 * beat, root, .45, .008, .25 * beat); for (const k of [1, 2]) chord.forEach(m => pluck(WALK[k === 1 ? 1 : 4], s + (bar * 3 + k) * beat, .5 * beat, m, .15, false)); }
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
    // A seat must hold a runner for 200 ms or more to read as a place rather than a texture (digest 05 R5); above 150 BPM each seat takes two eighths.
    const per = eighth < .2 ? 2 : 1;
    for (let r = 0; r < reps; r++) {
      const t0 = start + r * 8 * beat;
      run.forEach((m, i) => stacc(WALK[(walkHop + Math.floor(i / per)) % RING], t0 + i * eighth, eighth * .8, m, .4));
      run.forEach((m, i) => stacc(WALK[(walkHop + Math.floor(i / per) - 2 + RING * 2) % RING], t0 + (i + 4) * eighth, eighth * .8, 144 - m - 12, .3, 'triangle')); // the pursuer: inverted, an octave down, two seats behind
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
    // The creep is the one gesture whose whole point is WHERE; at 98 to 131 Hz a laptop barely passes it and nobody can place it, so every step carries its octave, twelfth and a tick (digest 05 R1, R2).
    for (const b of beats) { bassNote(WALK[seat], bt(bar, b), .16, root, .42, .003, .1); ev(WALK[seat], bt(bar, b), .03, null, .1, 'noise', .001, .02, null, 2500); }
    ev(WALK[seat], bt(bar, 0), .04, null, .2, 'noise', .001, .03, null, 2500);
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
  bassNote(WALK[4], bt(19, 0), 1.5 * beat, 36, .5, .003, .9 * beat);
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
chapter('VIII · The Lift', 'up a whole step: the theme leaves the hands and hops the ring over kick and snare; the blast; the eight-turn spin; the tutti', 1, () => {
  const D = { tr: 2, register: -1, walkWhere: 'ring', walkPartials: false, theme: true, themeWhere: 'orbit', drums: true, taps: false, pads: true, echoes: 2, fill: false, cadence: false };
  hopOrigin = cursor; // the theme leaves the hands and lands at the front seat, then hops clockwise one seat every 2.5 s
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
  for (let bar = 8; bar < 16; bar++) { bassNote(BASS, bt(bar, 0), .5 * beat, 43, .5, .008, .2); bassNote(BASS, bt(bar, 2), .5 * beat, 50, .45, .008, .2); }
  // call and answer once more, then the stinger
  melody(CALL, bt(16, 0), front, .5, brass, .85);
  melody(ANS.map(([m, d]) => [m + 12, d]), bt(18, 0), back, .45, brass, .85);
  for (let b = 0; b < 4; b++) tap(TAP_F, bt(16 + b, 0), true, .22);
  G.forEach((m, k) => brass(WALK[k], bt(20, 0), .35 * beat, m + 12, .6));
  bell(VOICE, bt(20, 0), .35 * beat, 91, .6);
  bassNote(BASS, bt(20, 0), .35 * beat, 43, .6, .005, .1);
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
  tap(TAP_BL, t + 10 + 2.6, true, .3); hits.push(t + 10 + 2.6); // the hang-up
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
  name: 'Note(s)pat(ial) Native' + (TAG ? ` · ${TAG.slice(1)}` : ''), geometry: 'ring', seats: SEATS, ring: RING, center: CENTER, seatColors,
  dur: r4(END), gain: .36, swing: .6, tempo, movements, fieldShift: turns, lanes, voicing: table,
};

// ── effects, as ribbons the runtime applies per seat ──────────────────
// --fx studio: the engine's room (a delay-line reverb), drive (tanh soft
// clip), wobble (a flanger) and glitch (sample-hold) as dry/wet mixes over
// time, sampled at 4 Hz: global ribbons plus a wetter room for the held
// laptop. Chapter values glide over the first second after each door. An
// effect is colour on a seat; digest 05 R3 still holds, it never moves a
// sound. Every mix returns to zero when the piece stops.
const FX_HZ = 4, FX_KEYS = ['fxRoom', 'fxDrive', 'fxWobble', 'fxGlitch'];
if (FX === 'studio') {
  const short = m => m.name.split(' · ')[1];
  const ROOM = { Overture: .25, 'The Walk': .1, Waltz: .15, Chase: 0, Sneak: .1, Lullaby: .4, 'The Climb': .15, 'The Lift': .1, Fanfare: .2, Return: .25, Vanish: .5 };
  const DRIVE = { 'The Lift': .35, Fanfare: .25 }, WOBBLE = { Sneak: .3, 'The Climb': .15 };
  const at = t => movements.find(m => t >= m.t0 && t < m.t1) || movements.at(-1);
  const glide = tableOf => t => { const m = at(t), prev = movements[movements.indexOf(m) - 1], v = tableOf[short(m)] ?? 0; if (!prev) return v; const p = tableOf[short(prev)] ?? 0, u = t - m.t0; return u < 1 ? p + (v - p) * u : v; };
  const roomOf = glide(ROOM), driveOf = glide(DRIVE), wobbleOf = glide(WOBBLE);
  const glitchOf = t => hits.some(h => t >= h && t < h + .5) ? .7 : 0;
  const sample = f => Array.from({ length: Math.ceil(END * FX_HZ) + 1 }, (_, i) => +f(i / FX_HZ).toFixed(3));
  score.fxRoom = sample(roomOf); score.fxDrive = sample(driveOf); score.fxWobble = sample(wobbleOf); score.fxGlitch = sample(glitchOf);
  score.seatFx = { [CENTER]: { fxRoom: sample(t => Math.min(1, roomOf(t) + .15)) } };
}
const ribbonAt = (arr, t) => { const u = Math.max(0, Math.min(1, t / END)) * (arr.length - 1), i = Math.floor(u), j = Math.min(i + 1, arr.length - 1); return arr[i] + (arr[j] - arr[i]) * (u - i); };
const resample = (arr, t0, dur) => { const n = Math.ceil(dur * FX_HZ) + 1; return Array.from({ length: n }, (_, k) => +ribbonAt(arr, t0 + k * dur / (n - 1)).toFixed(3)); };

// ── write ─────────────────────────────────────────────────────────────
const dir = new URL('../scores/', import.meta.url);
await mkdir(dir, { recursive: true });
await writeFile(new URL(`notespatial-native${TAG}.nsscore`, dir), JSON.stringify(score) + '\n');
const slug = s => s.toLowerCase().replace(/^[ivx]+ · /, '').replace(/[^a-z]+/g, '-').replace(/^-|-$/g, '');
await Promise.all(movements.map((m, n) => {
  const dur = r4(m.t1 - m.t0 + 2);
  const part = {
    ...score, fieldShift: undefined, name: `${score.name} — ${m.name}`, dur, movements: [{ ...m, t0: 0, t1: r4(m.t1 - m.t0) }],
    tempo: tempo.filter(x => x.t >= m.t0 && x.t < m.t1).map(x => ({ ...x, t: r4(x.t - m.t0) })),
    lanes: lanes.map(l => ({ ...l, events: l.events.filter(e => e.t >= m.t0 && e.t < m.t1).map(e => ({ ...e, t: r4(e.t - m.t0) })) })),
  };
  for (const k of FX_KEYS) if (score[k]) part[k] = resample(score[k], m.t0, dur);
  if (score.seatFx) part.seatFx = Object.fromEntries(Object.entries(score.seatFx).map(([seat, fx]) => [seat, Object.fromEntries(Object.entries(fx).map(([k, arr]) => [k, resample(arr, m.t0, dur)]))]));
  return writeFile(new URL(`notespatial-native${TAG}-${n + 1}-${slug(m.name)}.nsscore`, dir), JSON.stringify(part) + '\n');
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
console.log(`voicing ${VOICING}${SETS.length ? ' + ' + SETS.map(s => s.join('=')).join(' ') : ''}: ${Object.entries(table).map(([f, r]) => `${f}=${r}`).join(' ')}${FX === 'studio' ? '; studio effects ribbons' : ''}`);
console.log(`→ scores/notespatial-native${TAG}.nsscore and one file per chapter`);
console.log('chapter          start   length  events  maxvoices  tempo');
for (const m of movements) {
  const evs = all.filter(e => e.t >= m.t0 && e.t < m.t1);
  const bpms = tempo.filter(x => x.t >= m.t0 && x.t < m.t1 && x.bpm).map(x => x.bpm);
  const tp = bpms.length ? `${bpms[0]}→${bpms.at(-1)}` : 'free';
  console.log(`${m.name.padEnd(16)} ${mmss(m.t0).padStart(6)}  ${mmss(m.t1 - m.t0).padStart(6)}  ${String(evs.length).padStart(6)}  ${String(maxVoices(m.t0, m.t1)).padStart(9)}  ${tp}`);
}
