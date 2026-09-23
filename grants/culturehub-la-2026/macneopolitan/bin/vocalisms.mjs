#!/usr/bin/env node
// vocalisms.mjs — the trio's ornament kit: the small moves a singer makes
// between notes, written for a synthesizer that can only lock one pitch per
// note. A trill here is not a modulation; it is literally two notes traded
// fast, one syllable each, because that is the only trill this instrument
// has. Everything returns a FIGURE — {notes, tokens} — where notes are the
// score's own [[midi|'r', beats], …] and tokens are one syllable per
// SOUNDING note, ready to join with " " into a lyric line.
//
//   import {trill, turn, roll, line, PALETTES, fitTokens} from './vocalisms.mjs';
//   const f = line(trill(59, 2, 1, 0.25, {tok: 'lee'}), turn(57, {beats: 1}));
//   f.notes  → [[59,.25],[61,.25],[59,.25],[61,.25],[57,.25],…]
//   f.tokens → ['lee','lee','lee','lee',…]
//
// What each shape is FOR, measured on Menu Band's singer (see VOCALISMS.md):
// a trill wants 0.25 beats a note or slower; below that the voice smears the
// two pitches into one. A shake is the slow wide cousin that always reads. A
// gliss is stepwise because the singer cannot bend inside a note.

// ---- syllables ------------------------------------------------------------
// Chosen for how each member's voice ARTICULATES, not for meaning. Bright
// front vowels for Noelle, closed dark ones for Aaron, small light ones for
// Zoe. Every token here renders as a sound, not as spelled letters.
export const PALETTES = {
  neo: ['dee', 'dah', 'lee', 'nee', 'tee'],        // bright — front vowels, tongue-tip attacks
  // 'mm' and 'boh' were in the first draft and are OUT: the speech engine
  // spells them "M-M" and "B-O-H" (0.49 s and 0.64 s a repeat, VOCALISMS.md).
  blueberry: ['doom', 'bom', 'dum', 'hmm', 'doo'],  // dark — closed lips, nasal tails
  frisbee: ['loo', 'la', 'lee', 'bip', 'poo'],     // light — small mouth, quick stops
};
// Mouth percussion: candidates only until VOCALISMS.md says which ones the
// speech engine SAYS instead of SPELLING. A spelled token sings its letters.
// MEASURED: only 'dm' survives. tk ts pf kh tss bm are all spelled out as
// letter names, which sings as two syllables on one note. 'ch' is marginal.
export const PERCUSSION = ['dm'];
export const PERCUSSION_SPELLED = ['tk', 'ts', 'pf', 'kh', 'tss', 'bm', 'ch'];
// Breath and glottal — the noise between the notes.
export const BREATH = ['hah', 'huh', 'uh'];

// ---- figures --------------------------------------------------------------
const round = (d) => +d.toFixed(6);
const isRest = (n) => n === 'r';
/// Beats a figure (or a raw note array) occupies.
export function beatsOf(x) {
  return (Array.isArray(x) ? x : x.notes).reduce((s, [, d]) => s + d, 0);
}
/// Sounding notes in a figure (or a raw note array).
export function countOf(x) {
  return (Array.isArray(x) ? x : x.notes).filter(([n]) => !isRest(n)).length;
}
/// A figure of nothing: time passing.
export function rest(beats) {
  return { notes: [['r', round(beats)]], tokens: [] };
}
/// One note, one syllable — how a phrase starts, and how an ornament ends.
export function hold(note, beats = 1, tok = 'ah') {
  return { notes: [[note, round(beats)]], tokens: [tok] };
}
/// Lay figures end to end. Strings and note-arrays are accepted too, so a
/// composer can drop a plain [[60,1]] in the middle of a phrase.
export function line(...figures) {
  const notes = [], tokens = [];
  for (const f of figures) {
    if (!f) continue;
    const g = Array.isArray(f) ? { notes: f, tokens: [] } : f;
    notes.push(...g.notes);
    tokens.push(...(g.tokens || []));
  }
  return { notes, tokens };
}
/// Even durations that sum EXACTLY to `beats` — the last one absorbs the
/// remainder, so a figure never drifts a score's cursor by a rounding crumb.
function split(beats, n) {
  const step = round(beats / n), out = Array(n).fill(step);
  out[n - 1] = round(beats - step * (n - 1));
  return out;
}
/// One token per sounding note: a string repeats, an array cycles.
function spread(tok, n) {
  const a = Array.isArray(tok) ? tok : [tok];
  return Array.from({ length: n }, (_, i) => a[i % a.length]);
}

/// trill — two notes traded fast. `interval` is 1 (half step), 2 (whole), or
/// negative to bite downward; `rate` is the length of ONE alternation in
/// beats. A pure alternation: the composer writes the resolution afterwards
/// with hold(), because a trill that lands on its own main note twice reads
/// as a repeated syllable, not as a termination.
export function trill(note, interval = 1, beats = 1, rate = 0.25, { tok = 'lee', from = 'main' } = {}) {
  const n = Math.max(2, Math.round(beats / rate));
  const durs = split(beats, n);
  const hi = note + interval;
  const pair = from === 'upper' ? [hi, note] : [note, hi];
  return { notes: durs.map((d, i) => [pair[i % 2], d]), tokens: spread(tok, n) };
}

/// mordent — main, neighbour, main. `dir` +1 bites above, -1 below. The bite
/// is short and the main note keeps the rest of the time.
export function mordent(note, dir = 1, { beats = 1, step = 0.125, tok = 'dee' } = {}) {
  const bite = Math.min(step, beats / 3);
  const notes = [[note, round(bite)], [note + dir, round(bite)], [note, round(beats - 2 * bite)]];
  return { notes, tokens: spread(tok, 3) };
}

/// turn — main, upper, main, lower, main: five notes, the middle three quick
/// and the last one held. The most vocal of these shapes; it reads at 1 beat.
export function turn(note, { beats = 1, above = 2, below = 1, tok = 'dah' } = {}) {
  const q = round(Math.min(beats / 8, 0.25));
  const notes = [[note, q], [note + above, q], [note, q], [note - below, q], [note, round(beats - 4 * q)]];
  return { notes, tokens: spread(tok, 5) };
}

/// shake — the wide slow trill: a whole tone or more, half a beat a note. It
/// is the ornament that survives any tempo, because every note is long
/// enough for the voice to actually arrive on its pitch.
export function shake(note, beats = 2, { interval = 2, step = 0.5, tok = 'ah' } = {}) {
  return trill(note, interval, beats, step, { tok });
}

/// roll — an arpeggio sung as one gesture: a chord let out a note at a time.
/// `notes` is the pitch list; they share `beats` evenly.
export function roll(pitches, beats = 1, { tok = 'la', down = false } = {}) {
  const p = down ? [...pitches].reverse() : pitches;
  const durs = split(beats, p.length);
  return { notes: p.map((n, i) => [n, durs[i]]), tokens: spread(tok, p.length) };
}

/// gliss — a slide, spelled out. The singer locks a pitch per note, so a
/// slide is a staircase: `steps` notes walking from `from` to `to`. Under
/// about 0.1 beats a step the staircase stops reading as separate treads.
export function gliss(from, to, beats = 1, steps = 0, { tok = 'ooh' } = {}) {
  const span = Math.abs(to - from);
  const n = Math.max(2, steps || span + 1);
  const durs = split(beats, n);
  const notes = durs.map((d, i) => [Math.round(from + (to - from) * (i / (n - 1))), d]);
  return { notes, tokens: spread(tok, n) };
}

/// pulse — one pitch, one vowel, repeated: the Reich figure. A held note that
/// keeps its consonant, so the ear hears rhythm instead of a drone.
export function pulse(note, beats = 2, vowel = 'doo', { step = 0.25 } = {}) {
  const n = Math.max(1, Math.round(beats / step));
  const durs = split(beats, n);
  return { notes: durs.map((d) => [note, d]), tokens: spread(vowel, n) };
}

// ---- token fitting --------------------------------------------------------
function hash32(s) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619) >>> 0; }
  return h >>> 0;
}
/// One token per sounding note, chosen from `palette` deterministically: the
/// same notes and the same seed always give the same syllables, so a score is
/// reproducible and a diff means someone changed the music.
export function fitTokens(notes, palette, seed = '') {
  const src = Array.isArray(notes) ? notes : notes.notes;
  const out = [];
  let k = 0;
  for (const [n] of src) {
    if (isRest(n)) continue;
    out.push(palette[hash32(`${seed}|${k}|${n}`) % palette.length]);
    k++;
  }
  return out;
}
/// Re-syllable a figure in place: same notes, new tokens from a palette.
export function recolor(figure, palette, seed = '') {
  return { notes: figure.notes, tokens: fitTokens(figure.notes, palette, seed) };
}

// ---- registers ------------------------------------------------------------
// Measured, not guessed. neo and blueberry from the lane's own auditions;
// frisbee (Zoe (Premium)) measured 2026-09-23 with pyworld.harvest over 62
// spoken words: median MIDI 55.6 (203 Hz), p10 52.5, p90 59.4.
export const BANDS = {
  neo: { voice: 'Noelle (Enhanced)', median: 59.6, lo: 54, hi: 66 },
  blueberry: { voice: 'Aaron (Enhanced)', median: 48.8, lo: 43, hi: 55 },
  frisbee: { voice: 'Zoe (Premium)', median: 55.6, lo: 52, hi: 61 },
};
/// Every sounding note inside the band, and the mean within `drift` of the
/// member's speaking pitch — the rule that keeps a neural voice from croaking.
export function checkBand(member, notes, drift = 2) {
  const b = BANDS[member];
  const p = (Array.isArray(notes) ? notes : notes.notes).filter(([n]) => !isRest(n)).map(([n]) => n);
  if (!p.length) return { ok: true, mean: null, lo: null, hi: null };
  const mean = p.reduce((a, n) => a + n, 0) / p.length;
  return {
    ok: Math.min(...p) >= b.lo && Math.max(...p) <= b.hi && Math.abs(mean - b.median) <= drift,
    mean: +mean.toFixed(2), lo: Math.min(...p), hi: Math.max(...p), band: [b.lo, b.hi], median: b.median,
  };
}
