#!/usr/bin/env node
// Sums — the trio teaches addition. 4/4, D major, 96 bpm, 33 bars. A lesson in
// five rounds, each one the same three moves: neo asks, frisbee answers,
// blueberry says the whole sentence back in the bottom of the room. Doubles
// only (1+1 through 5+5), because a double is the sum a child learns first and
// because every answer is then one syllable.
//
// The teaching is in the tune, not only the words:
//   · every question ends on A3 (57) — the dominant, the note that waits;
//   · every answer ends on B3 (59) — one step ABOVE the question, so the
//     child hears the sum go up before the word arrives;
//   · every confirmation ends on D3 (50) — the tonic, the floor, done.
// Three asserts at the bottom of add() hold those three rules.
//
// Lyric shapes were not chosen by ear alone; they were rendered through
// bin/hear.mjs first and kept only if whisper small.en heard them (tags
// arith-probe*-{neo,bb,fri}). What that cost:
//   · a bare sum ("one plus one") is heard perfectly and TRANSCRIBED "1 + 1",
//     which the scorer reads as a lost word. "what is one plus one" keeps
//     whisper in prose — 0 % three times out of three.
//   · a short answer dies. "is two" came back "Mm-shoo", "two" alone came
//     back "TEEEWWWWWWW", "is four" came back "is for". Every number word
//     needs a whole sentence around it, so frisbee answers "the answer is N".
//   · "that is two" came back "that is too" and "that makes four" came back
//     "that makes for" — so blueberry does not confirm with a short phrase.
//     He confirms with the equation, "N and N is 2N", which scored 0 % on
//     all five doubles. ("and" is free: the scorer drops it either way.)
//
// Registers: neo = Noelle (Enhanced), speaks MIDI 59.6. blueberry = Aaron
// (Enhanced), 48.8 — NOT installed on this host, so the offline render
// substitutes Tom (Enhanced) (median 45.8, p90 48.9) with --voice-name; the
// score's cast is unchanged, and blueberry's lines are written with a mean
// near 47.4 so they sit inside both men. frisbee = Zoe (Premium), 55.6.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import {checkBand, BANDS} from './vocalisms.mjs';

const members = ['neo', 'blueberry', 'frisbee'];
const cast = ['Noelle (Enhanced)', 'Aaron (Enhanced)', 'Zoe (Premium)'];
const BPM = 96, BAR = 4, BARS = 33, TOTAL = BAR * BARS;   // 132 beats ≈ 82.5 s
const TONIC = 2;                                          // D, as a pitch class
const parts = [[], [], []];

// numberWords, ONES and TENS are copied from bin/compose.mjs (the trio's first
// score generator) so this file does not have to import a script that writes
// scores on load. Syllables are joined by "-"; the doubles used here are all
// one syllable, which is half the reason the piece is doubles.
const ONES = ['ze-ro', 'one', 'two', 'three', 'four', 'five', 'six', 'se-ven', 'eight', 'nine', 'ten',
  'e-le-ven', 'twelve', 'thir-teen', 'four-teen', 'fif-teen', 'six-teen', 'se-ven-teen', 'eigh-teen', 'nine-teen'];
const TENS = ['', '', 'twen-ty', 'thir-ty', 'for-ty', 'fif-ty', 'six-ty', 'se-ven-ty', 'eigh-ty', 'nine-ty'];
function numberWords(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [TENS[Math.floor(n / 10)], ONES[n % 10]] : [TENS[Math.floor(n / 10)]];
  const h = Math.floor(n / 100), r = n % 100;
  return [ONES[h], 'hun-dred', ...(r ? numberWords(r) : [])];
}

const words = new Set(['what', 'is', 'plus', 'and', 'the', 'an-swer', 'ooh', 'hmm', 'dm',
  ...[0, 1, 2, 3, 4, 5, 6, 8, 10].flatMap(numberWords)]);

const syllables = (text) => text.split(' ').reduce((a, t) => a + t.split('-').length, 0);
function add(i, at, notes, text, gain, role = 'lead') {
  const sung = notes.filter(([n]) => n !== 'r');
  assert.equal(sung.length, syllables(text), `${members[i]}: "${text}" is ${syllables(text)} syllables over ${sung.length} notes`);
  assert(text.split(' ').every((w) => words.has(w)), `${members[i]}: a word outside the set in "${text}"`);
  if (role === 'lead') for (const [, d] of sung) {
    // Both ends of the day's measured window. Under a beat a syllable the
    // line stops being words; past ~1.4 s a sung syllable stops being one.
    assert(d >= 1, `${members[i]}: "${text}" gives a syllable ${d} beats — under one beat it is not a word`);
    assert(d * 60 / BPM <= 1.45, `${members[i]}: "${text}" holds a syllable ${(d * 60 / BPM).toFixed(2)} s`);
  }
  const b = checkBand(members[i], sung);
  assert(b.ok, `${members[i]}: "${text}" mean ${b.mean}, notes ${b.lo}–${b.hi}, against ${BANDS[members[i]].median} in ${b.band}`);
  parts[i].push({at, notes, text, gain, role, mean: b.mean, last: sung[sung.length - 1][0]});
}
const lastOf = (i, text) => parts[i].find((l) => l.text === text).last;

// ---- the five rounds ------------------------------------------------------
// One round is 20 beats: question (6), a beat of air, answer (6), a beat of
// air, confirmation (6). Nobody shares a line with anybody; one machine, one
// thought, which is the only way the words survive the room.
// Every pitch is in D major — 57 A3, 59 B3, 61 C#4, 62 D4 up top; 52 E3, 54
// F#3, 55 G3, 57, 59 in the middle; 43 G2, 45 A2, 47 B2, 49 C#3, 50 D3 down
// below. No borrowed notes: a lesson is not the place for a flat seventh.
const ROUNDS = [
  {n: 1, at:  8, q: [61, 59, 59, 59, 57], a: [54, 57, 55, 54, 59], c: [47, 45, 47, 49, 50]},
  // Round two carries round one's confirmation tune on purpose. Written to
  // enter low, on A2, "two and two is four" came back "2-1-2-1-4" — the low
  // entry ate the first word and whisper read the two "and"s as ones. Off
  // B2 it is 0 %. The formula sounding the same twice is a teaching song's
  // problem to have.
  {n: 2, at: 28, q: [59, 59, 61, 62, 57], a: [55, 57, 55, 54, 59], c: [47, 45, 47, 49, 50]},
  {n: 3, at: 56, q: [57, 59, 61, 59, 57], a: [52, 55, 54, 57, 59], c: [49, 47, 45, 47, 50]},
  {n: 4, at: 76, q: [59, 61, 62, 61, 57], a: [54, 57, 55, 52, 59], c: [47, 49, 47, 45, 50]},
  {n: 5, at: 96, q: [62, 61, 59, 59, 57], a: [55, 57, 54, 55, 59], c: [50, 49, 47, 45, 50]},
];
const five = (p, d = [1, 1, 1, 1, 2]) => p.map((n, j) => [n, d[j]]);   // six beats
for (const {n, at, q, a, c, cd} of ROUNDS) {
  const num = numberWords(n)[0], sum = numberWords(n + n)[0];
  add(0, at,      five(q), `what is ${num} plus ${num}`, .56);
  add(2, at + 7,  five(a), `the an-swer is ${sum}`, .54);
  add(1, at + 14, five(c, cd), `${num} and ${num} is ${sum}`, .52);
  // The bed: blueberry ticks under the question, the other two hold a third
  // under the confirmation so the answer is never sung over.
  add(1, at,      [[50, 1.5], [45, 1.5], [47, 1.5], [47, 1.5]], 'dm dm dm dm', .32, 'hum');
  add(0, at + 14, [[59, 3], [57, 3]], 'ooh ooh', .26, 'hum');
  add(2, at + 14, [[57, 3], [54, 3]], 'ooh ooh', .26, 'hum');
}

// ---- refrain --------------------------------------------------------------
// The one sentence everybody already knows, sung by all three at once on
// their own pitches, landing D–A–D. It is the most reliable line the probes
// found: 0 % on every voice, every time it was rendered.
const REFRAIN = [[59, 57, 59, 61, 62], [47, 45, 47, 49, 50], [54, 55, 54, 59, 57]];
function refrain(at) {
  REFRAIN.forEach((p, i) => add(i, at, five(p), 'one and one is two', [.58, .52, .54][i]));
}
refrain(48);
refrain(116);

// ---- the room before and after --------------------------------------------
add(0,   0, [[59, 4], [57, 4]], 'ooh ooh', .26, 'hum');
add(2,   0, [[57, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(1,   0, [[50, 2], [45, 2], [47, 2], [47, 2]], 'dm dm dm dm', .32, 'hum');
add(0, 124, [[62, 4], [59, 4]], 'hmm hmm', .30, 'hum');
add(1, 124, [[50, 4], [45, 4]], 'hmm hmm', .30, 'hum');
add(2, 124, [[57, 4], [54, 4]], 'hmm hmm', .28, 'hum');
for (const lines of parts) lines.sort((a, b) => a.at - b.at);

// ---- the three teaching rules, asserted -----------------------------------
for (const {n} of ROUNDS) {
  const num = numberWords(n)[0], sum = numberWords(n + n)[0];
  const q = lastOf(0, `what is ${num} plus ${num}`);
  const a = lastOf(2, `the an-swer is ${sum}`);
  const c = lastOf(1, `${num} and ${num} is ${sum}`);
  assert.equal(a - q, 2, `${n}+${n}: the answer must land one step above the question (${q} → ${a})`);
  assert.equal(c % 12, TONIC, `${n}+${n}: the confirmation must land on the tonic (got ${c})`);
}

// Dynamics: the question leans on its first syllable, the answer on "answer",
// the confirmation on the number. hear.mjs does not forward these — they are
// for the live room.
for (const [i, lines] of parts.entries()) for (const line of lines) {
  const n = line.notes.filter(([note]) => note !== 'r').length;
  const pattern = i === 1 ? [1, .72, .9, .68] : [1, .78, .94, .74, .9];
  line.accents = Array.from({length: n}, (_, j) => (n === 1 ? 1 : Math.max(.64, pattern[j % pattern.length])));
  line.gain = Math.max(.26, +line.gain.toFixed(3));
}

// A classroom, not a bedside: a small room, and echo thrown only off the
// refrain, so "one and one is two" is the one thing that keeps ringing.
function performanceKeys(lines, i) {
  const ROOM = i === 1 ? .32 : .46, THROW = .8;
  const keys = [{beat: 0, space: ROOM, pitch: 0}];
  for (const line of lines) {
    if (line.text !== 'one and one is two') continue;
    let lastOn = line.at, t = line.at;
    for (const [n, d] of line.notes) { if (n !== 'r') lastOn = t; t += d; }
    keys.push({beat: line.at - .4, space: ROOM, pitch: 0, echo: 0}, {beat: lastOn + .3, space: ROOM, pitch: 0},
      {beat: lastOn + .8, space: ROOM, pitch: 0, echo: THROW}, {beat: t + 2, space: ROOM, pitch: 0, echo: THROW});
  }
  keys.push({beat: TOTAL + 4, space: ROOM, pitch: 0, echo: THROW}, {beat: TOTAL + 8, space: 0, pitch: 0, echo: 0});
  return keys.sort((a, b) => a.beat - b.beat);
}

const voices = parts.map((lines, i) => {
  const notes = []; let cursor = 0;
  for (const line of lines) {
    assert(line.at >= cursor - 1e-6, `${members[i]}: "${line.text}" overlaps the line before it at ${line.at}`);
    if (line.at > cursor + 1e-6) notes.push(['r', line.at - cursor]);
    notes.push(...line.notes); cursor = line.at + line.notes.reduce((s, [, d]) => s + d, 0);
  }
  if (cursor < TOTAL) notes.push(['r', TOTAL - cursor]);
  assert(cursor <= TOTAL + 1e-6, `${members[i]}: runs long (${cursor})`);
  return {name: `${members[i]} sings (${cast[i]})`, program: 78, velocity: 64,
    notes: notes.map(([n, d]) => `${n}:${+d.toFixed(6)}`).join(','), lyrics: lines.map((l) => l.text).join(' / '),
    lineRoles: lines.map((l) => l.role), lineGains: lines.map((l) => l.gain), noteGains: lines.map((l) => l.accents),
    singVoice: cast[i], sayVoice: cast[i], singLock: 1, singVibCents: 10, singVibratoHz: [5, 3.5, 5.5][i],
    singF0Floor: i === 1 ? 70 : 80, double: false, faceAlpha: 0.95,
    performance: {expression: .6, keys: performanceKeys(lines, i)}};
});

const score = {title: 'The MacNeoPolitan Trio — Sums', composer: 'The machines, arr. compose-sums.mjs',
  bpm: BPM, machines: 3, lead: 0,
  description: 'An addition lesson in five rounds. 4/4, D major, 96 bpm, 132 beats. neo asks (what is N plus N), frisbee answers (the answer is 2N), blueberry says the equation back in the bottom of the room (N and N is 2N), and twice all three sing one and one is two together. The tune carries the arithmetic: every question ends on the dominant, every answer one step above it, every confirmation on the tonic.',
  arrangement: {total: TOTAL, meter: '4/4', sections: [{beat: 0, name: 'Room'},
    ...ROUNDS.flatMap(({n, at}) => [{beat: at, name: `${n} + ${n}`}]),
    {beat: 48, name: 'All: one and one is two'}, {beat: 116, name: 'All: one and one is two'},
    {beat: 124, name: 'Room'}].sort((a, b) => a.beat - b.beat)}, voices};
writeFileSync(new URL('../scores/trio-sums.mbscore', import.meta.url), JSON.stringify(score, null, 2) + '\n');

const spread = parts.map((p, i) => {
  const ns = p.flatMap((l) => l.notes.filter(([n]) => n !== 'r').map(([n]) => n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a, b) => a + b, 0) / ns.length).toFixed(1)}, speaks ${BANDS[members[i]].median})`;
});
console.log(`Sums: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL * 60 / BPM).toFixed(1)} s; ${parts.map((p, i) => `${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
console.log(`  words: ${parts.flat().filter((l) => l.role === 'lead').length} sung lines, ${parts.flat().filter((l) => l.role === 'lead').reduce((a, l) => a + l.text.split(' ').length, 0)} words.`);
