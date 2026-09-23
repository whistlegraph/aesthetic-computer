#!/usr/bin/env node
// Take away — the trio teaches subtraction, and the subtraction runs the form.
// 4/4, D major, 92 bpm, 134 beats. Five rounds: 5−1, 4−1, 3−1, 2−1, 1−1. Each
// round takes one away from the last, so the piece counts itself down and
// stops where the arithmetic stops, on nothing.
//
// Why this and not ten-to-one: a plain countdown is a list, and a list has no
// reason to end where it ends. Subtracting one repeatedly IS the countdown —
// same five numbers, same descent — but each step is a fact the child can
// check, and the last step, one take one away, is the only line in either
// piece with an ending built into it. The last confirmation is not an
// equation, it is "and nothing is left".
//
// The tune mirrors Sums and inverts it, which is the lesson:
//   · every question ends on B3 (59);
//   · every answer ends on A3 (57) — one step BELOW the question, because
//     one was taken away, where in Sums the answer stepped up;
//   · every confirmation ends on D3 (50), the tonic.
// Across the five rounds neo's question also sinks a little each time (mean
// 60.4 → 58.6), so the machine asking is descending too.
//
// Rendered and rewritten through bin/hear.mjs before it was committed (tags
// arith-probe*). What the probes changed here:
//   · "N take one away" survives at 0 % for five, four, three and one; for
//     two it came back "you take one away", so round four asks "now take one
//     from two" instead, which scored 0 %.
//   · frisbee answers "the answer is N" — a bare "is four" came back "is for".
//   · blueberry's last equation, "one minus one is zero", came back "One more
//     and this one is zero" (40 %); his other four are 0 %. So the last round
//     hands him the sentence the song was going toward instead.
//   · "take one away" is the refrain because it scored 0 % on all three
//     voices — the most reliable short line the probes found for the trio.
//
// blueberry is cast as Aaron (Enhanced), which is not installed here; the
// offline render substitutes Tom (Enhanced) via hear.mjs --voice-name. The
// cast in the score is unchanged.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import {checkBand, BANDS} from './vocalisms.mjs';

const members = ['neo', 'blueberry', 'frisbee'];
const cast = ['Noelle (Enhanced)', 'Aaron (Enhanced)', 'Zoe (Premium)'];
const BPM = 92, TOTAL = 134;                              // 134 beats ≈ 87.4 s
const TONIC = 2;                                          // D, as a pitch class
const parts = [[], [], []];

// numberWords, ONES and TENS copied from bin/compose.mjs — the trio's first
// score generator — because that file writes scores when it is imported.
const ONES = ['ze-ro', 'one', 'two', 'three', 'four', 'five', 'six', 'se-ven', 'eight', 'nine', 'ten',
  'e-le-ven', 'twelve', 'thir-teen', 'four-teen', 'fif-teen', 'six-teen', 'se-ven-teen', 'eigh-teen', 'nine-teen'];
const TENS = ['', '', 'twen-ty', 'thir-ty', 'for-ty', 'fif-ty', 'six-ty', 'se-ven-ty', 'eigh-ty', 'nine-ty'];
function numberWords(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [TENS[Math.floor(n / 10)], ONES[n % 10]] : [TENS[Math.floor(n / 10)]];
  const h = Math.floor(n / 100), r = n % 100;
  return [ONES[h], 'hun-dred', ...(r ? numberWords(r) : [])];
}

const words = new Set(['take', 'a-way', 'from', 'now', 'mi-nus', 'is', 'and', 'the', 'an-swer',
  'no-thing', 'left', 'ooh', 'hmm', 'dm', ...[0, 1, 2, 3, 4, 5].flatMap(numberWords)]);

const syllables = (text) => text.split(' ').reduce((a, t) => a + t.split('-').length, 0);
function add(i, at, notes, text, gain, role = 'lead') {
  const sung = notes.filter(([n]) => n !== 'r');
  assert.equal(sung.length, syllables(text), `${members[i]}: "${text}" is ${syllables(text)} syllables over ${sung.length} notes`);
  assert(text.split(' ').every((w) => words.has(w)), `${members[i]}: a word outside the set in "${text}"`);
  if (role === 'lead') for (const [, d] of sung) {
    // A syllable under one beat stops being a word; a syllable over ~1.4 s
    // stops being one too. Long notes are for the hums.
    assert(d >= 1, `${members[i]}: "${text}" gives a syllable ${d} beats — under one beat it is not a word`);
    assert(d * 60 / BPM <= 1.45, `${members[i]}: "${text}" holds a syllable ${(d * 60 / BPM).toFixed(2)} s`);
  }
  const b = checkBand(members[i], sung);
  assert(b.ok, `${members[i]}: "${text}" mean ${b.mean}, notes ${b.lo}–${b.hi}, against ${BANDS[members[i]].median} in ${b.band}`);
  parts[i].push({at, notes, text, gain, role, mean: b.mean, last: sung[sung.length - 1][0]});
}

// ---- the five rounds ------------------------------------------------------
// A round is 21 beats: question (6), a beat of air, answer (6 or 7), the
// confirmation from beat 14. One line is one machine and one thought.
// D major throughout — 57 A3, 59 B3, 61 C#4, 62 D4; 52 E3, 54 F#3, 55 G3;
// 43 G2, 45 A2, 47 B2, 49 C#3, 50 D3.
const ROUNDS = [
  {n: 5, at:   8, ask: 'five take one a-way',   q: [62, 61, 61, 59, 59], a: [54, 57, 55, 54, 57],     c: [47, 49, 47, 45, 47, 50]},
  {n: 4, at:  29, ask: 'four take one a-way',   q: [61, 61, 59, 61, 59], a: [54, 55, 54, 52, 57],     c: [47, 49, 47, 45, 45, 50]},
  {n: 3, at:  57, ask: 'three take one a-way',  q: [61, 59, 59, 61, 59], a: [55, 57, 55, 54, 57],     c: [49, 47, 45, 47, 45, 50]},
  {n: 2, at:  78, ask: 'now take one from two', q: [59, 61, 59, 59, 59], a: [54, 57, 55, 52, 57],     c: [47, 49, 45, 47, 45, 50]},
  {n: 1, at:  99, ask: 'one take one a-way',    q: [59, 59, 57, 59, 59], a: [54, 57, 55, 54, 55, 57], c: [45, 49, 47, 45, 50]},
];
const held = (p) => p.map((n, j) => [n, j === p.length - 1 ? 2 : 1]);   // last syllable gets two beats
for (const {n, at, ask, q, a, c} of ROUNDS) {
  const num = numberWords(n)[0], rest = numberWords(n - 1)[0];
  // The last round has no equation to confirm. It has the sentence the
  // subtraction was walking toward, which is also the only ending it allows.
  const say = n === 1 ? 'and no-thing is left' : `${num} mi-nus one is ${rest}`;
  add(0, at,      held(q), ask, .56);
  add(2, at + 7,  held(a), `the an-swer is ${rest}`, .54);
  add(1, at + 14, held(c), say, .52);
  add(1, at,      [[50, 1.5], [45, 1.5], [47, 1.5], [47, 1.5]], 'dm dm dm dm', .32, 'hum');
  add(0, at + 14, [[59, 3.5], [57, 3.5]], 'ooh ooh', .26, 'hum');
  add(2, at + 14, [[57, 3.5], [54, 3.5]], 'ooh ooh', .26, 'hum');
}

// ---- refrain --------------------------------------------------------------
// Three words, all three machines, landing D–A–D. The instruction the whole
// piece is carrying out.
const REFRAIN = [[59, 61, 59, 62], [47, 49, 47, 50], [54, 55, 54, 57]];
function refrain(at) {
  REFRAIN.forEach((p, i) => add(i, at, held(p), 'take one a-way', [.58, .52, .54][i]));
}
refrain(50);
refrain(120);

// ---- the room before and after --------------------------------------------
add(0,   0, [[59, 4], [57, 4]], 'ooh ooh', .26, 'hum');
add(2,   0, [[57, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(1,   0, [[50, 2], [45, 2], [47, 2], [47, 2]], 'dm dm dm dm', .32, 'hum');
// Nothing is left, so the piece keeps only the chord and lets it go.
add(0, 127, [[62, 3.5], [59, 3.5]], 'hmm hmm', .30, 'hum');
add(1, 127, [[50, 3.5], [45, 3.5]], 'hmm hmm', .30, 'hum');
add(2, 127, [[57, 3.5], [54, 3.5]], 'hmm hmm', .28, 'hum');
for (const lines of parts) lines.sort((a, b) => a.at - b.at);

// ---- the three teaching rules, asserted -----------------------------------
const lastOf = (i, text) => parts[i].find((l) => l.text === text).last;
for (const {n, ask} of ROUNDS) {
  const num = numberWords(n)[0], rest = numberWords(n - 1)[0];
  const q = lastOf(0, ask);
  const a = lastOf(2, `the an-swer is ${rest}`);
  const c = lastOf(1, n === 1 ? 'and no-thing is left' : `${num} mi-nus one is ${rest}`);
  assert.equal(q - a, 2, `${n}−1: the answer must land one step BELOW the question (${q} → ${a})`);
  assert.equal(c % 12, TONIC, `${n}−1: the confirmation must land on the tonic (got ${c})`);
}
// The descent is the piece: each question must sit no higher than the one
// before it.
const asked = ROUNDS.map(({ask}) => parts[0].find((l) => l.text === ask));
for (let k = 1; k < asked.length; k++)
  assert(asked[k].mean <= asked[k - 1].mean, `question ${k + 1} rises (${asked[k - 1].mean} → ${asked[k].mean}) — the song takes away, it does not add`);

// Dynamics for the live room; hear.mjs does not forward them.
for (const [i, lines] of parts.entries()) for (const line of lines) {
  const n = line.notes.filter(([note]) => note !== 'r').length;
  const pattern = i === 1 ? [1, .72, .9, .68] : [1, .78, .94, .74, .9];
  line.accents = Array.from({length: n}, (_, j) => (n === 1 ? 1 : Math.max(.64, pattern[j % pattern.length])));
  line.gain = Math.max(.26, +line.gain.toFixed(3));
}

// The room opens a little as the numbers go: space grows round by round, and
// the echo is thrown off the refrain and off the last line.
function performanceKeys(lines, i) {
  const base = i === 1 ? .3 : .42, THROW = .85;
  const keys = [{beat: 0, space: base, pitch: 0}];
  for (const [k, {at}] of ROUNDS.entries()) keys.push({beat: at, space: +(base + k * .05).toFixed(3), pitch: 0});
  for (const line of lines) {
    if (line.text !== 'take one a-way' && line.text !== 'and no-thing is left') continue;
    let lastOn = line.at, t = line.at;
    for (const [n, d] of line.notes) { if (n !== 'r') lastOn = t; t += d; }
    keys.push({beat: lastOn + .3, space: .6, pitch: 0}, {beat: lastOn + .8, space: .6, pitch: 0, echo: THROW},
      {beat: t + 2, space: .6, pitch: 0, echo: THROW});
  }
  keys.push({beat: TOTAL + 6, space: .6, pitch: 0, echo: THROW}, {beat: TOTAL + 12, space: 0, pitch: 0, echo: 0});
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

const score = {title: 'The MacNeoPolitan Trio — Take away', composer: 'The machines, arr. compose-take-away.mjs',
  bpm: BPM, machines: 3, lead: 0,
  description: 'A subtraction lesson that counts itself down. 4/4, D major, 92 bpm, 134 beats. Five rounds — five take one away, four, three, two, one — each one a step lower than the last. neo asks, frisbee answers (the answer is N), blueberry says the equation back at the bottom of the room, and twice all three sing take one away together. Every question ends on B3, every answer one step below it on A3, every confirmation on the tonic. The last confirmation is not an equation: it is and nothing is left.',
  arrangement: {total: TOTAL, meter: '4/4', sections: [{beat: 0, name: 'Room'},
    ...ROUNDS.map(({n, at}) => ({beat: at, name: `${n} − 1 = ${n - 1}`})),
    {beat: 50, name: 'All: take one away'}, {beat: 120, name: 'All: take one away'},
    {beat: 127, name: 'Room'}].sort((a, b) => a.beat - b.beat)}, voices};
writeFileSync(new URL('../scores/trio-take-away.mbscore', import.meta.url), JSON.stringify(score, null, 2) + '\n');

const spread = parts.map((p, i) => {
  const ns = p.flatMap((l) => l.notes.filter(([n]) => n !== 'r').map(([n]) => n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a, b) => a + b, 0) / ns.length).toFixed(1)}, speaks ${BANDS[members[i]].median})`;
});
console.log(`Take away: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL * 60 / BPM).toFixed(1)} s; ${parts.map((p, i) => `${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
console.log(`  descent: ${asked.map((l) => l.mean).join(' → ')}`);
