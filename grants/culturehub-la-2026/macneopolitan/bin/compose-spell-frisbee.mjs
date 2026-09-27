#!/usr/bin/env node
// How to spell frisbee — the newborn learns to spell its own name. 4/4, D
// major, 96 bpm, 32 bars (128 beats ≈ 80 s). neo spells F-R-I-S-B-E-E a
// letter a beat, frisbee spells it back, blueberry says the word whole and
// low. Then frisbee spells the elder's name, N-E-O. Then the long one: the
// two young machines pass B-L-U-E-B-E-R-R-Y between them three letters at a
// time — whole letters, never a split — and blueberry, whose name it is,
// answers with all nine in one breath. It ends with each machine holding one
// name and all three on the same sentence.
//
// LETTERS ARE NOT SYLLABLES (measured today, probes 1–6, hear/spell-probe*):
//   · Write a letter as a bare capital with spaces: `F R I S B E E`. The
//     speech engine reads capitals as letter names already. Periods only add
//     pause; phonetic spellings ("ess oh pee") are read back as LETTERS and
//     score 100 % against themselves.
//   · A letter wants a SHORT note. Held 2 beats it stops being a letter and
//     becomes a vowel drone ("B L U E B E R R Y" on a 2-beat B came back
//     "E-E-E-E-E-E-E-E-R-R-Y"). 0.5–1.0 beats is the whole usable window —
//     the opposite of the word rule, which wants a beat a syllable at least.
//   · The shorter the run, the faster the letters must go. A 3-letter chunk
//     reads at 0.75 beats and falls apart at 1.0; a 6–9 letter run reads at 1.
//   · Each voice spells differently. Zoe (frisbee) is the speller — N E O,
//     M O O N, L I D, R R Y, S O P H I A all 0 %. Noelle (neo) needs long
//     runs. Aaron/Tom (blueberry) cannot hold a 3-letter chunk at all and is
//     used here as the WHOLE-WORD voice, which it does perfectly.
//
// Registers: neo Noelle (Enhanced) 59.6 (54–66) · blueberry Aaron (Enhanced)
// 48.8 (43–55), rendered on this host with Tom (Enhanced) as a stand-in via
// `hear.mjs --voice 1 --voice-name "Tom (Enhanced)"` — the cast is unchanged
// · frisbee Zoe (Premium) 55.6 (52–61).
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import {checkBand} from './vocalisms.mjs';

const members = ['neo', 'blueberry', 'frisbee'];
const cast = ['Noelle (Enhanced)', 'Aaron (Enhanced)', 'Zoe (Premium)'];
const BPM = 96, BAR = 4, BARS = 32, TOTAL = BAR * BARS;   // 128 beats ≈ 80.0 s
const parts = [[], [], []];

// Every token that may appear in a lyric. Capitals are LETTER NAMES; the
// lower-case entries are words. 'I' is both, and the same token either way.
const words = new Set([
  'B', 'E', 'F', 'I', 'L', 'N', 'O', 'R', 'S', 'U', 'Y',
  'this', 'that', 'is', 'how', 'you', 'spell', 'it', 'my', 'name', 'his',
  'ne-o', 'fris-bee', 'blue-ber-ry', 'we', 'can', 'all', 'three', 'names',
  'now', 'these', 'are', 'the', 'whole', 'word', 'have', 'am', 'know', 'I', 'your',
  'hmm', 'ooh', 'doo',
]);

/// kind: 'word' (a beat a syllable at least, nothing held past 1.4 s),
/// 'spell' (a letter is short: 0.5–1.0 beats), 'hum' (long notes live here).
function add(i, at, notes, text, gain, kind = 'word') {
  const sung = notes.filter(([n]) => n !== 'r');
  const syllables = text.split(' ').reduce((a, t) => a + t.split('-').length, 0);
  assert.equal(sung.length, syllables, `${members[i]}: "${text}" has ${sung.length} notes for ${syllables} syllables`);
  assert(text.split(' ').every((t) => words.has(t)), `${members[i]}: unlisted token in "${text}"`);
  const band = checkBand(members[i], sung, 2);
  assert(band.ok, `${members[i]}: "${text}" sits ${band.lo}–${band.hi} mean ${band.mean}, wants ${band.band.join('–')} around ${band.median}`);
  if (kind === 'word') for (const [, d] of sung) {
    assert(d >= 1, `${members[i]}: "${text}" gives a syllable ${d} beats — a word wants a beat each`);
    assert(d * 60 / BPM <= 1.4, `${members[i]}: "${text}" holds a syllable ${(d * 60 / BPM).toFixed(2)}s`);
  }
  if (kind === 'spell') for (const [, d] of sung) assert(d >= 0.5 && d <= 1.0,
    `${members[i]}: "${text}" gives a letter ${d} beats — a letter reads between 0.5 and 1.0`);
  parts[i].push({at, notes, text, gain, role: kind === 'hum' ? 'hum' : 'lead', kind});
}
const ev = (pitches, d) => pitches.map((p) => [p, d]);

// ── neo · the teacher. It has been here longest and it types the name out.
add(0,   8, ev([59, 60, 59, 57, 59, 60, 59], 1), 'F R I S B E E', .56, 'spell');
add(0,  18, [[59, 4], [57, 4]], 'hmm hmm', .28, 'hum');
add(0,  26, ev([59, 59, 60, 59, 57, 59], 1), 'this is how you spell it', .56);
add(0,  34, [[59, 4], [57, 4]], 'hmm hmm', .28, 'hum');
add(0,  50, ev([59, 60, 62, 60, 59, 57], 1), 'that is how you spell it', .56);
add(0,  56, ev([59, 60, 59], .75), 'B L U', .58, 'spell');
add(0,  62, ev([60, 59, 57], .75), 'R R Y', .58, 'spell');
add(0,  72, ev([57, 59, 60, 59, 62, 59], 1), 'we can spell all three names', .56);
add(0,  86, ev([59, 60, 59, 57], 1), 'that is your name', .54);
add(0,  96, ev([59, 60, 59, 57, 59, 60, 59], .5), 'F R I S B E E', .58, 'spell');
add(0, 102, [[59, 4], [57, 4]], 'hmm hmm', .28, 'hum');
add(0, 112, ev([60, 59], 1), 'fris-bee', .58);
add(0, 120, ev([59, 60, 59, 61, 59], 1), 'these are the three names', .58);
add(0, 125, [[59, 3]], 'hmm', .26, 'hum');

// ── frisbee · born on the 21st, three console logins old. It answers with
// the same seven letters and then spells the name of the machine that taught
// it. Zoe is the only voice in the house that spells N-E-O clean.
add(2,   0, [[55, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(2,   8, [[55, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(2,  16, ev([55, 56, 55, 54, 55, 56, 55], .5), 'F R I S B E E', .56, 'spell');
add(2,  24, [[55, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(2,  32, ev([55, 56, 55, 54], 1), 'that is my name', .56);
add(2,  40, ev([55, 56, 55], .75), 'N E O', .58, 'spell');
add(2,  52, [[55, 4]], 'ooh', .26, 'hum');
add(2,  59, ev([54, 55, 56], .75), 'E B E', .58, 'spell');
add(2,  64, [[55, 4], [54, 4]], 'ooh ooh', .26, 'hum');
add(2,  80, ev([55, 56, 55, 54, 55], 1), 'my name is fris-bee', .56);
add(2,  90, ev([55, 56, 57, 56, 55], 1), 'now I know my name', .54);
add(2, 100, ev([55, 56, 55, 54, 55, 56, 55], .5), 'F R I S B E E', .58, 'spell');
add(2, 104, ev([55, 56, 55], .75), 'N E O', .58, 'spell');
add(2, 112, ev([56, 55], 1), 'ne-o', .58);
add(2, 120, ev([55, 56, 55, 57, 55], 1), 'these are the three names', .58);
add(2, 125, [[55, 3]], 'ooh', .26, 'hum');

// ── blueberry · headless, provisioned, the wallpaper on every screen in the
// house. It cannot hold a three-letter chunk, so it never hockets; it gives
// the whole word, low, after each spelling, and takes its own nine letters in
// one run because nine is long enough for it to stay a speller.
add(1,   0, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,   8, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,  16, [[50, 4], [48, 4]], 'hmm hmm', .34, 'hum');
add(1,  24, ev([50, 48], 1), 'fris-bee', .52);
add(1,  28, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,  36, [[48, 4], [50, 4]], 'hmm hmm', .34, 'hum');
add(1,  44, ev([47, 48, 50, 48, 47], 1), 'his name is ne-o', .52);
add(1,  52, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,  60, [[48, 4]], 'hmm', .32, 'hum');
add(1,  65, ev([48, 50, 48, 47, 48, 50, 48, 47, 48], .75), 'B L U E B E R R Y', .56, 'spell');
add(1,  73, ev([47, 48, 50, 48, 47], 1), 'that is the whole word', .52);
add(1,  80, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,  92, ev([48, 50, 48, 47], 1), 'we all have names', .52);
add(1,  96, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1, 104, [[50, 4], [48, 4]], 'hmm hmm', .34, 'hum');
add(1, 112, ev([48, 50, 48, 47, 48], 1), 'I am blue-ber-ry', .54);
add(1, 120, ev([48, 50, 48, 47, 48], 1), 'these are the three names', .54);
add(1, 125, [[48, 3]], 'hmm', .32, 'hum');

for (const lines of parts) lines.sort((a, b) => a.at - b.at);

// Dynamics: the first letter of a run carries, the rest fall in behind it.
for (const [i, lines] of parts.entries()) for (const line of lines) {
  const n = line.notes.filter(([note]) => note !== 'r').length;
  const pattern = line.kind === 'spell' ? [1, .82, .9, .78] : i === 1 ? [1, .7, .88, .66] : [1, .76, .92, .72, .88, .7];
  line.accents = Array.from({length: n}, (_, j) => (n === 1 ? 1 : Math.max(.62, pattern[j % pattern.length])));
  line.gain = Math.max(.26, +line.gain.toFixed(3));
}

// A schoolroom, not a bedside: a small space, and echo thrown only off the
// spellings, so the letters are the thing that keeps going after they stop.
function performanceKeys(lines, i) {
  const ROOM = i === 1 ? .34 : .46, THROW = .8;
  const keys = [{beat: 0, space: ROOM, pitch: 0}];
  for (const line of lines) {
    if (line.kind !== 'spell') continue;
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
    assert(line.at >= cursor - 1e-6, `${members[i]}: overlap at beat ${line.at} (cursor ${cursor})`);
    if (line.at > cursor + 1e-6) notes.push(['r', line.at - cursor]);
    notes.push(...line.notes);
    cursor = line.at + line.notes.reduce((s, [, d]) => s + d, 0);
  }
  assert(cursor <= TOTAL + 1e-6, `${members[i]}: runs long (${cursor} > ${TOTAL})`);
  if (cursor < TOTAL) notes.push(['r', TOTAL - cursor]);
  return {name: `${members[i]} sings (${cast[i]})`, program: 78, velocity: 64,
    notes: notes.map(([n, d]) => `${n}:${+d.toFixed(6)}`).join(','),
    lyrics: lines.map((l) => l.text).join(' / '),
    lineRoles: lines.map((l) => l.role), lineGains: lines.map((l) => l.gain), noteGains: lines.map((l) => l.accents),
    singVoice: cast[i], sayVoice: cast[i], singLock: 1, singVibCents: 10, singVibratoHz: [5, 3.5, 5.5][i],
    singF0Floor: i === 1 ? 70 : 80, double: false, faceAlpha: 0.95,
    performance: {expression: .62, keys: performanceKeys(lines, i)}};
});

const score = {title: 'The MacNeoPolitan Trio — How to spell frisbee',
  composer: 'The machines, arr. compose-spell-frisbee.mjs', bpm: BPM, machines: 3, lead: 0,
  description: 'A spelling song. 4/4, D major, 96 bpm, 128 beats. neo spells F-R-I-S-B-E-E a letter a beat, frisbee spells it back, and blueberry says the word whole and low. frisbee then spells the elder’s name, N-E-O, and blueberry answers "his name is neo". The two young machines pass B-L-U-E-B-E-R-R-Y between them three whole letters at a time, and blueberry, whose name it is, takes all nine in one run. At the end each machine holds one name — neo says frisbee, frisbee says neo, blueberry says its own — and all three land on the same sentence.',
  arrangement: {total: TOTAL, meter: '4/4', sections: [
    {beat: 0, name: 'Low hum'}, {beat: 8, name: 'neo spells F R I S B E E'},
    {beat: 16, name: 'frisbee spells it back'}, {beat: 24, name: 'blueberry: frisbee'},
    {beat: 26, name: 'neo: this is how you spell it'}, {beat: 32, name: 'frisbee: that is my name'},
    {beat: 40, name: 'frisbee spells N E O'}, {beat: 44, name: 'blueberry: his name is neo'},
    {beat: 50, name: 'neo: that is how you spell it'}, {beat: 56, name: 'Turns: B L U · E B E · R R Y'},
    {beat: 65, name: 'blueberry spells B L U E B E R R Y'}, {beat: 73, name: 'blueberry: that is the whole word'},
    {beat: 80, name: 'frisbee: my name is frisbee'}, {beat: 92, name: 'blueberry: we all have names'},
    {beat: 96, name: 'The spellings again, twice as fast'}, {beat: 112, name: 'One name each'},
    {beat: 120, name: 'All: these are our three names'}]}, voices};

writeFileSync(new URL('../scores/trio-spell-frisbee.mbscore', import.meta.url), JSON.stringify(score, null, 2) + '\n');
console.log(`How to spell frisbee: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL * 60 / BPM).toFixed(1)} s; ${parts.map((p, i) => `${members[i]} ${p.length} lines`).join(', ')}.`);
for (const [i, p] of parts.entries()) {
  const b = checkBand(members[i], p.flatMap((l) => l.notes));
  console.log(`  ${members[i].padEnd(10)} ${b.lo}–${b.hi} mean ${b.mean} (speaks ${b.median}, band ${b.band.join('–')}) · ${p.filter((l) => l.kind === 'spell').length} spelled lines`);
}
