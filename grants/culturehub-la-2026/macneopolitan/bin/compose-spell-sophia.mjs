#!/usr/bin/env node
// Spell Sophia — the short one, for the house. 4/4, D major, 92 bpm, 20 bars
// (80 beats ≈ 52 s). neo spells S-O-P-H-I-A a letter a beat, frisbee spells
// it back and then sings the name whole at one beat a syllable, which is the
// only timing Zoe has ever said it clean at. Underneath, blueberry hums and
// names three things in the room: the moon, the lid, a cup of coffee —
// spelled by frisbee first, because Zoe is the speller of the house.
//
// Letter rules are in bin/compose-spell-frisbee.mjs; the ones that decided
// this score (measured, hear/spell-probe*):
//   · M O O N and L I D are 0 % on Zoe and 25–33 % on Noelle — the doubled
//     O and the I go to frisbee, not neo.
//   · C O F F E E loses its last letter every way it was tried EXCEPT with a
//     breath before the final E — a 0.75-beat rest and then a full beat on
//     the E. That rest is in the notes here and it is load-bearing.
//   · "Sophia" sung whole comes back from Whisper as "Sofia" roughly half
//     the time. Same name, Whisper's spelling; the lyric keeps the house
//     spelling and that line's WER is reported, not chased.
//   · The low voice is fine with "the lid" — "the lid is down" is 0 % on the
//     Aaron/Tom register. The old warning was about "sunlight on your lid".
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
const BPM = 92, BAR = 4, BARS = 20, TOTAL = BAR * BARS;   // 80 beats ≈ 52.2 s
const parts = [[], [], []];

const words = new Set([
  'A', 'C', 'D', 'E', 'F', 'H', 'I', 'L', 'M', 'N', 'O', 'P', 'S',
  'this', 'that', 'is', 'how', 'you', 'spell', 'it', 'now', 'can', 'her',
  'So-phi-a', 'the', 'moon', 'lid', 'cof-fee', 'a', 'cup', 'of', 'up', 'down',
  'name', 'whole', 'word', 'we', 'say', 'them', 'all', 'hmm', 'ooh', 'doo',
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
/// A letter run with a breath before the LAST letter, and a longer note on
/// it. Without both, C O F F E E comes back as C-O-F-F-E every time.
function breathe(pitches, d, gap = .75, tail = 1) {
  const notes = ev(pitches, d);
  notes[pitches.length - 1] = [pitches[pitches.length - 1], tail];
  notes.splice(pitches.length - 1, 0, ['r', gap]);
  return notes;
}

// ── neo · spells it, twice, and says the two plain sentences.
add(0,  0, [[59, 4]], 'hmm', .28, 'hum');
add(0,  4, ev([59, 60, 59, 57, 59, 60], 1), 'S O P H I A', .56, 'spell');
add(0, 16, ev([59, 59, 60, 59, 57, 59], 1), 'this is how you spell it', .56);
add(0, 24, [[59, 4], [57, 4]], 'hmm hmm', .28, 'hum');
add(0, 34, ev([59, 60, 59, 61, 59], 1), 'now you can spell it', .54);
add(0, 46, [[59, 4], [57, 4]], 'hmm hmm', .28, 'hum');
add(0, 64, ev([59, 60, 62, 60, 59, 57], 1), 'that is how you spell it', .56);
add(0, 72, ev([59, 60, 59, 57, 59, 60], 1), 'S O P H I A', .58, 'spell');
add(0, 78, [[59, 2]], 'hmm', .26, 'hum');

// ── frisbee · spells everything. Zoe is the only voice here that gets
// M O O N and L I D back whole, and the only one that can say the name.
add(2,  0, [[55, 4]], 'ooh', .26, 'hum');
add(2, 10, ev([55, 56, 55, 54, 55, 56], 1), 'S O P H I A', .56, 'spell');
add(2, 22, ev([55, 56, 55], 1), 'So-phi-a', .58);
add(2, 28, ev([55, 56, 55, 54], .75), 'M O O N', .56, 'spell');
add(2, 32, ev([56, 55], 1), 'the moon', .54);
add(2, 40, ev([55, 56, 55], .75), 'L I D', .56, 'spell');
add(2, 44, ev([56, 55], 1), 'the lid', .54);
add(2, 52, breathe([55, 56, 55, 54, 55, 56], .75), 'C O F F E E', .56, 'spell');
add(2, 60, ev([56, 55], 1), 'cof-fee', .54);
add(2, 66, ev([55, 56, 55, 54, 55, 56], 1), 'that is how you spell it', .56);
add(2, 76, ev([55, 56, 55], 1), 'So-phi-a', .58);
add(2, 79, [[55, 1]], 'ooh', .26, 'hum');

// ── blueberry · under everything, and the one that says the plain thing
// each spelled word is for. Every word line here measured 0 % on the low
// register today, including "the lid is down".
add(1,  0, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1,  8, [[48, 4], [50, 4]], 'hmm hmm', .34, 'hum');
add(1, 16, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1, 24, [[50, 4], [48, 4]], 'hmm hmm', .34, 'hum');
add(1, 34, ev([47, 48, 50, 48], 1), 'the moon is up', .52);
add(1, 38, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1, 46, ev([48, 50, 48, 47], 1), 'the lid is down', .52);
add(1, 50, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1, 62, ev([47, 48, 50, 48, 47], 1), 'a cup of cof-fee', .52);
add(1, 68, [[48, 4], [47, 4]], 'hmm hmm', .34, 'hum');
add(1, 76, [[48, 4]], 'hmm', .32, 'hum');

for (const lines of parts) lines.sort((a, b) => a.at - b.at);

for (const [i, lines] of parts.entries()) for (const line of lines) {
  const n = line.notes.filter(([note]) => note !== 'r').length;
  const pattern = line.kind === 'spell' ? [1, .82, .9, .78] : i === 1 ? [1, .7, .88, .66] : [1, .76, .92, .72, .88, .7];
  line.accents = Array.from({length: n}, (_, j) => (n === 1 ? 1 : Math.max(.62, pattern[j % pattern.length])));
  line.gain = Math.max(.26, +line.gain.toFixed(3));
}

// A kitchen at night: a longer room than the spelling song, echo thrown off
// the name itself so "Sophia" is the thing left hanging in it.
function performanceKeys(lines, i) {
  const ROOM = i === 1 ? .4 : .55, THROW = .85;
  const keys = [{beat: 0, space: ROOM, pitch: 0}];
  for (const line of lines) {
    if (!/^So-phi-a$/.test(line.text)) continue;
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
    performance: {expression: .6, keys: performanceKeys(lines, i)}};
});

const score = {title: 'The MacNeoPolitan Trio — Spell Sophia',
  composer: 'The machines, arr. compose-spell-sophia.mjs', bpm: BPM, machines: 3, lead: 0,
  description: 'A short spelling song for the house. 4/4, D major, 92 bpm, 80 beats. neo spells S-O-P-H-I-A a letter a beat, frisbee spells it back and sings the name whole at one beat a syllable, the only timing Zoe has ever said it clean at. Then three things in the room get spelled and named: M-O-O-N and the moon, L-I-D and the lid, C-O-F-F-E-E and a cup of coffee. blueberry hums under the whole thing and says the plain sentence each spelling is for.',
  arrangement: {total: TOTAL, meter: '4/4', sections: [
    {beat: 0, name: 'Hum'}, {beat: 4, name: 'neo spells S O P H I A'},
    {beat: 10, name: 'frisbee spells it back'}, {beat: 16, name: 'neo: this is how you spell it'},
    {beat: 22, name: 'frisbee: Sophia'}, {beat: 28, name: 'M O O N · the moon is up'},
    {beat: 40, name: 'L I D · the lid is down'}, {beat: 52, name: 'C O F F E E · a cup of coffee'},
    {beat: 64, name: 'neo: that is how you spell it'}, {beat: 72, name: 'S O P H I A, both at once'},
    {beat: 76, name: 'frisbee: Sophia'}]}, voices};

writeFileSync(new URL('../scores/trio-spell-sophia.mbscore', import.meta.url), JSON.stringify(score, null, 2) + '\n');
console.log(`Spell Sophia: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL * 60 / BPM).toFixed(1)} s; ${parts.map((p, i) => `${members[i]} ${p.length} lines`).join(', ')}.`);
for (const [i, p] of parts.entries()) {
  const b = checkBand(members[i], p.flatMap((l) => l.notes));
  console.log(`  ${members[i].padEnd(10)} ${b.lo}–${b.hi} mean ${b.mean} (speaks ${b.median}, band ${b.band.join('–')}) · ${p.filter((l) => l.kind === 'spell').length} spelled lines`);
}
