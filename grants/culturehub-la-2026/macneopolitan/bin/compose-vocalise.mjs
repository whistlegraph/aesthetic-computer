#!/usr/bin/env node
// Vocalise — a wordless study for the three machines, built entirely out of
// bin/vocalisms.mjs. 90 bpm, 4/4, 28 bars (112 beats ≈ 74.7 s), D major.
//
// The argument: a voice that can only lock one pitch per note can still
// ornament, as long as every ornament is spelled out as notes. So the trills
// are written, the slides are staircases, and the vibrato is left alone.
//
// Who does what: frisbee trills and bips on top, neo turns and pulses in the
// middle, Aaron walks a doom bass underneath with a mouth-percussion tick
// from whichever voice is free. At bar 20 all three trill the same D major
// chord at once — Aaron on D3 biting down, frisbee on F#3 and neo on A3
// biting up — which is the loudest thing in the piece and the only moment
// the ornament IS the harmony.
//
// Registers are the measured ones (see BANDS in vocalisms.mjs): every line's
// mean sits within 2 semitones of its member's speaking pitch and every note
// inside the band, asserted below. frisbee's Zoe speaks a fourth BELOW neo's
// Noelle, so "on top" here is tessitura and speed, not absolute pitch.
//
//   node bin/compose-vocalise.mjs
//   node bin/hear.mjs scores/trio-vocalise.mbscore --tag vocalise \
//        --keep ~/Shelf/macneopolitan-songs/vocalisms/ --no-spoken
import { writeFileSync } from 'node:fs';
import assert from 'node:assert/strict';
import { trill, mordent, turn, shake, roll, gliss, pulse, hold, rest, line, BANDS, checkBand } from './vocalisms.mjs';

const members = ['neo', 'blueberry', 'frisbee'];
const cast = ['Noelle (Enhanced)', 'Aaron (Enhanced)', 'Zoe (Premium)'];
const BPM = 90, BAR = 4, BARS = 28, TOTAL = BAR * BARS;   // 112 beats ≈ 74.7 s
const TICK = 'dm';            // the one mouth-percussion token already proven to render
const parts = [[], [], []];
function add(i, at, fig, gain, role = 'phoneme') {
  assert.equal(fig.tokens.length, fig.notes.filter(([n]) => n !== 'r').length, `${members[i]} @${at}: token/note mismatch`);
  const b = checkBand(members[i], fig.notes);
  assert(b.ok, `${members[i]} @${at}: mean ${b.mean} / notes ${b.lo}-${b.hi} leaves ${b.median}±2 in band ${b.band}`);
  parts[i].push({ at, notes: fig.notes, text: fig.tokens.join(' '), gain, role, mean: b.mean });
}

// ---- blueberry: the doom bass ---------------------------------------------
// One bar = root, root, its fifth, root. D's fifth would leave the top of
// Aaron's band, so the D bar takes the fifth BELOW instead.
const bassBar = (root) => line(pulse(root, 2, 'doom', { step: 1 }),
  roll([root + 7 <= BANDS.blueberry.hi ? root + 7 : root - 5, root], 2, { tok: 'bom' }));
const cycle = () => line(bassBar(50), bassBar(47), bassBar(43), bassBar(45));   // D Bm G A

add(1, 0, line(bassBar(50), bassBar(50), bassBar(50), bassBar(50)), .52, 'bass');
add(1, 16, cycle(), .58, 'bass');
add(1, 32, cycle(), .58, 'bass');
add(1, 48, line(bassBar(50), bassBar(47), bassBar(43)), .56, 'bass');
add(1, 60, line(bassBar(45), bassBar(50)), .56, 'bass');
add(1, 68, line(bassBar(50), bassBar(50), gliss(50, 45, 4, 6, { tok: 'dum' })), .56, 'bass');
add(1, 80, shake(50, 8, { interval: -2, tok: 'doom' }), .60, 'bass');            // the shared trill
add(1, 88, line(hold(50, 2, 'doom'), hold(48, 1, 'bom'), hold(50, 1, 'doom'),
  pulse(50, 2, 'dum', { step: 1 }), hold(45, 2, 'doo')), .54, 'bass');
add(1, 96, line(pulse(50, 4, 'doom', { step: 1 }), roll([45, 50], 2, { tok: 'bom' }), hold(50, 2, 'hmm')), .50, 'bass');
add(1, 104, hold(50, 8, 'hmm'), .42, 'hum');

// ---- neo: turns and pulses in the middle ----------------------------------
add(0, 12, pulse(59, 2, TICK, { step: .5 }), .34, 'tick');                        // the free voice ticks
add(0, 18, line(turn(57, { beats: 2, tok: 'dah' }), turn(59, { beats: 2, tok: 'dee' })), .48);
add(0, 24, line(pulse(59, 2, 'nee', { step: .5 }), turn(61, { beats: 2, tok: 'dah' })), .48);
add(0, 30, line(mordent(59, 1, { beats: 1, tok: 'dee' }), turn(62, { beats: 1, tok: 'dah' })), .46);
add(0, 34, line(mordent(62, -1, { beats: 1, tok: 'dee' }), turn(62, { beats: 1.5, tok: 'dah' }),
  pulse(59, 1.5, 'lee', { step: .5 })), .50);
add(0, 40, line(pulse(59, 2, 'dee', { step: .25 }), turn(61, { beats: 2, tok: 'dah' })), .50);
add(0, 48, pulse(59, 8, 'nee', { step: .5 }), .46, 'pulse');                      // the Reich bed
add(0, 58, line(pulse(62, 3, 'dee', { step: .25 }), turn(59, { beats: 3, tok: 'dah' })), .50, 'pulse');
add(0, 66, line(shake(59, 2, { tok: 'tee' }), pulse(57, 2, 'lee', { step: .5 }), turn(59, { beats: 2, tok: 'dah' })), .50);
add(0, 74, line(gliss(57, 64, 2, 5, { tok: 'loo' }), pulse(62, 2, 'dee', { step: .5 }), turn(59, { beats: 2, tok: 'dah' })), .52);
add(0, 80, shake(57, 8, { interval: 2, tok: 'dah' }), .58);                       // the shared trill
add(0, 88, line(turn(59, { beats: 2, tok: 'dah' }), shake(57, 2, { tok: 'lee' }), hold(57, 4, 'ah')), .48);
add(0, 96, line(pulse(59, 4, 'nee', { step: .5 }), gliss(59, 62, 2, 4, { tok: 'ooh' }), hold(62, 2, 'ah')), .44);
add(0, 104, line(hold(59, 2, 'ah'), hold(62, 6, 'hmm')), .34, 'hum');

// ---- frisbee: trills and bips above ---------------------------------------
add(2, 10, line(pulse(57, 1, 'bip', { step: .5 }), rest(.5), pulse(54, .5, 'bip', { step: .5 }), rest(1),
  trill(57, 2, 2, .5, { tok: 'lee' })), .44);
// a bip is a stop, a vowel and a stop: under half a beat its pitch never
// gets out (measured, VOCALISMS.md), so every bip here is at least 0.5.
add(2, 16, line(trill(54, 2, 2, .5, { tok: 'lee' }), hold(54, .5, 'la'), rest(.5), pulse(57, .5, 'bip', { step: .5 }), rest(.5)), .46);
add(2, 21, line(trill(57, 2, 1, .25, { tok: 'lee' }), hold(57, .5, 'la'), rest(.5), roll([59, 57, 54], 1, { tok: 'loo' })), .48);
add(2, 26, line(trill(55, 2, 2, .25, { tok: 'lee' }), roll([57, 55, 52], 1, { tok: 'loo' }), hold(52, 1, 'poo')), .48);
add(2, 32, line(pulse(57, 1, 'bip', { step: .5 }), rest(.5), trill(54, 2, 2, .5, { tok: 'lee' }), hold(54, .5, 'la')), .46);
add(2, 37, line(shake(57, 2, { tok: 'loo' }), gliss(59, 54, 1, 3, { tok: 'poo' })), .48);
add(2, 42, line(trill(55, 2, 2, .25, { tok: 'lee' }), pulse(52, 1, 'bip', { step: .5 }), roll([54, 57, 59], 1, { tok: 'loo' })), .50);
add(2, 50, roll([52, 54, 57, 59], 2, { tok: 'loo' }), .46);
add(2, 53, line(pulse(57, 1, TICK, { step: .5 }), rest(.5), roll([59, 57, 54, 52], 1.5, { tok: 'la' })), .42, 'tick');
add(2, 57, line(trill(57, 2, 1.5, .25, { tok: 'lee' }), roll([55, 52], 1, { tok: 'loo' }), hold(52, .5, 'poo')), .48);
add(2, 62, line(pulse(54, 2, 'bip', { step: .5 }), shake(57, 2, { tok: 'loo' })), .48);
add(2, 68, line(gliss(52, 59, 2, 8, { tok: 'ooh' }), trill(57, 2, 2, .25, { tok: 'lee' })), .50);
add(2, 74, line(pulse(57, 2, 'bip', { step: .5 }), roll([59, 57, 54, 52], 2, { tok: 'loo' }),
  trill(54, 2, 2, .5, { tok: 'lee' })), .52);
add(2, 80, shake(54, 8, { interval: 2, tok: 'loo' }), .56);                       // the shared trill
add(2, 88, line(gliss(54, 59, 2, 5, { tok: 'ooh' }), shake(57, 2, { tok: 'loo' }), hold(57, 2, 'la'), hold(54, 2, 'ah')), .48);
add(2, 96, line(pulse(54, 2, 'bip', { step: .5 }), trill(54, 2, 2, .5, { tok: 'lee' }),
  gliss(54, 57, 2, 4, { tok: 'ooh' }), hold(57, 2, 'ah')), .44);
add(2, 104, hold(54, 8, 'hmm'), .32, 'hum');

// ---- accents: the first note of a figure leans, the rest fall away --------
for (const [i, lines] of parts.entries()) for (const l of lines) {
  const n = l.notes.filter(([note]) => note !== 'r').length;
  const pattern = i === 1 ? [1, .78, .88, .74] : [1, .76, .9, .72, .86, .8];
  l.accents = Array.from({ length: n }, (_, j) => (n === 1 ? 1 : Math.max(.6, pattern[j % pattern.length])));
}

// ---- room and echo: the same slide the lullabies use ----------------------
function performanceKeys(lines, i) {
  const ROOM = i === 1 ? .55 : .75, THROW = .8;
  if (i === 1) return [{ beat: 0, space: ROOM, pitch: 0 }, { beat: TOTAL + 6, space: ROOM, pitch: 0 }];
  const keys = [{ beat: 0, space: ROOM, pitch: 0 }];
  for (const l of lines) {
    let lastOn = l.at, t = l.at;
    for (const [n, d] of l.notes) { if (n !== 'r') lastOn = t; t += d; }
    keys.push({ beat: l.at - .4, space: ROOM, pitch: 0, echo: 0 }, { beat: l.at, space: ROOM, pitch: 0 },
      { beat: lastOn + .2, space: ROOM, pitch: 0 }, { beat: lastOn + .6, space: 0, pitch: 0, echo: THROW },
      { beat: t - .8, space: 0, pitch: 0, echo: THROW });
  }
  keys.push({ beat: TOTAL + 8, space: 0, pitch: 0, echo: THROW }, { beat: TOTAL + 12, space: 0, pitch: 0, echo: 0 });
  return keys.sort((a, b) => a.beat - b.beat);
}

// ---- assemble -------------------------------------------------------------
const voices = parts.map((lines, i) => {
  const notes = []; let cursor = 0;
  for (const l of lines) {
    assert(l.at >= cursor - 1e-6, `${members[i]}: one mouth, one part — overlap at ${l.at}`);
    if (l.at > cursor + 1e-6) notes.push(['r', +(l.at - cursor).toFixed(6)]);
    notes.push(...l.notes);
    cursor = l.at + l.notes.reduce((s, [, d]) => s + d, 0);
    assert(l.notes.filter(([n]) => n !== 'r').length <= 40, `${members[i]} @${l.at}: line too long for one caption`);
  }
  assert(cursor <= TOTAL + 1e-6, `${members[i]}: runs long (${cursor})`);
  if (cursor < TOTAL) notes.push(['r', +(TOTAL - cursor).toFixed(6)]);
  return {
    name: `${members[i]} sings (${cast[i]})`, program: 78, velocity: 72,
    notes: notes.map(([n, d]) => `${n}:${+d.toFixed(6)}`).join(','),
    lyrics: lines.map((l) => l.text).join(' / '),
    lineRoles: lines.map((l) => l.role), lineGains: lines.map((l) => +l.gain.toFixed(3)),
    noteGains: lines.map((l) => l.accents),
    singVoice: cast[i], sayVoice: cast[i], singLock: 1, singVibCents: 8, singVibratoHz: [5, 3.5, 5.5][i],
    singF0Floor: i === 1 ? 70 : 80, double: false, faceAlpha: 0.95,
    performance: { expression: .55, keys: performanceKeys(lines, i) },
  };
});

const score = {
  title: 'The MacNeoPolitan Trio — Vocalise', composer: 'The machines, arr. compose-vocalise.mjs',
  bpm: BPM, machines: 3, lead: 0, phonemeOnly: true,
  description: 'A wordless study in ornaments. frisbee trills and bips on top, neo turns and pulses in the middle, Aaron walks a doom bass with a mouth-percussion tick from whichever voice is free; at bar 20 all three trill one D major chord together. Every ornament is spelled out as notes because the singer locks one pitch per note. No words, no instruments, no spoken material.',
  arrangement: {
    total: TOTAL, meter: '4/4',
    sections: [{ beat: 0, name: 'Ground: doom bass, a tick, first bips' },
      { beat: 16, name: 'Ornaments: trills above, turns in the middle' },
      { beat: 48, name: 'Pulse: a Reich bed and rolls over it' },
      { beat: 80, name: 'All three trill one chord' },
      { beat: 96, name: 'Settle' }],
  },
  voices,
};
writeFileSync(new URL('../scores/trio-vocalise.mbscore', import.meta.url), JSON.stringify(score, null, 2) + '\n');
const dur = (TOTAL * 60 / BPM).toFixed(1);
console.log(`Vocalise: ${TOTAL} beats at ${BPM} bpm ≈ ${dur} s.`);
for (const [i, lines] of parts.entries()) {
  const n = lines.reduce((s, l) => s + l.notes.filter(([x]) => x !== 'r').length, 0);
  const b = checkBand(members[i], lines.flatMap((l) => l.notes));
  console.log(`  ${members[i].padEnd(10)} ${String(lines.length).padStart(2)} lines, ${String(n).padStart(3)} notes, `
    + `mean ${b.mean} (speaks ${b.median}), ${b.lo}–${b.hi} in band ${b.band.join('–')}`);
}
