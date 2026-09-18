#!/usr/bin/env node
// One ascending sine arpeggio. Each note arrives at the next physical seat;
// pitch rhythm and circular speed share the same accelerating transport.
import { mkdir, writeFile } from 'node:fs/promises';
const notes = [60, 64, 67, 72, 76, 79, 84, 88];
const events = [], doors = [0];
let time = 0;
for (let i = 0; i < 120; i++) {
  // Three smooth breaths: 1.05 sec/note -> .23 -> 1.05.
  const speed = .5 - .5 * Math.cos(2 * Math.PI * i / 40);
  const step = 1.05 - .82 * speed;
  events.push({ t: time, dur: step * .92, hz: 440 * 2 ** ((notes[i % notes.length] - 69) / 12),
    g: .95, wave: 'sine', attack: .008, decay: Math.min(.06, step * .13) });
  time += step; doors.push(time);
}
const dur = time + .4, fieldShift = [];
let cursor = 0;
// The one-lane score has zero default spin. An unwrapped fieldShift ribbon
// describes the exact continuous angle, sampled at 100 Hz for interpolation.
const points = Math.ceil(dur * 100);
for (let i = 0; i <= points; i++) {
  const t = i / points * dur;
  while (cursor + 1 < doors.length && t >= doors[cursor + 1]) cursor++;
  const part = cursor + 1 < doors.length ? (t - doors[cursor]) / (doors[cursor + 1] - doors[cursor]) : 0;
  fieldShift.push(+((cursor + part) * 2 / 5).toFixed(7));
}
const score = { name: 'Sine Ring', dur, gain: .65,
  movements: [{ name: 'Breathe around the ring', t0: 0, t1: dur }], fieldShift,
  lanes: [{ name: 'sine', color: [255, 209, 80], events }] };
const directory = new URL('../scores/', import.meta.url);
await mkdir(directory, { recursive: true });
await writeFile(new URL('sine-ring.nsscore', directory), JSON.stringify(score) + '\n');
console.log(`Sine Ring: ${dur.toFixed(1)} sec, 120 notes, 24 circuits, three accelerations/decelerations`);
