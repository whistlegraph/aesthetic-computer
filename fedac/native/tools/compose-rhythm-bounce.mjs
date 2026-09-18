#!/usr/bin/env node
import { writeFile } from 'node:fs/promises';

// A two-speaker call and response: hold each beat on its laptop, then cross.
const bpm = 112, beat = 60 / bpm, bars = 32, dur = bars * 4 * beat;
const events = [], linePosition = [], points = Math.ceil(dur * 200);
const phrases = [
  [72, 76, 79, 76, 74, 76, 79, 84],
  [69, 72, 76, 79, 76, 72, 71, 69],
  [65, 69, 72, 77, 76, 72, 69, 72],
  [67, 71, 74, 79, 77, 74, 71, 74],
];
const hz = midi => 440 * 2 ** ((midi - 69) / 12);
function sound(t, length, frequency, g, wave = 'sine', decay = .035) {
  events.push({ t, dur: length, hz: frequency, g, wave, attack: .003, decay });
}
for (let b = 0; b < bars * 4; b++) {
  const t = b * beat, phrase = phrases[Math.floor(b / 8) % phrases.length];
  const pitch = phrase[b % 8] + (b >= 64 && b < 96 ? 12 : 0);
  sound(t, beat * .62, hz(pitch), .56);
  if (b % 2 === 0) {
    sound(t, .045, 155, .24);
    sound(t + .03, .13, 85, .27, 'triangle', .09);
  } else {
    sound(t, .085, 2300, .16, 'noise', .065);
    sound(t, .09, 190, .18, 'triangle', .07);
  }
  sound(t, .022, 7000, .055, 'noise', .017);
  if (b % 4 === 3) {
    sound(t + beat * .5, beat * .18, hz(pitch + 7), .32);
    sound(t + beat * .5, .035, 6500, .08, 'noise', .025);
  }
}
for (let i = 0; i <= points; i++) {
  const b = i / points * dur / beat, n = Math.floor(b), f = b - n;
  const cross = Math.max(0, Math.min(1, (f - .74) / .26));
  linePosition.push(n % 2 === 0 ? cross : 1 - cross);
}
events.sort((a, b) => a.t - b.t);
const score = { name: 'Rhythm Bounce', bpm, geometry: 'line', motion: 'bounce',
  seatOrder: [0, 1], dur, gain: .65, linePosition,
  lanes: [{ name: 'melody + drums', color: [255, 195, 90], events }] };
await writeFile(new URL('../scores/rhythm-bounce.nsscore', import.meta.url), JSON.stringify(score) + '\n');
console.log(`Rhythm Bounce: ${bpm} BPM, ${dur.toFixed(2)} seconds, ${bars * 4} alternating beats`);
