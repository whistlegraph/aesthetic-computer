#!/usr/bin/env node
import { writeFile } from 'node:fs/promises';

// Three melodic pulses occupy the same time as two drum pulses.
const bpm = 112, cycle = 120 / bpm, cycles = 64, dur = cycle * cycles;
const lanes = [
  { name: '3 melody', color: [255, 205, 85], events: [], linePosition: [] },
  { name: '2 drums', color: [100, 210, 255], events: [], linePosition: [] },
];
const chords = [[72,76,79], [69,72,76], [65,69,72], [67,71,74]];
const hz = n => 440 * 2 ** ((n - 69) / 12);
function event(lane, t, length, frequency, g, wave = 'sine', decay = .04) {
  lanes[lane].events.push({t, dur:length, hz:frequency, g, wave, attack:.003, decay});
}
for (let c = 0; c < cycles; c++) {
  const chord = chords[Math.floor(c / 4) % chords.length];
  for (let j = 0; j < 3; j++) {
    const t = c * cycle + j * cycle / 3;
    event(0, t, cycle / 3 * .62, hz(chord[j] + (c >= 32 && c < 48 ? 12 : 0)), .58);
  }
  for (let j = 0; j < 2; j++) {
    const t = c * cycle + j * cycle / 2;
    if (j === 0) {
      event(1, t, .045, 160, .22);
      event(1, t + .025, .14, 95, .3, 'triangle', .10);
    } else {
      event(1, t, .085, 2500, .15, 'noise', .065);
      event(1, t, .095, 190, .21, 'triangle', .07);
    }
    event(1, t, .024, 6500, .065, 'noise', .017);
  }
}
const points = Math.ceil(dur * 200);
for (let i = 0; i <= points; i++) {
  const t = i / points * dur;
  for (let lane = 0; lane < 2; lane++) {
    const pulse = t / cycle * (lane === 0 ? 3 : 2);
    const n = Math.floor(pulse), f = pulse - n;
    const cross = Math.max(0, Math.min(1, (f - .74) / .26));
    const side = (n + (lane === 0 ? 1 : 0)) % 2;
    lanes[lane].linePosition.push(side ? 1 - cross : cross);
  }
}
for (const lane of lanes) lane.events.sort((a,b)=>a.t-b.t);
const score = {name:'3 against 2', bpm, geometry:'line', motion:'bounce',
  seatOrder:[0,1], dur, gain:.65, lanes};
await writeFile(new URL('../scores/polyrhythm-bounce.nsscore', import.meta.url), JSON.stringify(score)+'\n');
console.log(`3 against 2: ${bpm} BPM, ${dur.toFixed(2)} seconds, ${cycles} cycles`);
