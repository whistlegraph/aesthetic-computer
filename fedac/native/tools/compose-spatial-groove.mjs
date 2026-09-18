#!/usr/bin/env node
// Original D-major theme and drum choreography for the CultureHub rehearsal.
import { mkdir, writeFile } from 'node:fs/promises';
const beat = 60 / 96, bars = 32;
const lanes = [
  { name: 'melody', color: [255, 194, 72], orbitSeconds: 12.5 },
  { name: 'answer', color: [116, 210, 235], orbitSeconds: 15, orbitDirection: -1 },
  { name: 'bass', color: [234, 128, 88], orbitSeconds: 20 },
  { name: 'chords', color: [156, 179, 237], orbitSeconds: 30 },
  { name: 'kick', color: [255, 107, 125], orbitSeconds: 10 },
  { name: 'snare', color: [122, 223, 153], orbitSeconds: 10, orbitDirection: -1 },
  { name: 'hats', color: [230, 233, 241], orbitSeconds: 5 },
].map(l => ({ ...l, events: [] }));
const hz = midi => 440 * 2 ** ((midi - 69) / 12);
function note(lane, b, duration, midi, gain, wave = 'sine', attack = .008, decay = .06) {
  lanes[lane].events.push({ t: +(b * beat).toFixed(5), dur: +(duration * beat).toFixed(5), hz: hz(midi), g: gain, wave, attack, decay });
}
function drum(lane, b, duration, frequency, gain, wave) {
  lanes[lane].events.push({ t: +(b * beat).toFixed(5), dur: duration, hz: frequency, g: gain, wave, attack: .001, decay: duration * .7 });
}
const harmony = [[62, 66, 69], [59, 62, 66], [55, 59, 62], [57, 61, 64]];
const phrases = [
  [[0,1,74],[1,.5,78],[1.5,.5,81],[2,1.5,78],[3.5,.5,76],[4,1,78],[5,1,76],[6,2,74]],
  [[0,1,78],[1,.5,81],[1.5,.5,83],[2,1,81],[3,1,78],[4,1.5,76],[5.5,.5,74],[6,2,71]],
  [[0,1,74],[1,1,79],[2,.5,81],[2.5,.5,83],[3,1,81],[4,1,79],[5,.5,78],[5.5,.5,76],[6,2,74]],
  [[0,1,76],[1,.5,78],[1.5,.5,81],[2,1.5,85],[3.5,.5,81],[4,1,78],[5,1,76],[6,2,74]],
];
for (let bar = 0; bar < bars; bar++) {
  const b = bar * 4, h = harmony[Math.floor(bar / 2) % 4];
  // A recognisable phrase repeats, opens upward in the third section, then returns.
  if (bar % 2 === 0) {
    const phrase = phrases[Math.floor(bar / 2) % 4];
    for (const [at, dur, pitch] of phrase) {
      const lift = bar >= 16 && bar < 24 && at >= 4 ? 12 : 0;
      note(0, b + at, dur * .86, pitch + lift, .8);
      if (bar >= 8 && at >= 4) note(1, b + at + .5, dur * .6, pitch - 12, .3, 'triangle');
    }
  }
  for (const at of [0, 1.5, 2, 3.5]) note(2, b + at, .42, h[0] - 12, .7, 'triangle');
  for (const p of h) note(3, b, 3.7, p, .12, 'sine', .16, .25);
  for (const at of [0, 2, ...(bar % 4 === 3 ? [3.5] : [])]) {
    drum(4, b + at, .055, 150, .9, 'sine');
    drum(4, b + at + .06, .16, 78, .85, 'sine');
    drum(4, b + at, .025, 1800, .15, 'noise');
  }
  for (const at of [1, 3]) {
    drum(5, b + at, .11, 2400, .42, 'noise');
    drum(5, b + at, .095, 185, .5, 'triangle');
  }
  for (let at = 0; at < 4; at += .5)
    drum(6, b + at, at % 1 ? .045 : .027, 7000, at % 1 ? .24 : .15, 'noise');
}
for (const l of lanes) l.events.sort((a, b) => a.t - b.t);
const score = { name: 'Melodic Orbit', bpm: 96, dur: bars * 4 * beat + 1, gain: .6,
  movements: [{ name: 'Theme', t0: 0, t1: 20 }, { name: 'Answer', t0: 20, t1: 40 },
    { name: 'Lift', t0: 40, t1: 60 }, { name: 'Return', t0: 60, t1: 81 }], lanes };
const directory = new URL('../scores/', import.meta.url);
await mkdir(directory, { recursive: true });
await writeFile(new URL('melodic-orbit.nsscore', directory), JSON.stringify(score) + '\n');
console.log(`Melodic Orbit: ${score.dur}s, ${lanes.length} moving voices, ${lanes.reduce((n,l)=>n+l.events.length,0)} events`);
