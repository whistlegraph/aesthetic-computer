#!/usr/bin/env node
import { mkdir, writeFile } from 'node:fs/promises';
// Seat numbers are given left to right. A silent gap returns to the left.
const order = process.argv[2] ? process.argv[2].split(',').map(n => Number(n) - 1) : [0,1,2,3,4,5];
if (order.length < 2 || order.some(n => !Number.isInteger(n) || n < 0 || n > 15) || new Set(order).size !== order.length) throw Error('Use unique seat numbers, e.g. 1,2,3,4,5,6');
const scoreFile = process.argv[3] || 'sine-line';
if (!/^[a-z0-9-]+$/.test(scoreFile)) throw Error('Invalid score filename');
const events = [], passes = [];
let time = 0;
for (let pass = 0; pass < 24; pass++) {
  const step = .95 - .7 * (.5 - .5 * Math.cos(2 * Math.PI * pass / 12));
  passes.push({ at: time, step });
  for (let i = 0; i < order.length; i++) events.push({ t: time + i * step, dur: step * .8,
    hz: 440 * 2 ** (([60,64,67,72,76,79,84,88][i % 8] - 69) / 12), g: .95, wave: 'sine', attack: .008, decay: .04 });
  time += step * (order.length + .8);
}
const linePosition = [], dur = time, points = Math.ceil(time * 100);
let pass = 0;
for (let i = 0; i <= points; i++) {
  const t = i / points * dur;
  while (pass + 1 < passes.length && t >= passes[pass + 1].at) pass++;
  const u = (t - passes[pass].at) / passes[pass].step;
  linePosition.push(u <= order.length - 1 ? Math.max(0,u/(order.length - 1)) : u < order.length ? 1 : Math.max(0,1-(u-order.length)/.8));
}
const score = { name: 'Sine Line', geometry: 'line', seatOrder: order, linePasses: passes, dur, gain: .65, linePosition,
  lanes: [{ name: 'sine', color: [255,210,80], events }] };
const dir = new URL('../scores/', import.meta.url);
await mkdir(dir, { recursive: true });
await writeFile(new URL(scoreFile + '.nsscore', dir), JSON.stringify(score)+'\n');
console.log(`Sine Line: ${dur.toFixed(1)} seconds, 24 left-to-right passes, seats ${order.map(i=>i+1).join(' → ')}`);
