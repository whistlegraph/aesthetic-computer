#!/usr/bin/env node
// Inspect a ring .nsscore the way the seats will hear it: per window, the
// routed power landing on each laptop (through the runtime's own math),
// then a full dry run of the spatial-rehearsal piece against a mock API.
import { readFileSync } from 'node:fs';
import { voicePosition, sourceGain } from '../lib/spatial-rehearsal.mjs';

const file = process.argv[2] || new URL('../scores/notespatial-native.nsscore', import.meta.url);
const score = JSON.parse(readFileSync(file, 'utf8'));
const seats = score.seats || 6, WIN = 20;
for (const l of score.lanes) if (!(Number.isFinite(l.az) || l.orbitSeconds > 0)) { console.error(`${l.name}: neither pinned nor orbiting`); process.exit(1); }
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const shade = p => p <= 0 ? '·' : p < .5 ? '░' : p < 2 ? '▒' : p < 6 ? '▓' : '█';

console.log(`${score.name} — ${mmss(score.dur)} — routed power per laptop, ${WIN}s windows (· none  ░ faint  ▒ some  ▓ busy  █ loud)`);
console.log('time    ' + Array.from({ length: seats }, (_, k) => ` ${k + 1}`).join('') + '   section');
for (let w0 = 0; w0 < score.dur; w0 += WIN) {
  const power = new Array(seats).fill(0);
  score.lanes.forEach((l, i) => l.events.forEach(e => {
    if (e.t < w0 || e.t >= w0 + WIN) return;
    const pos = voicePosition(score, i, e.t);
    for (let k = 0; k < seats; k++) power[k] += (e.g * sourceGain(score, pos, k, seats)) ** 2 * e.dur;
  }));
  const mv = (score.movements || []).find(m => w0 >= m.t0 && w0 < m.t1);
  console.log(`${mmss(w0).padStart(6)}  ${power.map(p => ' ' + shade(p * 10)).join('')}   ${mv?.name || ''}`);
}

// The walk, note by note: which laptop holds each of the first hops.
const hops = score.lanes.flatMap((l, i) => l.events.map(e => ({ t: e.t, seat: 1 + Array.from({ length: seats }, (_, k) => k)
  .reduce((b, k) => sourceGain(score, voicePosition(score, i, e.t), k, seats) > sourceGain(score, voicePosition(score, i, e.t), b, seats) ? k : b, 0) })))
  .sort((a, b) => a.t - b.t);
const walkStart = hops.findIndex((h, i) => i > 0 && h.seat !== hops[i - 1].seat);
console.log('\nfirst hops of the walk (laptop numbers): ' + hops.slice(walkStart, walkStart + 20).map(h => h.seat).join(' '));
console.log('last twelve notes: ' + hops.slice(-12).map(h => `${mmss(h.t)}@${h.seat}`).join(' '));

// Dry run: drive pieces/spatial-rehearsal.mjs for one seat at 40 Hz.
const piece = await import('../pieces/spatial-rehearsal.mjs?check');
let cmd = null, synths = 0, updates = 0, killed = 0, statuses = 0, maxLive = 0, live = new Set();
const system = {
  startSSH() {},
  readFile(p) {
    if (p.endsWith('config.json')) return JSON.stringify({ seat: 2, seats, maxSeconds: score.dur });
    if (p.endsWith('presence.json')) return JSON.stringify({ seats: Array.from({ length: seats }, (_, i) => ({ seat: i, state: 'online' })) });
    return JSON.stringify(cmd);
  },
  readFileBytes(p) { return p.endsWith('.nsscore') ? new TextEncoder().encode(JSON.stringify(score)).buffer : null; },
  writeFile(p) { if (p.endsWith('status.json')) statuses++; },
  battery: { percent: 80, charging: true },
};
let id = 0;
const sound = {
  time: 0, microphone: { hot: false, recording: false, close() {} },
  speaker: { amplitudes: { left: .1, right: .1 } },
  synth() { synths++; const v = { id: id++, update() { updates++; }, kill() { killed++; live.delete(v.id); } }; live.add(v.id); return v; },
};
const api = { sound, system, screen: { width: 455, height: 256 }, wifi: { ip: '0.0.0.0' } };
piece.boot(api);
cmd = { id: 'prepare', action: 'prepare', startAt: 3, mode: 'score' }; sound.time = .1; piece.sim(api);
cmd = { id: 'play', action: 'play' }; sound.time = .2; piece.sim(api);
const t1 = performance.now();
for (let t = 3; t < 3 + score.dur + 1; t += .025) { sound.time = t; piece.sim(api); maxLive = Math.max(maxLive, live.size); }
const ms = performance.now() - t1;
const total = score.lanes.reduce((a, l) => a + l.events.length, 0);
console.log(`\ndry run seat 3: ${synths}/${total} events voiced, ${updates} gain updates, ${statuses} status writes, sim cost ${(ms / (score.dur * 40)).toFixed(3)} ms/frame on this Mac`);
if (synths !== total) { console.error('some events were skipped'); process.exit(1); }
